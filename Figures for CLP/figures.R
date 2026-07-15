library(brms)
library(ggplot2)
library(cmdstanr)
library(dplyr)
library(stringr)
library(ggeffects)
library(rstan)

# --------------------------------------------------
# Load models
# --------------------------------------------------

m_10M <- readRDS("C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/results_bayesian_experiment_1_10M_treatment.rds")

m_100M <- readRDS("C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/results_bayesian_experiment_1_100M_treatment.rds")


# ==================================================
# FIGURE 1: Marginal means plot, 10M and 100M panels
# ==================================================

get_marginal_predictions <- function(model, training_size) {
  
  p <- ggeffects::ggemmeans(model, c("regularity", "plurality"))
  p$x <- factor(p$x, levels = c("Regular", "Irregular"))
  
  as.data.frame(p) %>%
    mutate(
      training_size = training_size,
      group = factor(group, levels = c("Singular", "Plural"))
    )
}

m_preds_10M <- get_marginal_predictions(m_10M, "10M")
m_preds_100M <- get_marginal_predictions(m_100M, "100M")

m_preds_all <- bind_rows(m_preds_10M, m_preds_100M)

pd <- position_dodge(width = 0.3)

y_pad <- 0.05 * (max(m_preds_all$conf.high) - min(m_preds_all$conf.low))

p_marginal <- ggplot(
  m_preds_all,
  aes(x = x, y = predicted, colour = group)
) +
  geom_point(size = 3, position = pd) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0,
    linewidth = 0.8,
    position = pd
  ) +
  facet_wrap(~ training_size, nrow = 1) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.8))) +
  coord_cartesian(
    ylim = c(
      min(m_preds_all$conf.low) - y_pad,
      max(m_preds_all$conf.high) + y_pad
    )
  ) +
  labs(
    x = NULL,
    y = "Surprisal",
    colour = NULL
  ) +
  theme_light() +
  theme(
    text = element_text(size = 14, colour = "gray28"),
    axis.title = element_text(size = 23),
    axis.text = element_text(size = 20),
    strip.text = element_text(size = 22),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 19),
    legend.position = "bottom"
  )

print(p_marginal)

ggsave(
  "plot_regularity_x_plurality_10M_100M_panel.png",
  plot = p_marginal,
  width = 8,
  height = 5,
  dpi = 300
)


# ==================================================
# FIGURE 2: Combined forest plot, 10M and 100M models
# ==================================================

get_model_effects <- function(model, training_size) {
  
  m_eff_int <- hypothesis(
    model,
    "regularityIrregular:pluralitySingular > 0",
    scope = "coef",
    group = "model",
    alpha = 0.025
  )$hypothesis
  
  m_eff_reg <- hypothesis(
    model,
    "-pluralitySingular > 0",
    scope = "coef",
    group = "model",
    alpha = 0.025
  )$hypothesis
  
  m_eff_irreg <- hypothesis(
    model,
    "-(pluralitySingular + regularityIrregular:pluralitySingular) > 0",
    scope = "coef",
    group = "model",
    alpha = 0.025
  )$hypothesis
  
  m_eff_int$effect <- "Regularity × Plurality\ninteraction"
  m_eff_reg$effect <- "Regular plural\neffect"
  m_eff_irreg$effect <- "Irregular plural\neffect"
  
  bind_rows(m_eff_int, m_eff_reg, m_eff_irreg) %>%
    mutate(
      training_size = training_size,
      model_label = paste0(Group, " — ", training_size)
    )
}

plot_dat_10M <- get_model_effects(m_10M, "10M")
plot_dat_100M <- get_model_effects(m_100M, "100M")

plot_dat <- bind_rows(plot_dat_10M, plot_dat_100M) %>%
  mutate(
    training_size = training_size,
    model_clean = Group %>%
      str_remove_all(regex("(^|[_ -])(10M|100M)(?=$|[_ -])", ignore_case = TRUE)) %>%
      str_replace_all("_{2,}", "_") %>%
      str_replace_all("[- ]{2,}", " ") %>%
      str_replace_all("^[_ -]+|[_ -]+$", ""),
    model_label = paste0(model_clean, " — ", training_size)
  )

# Order models by the interaction estimate, keeping 10M first and 100M second
interaction_order_10M <- plot_dat %>%
  filter(
    effect == "Regularity × Plurality\ninteraction",
    training_size == "10M"
  ) %>%
  arrange(Estimate) %>%
  pull(model_label)

interaction_order_100M <- plot_dat %>%
  filter(
    effect == "Regularity × Plurality\ninteraction",
    training_size == "100M"
  ) %>%
  arrange(Estimate) %>%
  pull(model_label)

model_order <- c(interaction_order_10M, interaction_order_100M)

# Reverse order so that 10M appears at the top of the forest plot
plot_dat$model_label <- factor(plot_dat$model_label, levels = rev(model_order))

separator_position <- length(interaction_order_100M) + 0.5

p_forest <- ggplot(plot_dat, aes(x = model_label, y = Estimate)) +
  coord_flip() +
  geom_point(aes(shape = training_size), size = 2.5) +
  geom_errorbar(
    aes(ymin = CI.Lower, ymax = CI.Upper),
    width = 0
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "blue"
  ) +
  geom_vline(
    xintercept = separator_position,
    linetype = "dotted",
    colour = "grey40"
  ) +
  facet_wrap(~ effect, ncol = 3) +
  labs(
    x = NULL,
    y = "Estimate",
    shape = NULL
  ) +
  theme(
    text = element_text(size = 18, colour = "grey28"),
    strip.text = element_text(size = 18),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 15),
    axis.title = element_text(size = 18),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "white", colour = "black")
  )

print(p_forest)

ggsave(
  "forest_plot_treatment_3panels_10M_100M_combined.png",
  plot = p_forest,
  width = 12,
  height = 9,
  dpi = 300
)