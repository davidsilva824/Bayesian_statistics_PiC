library(brms)
library(ggplot2)
library(cmdstanr)
library(dplyr)
library(stringr)
library(ggeffects)
library(rstan)

setwd("C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/for the thesis/Results")

m <- readRDS("C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/for the thesis/Models/model_bayesean_experiment_1_100M_treatment.rds")

summary(m)

### Main effect graphic: plurality on x-axis, regularity in legend
p <- ggeffects::ggemmeans(m, c("plurality", "regularity"))
p$x <- factor(p$x, levels = c("Singular", "Plural"))

m_preds <- as.data.frame(p)

pd <- position_dodge(width = 0.3)
y_pad <- 0.05 * (max(m_preds$conf.high) - min(m_preds$conf.low))


#######

p <- ggeffects::ggemmeans(m, c("regularity", "plurality"))
p$x <- factor(p$x, levels = c("Regular", "Irregular"))

m_preds <- as.data.frame(p)

y_pad <- 0.05 * (max(m_preds$conf.high) - min(m_preds$conf.low))

p <- ggplot(
  m_preds,
  aes(x = x, y = predicted, colour = group)
) +
  geom_point(size = 3, position = pd) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0,
    linewidth = 0.8,
    position = pd
  ) +
  scale_x_discrete(expand = expansion(add = c(0.5, 1.2))) +
  coord_cartesian(
    ylim = c(min(m_preds$conf.low) - y_pad, max(m_preds$conf.high) + y_pad)
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
    axis.text = element_text(size = 22),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 19),
    legend.position = c(0.60, 0.5),
    legend.justification = c(0, 0.5)
  )


print(p)

# to export to png file 
ggsave(
  "plot_regularity_x_plurality_legend_100M.png",
  plot = p,
  width = 4.5,
  height = 5,
  dpi = 300
)

### Interaction graphic per model

(m_eff_int <- hypothesis(m, "regularityIrregular:pluralitySingular > 0",
                         scope = "coef", group = "model", alpha = 0.025)$hypothesis)

(m_eff_reg <- hypothesis(m, "-pluralitySingular > 0",
                         scope = "coef", group = "model", alpha = 0.025)$hypothesis)

(m_eff_irreg <- hypothesis(m, "-(pluralitySingular + regularityIrregular:pluralitySingular) > 0",
                           scope = "coef", group = "model", alpha = 0.025)$hypothesis)

m_eff_int$effect <- "Regularity × Plurality\n interaction"
m_eff_reg$effect <- "Regular plural\n effect"
m_eff_irreg$effect <- "Irregular plural\n effect"

plot_dat <- bind_rows(m_eff_int, m_eff_reg, m_eff_irreg) %>%
  mutate(
    effect = factor(
      effect,
      levels = c(
        "Regularity × Plurality\n interaction",
        "Regular plural\n effect",
        "Irregular plural\n effect"
      )
    )
  )

interaction_order <- plot_dat %>%
  filter(effect == "Regularity × Plurality\n interaction") %>%
  arrange(Estimate) %>%
  pull(Group)

plot_dat$Group <- factor(plot_dat$Group, levels = interaction_order)

### Save plot_dat into a file
write.csv(plot_dat, "forest_plot_treatment_values_100M.csv", row.names = FALSE)

p <- ggplot(plot_dat, aes(x = Group, y = Estimate)) +
  coord_flip() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = CI.Lower, ymax = CI.Upper), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue") +
  facet_wrap(~ effect, ncol = 3) +
  labs(
    x = NULL,
    y = "Estimate"
  ) +
  theme(
    text = element_text(size = 18, colour = "grey28"),
    strip.text = element_text(size = 18),
    axis.text.y = element_text(size = 17),
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "white", colour = "black")
  )

print(p)

ggsave(
  "forest_plot_treatment_3panels_100M.png",
  plot = p,
  width = 11,
  height = 7,
  dpi = 300
)

hypothesis(m, "pluralitySingular = 0")
hypothesis(m, "pluralitySingular + regularityIrregular:pluralitySingular = 0")
hypothesis(m, "regularityIrregular:pluralitySingular = 0")


#hyp <- hypothesis(...)
#1 / hyp$hypothesis$Evid.Ratio
