setwd("C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/Figures for CLP abstract")




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
m_preds_all$training_size <- factor(m_preds_all$training_size, levels = c("10M", "100M"))

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
  facet_wrap(~ training_size, ncol = 1) +
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
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 22, colour = "black"),
    text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 20,  colour = "black"),
    legend.title = element_text(size = 21),
    legend.text = element_text(size = 19),
    legend.position = c(0.55, 0.965),
    legend.justification = c(0, 0.5),
    panel.grid = element_blank()
  )
print(p_marginal)

ggsave(
  "plot_regularity_x_plurality_10M_100M_panel.png",
  plot = p_marginal,
  width = 4,
  height =11.3,
  dpi = 300
)


