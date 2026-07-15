setwd("C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/for the thesis")


# Load packages

library(brms)
library(ggplot2)
library(patchwork)


# Load Bayesian model

m <- readRDS(
  "C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/results_bayesian_experiment_1_10M_treatment.rds"
)


# Posterior predictive checks

p_density <- pp_check(m)

p_mean <- pp_check(
  m,
  type = "stat",
  stat = "mean"
)

p_sd <- pp_check(
  m,
  type = "stat",
  stat = "sd"
)


# Combine the three graphics into one panel

p_panel <- p_density + p_mean + p_sd +
  plot_layout(ncol = 3)

print(p_panel)


# Save the panel

ggsave(
  filename = "posterior_predictive_checks_panel.png",
  plot = p_panel,
  width = 18,
  height = 6,
  dpi = 300
)