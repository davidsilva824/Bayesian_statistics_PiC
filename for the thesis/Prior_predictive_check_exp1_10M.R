library(brms)
library(ggplot2)
library(patchwork)


setwd("C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/for the thesis")

m <- readRDS(
  "C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/results_bayesian_experiment_1_10M_treatment.rds"
)

m_prior <- brm(
  formula = m$formula,
  data = m$data,
  family = m$family,
  prior = m$prior,
  sample_prior = "only",
  cores = 4
)

p_density <- pp_check(
  m_prior,
  prefix = "ppd",
  type = "dens_overlay",
  ndraws = 100 #meter 250 e com 1000
)

p_mean <- pp_check(
  m_prior,
  prefix = "ppd",
  type = "stat",
  stat = "mean",
  ndraws = 100 #meter 250 e com 1000
)

p_sd <- pp_check(
  m_prior,
  prefix = "ppd",
  type = "stat",
  stat = "sd",
  ndraws = 100 #meter 250 e com 1000
)

p_prior_predictive <- p_density / p_mean / p_sd

print(p_prior_predictive)

ggsave(
  "prior_predictive_checks_experiment_1.png",
  plot = p_prior_predictive,
  width = 9,
  height = 11,
  dpi = 300
)