library(brms)
library(ggplot2)
library(patchwork)

setwd("C:/Users/david/Documents/GitHub/Bayesian_statistics_PiC/thesis/extra_statistics")

### ------------------------------------------------------------
### Load fitted model
### ------------------------------------------------------------

m <- readRDS(
  "C:/Users/david/Documents/GitHub/Bayesian_statistics_PiC/thesis/models/model_bayesean_experiment_1_10M_treatment.rds"
)

### Check priors stored in the fitted model
prior_summary(m)

### ------------------------------------------------------------
### Prior-only model
### ------------------------------------------------------------

prior_model_file <- paste0(
  "C:/Users/david/Documents/GitHub/Bayesian_statistics_PiC/thesis/extra_statistics/",
  "prior_only_model_experiment_1_10M.rds"
)

if (file.exists(prior_model_file)) {
  
  m_prior <- readRDS(prior_model_file)
  
} else {
  
  m_prior <- brm(
    formula = m$formula,
    data = m$data,
    family = m$family,
    prior = m$prior,
    sample_prior = "only",
    cores = 4
  )
  
  saveRDS(
    m_prior,
    prior_model_file
  )
}

### ------------------------------------------------------------
### Prior predictive checks
### ------------------------------------------------------------

p_density <- pp_check(
  m_prior,
  prefix = "ppd",
  type = "dens_overlay",
  ndraws = 100
)

p_mean <- pp_check(
  m_prior,
  prefix = "ppd",
  type = "stat",
  stat = "mean",
  ndraws = 100
)

p_sd <- pp_check(
  m_prior,
  prefix = "ppd",
  type = "stat",
  stat = "sd",
  ndraws = 100
)

### ------------------------------------------------------------
### Combine plots
### ------------------------------------------------------------

p_prior_predictive <- p_density / p_mean / p_sd

print(p_prior_predictive)

### ------------------------------------------------------------
### Save figure
### ------------------------------------------------------------

ggsave(
  "prior_predictive_checks_experiment_1.png",
  plot = p_prior_predictive,
  width = 9,
  height = 11,
  dpi = 300
)