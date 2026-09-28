library(brms)
library(priorsense)
library(ggplot2)

setwd(
  "C:/Users/david/Documents/GitHub/Bayesian_statistics_PiC/thesis/extra_statistics"
)

# ------------------------------------------------------------
# Load fitted Bayesian model
# ------------------------------------------------------------

m <- readRDS(
  "C:/Users/david/Documents/GitHub/Bayesian_statistics_PiC/thesis/models/model_bayesean_experiment_1_10M_treatment.rds"
)


# ------------------------------------------------------------
# Parameters of interest
# ------------------------------------------------------------

variables <- c(
  "b_Intercept",
  "b_pluralitySingular",
  "b_regularityIrregular",
  "b_regularityIrregular:pluralitySingular",
  "sigma"
)


# ------------------------------------------------------------
# Prior and likelihood sensitivity
# ------------------------------------------------------------

sensitivity <- powerscale_sensitivity(
  m,
  variable = variables
)

print(sensitivity)


# ------------------------------------------------------------
# Save numerical results
# ------------------------------------------------------------

write.csv(
  as.data.frame(sensitivity),
  "priorsense_sensitivity_experiment_1_10M.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# Graphical diagnostic
# ------------------------------------------------------------

p_sensitivity <- powerscale_plot_dens(
  m,
  variable = variables
)

print(p_sensitivity)


# ------------------------------------------------------------
# Save graphic
# ------------------------------------------------------------

ggsave(
  "priorsense_density_experiment_1_10M.png",
  plot = p_sensitivity,
  width = 12,
  height = 8,
  dpi = 300
)