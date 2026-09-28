library(brms)
library(posterior)
library(dplyr)
library(bayesplot)
library(ggplot2)

setwd("C:/Users/david/Documents/GitHub/Bayesian_statistics_PiC/thesis/extra_statistics")

m <- readRDS(
  "C:/Users/david/Documents/GitHub/Bayesian_statistics_PiC/thesis/models/model_bayesean_experiment_1_10M_treatment.rds"
)

# Convergence diagnostics
diagnostics <- posterior::summarise_draws(
  posterior::as_draws_df(m)
) %>%
  select(variable, rhat, ess_bulk, ess_tail)

print(diagnostics)

# Save full diagnostics table
write.csv(
  diagnostics,
  "convergence_diagnostics_experiment_1_10M.csv",
  row.names = FALSE
)

# Largest Rhat
max_rhat <- diagnostics %>%
  filter(!is.na(rhat)) %>%
  arrange(desc(rhat)) %>%
  slice(1)

# Smallest bulk ESS
min_bulk_ess <- diagnostics %>%
  filter(!is.na(ess_bulk)) %>%
  arrange(ess_bulk) %>%
  slice(1)

# Smallest tail ESS
min_tail_ess <- diagnostics %>%
  filter(!is.na(ess_tail)) %>%
  arrange(ess_tail) %>%
  slice(1)

cat("\nLargest Rhat:\n")
print(max_rhat)

cat("\nSmallest Bulk ESS:\n")
print(min_bulk_ess)

cat("\nSmallest Tail ESS:\n")
print(min_tail_ess)

# Save summary table with the key values
key_diagnostics <- bind_rows(
  max_rhat %>% mutate(check = "Largest Rhat"),
  min_bulk_ess %>% mutate(check = "Smallest Bulk ESS"),
  min_tail_ess %>% mutate(check = "Smallest Tail ESS")
)

write.csv(
  key_diagnostics,
  "key_convergence_diagnostics_experiment_1_10M.csv",
  row.names = FALSE
)

# Traceplots for fixed effects
p_trace <- mcmc_plot(
  m,
  type = "trace",
  variable = "^b_",
  regex = TRUE,
  facet_args = list(ncol = 1)
) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    strip.background = element_rect(fill = "white", colour = "black")
  )

print(p_trace)

ggsave(
  filename = "traceplots_fixed_effects_experiment_1_10M.png",
  plot = p_trace,
  width = 8,
  height = 10,
  dpi = 300,
  bg = "white"
)