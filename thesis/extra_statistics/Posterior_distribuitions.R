setwd("C:/Users/david/Documents/GitHub/Bayesian_statistics_PiC/thesis/extra_statistics")

library(posterior)
library(tidyr)
library(ggplot2)
library(ggdist)
library(dplyr)

# Load Bayesian model
m <- readRDS(
  "C:/Users/david/Documents/GitHub/Bayesian_statistics_PiC/thesis/models/model_bayesean_experiment_1_10M_treatment.rds"
)

# Extract posterior samples
ps <- as_draws_df(m)

# Derive posterior samples for the effects of interest
plot_dat <- ps |>
  mutate(
    regular_effect = -b_pluralitySingular,
    irregular_effect = -(b_pluralitySingular + `b_regularityIrregular:pluralitySingular`),
    interaction_effect = `b_regularityIrregular:pluralitySingular`,
    sigma_effect = sigma
  ) |>
  select(
    regular_effect,
    irregular_effect,
    interaction_effect,
    sigma_effect
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "effect",
    values_to = "value"
  ) |>
  mutate(
    effect = factor(
      effect,
      levels = c(
        "regular_effect",
        "irregular_effect",
        "interaction_effect",
        "sigma_effect"
      ),
      labels = c(
        "Regular plural effect",
        "Irregular plural effect",
        "Regularity × Plurality interaction",
        "Sigma"
      )
    )
  )

# Half-eye plot in one panel
p <- ggplot(plot_dat, aes(x = value, y = 0)) +
  stat_halfeye(
    .point = mean,
    .width = 0.95,
    normalize = "panels"
  ) +
  facet_wrap(
    ~ effect,
    nrow = 1,
    scales = "free_x"
  ) +
  labs(
    x = "Value",
    y = NULL
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 14),
    strip.text = element_text(size = 16)
  )

print(p)

ggsave(
  filename = "posterior_halfeye_effects_sigma_10M.png",
  plot = p,
  width = 14,
  height = 5,
  dpi = 300
)