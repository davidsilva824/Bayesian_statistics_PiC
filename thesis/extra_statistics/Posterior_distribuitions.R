setwd("C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/for the thesis")


library(posterior)
library(tidyr)
library(ggplot2)
library(ggdist)


# Load Bayesian model

m <- readRDS(
  "C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/results_bayesian_experiment_1_10M_treatment.rds"
)


# Extract posterior samples

ps <- as_draws_df(m)


# Figure 2 style: posterior density plots

p1 <- pivot_longer(
  ps,
  cols = c(starts_with("b_"), "sigma")
) |>
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(vars(name), scales = "free")

print(p1)

ggsave(
  filename = "posterior_density_plots.png",
  plot = p1,
  width = 12,
  height = 7,
  dpi = 300
)


# Figure 9 style: posterior means and 95% credible intervals

p2 <- pivot_longer(
  ps,
  cols = c(
    starts_with("b_regularity"),
    starts_with("b_plurality")
  )
) |>
  ggplot(aes(x = value, y = name)) +
  stat_halfeye(.point = mean)

print(p2)

ggsave(
  filename = "posterior_halfeye_plots.png",
  plot = p2,
  width = 10,
  height = 5,
  dpi = 300
)