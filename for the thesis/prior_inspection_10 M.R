library(ggplot2)
library(dplyr)
library(patchwork)

### Plot prior distributions for Experiment 1
### A. Intercept slopes
### B. Random effects
### C. LKJ

setwd("C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/for the thesis")

output_file <- "prior_distributions_experiment_1_10M.png"

### Helper functions

dtruncnorm_lower <- function(x, mean, sd, lower = 0) {
  dens <- dnorm(x, mean = mean, sd = sd)
  normalizing_constant <- 1 - pnorm(lower, mean = mean, sd = sd)
  ifelse(x >= lower, dens / normalizing_constant, 0)
}

exp_label <- function(rate, label) {
  mean_value <- 1 / rate
  ci_values <- qexp(c(0.025, 0.975), rate = rate)
  paste0(
    label,
    "\nMean: ", round(mean_value, 2),
    "; 95% CI [", round(ci_values[1], 2), ", ", round(ci_values[2], 2), "]"
  )
}

lkj_corr_marginal <- function(rho, eta, K) {
  shape <- eta - 1 + K / 2
  dbeta((rho + 1) / 2, shape1 = shape, shape2 = shape) / 2
}

lkj_ci <- function(eta, K) {
  shape <- eta - 1 + K / 2
  qbeta(c(0.025, 0.975), shape1 = shape, shape2 = shape) * 2 - 1
}

### ------------------------------------------------------------
### A. Intercept slopes
### ------------------------------------------------------------

x_intercept <- seq(0, 30, length.out = 2000)
x_slopes <- seq(-4, 4, length.out = 2000)

intercept_slopes_data <- bind_rows(
  data.frame(
    parameter = "Intercept\nNormal(10, 5), truncated at 0",
    value = x_intercept,
    density = dtruncnorm_lower(x_intercept, mean = 10, sd = 5, lower = 0)
  ),
  data.frame(
    parameter = "Slopes\nNormal(0, 1)",
    value = x_slopes,
    density = dnorm(x_slopes, mean = 0, sd = 1)
  )
)

p_intercept_slopes <- ggplot(intercept_slopes_data, aes(x = value, y = density)) +
  geom_area(fill = "lightblue", alpha = 0.7) +
  geom_line(linewidth = 1) +
  facet_wrap(~ parameter, scales = "free", nrow = 1) +
  labs(
    title = "A. Intercept slopes",
    x = "Parameter value",
    y = "Density"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 13),
    plot.title = element_text(size = 16, hjust = 0.5),
    strip.background = element_rect(fill = "grey70", colour = NA),
    strip.text = element_text(size = 11, colour = "white"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11)
  )

### ------------------------------------------------------------
### B. Random effects
### ------------------------------------------------------------

x_sd <- seq(0, 4, length.out = 2000)

random_effects_data <- bind_rows(
  data.frame(
    prior = exp_label(2.5, "Rate = 2.5 (random-intercept SDs)"),
    value = x_sd,
    density = dexp(x_sd, rate = 2.5)
  ),
  data.frame(
    prior = exp_label(5, "Rate = 5 (random-slope SDs)"),
    value = x_sd,
    density = dexp(x_sd, rate = 5)
  ),
  data.frame(
    prior = exp_label(1, "Rate = 1 (residual SD)"),
    value = x_sd,
    density = dexp(x_sd, rate = 1)
  )
)

p_random_effects <- ggplot(random_effects_data, aes(x = value, y = density, colour = prior)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "B. Random effects",
    x = "SD",
    y = "Density",
    colour = "Prior distribution"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 13),
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9),
    legend.position = "bottom"
  )

### ------------------------------------------------------------
### C. LKJ
### ------------------------------------------------------------

K <- 4
rho <- seq(-0.999, 0.999, length.out = 2000)

lkj_data <- bind_rows(
  lapply(c(1), function(shape_value) {
    ci <- lkj_ci(eta = shape_value, K = K)
    
    data.frame(
      prior = paste0(
        "Shape = ", shape_value,
        ifelse(shape_value == 1, " (used)", ""),
        "\n95% CI [", round(ci[1], 2), ", ", round(ci[2], 2), "]"
      ),
      correlation = rho,
      density = lkj_corr_marginal(rho, eta = shape_value, K = K)
    )
  })
)

p_lkj <- ggplot(lkj_data, aes(x = correlation, y = density, colour = prior)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "C. LKJ",
    x = "Correlation",
    y = "Density",
    colour = "Prior distribution"
  ) +
  theme_classic() +
  theme(
    text = element_text(size = 13),
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9),
    legend.position = "bottom"
  )

### ------------------------------------------------------------
### Combined panel
### ------------------------------------------------------------

p_panel <- p_intercept_slopes / p_random_effects / p_lkj

print(p_panel)

ggsave(
  output_file,
  plot = p_panel,
  width = 9,
  height = 11,
  dpi = 300
)