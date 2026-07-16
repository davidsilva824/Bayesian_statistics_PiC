library(brms)
library(ggplot2)
library(cmdstanr)
library(dplyr)
library(stringr)
library(ggeffects)

m_10M <- readRDS("C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/results_bayesian_experiment_1_10M_treatment.rds")

m_100M <- readRDS("C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/results_bayesian_experiment_1_100M_treatment.rds")


### 10M models

m10_eff_int <- hypothesis(
  m_10M,
  "regularityIrregular:pluralitySingular > 0",
  scope = "coef",
  group = "model",
  alpha = 0.025
)$hypothesis

m10_eff_reg <- hypothesis(
  m_10M,
  "-pluralitySingular > 0",
  scope = "coef",
  group = "model",
  alpha = 0.025
)$hypothesis

m10_eff_irreg <- hypothesis(
  m_10M,
  "-(pluralitySingular + regularityIrregular:pluralitySingular) > 0",
  scope = "coef",
  group = "model",
  alpha = 0.025
)$hypothesis


### 100M models

m100_eff_int <- hypothesis(
  m_100M,
  "regularityIrregular:pluralitySingular > 0",
  scope = "coef",
  group = "model",
  alpha = 0.025
)$hypothesis

m100_eff_reg <- hypothesis(
  m_100M,
  "-pluralitySingular > 0",
  scope = "coef",
  group = "model",
  alpha = 0.025
)$hypothesis

m100_eff_irreg <- hypothesis(
  m_100M,
  "-(pluralitySingular + regularityIrregular:pluralitySingular) > 0",
  scope = "coef",
  group = "model",
  alpha = 0.025
)$hypothesis


### Add effect names

m10_eff_int$effect <- "Regularity\n ×\n Plurality"
m10_eff_reg$effect <- "Regular\n plural"
m10_eff_irreg$effect <- "Irregular\n plural"

m100_eff_int$effect <- "Regularity\n ×\n Plurality"
m100_eff_reg$effect <- "Regular\n plural"
m100_eff_irreg$effect <- "Irregular\n plural"


### Add training sizes

m10_eff_int$training_size <- "10M"
m10_eff_reg$training_size <- "10M"
m10_eff_irreg$training_size <- "10M"

m100_eff_int$training_size <- "100M"
m100_eff_reg$training_size <- "100M"
m100_eff_irreg$training_size <- "100M"


### Combine results

plot_dat <- bind_rows(
  m10_eff_int,
  m10_eff_reg,
  m10_eff_irreg,
  m100_eff_int,
  m100_eff_reg,
  m100_eff_irreg
) %>%
  mutate(
    Group = recode(
      Group,
      "babble_txt_BPE_with_spaces" = "babbles"
    ),
    Group = paste0(Group, " — ", training_size),
    effect = factor(
      effect,
      levels = c(
        "Regularity\n ×\n Plurality",
        "Regular\n plural",
        "Irregular\n plural"
      )
    )
  )


### Order 10M models first, followed by 100M models

interaction_order_10M <- plot_dat %>%
  filter(
    effect == "Regularity\n ×\n Plurality",
    training_size == "10M"
  ) %>%
  arrange(Estimate) %>%
  pull(Group)

interaction_order_100M <- plot_dat %>%
  filter(
    effect == "Regularity\n ×\n Plurality",
    training_size == "100M"
  ) %>%
  arrange(Estimate) %>%
  pull(Group)

model_order <- c(interaction_order_10M, interaction_order_100M)

plot_dat$Group <- factor(
  plot_dat$Group,
  levels = rev(model_order)
)


### Save values

write.csv(
  plot_dat,
  "forest_plot_treatment_values_10M_100M.csv",
  row.names = FALSE
)


### Create plot

p <- ggplot(plot_dat, aes(x = Group, y = Estimate)) +
  coord_flip() +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(ymin = CI.Lower, ymax = CI.Upper),
    width = 0
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "blue"
  ) +
  facet_wrap(~ effect, ncol = 3) +
  labs(
    x = NULL,
    y = "Estimate"
  ) +
  theme(
    text = element_text(size = 20, colour = "black"),
    strip.text = element_text(size = 25, colour = "black"),
    axis.text.y = element_text(size = 19, colour = "black"),
    axis.text.x = element_text(size = 13, colour = "black"),
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "white", colour = "black"),
    panel.background = element_rect(fill = "white", colour = NA)
  )

print(p)

ggsave(
  "forest_plot_treatment_3panels_10M_100M_combined.png",
  plot = p,
  width = 10,
  height = 10,
  dpi = 300
)