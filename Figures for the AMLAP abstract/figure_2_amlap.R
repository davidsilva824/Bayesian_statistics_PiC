library(brms)
library(ggplot2)
library(cmdstanr)
library(dplyr)
library(stringr)
library(ggeffects)

m <- readRDS("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/Bayesean_Statistics/for the thesis/Models)



### Interaction graphic per model

(m_eff_int <- hypothesis(m, "regularityIrregular:pluralitySingular > 0",
                         scope = "coef", group = "model", alpha = 0.025)$hypothesis)

(m_eff_reg <- hypothesis(m, "-pluralitySingular > 0",
                         scope = "coef", group = "model", alpha = 0.025)$hypothesis)

(m_eff_irreg <- hypothesis(m, "-(pluralitySingular + regularityIrregular:pluralitySingular) > 0",
                           scope = "coef", group = "model", alpha = 0.025)$hypothesis)

m_eff_int$effect <- "Regularity\n ×\n Plurality"
m_eff_reg$effect <- "Regular\n plural"
m_eff_irreg$effect <- "Irregular\n plural"

plot_dat <- bind_rows(m_eff_int, m_eff_reg, m_eff_irreg) %>%
  mutate(
    effect = factor(
      effect,
      levels = c(
        "Regularity\n ×\n Plurality",
        "Regular\n plural",
        "Irregular\n plural"
      )
    )
  )

interaction_order <- plot_dat %>%
  filter(effect == "Regularity\n ×\n Plurality") %>%
  arrange(Estimate) %>%
  pull(Group)

plot_dat$Group <- factor(plot_dat$Group, levels = interaction_order)

write.csv(plot_dat, "forest_plot_treatment_values_10M.csv", row.names = FALSE)

p <- ggplot(plot_dat, aes(x = Group, y = Estimate)) +
  coord_flip() +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = CI.Lower, ymax = CI.Upper), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue") +
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
  "forest_plot_treatment_3panels_10M_2.png",
  plot = p,
  width = 10,
  height = 8,
  dpi = 300
)

hypothesis(m, "pluralitySingular = 0")
hypothesis(m, "pluralitySingular + regularityIrregular:pluralitySingular = 0")
hypothesis(m, "regularityIrregular:pluralitySingular = 0")


#hyp <- hypothesis(...)
#1 / hyp$hypothesis$Evid.Ratio
