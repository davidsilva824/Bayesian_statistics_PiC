library(brms)
library(ggplot2)
library(cmdstanr)
library(dplyr)
library(stringr)
library(ggeffects)
library(rstan)

m <- readRDS("C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/results_bayesian_experiment_3_without_stories_10M_treatment.rds")


summary(m)

### Main effect graphic: type on x-axis, plurality in legend

p <- ggeffects::ggemmeans(m, c("type", "plurality"))
p$x <- factor(p$x, levels = c("Sibilant", "Regular"))

m_preds <- as.data.frame(p)

pd <- position_dodge(width = 0.3)
y_pad <- 0.05 * (max(m_preds$conf.high) - min(m_preds$conf.low))

p <- ggplot(
  m_preds,
  aes(x = x, y = predicted, colour = group)
) +
  geom_point(size = 3, position = pd) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0,
    linewidth = 0.8,
    position = pd
  ) +
  scale_x_discrete(expand = expansion(add = c(0.5, 1.2))) +
  coord_cartesian(
    ylim = c(min(m_preds$conf.low) - y_pad, max(m_preds$conf.high) + y_pad)
  ) +
  labs(
    x = NULL,
    y = "Surprisal",
    colour = NULL
  ) +
  theme_light() +
  theme(
    text = element_text(size = 14, colour = "gray28"),
    axis.title = element_text(size = 23),
    axis.text = element_text(size = 22),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 19),
    legend.position = c(0.60, 0.5),
    legend.justification = c(0, 0.5)
  )

print(p)

#ggsave(
#  "plot_type_x_plurality_legend_10M.png",
#  plot = p,
#  width = 4.5,
#  height = 5,
#  dpi = 300
#)

### Interaction graphic per model

coef_names <- rownames(fixef(m))
int_term <- grep("type.*:plurality|plurality.*:type", coef_names, value = TRUE)

if(length(int_term) != 1){
  stop("Could not find the type × plurality interaction term. Run rownames(fixef(m)) to check coefficient names.")
}

plurality_term <- ifelse(grepl("pluralityPlural", int_term), "pluralityPlural", "pluralitySingular")
other_type <- ifelse(grepl("typeRegular", int_term), "Regular", "Sibilant")
base_type <- ifelse(other_type == "Regular", "Sibilant", "Regular")

base_expr <- if(plurality_term == "pluralityPlural"){
  plurality_term
} else {
  paste0("-(", plurality_term, ")")
}

other_expr <- if(plurality_term == "pluralityPlural"){
  paste0(plurality_term, " + ", int_term)
} else {
  paste0("-(", plurality_term, " + ", int_term, ")")
}

if(base_type == "Sibilant"){
  sibilant_expr <- base_expr
  regular_expr <- other_expr
} else {
  regular_expr <- base_expr
  sibilant_expr <- other_expr
}

int_expr <- if(
  (other_type == "Sibilant" && plurality_term == "pluralityPlural") ||
  (other_type == "Regular" && plurality_term == "pluralitySingular")
){
  int_term
} else {
  paste0("-(", int_term, ")")
}

(m_eff_int <- hypothesis(m, paste0(int_expr, " > 0"),
                         scope = "coef", group = "model", alpha = 0.025)$hypothesis)

(m_eff_sibilant <- hypothesis(m, paste0(sibilant_expr, " > 0"),
                              scope = "coef", group = "model", alpha = 0.025)$hypothesis)

(m_eff_regular <- hypothesis(m, paste0(regular_expr, " > 0"),
                             scope = "coef", group = "model", alpha = 0.025)$hypothesis)

m_eff_int$effect <- "Type × Plurality\n interaction"
m_eff_sibilant$effect <- "Sibilant plural\n effect"
m_eff_regular$effect <- "Regular plural\n effect"

plot_dat <- bind_rows(m_eff_int, m_eff_sibilant, m_eff_regular) %>%
  mutate(
    effect = factor(
      effect,
      levels = c(
        "Type × Plurality\n interaction",
        "Sibilant plural\n effect",
        "Regular plural\n effect"
      )
    )
  )

interaction_order <- plot_dat %>%
  filter(effect == "Type × Plurality\n interaction") %>%
  arrange(Estimate) %>%
  pull(Group)

plot_dat$Group <- factor(plot_dat$Group, levels = interaction_order)

# write.csv(plot_dat, "forest_plot_experiment_2_without_stories_values_10M.csv", row.names = FALSE)

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
    text = element_text(size = 18, colour = "grey28"),
    strip.text = element_text(size = 18),
    axis.text.y = element_text(size = 17),
    plot.title = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "white", colour = "black")
  )

print(p)

#ggsave(
#  "forest_plot_experiment_3_without_stories_10M.png",
#  plot = p,
#  width = 11,
#  height = 7,
#  dpi = 300
#)

hypothesis(m, paste0(sibilant_expr, " = 0"))
hypothesis(m, paste0(regular_expr, " = 0"))
hypothesis(m, paste0(int_expr, " = 0"))