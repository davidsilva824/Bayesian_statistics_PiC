setwd("C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/Figures for CLP abstract")


library(brms)
library(ggplot2)
library(cmdstanr)
library(dplyr)
library(stringr)
library(ggeffects)


# --------------------------------------------------
# Load models
# --------------------------------------------------

m_10M <- readRDS(
  "C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/for the thesis/Models/model_bayesean_experiment_1_10M_treatment.rds"
)

m_100M <- readRDS(
  "C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/for the thesis/Models/model_bayesean_experiment_1_100M_treatment.rds"
)


# --------------------------------------------------
# 10M models
# --------------------------------------------------

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


# --------------------------------------------------
# 100M models
# --------------------------------------------------

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


# --------------------------------------------------
# Add effect names
# --------------------------------------------------

m10_eff_int$effect <- "Regularity\n×\nPlurality"
m10_eff_reg$effect <- "Regular\nplural"
m10_eff_irreg$effect <- "Irregular\nplural"

m100_eff_int$effect <- "Regularity\n×\nPlurality"
m100_eff_reg$effect <- "Regular\nplural"
m100_eff_irreg$effect <- "Irregular\nplural"


# --------------------------------------------------
# Add training sizes
# --------------------------------------------------

m10_eff_int$training_size <- "10M"
m10_eff_reg$training_size <- "10M"
m10_eff_irreg$training_size <- "10M"

m100_eff_int$training_size <- "100M"
m100_eff_reg$training_size <- "100M"
m100_eff_irreg$training_size <- "100M"


# --------------------------------------------------
# Combine results
# --------------------------------------------------

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
      "babble_txt_BPE_with_spaces" = "babble"
    ),
    training_size = factor(
      training_size,
      levels = c("10M", "100M")
    ),
    effect = factor(
      effect,
      levels = c(
        "Regularity\n×\nPlurality",
        "Regular\nplural",
        "Irregular\nplural"
      )
    )
  )


# --------------------------------------------------
# Order models
# --------------------------------------------------

interaction_order_10M <- plot_dat %>%
  filter(
    effect == "Regularity\n×\nPlurality",
    training_size == "10M"
  ) %>%
  arrange(Estimate) %>%
  pull(Group)

interaction_order_100M <- plot_dat %>%
  filter(
    effect == "Regularity\n×\nPlurality",
    training_size == "100M"
  ) %>%
  arrange(Estimate) %>%
  pull(Group)

model_order <- unique(
  c(interaction_order_10M, interaction_order_100M)
)

plot_dat$Group <- factor(
  plot_dat$Group,
  levels = model_order
)


# --------------------------------------------------
# Save values
# --------------------------------------------------

write.csv(
  plot_dat,
  "forest_plot_treatment_values_10M_100M.csv",
  row.names = FALSE
)


# --------------------------------------------------
# Create plot
# --------------------------------------------------

p <- ggplot(
  plot_dat,
  aes(x = Estimate, y = Group)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "blue"
  ) +
  geom_errorbar(
    aes(
      xmin = CI.Lower,
      xmax = CI.Upper
    ),
    width = 0,
    orientation = "y"
  ) +
  geom_point(size = 2.5) +
  facet_grid(
    rows = vars(training_size),
    cols = vars(effect),
    scales = "free_y",
    space = "free_y"
  ) +
  scale_y_discrete(
    drop = TRUE,
    expand = expansion(add = 0.5)
  ) +
  labs(
    x = "Estimate",
    y = NULL
  ) +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 1
    ),
    panel.spacing.x = grid::unit(0.1, "lines"),
    
    # Controls the space between 10M and 100M
    panel.spacing.y = grid::unit(0.35, "cm"),
    
    panel.grid = element_blank(),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    text = element_text(
      size = 20,
      colour = "black"
    ),
    strip.text.x = element_text(
      size = 25,
      colour = "black"
    ),
    strip.text.y = element_text(
      size = 25,
      colour = "black"
    ),
    axis.text.y = element_text(
      size = 19,
      colour = "black"
    ),
    axis.text.x = element_text(
      size = 13,
      colour = "black"
    ),
    strip.background = element_rect(
      fill = "white",
      colour = "black"
    )
  )


# --------------------------------------------------
# Put a black box around each model-name section
# --------------------------------------------------

plot_grob <- ggplotGrob(p)

axis_indices <- which(
  grepl("^axis-l", plot_grob$layout$name)
)

active_axis_indices <- axis_indices[
  !vapply(
    plot_grob$grobs[axis_indices],
    function(x) inherits(x, "zeroGrob"),
    logical(1)
  )
]

axis_cells <- plot_grob$layout[
  active_axis_indices,
  c("t", "l", "b", "r", "z")
]

for (i in seq_len(nrow(axis_cells))) {
  
  plot_grob <- gtable::gtable_add_grob(
    plot_grob,
    grid::rectGrob(
      gp = grid::gpar(
        colour = "black",
        fill = "white",
        lwd = 1
      )
    ),
    t = axis_cells$t[i],
    l = axis_cells$l[i],
    b = axis_cells$b[i],
    r = axis_cells$r[i],
    z = axis_cells$z[i] - 0.1,
    clip = "off"
  )
}


# --------------------------------------------------
# Display
# --------------------------------------------------

grid::grid.newpage()
grid::grid.draw(plot_grob)


# --------------------------------------------------
# Save
# --------------------------------------------------

ggsave(
  "forest_plot_treatment_3panels_10M_100M_combined.png",
  plot = plot_grob,
  width = 10,
  height = 10,
  dpi = 300,
  bg = "white"
)