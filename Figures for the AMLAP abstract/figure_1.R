library(brms)
library(ggplot2)
library(cmdstanr)
library(dplyr)
library(stringr)
library(ggeffects)

m <- readRDS("C:/Users/Admin/Desktop/Dissertação/código/satistics_PiC/Statistics_PiC/Bayesean_Statistics/surprisal_gaussian_experiment_1_10M_treatment.rds")


### Alternative graphic: regularity on x-axis, plurality in legend

p <- ggeffects::ggemmeans(m, c("regularity", "plurality"))
p$x <- factor(p$x, levels = c("Regular", "Irregular"))

m_preds <- as.data.frame(p)

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
  scale_x_discrete(expand = expansion(add = c(0.8, 0.8))) +
  coord_cartesian(
    ylim = c(min(m_preds$conf.low) - y_pad, 16.25)
  ) +
  labs(
    x = NULL,
    y = "Surprisal",
    colour = NULL
  ) +
  theme_light() +
  theme(
    text = element_text(size = 14, colour = "black"),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 20,  colour = "black"),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.position = c(0.55, 0.9),
    legend.justification = c(0, 0.5),
    panel.grid = element_blank()
  )


print(p)

ggsave(
  "plot_regularity_x_plurality_legend.png",
  plot = p,
  width = 4,
  height = 6,
  dpi = 300
)
