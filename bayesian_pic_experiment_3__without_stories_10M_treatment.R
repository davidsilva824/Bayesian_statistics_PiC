library(lme4)
library(brms)
library(ggplot2)
library(cmdstanr)
library(dplyr)
library(stringr)
library(ggeffects)


setwd("C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/results_berent&pinker/experiment_3_without_stories/10M")

file_list <- list.files(pattern = "\\.csv$", full.names = FALSE)

get_model_name <- function(fn){
  x <- fn
  x <- str_replace(x, "\\.csv$", "")
  x <- str_replace(x, "^results_experiment_\\d+_", "")
  x
}

dat <- lapply(file_list, function(f){
  d <- read.csv(f, check.names = TRUE)
  d$model <- get_model_name(f)
  d$File <- f
  d
}) |> bind_rows()

dat <- dat %>%
  mutate(
    type = ifelse(grepl("Sibilant", Category), "Sibilant", "Regular"),
    plurality = ifelse(grepl("Plural", Category), "Plural", "Singular"),
    type = factor(type, levels = c("Sibilant", "Regular")),
    plurality = factor(plurality, levels = c("Singular", "Plural"))
  )


dat <- dat %>%
  mutate(
    set = case_when(
      Non.Head %in% c("hose","hoses","hoe","hoes") ~ "set_hose_hoe",
      Non.Head %in% c("rose","roses","row","rows") ~ "set_rose_row",
      Non.Head %in% c("rise","rises","lie","lies") ~ "set_rise_lie",
      Non.Head %in% c("clause","clauses","claw","claws") ~ "set_clause_claw",
      Non.Head %in% c("gaze","gazes","guy","guys") ~ "set_gaze_guy",
      Non.Head %in% c("box","boxes","book","books") ~ "set_box_book",
      Non.Head %in% c("sex","sexes","sack","sacks") ~ "set_sex_sack",
      Non.Head %in% c("tax","taxes","tack","tacks") ~ "set_tax_tack",
      Non.Head %in% c("size","sizes","sigh","sighs") ~ "set_size_sigh",
      Non.Head %in% c("praise","praises","tray","trays") ~ "set_praise_tray",
      Non.Head %in% c("bruise","bruises","brew","brews") ~ "set_bruise_brew",
      Non.Head %in% c("raise","raises","ray","rays") ~ "set_raise_ray",
      Non.Head %in% c("blaze","blazes","play","plays") ~ "set_blaze_play",
      Non.Head %in% c("vase","vases","bee","bees") ~ "set_vase_bee",
      Non.Head %in% c("fox","foxes","shock","shocks") ~ "set_fox_shock",
      Non.Head %in% c("maze","mazes","bay","bays") ~ "set_maze_bay",
      Non.Head %in% c("breeze","breezes","tree","trees") ~ "set_breeze_tree",
      Non.Head %in% c("cause","causes","paw","paws") ~ "set_cause_paw",
      Non.Head %in% c("phase","phases","fee","fees") ~ "set_phase_fee",
      Non.Head %in% c("fax","faxes","shack","shacks") ~ "set_fax_shack",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(set))

### Running the frequentist model for comparison
m_freq <- lmer(Surprisal.head ~ 1 + type * plurality +
                 (1 + type * plurality | model) + (1 + type * plurality | set) + (1 + type * plurality | Head),
               data = dat)
summary(m_freq) |> print(cor = F)


predictions <-ggemmeans(m_freq, c("type", "plurality"))

plot(predictions)



### Priors
priors_surprisal <-
  prior(normal(10, 5), class = Intercept, lb = 0) +
  prior(normal(0, 1), class = b) +
  prior(exponential(2.5), class = sd, group = model, coef = Intercept) +
  prior(exponential(2.5), class = sd, group = set, coef = Intercept) +
  prior(exponential(2.5), class = sd, group = Head, coef = Intercept) +
  prior(exponential(5), class = sd, group = model) +
  prior(exponential(5), class = sd, group = set) +
  prior(exponential(5), class = sd, group = Head) +
  prior(lkj(1), class=cor) +
  prior(exponential(1), class=sigma)

m <- brm(Surprisal.head ~ 1 + type * plurality +
           (1 + type * plurality | model) +
           (1 + type * plurality | set) +
           (1 + type * plurality | Head),
         data = dat,
         prior = priors_surprisal,
         sample_prior = "yes",
         chains = 4, iter = 12000, warmup = 2000,
         cores = 4,
         backend = "cmdstanr",
         file = "C:/Users/Admin/Desktop/Dissertação/código/Bayesian_statistics_PiC/results_bayesian_experiment_3_without_stories_10M_treatment")
summary(m)