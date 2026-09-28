library(brms)
library(ggplot2)
library(dplyr)

### ------------------------------------------------------------
### Paths
### ------------------------------------------------------------

model_dir <- "C:/Users/david/Documents/GitHub/Bayesian_statistics_PiC/thesis/sensitivity_analysis"

base_model_file <- paste0(
  "C:/Users/david/Documents/GitHub/Bayesian_statistics_PiC/",
  "thesis/models/model_bayesean_experiment_1_100M_treatment.rds"
)

setwd(model_dir)

### ------------------------------------------------------------
### Load main model
### ------------------------------------------------------------

base_model <- readRDS(base_model_file)

### ------------------------------------------------------------
### Models included in the 100M analysis
### ------------------------------------------------------------

file_list <- c(
  "results_experiment_1_gpt_2_100M.csv",
  "results_experiment_1_babyLlama_100M.csv",
  "results_experiment_1_babble_txt_BPE_with_spaces.csv",
  "results_experiment_1_CLASS_IT.csv",
  "results_experiment_1_OPT_100M.csv",
  "results_experiment_1_gpt_bert_100M_causal.csv",
  "results_experiment_1_gpt_bert_100M_mixed.csv",
  "results_experiment_1_gpt_bert_100M_masked.csv"
)

selected_models <- file_list |>
  sub("^results_experiment_1_", "", x = _) |>
  sub("\\.csv$", "", x = _)

analysis_data <- base_model$data %>%
  filter(model %in% selected_models) %>%
  droplevels()

### ------------------------------------------------------------
### Priors included in the sensitivity analysis
### ------------------------------------------------------------

prior_sds <- c(0.625, 1.25, 2.5, 5, 10)

### ------------------------------------------------------------
### Function to obtain ln(BF10)
### ------------------------------------------------------------

get_lnbf10 <- function(model, hyp){
  h <- hypothesis(model, hyp)
  bf01 <- h$hypothesis$Evid.Ratio
  log(1 / bf01)
}

### ------------------------------------------------------------
### Load existing models or fit missing models
### ------------------------------------------------------------

sensitivity_results <- lapply(prior_sds, function(prior_sd){
  
  model_file <- paste0(
    model_dir,
    "/results_bayesean_experiment_1_100M_treatment_prior_",
    prior_sd,
    ".rds"
  )
  
  if (file.exists(model_file)) {
    
    cat("\nLoading existing model:", prior_sd, "\n")
    
    m <- readRDS(model_file)
    
  } else {
    
    cat("\nModel not found. Fitting prior SD =", prior_sd, "\n")
    
    # Copy the priors from the main model
    priors_surprisal <- base_model$prior
    
    # Change ONLY the fixed-effects prior
    priors_surprisal$prior[
      priors_surprisal$class == "b"
    ] <- paste0(
      "normal(0, ",
      prior_sd,
      ")"
    )
    
    # Fit model
    m <- brm(
      formula = base_model$formula,
      data = analysis_data,
      family = base_model$family,
      prior = priors_surprisal,
      sample_prior = "yes",
      chains = 4,
      iter = 12000,
      warmup = 2000,
      cores = 4,
      backend = "cmdstanr"
    )
    
    # Save newly fitted model
    saveRDS(
      m,
      model_file
    )
  }
  
  ### ----------------------------------------------------------
  ### Extract Bayes factors
  ### ----------------------------------------------------------
  
  data.frame(
    prior = paste0("N(0, ", prior_sd, ")"),
    prior_sd = prior_sd,
    effect = c(
      "Regular plural",
      "Irregular plural",
      "Regularity × Plurality"
    ),
    lnBF10 = c(
      get_lnbf10(
        m,
        "pluralitySingular = 0"
      ),
      get_lnbf10(
        m,
        "pluralitySingular + regularityIrregular:pluralitySingular = 0"
      ),
      get_lnbf10(
        m,
        "regularityIrregular:pluralitySingular = 0"
      )
    )
  )
  
}) |> bind_rows()

### ------------------------------------------------------------
### Save results
### ------------------------------------------------------------

write.csv(
  sensitivity_results,
  "sensitivity_analysis_experiment_1_100M.csv",
  row.names = FALSE
)

print(sensitivity_results)

### ------------------------------------------------------------
### Sensitivity analysis plot
### ------------------------------------------------------------

sensitivity_results$prior <- factor(
  sensitivity_results$prior,
  levels = paste0(
    "N(0, ",
    prior_sds,
    ")"
  )
)

y_breaks <- sort(
  unique(
    c(
      pretty(sensitivity_results$lnBF10),
      -1,
      1
    )
  )
)

p <- ggplot(
  sensitivity_results,
  aes(
    x = prior,
    y = lnBF10,
    group = 1
  )
) +
  geom_point() +
  geom_line() +
  geom_hline(
    yintercept = 1,
    linetype = "dotted"
  ) +
  geom_hline(
    yintercept = -1,
    linetype = "dotted"
  ) +
  facet_wrap(
    ~ effect,
    ncol = 3
  ) +
  scale_y_continuous(
    breaks = y_breaks
  ) +
  labs(
    x = "Prior",
    y = "lnBF10"
  ) +
  theme_light() +
  theme(
    strip.text = element_text(size = 16),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 16)
  )

print(p)

### ------------------------------------------------------------
### Save plot
### ------------------------------------------------------------

ggsave(
  filename = "sensitivity_analysis_experiment_1_100M.png",
  plot = p,
  width = 14,
  height = 5,
  dpi = 300
)