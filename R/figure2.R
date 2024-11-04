

## load models
load(here(paste0(pathSaveTNModels,"app_models/TrialNet_strata_model_1-7.RData")))
tn1 <- res.cox
teddystrata_age4 <- teddyage4_res.cox
load(here("../teddy/models/Coxph_model_day_begin_1505.5.RData"))
teddyoriginal_age4 <- res.cox

load(here(paste0(pathSaveTNModels,"app_models/TrialNet_strata_model_8-17.RData")))
tn2 <- res.cox
teddystrata_age7 <- teddyage7_res.cox
load(here("../teddy/models/Coxph_model_day_begin_2600.5.RData"))
teddyoriginal_age7 <- res.cox

load(here(paste0(pathSaveTNModels,"app_models/TrialNet_strata_model_18+.RData")))
tn3 <- res.cox


## brier score calculation
test <- trialnet %>% mutate(status = t1d)



## age1-7
test_under8 <- test %>% filter(age < 8)
linear_predictor <- predict(teddystrata_age4, type = "lp", newdata=test_under8)
test_under8 <- test_under8 %>% mutate(lp = linear_predictor)

## age8-17
test_8to18 <- test %>% filter(age >=8) %>% filter(age < 18)
linear_predictor <- predict(teddystrata_age7, type = "lp", newdata=test_8to18)
test_8to18 <- test_8to18 %>% mutate(lp = linear_predictor)

## age18+
test_over18 <- test %>% filter(age >= 18)
linear_predictor <- predict(teddystrata_age7, type = "lp", newdata=test_over18)
test_over18 <- test_over18 %>% mutate(lp = linear_predictor)

# Estimate Brier Score
estimate_function <- function(model, data) {return(Brier_LF(model, data)[1,1])}

# Sample models for three scenarios
model_scenario <- list(teddystrata_age4, teddystrata_age7, teddystrata_age7)

# Corresponding datasets for three scenarios
data_scenario <- list(dataset1 = test_under8, dataset2 = test_8to18, dataset3 = test_over18)  # Replace with actual data

# Combine models and datasets into lists
models <- list(scenario1 = model_scenario)
datasets <- list(scenario1 = data_scenario)

# Number of bootstrap samples
n_bootstrap <- 1000

# Function to perform bootstrap and calculate mean and confidence intervals
bootstrap_estimates <- function(model, data) {
  estimates <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- data[sample(seq_len(nrow(data)), replace = TRUE), ]
    estimates[i] <- estimate_function(model, sample_data)
  }
  mean <- mean(estimates)
  ci <- quantile(estimates, probs = c(0.025, 0.975))
  list(mean = mean, ci_lower = ci[[1]], ci_upper = ci[[2]])
}

# Initialize an empty data frame to collect results
results_df <- data.frame(
  Scenario = character(),
  Model = integer(),
  Dataset = character(),
  Mean = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each scenario
for (scenario in names(models)) {
  cat("Processing", scenario, "\n")
  
  # Get the list of models and datasets for the current scenario
  scenario_models <- models[[scenario]]
  scenario_datasets <- datasets[[scenario]]
  
  # Loop through each model within the current scenario
  for (i in seq_along(scenario_models)) {
    model <- scenario_models[[i]]
    cat("  Using model", i, "\n")
    
    # Use the corresponding dataset for each model
    dataset_name <- names(scenario_datasets)[i]
    dataset <- scenario_datasets[[dataset_name]]
    
    # Print current operation for clarity
    cat("    Applying", dataset_name, "to model", i, "in", scenario, "\n")
    
    # Estimate mean and CI
    result <- bootstrap_estimates(model, dataset)
    
    # Collect the result in the data frame
    results_df <- rbind(results_df, data.frame(
      Scenario = scenario,
      Model = i,
      Dataset = dataset_name,
      Mean = result$mean,
      CI_Lower = result$ci_lower,
      CI_Upper = result$ci_upper,
      stringsAsFactors = FALSE
    ))
  }
}


cat("All scenarios processed.\n")


results_df <- results_df %>%
  mutate(Scenario =  "TEDDY strata model",
         Model = ifelse(Model == 1, "Age under 8", 
                        ifelse(Model == 2, "Age 8 - 17 years old",
                               "Age over 18")),
         Dataset = Model)

results_df <- results_df %>% mutate(Dataset = Model)

results_df <- results_df %>% mutate(Scenario = as.factor(Scenario),
                                    Model = as.factor(Model),
                                    Dataset = factor(Dataset, levels = c("Age under 8", "Age 8 - 17 years old", "Age over 18")),
                                    Mean = as.numeric(Mean),
                                    CI_Lower = as.numeric(CI_Lower),
                                    CI_Upper = as.numeric(CI_Upper))

ggplot(results_df, aes(x = Dataset, y = Mean, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=CI_Lower, ymax=CI_Upper), colour="black", position = position_dodge(0.9), width = 0.4)+
  xlab(paste0("")) +
  ylab(paste0("Brier Score")) +
  theme_articles()+
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position= "none")
 

