#### Load Data #####

################################################################
# Load libraries and functions
################################################################

source(here("R/ggplot_themes.R"))
source(here("R/calibration/calibration_LF2_function.R")) 
source(here("R/AUC_CV.R"))
source(here("R/calibration/brier_score.R"))

################################################################
# Load TrialNet Data
################################################################

trialnet <- as.data.frame(read.csv(here(paste0(TN_path_save_data, "data_demographics_OGTT2.csv"))))
trialnet <- trialnet %>% 
  mutate(new_ab = ifelse(n_AB_c == 1, n_AB_c, n_AB),
         number_autoantibody_trialnet = as.factor(new_ab),
         mAB_adults = ifelse(age_at_first_drawing >=18 & new_ab == 3, 2, new_ab),
         number_autoantibody = as.factor(mAB_adults),
         fdr = as.factor(fdr),
         t1d = as.numeric(T1D.Indicator),
         T1D.Indicator = as.factor(T1D.Indicator),
         age = age_at_first_drawing,
         study = "trialnet") %>%
  select("Mask.ID","time", "t1d", "T1D.Indicator", "number_autoantibody", "GRS2", "fdr", "age", "study") %>%
  filter(number_autoantibody != 0) %>%
  filter(!(is.na(fdr))) %>%
  filter(!(is.na(GRS2))) %>%
  filter(!(is.na(age))) %>%
  filter(time > 0)

trialnet_age1_7 <- trialnet %>% filter(age <= 7)
trialnet_age7_18 <- trialnet %>% filter(age >= 8 & age <= 17)
trialnet_age18 <- trialnet %>% filter(age >= 18) 


################################################################
#  Load TEDDY Data
################################################################
teddy_file_path <- here("../teddy/")
source(here(paste0(teddy_file_path, "R/main.R")))
dayend = Inf

day = 0
source(here(paste0(teddy_file_path,"R/Extract_information_per_date.R")))
teddy_all <- finaldata

day = 1140.5
source(here(paste0(teddy_file_path,"R/Extract_information_per_date.R")))
teddy_3years <- finaldata

day = 2600.5
source(here(paste0(teddy_file_path,"R/Extract_information_per_date.R")))
teddy_7years <- finaldata



################################################################
#  Old and Stratified Models
################################################################
model1 <- coxph(Surv(time, t1d) ~  strata(number_autoantibody)*(GRS2 + fdr), data = teddy_7years, x=TRUE, y=TRUE)
old_model <- coxph(Surv(time, t1d) ~  number_autoantibody + GRS2 + fdr, data = finaldata, x=TRUE, y=TRUE)



################################################################
#  Year Prediction values
################################################################
year_prediction <- 5
predicted_horizon <- 5


################################################################
#  Load TEDDY Stratified Models  
################################################################
### Age 1
years_landmark <- 410.25
load(here(paste0("../teddy/models/strata/Strata_Coxph_model_day_begin_", years_landmark, ".RData")))
teddyage1_res.cox <- res.cox
trialnet1 <- trialnet %>% filter(age == 1)

### Age 2
years_landmark <- 775.5
load(here(paste0("../teddy/models/strata/Strata_Coxph_model_day_begin_", years_landmark, ".RData")))
teddyage2_res.cox <- res.cox
trialnet2 <- trialnet %>% filter(age == 2)

### Age 3
years_landmark <- 1140.5
load(here(paste0("../teddy/models/strata/Strata_Coxph_model_day_begin_", years_landmark, ".RData")))
teddyage3_res.cox <- res.cox
trialnet3 <- trialnet %>% filter(age == 3)


### Age 4
years_landmark <- 1505.5
load(here(paste0("../teddy/models/strata/Strata_Coxph_model_day_begin_", years_landmark, ".RData")))
teddyage4_res.cox <- res.cox
load(here(paste0("../teddy/models/Coxph_model_day_begin_", years_landmark, ".RData")))
teddyage4_original_res.cox <- res.cox
load(here(paste0("../teddy/models/Coxph_model_day_begin_", years_landmark, ".RData")))
teddyage4_original_res.cox <- res.cox
trialnet4 <- trialnet %>% filter(age == 4)

### Age 5
years_landmark <- 1870.5
load(here(paste0("../teddy/models/strata/Strata_Coxph_model_day_begin_", years_landmark, ".RData")))
teddyage5_res.cox <- res.cox
trialnet5 <- trialnet %>% filter(age == 5)

### Age 6
years_landmark <- 2235.5
load(here(paste0("../teddy/models/strata/Strata_Coxph_model_day_begin_", years_landmark, ".RData")))
teddyage6_res.cox <- res.cox
trialnet6 <- trialnet %>% filter(age == 6)

### Age 7
years_landmark <- 2600.5
load(here(paste0("../teddy/models/strata/Strata_Coxph_model_day_begin_", years_landmark, ".RData")))
teddyage7_res.cox <- res.cox
load(here(paste0("../teddy/models/Coxph_model_day_begin_", years_landmark, ".RData")))
teddyage7_original_res.cox <- res.cox
trialnet7 <- trialnet %>% filter(age == 7)

### Age Over 7
years_landmark <- 2600.5
load(here(paste0("../teddy/models/strata/Strata_Coxph_model_day_begin_", years_landmark, ".RData")))
teddyage7_res.cox <- res.cox
load(here(paste0("../teddy/models/Coxph_model_day_begin_", years_landmark, ".RData")))
teddyage7_original_res.cox <- res.cox

trialnet_over7 <- trialnet %>% filter(age > 7)



################################################################
#  Load table1 functions
################################################################
library(table1)

# p-value function for table1 function
pvalue <- function(x, ...) {
  # Construct vectors of data y, and Stages (strata) g
  y <- unlist(x)
  g <- factor(ifelse(grepl("Age under",names(y)),1,2))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # p <- chisq.test(table(y, g))$p.value
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c(sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}


