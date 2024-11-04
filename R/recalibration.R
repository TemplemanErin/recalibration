#### Recalibration Code #####

################################################################
# pre-requisites / dependencies
################################################################

# source(here("R/recalibration_paper/load_data.R"))


################################################################
# Recalibration
################################################################
# set the seed to make partition reproducible
set.seed(123)

################################################################
# Age 1- 7
################################################################
four_year_model <- 4

## split data into train and test
# 75% of the sample size
smp_size <- floor(0.75 * nrow(trialnet_age1_7))

train_ind <- sample(seq_len(nrow(trialnet_age1_7)), size = smp_size)

train_age1_7 <- trialnet_age1_7[train_ind, ]
test_age1_7 <- trialnet_age1_7[-train_ind, ]


## generate linear predictors
# train data
train_age1_7_lp_5years <- predict(teddyage4_original_res.cox, type = "lp", newdata=train_age1_7)
train_age1_7 <- train_age1_7 %>% 
  mutate(lp_5years = train_age1_7_lp_5years)
# test data
test_age1_7_lp_5years <- predict(teddyage4_original_res.cox, type = "lp", newdata=test_age1_7)
test_age1_7 <- test_age1_7 %>% 
  mutate(lp_5years = test_age1_7_lp_5years)

# fit new Model with linear predictors (recalibrated model)
age1_7_new_model_5years <- coxph(Surv(time,t1d) ~ lp_5years + strata(number_autoantibody), data= train_age1_7, x = TRUE)

# Generate estimates from recalibrated model
estimates_age1_7_5years <- 1-predictSurvProb(age1_7_new_model_5years,newdata= test_age1_7,times= predicted_horizon+four_year_model)
test_age1_7$new.estimates <- estimates_age1_7_5years
test_age1_7$old.estimates <- 1-predictSurvProb(teddyage4_original_res.cox, newdata = test_age1_7, times = predicted_horizon+four_year_model)[,1]


# Calibration of recalibrated and original model
age1_7_recalibrate.cal_5years <- calibration_LF2_recalibration_test2(age1_7_new_model_5years, test_age1_7, year_prediction = predicted_horizon + four_year_model, estimates = estimates_age1_7_5years, group_name = "age1_7")
age1_7_recalibrate.cal_5years$model <- "Recalibrated Age 4 TEDDY Model in individuals aged 1-7"

age1_7_original.cal <- calibration_LF2(model = model1, newdataset = test_age1_7, model_prediction_year = four_year_model, year_prediction, model_type = "not strata")
age1_7_original.cal$model <- "Age 4 TEDDY Model applied in individuals aged 1-7"

dt_1_7 <- rbind(age1_7_original.cal, age1_7_recalibrate.cal_5years)




################################################################
# Age 8 - 17
################################################################
seven_year_model <- 7


## split data into train and test
# 75% of the sample size
smp_size <- floor (0.75 * nrow(trialnet_age7_18))

## set the seed to make partition reproducible
set.seed(124)

train_ind <- sample(seq_len(nrow(trialnet_age7_18)), size = smp_size)

train_age7_18 <- trialnet_age7_18[train_ind, ]
test_age7_18 <- trialnet_age7_18[-train_ind, ]

# generate linear predictors
# train data
train_age7_18_lp_5years<- predict(teddyage7_original_res.cox, type = "lp", newdata=train_age7_18)
train_age7_18 <- train_age7_18 %>%
  mutate(lp_5years = train_age7_18_lp_5years) 

# test data
test_age7_18_lp_5years <- predict(teddyage7_original_res.cox, type = "lp", newdata=test_age7_18)
test_age7_18 <- test_age7_18 %>% 
  mutate(lp_5years = test_age7_18_lp_5years)


# Recalibrated model
age7_18_new_model_5years <- coxph(Surv(time,t1d) ~ lp_5years + strata(number_autoantibody), data= train_age7_18, x = TRUE)


# generate new prediction estimates in test data
estimates_age7_18_5years <- 1-predictSurvProb(age7_18_new_model_5years, newdata= test_age7_18, times= predicted_horizon)
test_age7_18$new.estimates <- estimates_age7_18_5years
test_age7_18$old.estimates <- 1-predictSurvProb(teddyage7_original_res.cox, newdata = test_age7_18, times = predicted_horizon + seven_year_model)[,1]


# calibration of recalibrated and stratified teddy model
age7_18_recalibrate.cal_5years <- calibration_LF2_recalibration_test2(age7_18_new_model_5years,test_age7_18, year_prediction = predicted_horizon, estimates = estimates_age7_18_5years, group_name = "age7_18")
age7_18_recalibrate.cal_5years$model <- "Recalibrated Age 7 TEDDY Model in individuals aged 8 - 18"

age7_18_original.cal <- calibration_LF2(model = model1, newdataset = test_age7_18, seven_year_model, year_prediction, model_type = "not strata")
age7_18_original.cal$model <- "Age 7 TEDDY Model applied in individuals aged 8 - 18"

dt_7_18 <- rbind(age7_18_original.cal, age7_18_recalibrate.cal_5years)





################################################################
# Age 18+
################################################################

# split dataset to train and test
## 75% of the sample size
smp_size <- floor(0.75 * nrow(trialnet_age18))
## set the seed to make your partition reproducible
set.seed(193)

train_ind <- sample(seq_len(nrow(trialnet_age18)), size = smp_size)

train_age18 <- trialnet_age18[train_ind, ]
test_age18 <- trialnet_age18[-train_ind, ]

## generate linear predictors 
# train data
train_age18_lp_5years <- predict(teddyage7_original_res.cox, type = "lp", newdata=train_age18)
train_age18 <- train_age18 %>% 
  mutate(lp_5years = train_age18_lp_5years)
# test data
test_age18 <- test_age18 %>%
  mutate(number_autoantibody = ifelse(number_autoantibody == 3, 2, number_autoantibody),
         number_autoantibody = as.factor(number_autoantibody - 1))
test_age18_lp_5years <- predict(teddyage7_original_res.cox, type = "lp", newdata=test_age18)
test_age18 <- test_age18 %>% 
  mutate(lp_5years = test_age18_lp_5years)


# Recalibrated Model
age18_new_model_5years <- coxph(Surv(time,t1d) ~ lp_5years + strata(number_autoantibody), data= train_age18, x = TRUE)


# generate estimates
estimates_age18_5years <- 1-predictSurvProb(age18_new_model_5years,newdata= test_age18, times = predicted_horizon)
test_age18$old.estimates <- 1-predictSurvProb(teddyage7_original_res.cox, newdata = test_age18, times =  predicted_horizon + seven_year_model)[,1]
test_age18$new.estimates <- estimates_age18_5years


# calibration of stratified and recalibrated model
age18_recalibrate.cal_5years <- calibration_LF2_recalibration_test2(age18_new_model_5years,test_age18, year_prediction = predicted_horizon, estimates = estimates_age18_5years, group_name = "ageover18")
age18_recalibrate.cal_5years$model <- "Recalibrated Age 7 TEDDY Model in individuals aged 18+"

age18_original.cal <- calibration_LF2(model = model1, newdataset = test_age18, seven_year_model, year_prediction, model_type = "not strata")
age18_original.cal$model <- "Age 7 TEDDY Model applied in individuals aged 18+"

dt_18 <- rbind(age18_original.cal, age18_recalibrate.cal_5years)





################################################################
# Calibration plots 
################################################################
age1_7_graph <- ggplot(dt_1_7,aes(x = predict.grid, y = predict.calibrate, fill = model, color = model)) +
  geom_line() +
  geom_ribbon(aes(ymin = band.low, ymax = band.up), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) +
  geom_abline(intercept = 0, slope = 1, linetype="dotted") +
  xlab(paste0("Predicted probability - ",predicted_horizon,"-year horizon")) +
  ylab(paste0("Observed probability - ",predicted_horizon,"-year horizon")) +
  theme_presentation()
ggsave(here("figures/new_figures/calibration_age1_7.JPEG"), dpi = 300, width = 12, height = 7)

age7_18_graph <- ggplot(dt_7_18,aes(x = predict.grid, y = predict.calibrate, fill = model, color = model)) +
  geom_line() +
  geom_ribbon(aes(ymin = band.low, ymax = band.up), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) +
  geom_abline(intercept = 0, slope = 1, linetype="dotted") +
  xlab(paste0("Predicted probability - ",predicted_horizon,"-year horizon")) +
  ylab(paste0("Observed probability - ",predicted_horizon,"-year horizon")) +
  theme_presentation()
ggsave(here("figures/new_figures/calibration_age7_18.JPEG"), dpi = 300, width = 12, height = 7)

age18_graph <- ggplot(dt_18,aes(x = predict.grid, y = predict.calibrate, fill = model, color = model)) +
  geom_line() +
  geom_ribbon(aes(ymin = band.low, ymax = band.up), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) +
  geom_abline(intercept = 0, slope = 1, linetype="dotted") +
  xlab(paste0("Predicted probability - ",predicted_horizon,"-year horizon")) +
  ylab(paste0("Observed probability - ",predicted_horizon,"-year horizon")) +
  theme_presentation()
ggsave(here("figures/new_figures/calibration_age18.JPEG"), dpi = 300, width = 12, height = 7)




################################################################
# Density Plots
################################################################

density1_7 <- ggplot(test_age1_7) +
  geom_histogram(aes(x = old.estimates), color = "grey", fill="red", alpha = 0.5, bins = 15) + 
  geom_histogram(aes(x = new.estimates), color = "grey", fill="lightblue", alpha = 0.5, bins = 15) + theme_presentation() + 
  coord_cartesian(xlim = c(0, 1))
ggsave(here("figures/new_figures/density_age1_7.JPEG"), dpi = 300, width = 12, height = 7)

density7_18 <- ggplot(test_age7_18) +
  geom_histogram(aes(x = old.estimates), color = "grey", fill="red", alpha = 0.5, bins = 15) + 
  geom_histogram(aes(x = new.estimates), color = "grey", fill="lightblue", alpha = 0.5, bins = 15) + theme_presentation() +
  coord_cartesian(xlim = c(0, 1))
ggsave(here("figures/new_figures/density_age7_18.JPEG"), dpi = 300, width = 12, height = 7)

density18 <- ggplot(test_age18) +
  geom_histogram(aes(x = old.estimates), color = "grey", fill="red", alpha = 0.5, bins = 15) + 
  geom_histogram(aes(x = new.estimates), color = "grey", fill="lightblue", alpha = 0.5, bins = 15) + theme_presentation() + 
  coord_cartesian(xlim = c(0, 1))
ggsave(here("figures/new_figures/density_age18.JPEG"), dpi = 300, width = 12, height = 7)



################################################################
# All recalibration model calibration lines
################################################################

age1_7_recalibrate.cal_5years$`Screened Age (years)` <- "Age 1 - 7"
age7_18_recalibrate.cal_5years$`Screened Age (years)` <- "Age 8 - 17"
age18_recalibrate.cal_5years$`Screened Age (years)` <- "Age 18 +"
dt <- rbind(age1_7_recalibrate.cal_5years,age7_18_recalibrate.cal_5years,age18_recalibrate.cal_5years)
dt$`Screened Age (years)` <- factor(dt$`Screened Age (years)`, levels = c("Age 1 - 7", "Age 8 - 17", "Age 18 +"))

all_recalibration <- ggplot(dt,aes(x = predict.grid, y = predict.calibrate, fill = `Screened Age (years)`, color = `Screened Age (years)`)) +
  geom_line() +
  geom_ribbon(aes(ymin = band.low, ymax = band.up), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) +
  scale_colour_manual(values = c("#601A4A","#EE442F","#63ACBE"))+
  scale_fill_manual(values = c("#601A4A","#EE442F","#63ACBE"))+
  geom_abline(intercept = 0, slope = 1, linetype="dotted") +
  xlab(paste0("Predicted probability - ",predicted_horizon,"-year horizon")) +
  ylab(paste0("Observed probability - ",predicted_horizon,"-year horizon")) +
  theme_presentation() + 
  theme(legend.key.width = unit(3,"cm"))
all_recalibration
ggsave(here("figures/new_figures/all_recalibrarion.JPEG"), dpi = 300, width = 13, height = 7)



################################################################
# Supplementary figure 5
################################################################
age1_7_original.cal$`Screened Age (years)` <- "Age 1 - 7"
age7_18_original.cal$`Screened Age (years)` <- "Age 8 - 17"
age18_original.cal$`Screened Age (years)` <- "Age 18 +"
dt <- rbind(age1_7_original.cal,age7_18_original.cal,age18_original.cal)
dt$`Screened Age (years)` <- factor(dt$`Screened Age (years)`, levels = c("Age 1 - 7", "Age 8 - 17", "Age 18 +"))

all_original <- ggplot(dt,aes(x = predict.grid, y = predict.calibrate, fill = `Screened Age (years)`, color = `Screened Age (years)`)) +
  geom_line() +
  geom_ribbon(aes(ymin = band.low, ymax = band.up), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) +
  scale_colour_manual(values = c("#601A4A","#EE442F","#63ACBE"))+
  scale_fill_manual(values = c("#601A4A","#EE442F","#63ACBE"))+
  geom_abline(intercept = 0, slope = 1, linetype="dotted") +
  xlab(paste0("Predicted probability - ",predicted_horizon,"-year horizon")) +
  ylab(paste0("Observed probability - ",predicted_horizon,"-year horizon")) +
  theme_presentation() + 
  theme(legend.key.width = unit(3,"cm"))
all_original
ggsave(here("figures/new_figures/all_original.JPEG"), dpi = 300, width = 13, height = 7)


dt_50 <- dt %>%
  mutate(nearest_observed = 0.5 - predict.calibrate) %>%
  group_by(`Screened Age (years)`) %>%
  slice_min(abs(nearest_observed), n = 1) %>%
  ungroup()


################################################################
# Save recalibration models
################################################################

save(age1_7_new_model_5years, file = here(paste0(pathSaveTNModels, "paper_models/age1_7_recalibrated.RData")))
save(age7_18_new_model_5years, file = here(paste0(pathSaveTNModels, "paper_models/age8_17_recalibrated.RData")))
save(age18_new_model_5years, file = here(paste0(pathSaveTNModels, "paper_models/age18_recalibrated.RData")))

################################################################
# Save TN datasets with linear predictors
################################################################
new_trialnet_1_7 <- rbind(train_age1_7, test_age1_7[,c(0:10)])
new_trialnet_8_17 <- rbind(train_age7_18, test_age7_18[,c(0:10)])
new_trialnet_18 <- rbind(train_age18, test_age18[,c(0:10)])

