#### TrialNet age groups calibrated in TEDDY Model #####

################################################################
# pre-requisites / dependencies
################################################################


# source(here("R/recalibration_paper/load_data.R"))



################################################################
# TrialNet below 7 applied to TEDDY stratified model
################################################################
stratified.Scal_1 <- calibration_LF2(model = model1, newdataset = trialnet %>% filter(age <= 7), model_prediction_year = 1, year_prediction = predicted_horizon, model_type = "strata")
stratified.Scal_1$model <- "Stratified 1 teddy model, age birth to 7"

stratified.Scal_15 <- calibration_LF2(model = model1, newdataset = trialnet %>% filter(age <= 7), model_prediction_year = 1.5, year_prediction = predicted_horizon, model_type = "strata")
stratified.Scal_15$model <- "Stratified 1.5 teddy model, age birth to 7"

stratified.Scal_2 <- calibration_LF2(model = model1, newdataset = trialnet %>% filter(age <= 7), model_prediction_year = 2, year_prediction = predicted_horizon, model_type = "strata")
stratified.Scal_2$model <- "Stratified 2 teddy model, age birth to 7"

stratified.Scal_3 <- calibration_LF2(model = model1, newdataset = trialnet %>% filter(age <= 7), model_prediction_year = 3, year_prediction = predicted_horizon, model_type = "strata")
stratified.Scal_3$model <- "Stratified 3 teddy model, age birth to 7"

stratified.Scal_4 <- calibration_LF2(model = model1, newdataset = trialnet %>% filter(age <= 7), model_prediction_year = 4, year_prediction = predicted_horizon, model_type = "strata")
stratified.Scal_4$model <- "Stratified 4 teddy model, age birth to 7"

stratified.Scal_5 <- calibration_LF2(model = model1, newdataset = trialnet %>% filter(age <= 7), model_prediction_year = 5, year_prediction = predicted_horizon, model_type = "strata")
stratified.Scal_5$model <- "Stratified 5 teddy model, age birth to 7"

stratified.Scal_6 <- calibration_LF2(model = model1, newdataset = trialnet %>% filter(age <= 7), model_prediction_year = 6, year_prediction = predicted_horizon, model_type = "strata")
stratified.Scal_6$model <- "Stratified 6 teddy model, age birth to 7"

stratified.Scal_7 <- calibration_LF2(model = model1, newdataset = trialnet %>% filter(age <= 7), model_prediction_year = 7, year_prediction = predicted_horizon, model_type = "strata")
stratified.Scal_7$model <- "Stratified 7 teddy model, age birth to 7"


dt <- rbind(stratified.Scal_1, stratified.Scal_15, stratified.Scal_2, stratified.Scal_3, stratified.Scal_4, stratified.Scal_5, stratified.Scal_6, stratified.Scal_7)

plot2 <- ggplot(dt,aes(x = predict.grid, y = predict.calibrate, fill = model, color = model)) +
  geom_line() +
  geom_ribbon(aes(ymin = band.low, ymax = band.up), alpha = 0.3, show.legend = FALSE, linetype = 0) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 1)) +
  geom_abline(intercept = 0, slope = 1, linetype="dotted") +
  xlab(paste0("Predicted probability - ",predicted_horizon,"-year horizon")) +
  ylab(paste0("Observed probability - ",predicted_horizon,"-year horizon")) +
  theme_presentation()

ggsave(here("figures/paper_figures/choice_of_landmark_model.JPEG"), dpi = 300,
       width = 12, height = 7)