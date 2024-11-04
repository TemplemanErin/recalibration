####### Calibration ##########
# largely inspired from https://onlinelibrary.wiley.com/doi/epdf/10.1002/sim.8570
library(survival)
library(rms)
library(pec)
library(polspline)
library(riskRegression)
library(ggplot2)
library(here)

pickmodel <- function(i,newdataset,year_prediction){
  agedays <- newdataset[i,"age"] * 365.25
  possible_choice_of_model <- c(60,365.25,365.25 + 183,365.25*2 + seq(0,5*365.25,365)) + 45
  day <- which.min(abs(possible_choice_of_model- (agedays + 45)))
  horizon <- round(possible_choice_of_model[day]/365.25)+year_prediction
  mylist <- list("agedays" = day, "horizon" = horizon)
  return(mylist)
}

prediction_f <- function(newdataset, year_prediction, model_type){
  n <- nrow(newdataset)
  prediction <- rep(0,n)
  horizons <- rep(0,n)
  pathSaveModels <- here("../teddy/models/strata/")
  
  possible_choice_of_model <- c(60,365.25,365.25 + 183,365.25*2 + seq(0,5*365.25,365)) + 45
  
  # load all the models 
  for (landmark in possible_choice_of_model){
    if (model_type == "strata"){
      names_tAUC <- paste0("Strata_Coxph_model_day_begin_",landmark)
      load(file = paste0(pathSaveModels,names_tAUC,".RData"))
      assign(paste0("res.cox",landmark),res.cox)
    } else {
      newnames_tAUC <- paste0("Coxph_model_day_begin_",landmark)
      newpathSaveModels <- here("../teddy/models/")
      load(file = paste0(newpathSaveModels,newnames_tAUC,".RData"))
      assign(paste0("res.cox",landmark),res.cox)
    }}
  
  for(i in 1:n){
    # pickmodel
    models <- pickmodel(i,newdataset, year_prediction)
    day <- models$agedays
    horizon <- models$horizon
    
    # assign the model to predict 
    res.cox <- get(paste0("res.cox",possible_choice_of_model[day]))
    
    #  compute prediction
    prediction[i] <- 1-predictSurvProb(res.cox,newdata= newdataset[i,] ,times=horizon)
    
    horizons[i] <- horizon
  }
  prediction_f_output <- list("horizon" = horizons, "predictions" = prediction)
  return(prediction_f_output)
}

prediction_recalibrate <- function(model, newdataset, year_prediction){
   n <- nrow(newdataset)
   prediction <- rep(0,n)
   horizons <- rep(0,n)

  for(i in 1:n){
    #  compute prediction
    prediction[i] <- 1-predictSurvProb(model,newdata= newdataset[i,] ,times=year_prediction)
    
    # horizons[i] <- year_prediction + model_prediction_year ## with model_prediction year -> changed 30/10/2022
    horizons[i] <- year_prediction
  }
  prediction_f_output <- list("horizon" = horizons, "predictions" = prediction)
  return(prediction_f_output)
}

calibration_LF2 <- function(model, newdataset, model_prediction_year, year_prediction, model_type, input_knots = NULL) {
  # largely inspired from article and particularly code provided in 
  # supplementary material at
  # https://onlinelibrary.wiley.com/doi/epdf/10.1002/sim.8570
  
  # Return error if dataset is less than 20 people
  if (nrow(newdataset) < 20){
    stop('More than 20 people are required for Calibration')
  }
  
  # prediction
  prediction_f_output <- prediction_f(newdata= newdataset, year_prediction=year_prediction, model_type = model_type)
  horizon <- prediction_f_output$horizon
  prediction <- prediction_f_output$predictions
  
  predict.cox <- prediction
  newdataset$prediction <- ifelse(predict.cox==1,0.999,predict.cox)
  newdataset$prediction.cll <- log(-log(1-newdataset$prediction))
  
  ################################################################################
  # Calibration for predictions survival probabilities
  ################################################################################
  # note to future myself, you changed the initial rcs(prediction.cll,3) by 
  # rcs(prediction.cll,c(",paste(knots,collapse = ","),"))" the idea is to put the knots position at meaningful position. 
  # this is due to the fact that in Teddy 95% of children have a probability to develop T1D close to zeros and 
  # so the splines would provide a poor fit by fitting only on the children with very low risk ( by default 3 knots are put at 0.25,0.5 and 0.75 of the population) 
  # and so a poor fit was obtained for high risk
  # you might spend more hours on this problem.
  
  knots <-  quantile(newdataset$prediction.cll, na.rm = TRUE, probs = seq(0, 1, 0.1))[c(2,5,9)]
  if(length(unique(knots)) < 3 & !(is.null(input_knots))){
    knots <- quantile(newdataset$prediction.cll, na.rm = TRUE, probs = seq(0, 1, 0.1))[c(input_knots[1],input_knots[2],input_knots[3])]
  }
  formula <- formula(paste0(deparse( terms(model$formula)[[2]]), "~ rcs(prediction.cll,c(",paste(knots,collapse = ","),"))"))
  calibrate.cox <- coxph(formula,x=T, data=newdataset)
  
  # predict.grid.cox <- seq(quantile(newdataset$prediction,probs=0.01, na.rm = TRUE), quantile(newdataset$prediction,probs=0.99, na.rm=TRUE),length=100)
  # predict.grid.cox <- seq(0.01,0.99, 0.01)
  predict.grid.cox <- seq(quantile(newdataset$prediction,probs=10/dim(newdataset)[1], na.rm = TRUE), quantile(newdataset$prediction,probs=(dim(newdataset)[1]-10)/dim(newdataset)[1], na.rm=TRUE),length=100)
  # predict.grid.cox <- seq(quantile(newdataset$prediction,probs=10/dim(newdataset)[2], na.rm = TRUE), quantile(newdataset$prediction,probs=(dim(newdataset)[2]-10)/dim(newdataset)[2], na.rm=TRUE),length=100)
  predict.grid.cox.cll.df <- data.frame(prediction.cll = log(-log(1-predict.grid.cox)))
  
  fit.pred <- predictCox(calibrate.cox, newdata=predict.grid.cox.cll.df, times=model_prediction_year + year_prediction, type = "survival", se = TRUE, iid = TRUE, band = TRUE)
  predict.calibrate.cox <- 1 - fit.pred$survival
  band.cox.up <- 1 - fit.pred$survival.lower
  band.cox.low <- 1 - fit.pred$survival.upper
  
  calibration <- data.frame(predict.grid = predict.grid.cox, predict.calibrate = predict.calibrate.cox,band.low = band.cox.low, band.up = band.cox.up)
  # return(list(calibration = calibration,raw_prediction = newdataset))
  return(calibration)
}


calibration_LF2_recalibration <- function(model, newdataset, 
                                          year_prediction,
                                          estimates = NULL, input_knots = NULL,
                                          method = NULL) {
  # largely inspired from article and particularly code provided in 
  # supplementary material at
  # https://onlinelibrary.wiley.com/doi/epdf/10.1002/sim.8570
  
  ## Recalibrated model is passed to function ---- > model_prediction_year variable is no longer required
  
  # Return error if dataset is less than 20 people
  if (nrow(newdataset) < 20){
    stop('More than 20 people are required for Calibration')
  }
  if (is.null(estimates)){
  # prediction
    prediction_f_output <- prediction_recalibrate(model=model, newdata=newdataset, year_prediction=year_prediction)
    horizon <- prediction_f_output$horizon
    prediction <- prediction_f_output$predictions
  
    predict.cox <- prediction}
  else if(!is.null(estimates)){ predict.cox <- estimates }
  
  newdataset$prediction <- ifelse(predict.cox==1,0.99,predict.cox)
  newdataset$prediction.cll <- log(-log(1-newdataset$prediction))
  
  ################################################################################
  # Calibration for predictions survival probabilities
  ################################################################################
  # note to future myself, you changed the initial rcs(prediction.cll,3) by 
  # rcs(prediction.cll,c(",paste(knots,collapse = ","),"))" the idea is to put the knots position at meaningful position. 
  # this is due to the fact that in Teddy 95% of children have a probability to develop T1D close to zeros and 
  # so the splines would provide a poor fit by fitting only on the children with very low risk ( by default 3 knots are put at 0.25,0.5 and 0.75 of the population) 
  # and so a poor fit was obtained for high risk
  # you might spend more hours on this problem.
  
  all_knots <-  quantile(newdataset$prediction.cll, na.rm = TRUE, probs = seq(0, 1, 0.1))
  knots <- all_knots[c(2,5,9)]
  if(!(is.null(input_knots))){
    knots <- all_knots[c(input_knots[1], input_knots[2], input_knots[3])]
  }

  if(!is.null(method)){
    formula <- formula(paste0("Surv(time,t1d)", "~ rcs(prediction.cll,c(",paste(knots,collapse = ","),"))"))
  }
  else{
    formula <- formula(paste0(deparse( terms(model$formula)[[2]]), "~ rcs(prediction.cll,c(",paste(knots,collapse = ","),"))"))
  }
  calibrate.cox <- coxph(formula,x=T, data=newdataset)
  
  predict.grid.cox <- seq(quantile(newdataset$prediction,probs=0.01, na.rm = TRUE), quantile(newdataset$prediction,probs=0.99, na.rm=TRUE),length=100)
  prediction.cll <- log(-log(1-predict.grid.cox))
  prediction.cll[is.nan(prediction.cll)] <- 0
  predict.grid.cox.cll.df <- data.frame(prediction.cll)

  
  fit.pred <- predictCox(calibrate.cox, newdata=predict.grid.cox.cll.df, times=year_prediction, type = "survival", se = TRUE, iid = TRUE, band = TRUE)
  predict.calibrate.cox <- 1 - fit.pred$survival
  band.cox.up <- 1 - fit.pred$survival.lower
  band.cox.low <- 1 - fit.pred$survival.upper
  
  calibration <- data.frame(predict.grid = predict.grid.cox, predict.calibrate = predict.calibrate.cox,band.low = band.cox.low, band.up = band.cox.up)
  calibration <- calibration %>%  filter(!row_number() %in% which(prediction.cll == 0))
  # return(list(calibration = calibration,raw_prediction = newdataset))
  return(calibration)
}

trialnet_calibration <- function(model, newdataset, year_prediction) {
  # largely inspired from article and particularly code provided in 
  # supplementary material at
  # https://onlinelibrary.wiley.com/doi/epdf/10.1002/sim.8570
  
  # Return error if dataset is less than 20 people
  if (nrow(newdataset) < 20){
    stop('More than 20 people are required for Calibration')
  }
  n <- nrow(newdataset)
  prediction <- rep(0,n)
  
  # prediction
  for(i in 1:n){
    #  compute prediction
    prediction[i] <- 1-predictSurvProb(model,newdata= newdataset[i,] ,times=year_prediction)
  }
  
  predict.cox <- prediction
  newdataset$prediction <- ifelse(predict.cox==1,0.999,predict.cox)
  newdataset$prediction.cll <- log(-log(1-newdataset$prediction))
  
  ################################################################################
  # Calibration for predictions survival probabilities
  ################################################################################
  # note to future myself, you changed the initial rcs(prediction.cll,3) by 
  # rcs(prediction.cll,c(",paste(knots,collapse = ","),"))" the idea is to put the knots position at meaningful position. 
  # this is due to the fact that in Teddy 95% of children have a probability to develop T1D close to zeros and 
  # so the splines would provide a poor fit by fitting only on the children with very low risk ( by default 3 knots are put at 0.25,0.5 and 0.75 of the population) 
  # and so a poor fit was obtained for high risk
  # you might spend more hours on this problem.
  
  knots <-  quantile(newdataset$prediction.cll, na.rm = TRUE, probs = seq(0, 1, 0.1))[c(2,5,9)]
  formula <- formula(paste0(deparse( terms(model$formula)[[2]]), "~ rcs(prediction.cll,c(",paste(knots,collapse = ","),"))"))
  calibrate.cox <- coxph(formula,x=T, data=newdataset)
  
  predict.grid.cox <- seq(quantile(newdataset$prediction,probs=0.01, na.rm = TRUE), quantile(newdataset$prediction,probs=0.99, na.rm=TRUE),length=100)
  predict.grid.cox.cll.df <- data.frame(prediction.cll = log(-log(1-predict.grid.cox)))
  
  fit.pred <- predictCox(calibrate.cox, newdata=predict.grid.cox.cll.df, times=year_prediction, type = "survival", se = TRUE, iid = TRUE, band = TRUE)
  predict.calibrate.cox <- 1 - fit.pred$survival
  band.cox.up <- 1 - fit.pred$survival.lower
  band.cox.low <- 1 - fit.pred$survival.upper
  
  calibration <- data.frame(predict.grid = predict.grid.cox, predict.calibrate = predict.calibrate.cox,band.low = band.cox.low, band.up = band.cox.up)
  # return(list(calibration = calibration,raw_prediction = newdataset))
  return(calibration)
}

calibration_LF2_recalibration_test <- function(model, newdataset, year_prediction, estimates = NULL) {
  # largely inspired from article and particularly code provided in 
  # supplementary material at
  # https://onlinelibrary.wiley.com/doi/epdf/10.1002/sim.8570
  
  # Return error if dataset is less than 20 people
  if (nrow(newdataset) < 20){
    stop('More than 20 people are required for Calibration')
  }
  if (is.null(estimates)){
    # prediction
    prediction_f_output <- prediction_recalibrate(model=model, newdata=newdataset, year_prediction=year_prediction)
    horizon <- prediction_f_output$horizon
    prediction <- prediction_f_output$predictions
    
    predict.cox <- prediction}
  else if(!is.null(estimates)){ predict.cox <- estimates }
  
  newdataset$prediction <- ifelse(predict.cox==1,0.99,predict.cox)
  newdataset$prediction.cll <- log(-log(1-newdataset$prediction))
  
  ################################################################################
  # Calibration for predictions survival probabilities
  ################################################################################
  # note to future myself, you changed the initial rcs(prediction.cll,3) by 
  # rcs(prediction.cll,c(",paste(knots,collapse = ","),"))" the idea is to put the knots position at meaningful position. 
  # this is due to the fact that in Teddy 95% of children have a probability to develop T1D close to zeros and 
  # so the splines would provide a poor fit by fitting only on the children with very low risk ( by default 3 knots are put at 0.25,0.5 and 0.75 of the population) 
  # and so a poor fit was obtained for high risk
  # you might spend more hours on this problem.
  
  all_knots <-  quantile(newdataset$prediction.cll, na.rm = TRUE, probs = seq(0, 1, 0.1))
  knots <- all_knots[c(2,9,10)]
  #if(length(unique(knots))<3){
  #  knots <- all_knots[c(1,5,10)]
  #  if(length(unique(knots))<3){
  #    new_knot <- all_knots[all_knots > min(knots) & all_knots < max(knots)][1]
  #    knots <- c(knots[1], new_knot, knots[3])
  #    }
  #  }
  formula <- formula(paste0(deparse( terms(model$formula)[[2]]), "~ rcs(prediction.cll,c(",paste(knots,collapse = ","),"))"))
  calibrate.cox <- coxph(formula,x=T, data=newdataset)
  
  predict.grid.cox <- seq(quantile(newdataset$prediction,probs=0.01, na.rm = TRUE), quantile(newdataset$prediction,probs=0.99, na.rm=TRUE),length=100)
  predict.grid.cox.cll.df <- data.frame(prediction.cll = log(-log(1-predict.grid.cox)))
  
  fit.pred <- predictCox(calibrate.cox, newdata=predict.grid.cox.cll.df, times=year_prediction, type = "survival", se = TRUE, iid = TRUE, band = TRUE)
  predict.calibrate.cox <- 1 - fit.pred$survival
  band.cox.up <- 1 - fit.pred$survival.lower
  band.cox.low <- 1 - fit.pred$survival.upper
  
  calibration <- data.frame(predict.grid = predict.grid.cox, predict.calibrate = predict.calibrate.cox,band.low = band.cox.low, band.up = band.cox.up)
  # return(list(calibration = calibration,raw_prediction = newdataset))
  return(calibration)
}



calibration_LF2_recalibration_test2 <- function(model, newdataset, year_prediction, 
                                                group_name, estimates = NULL, 
                                                input_knots = NULL, method = NULL) {
  # largely inspired from article and particularly code provided in 
  # supplementary material at
  # https://onlinelibrary.wiley.com/doi/epdf/10.1002/sim.8570
  
  # Return error if dataset is less than 20 people
  if (nrow(newdataset) < 20){
    stop('More than 20 people are required for Calibration')
  }
  if (is.null(estimates)){
    # prediction
    prediction_f_output <- prediction_recalibrate(model=model, newdata=newdataset, year_prediction=year_prediction)
    horizon <- prediction_f_output$horizon
    prediction <- prediction_f_output$predictions
    
    predict.cox <- prediction}
  else if(!is.null(estimates)){ predict.cox <- estimates }
  
  newdataset$prediction <- ifelse(predict.cox==1,0.99,predict.cox)
  newdataset$prediction.cll <- log(-log(1-newdataset$prediction))
  
  ################################################################################
  # Calibration for predictions survival probabilities
  ################################################################################
  # note to future myself, you changed the initial rcs(prediction.cll,3) by 
  # rcs(prediction.cll,c(",paste(knots,collapse = ","),"))" the idea is to put the knots position at meaningful position. 
  # this is due to the fact that in Teddy 95% of children have a probability to develop T1D close to zeros and 
  # so the splines would provide a poor fit by fitting only on the children with very low risk ( by default 3 knots are put at 0.25,0.5 and 0.75 of the population) 
  # and so a poor fit was obtained for high risk
  # you might spend more hours on this problem.
  
  all_knots <-  quantile(newdataset$prediction.cll, na.rm = TRUE, probs = seq(0, 1, 0.1))
  knots <- all_knots[c(2,5,9)]
  if(!(is.null(input_knots))){
    knots <- all_knots[c(input_knots[1], input_knots[2], input_knots[3])]
  }
  #if(length(unique(knots))<3){
  #  knots <- all_knots[c(1,5,10)]
  #  if(length(unique(knots))<3){
  #    new_knot <- all_knots[all_knots > min(knots) & all_knots < max(knots)][1]
  #    knots <- c(knots[1], new_knot, knots[3])
  #    }
  #  }
  if(!is.null(method)){
    formula <- formula(paste0("Surv(time,t1d)", "~ rcs(prediction.cll,c(",paste(knots,collapse = ","),"))"))
  }
  else{
    formula <- formula(paste0(deparse( terms(model$formula)[[2]]), "~ rcs(prediction.cll,c(",paste(knots,collapse = ","),"))"))
  }
  calibrate.cox <- coxph(formula,x=T, data=newdataset)
  names_recal_model <- paste0("Recalibrated_model_",group_name)
  save(list = c("calibrate.cox"), file = paste0(pathSaveTNModels,names_recal_model,".RData"))
  
  predict.grid.cox <- seq(quantile(newdataset$prediction,probs=10/dim(newdataset)[1], na.rm = TRUE), quantile(newdataset$prediction,probs=(dim(newdataset)[1]-10)/dim(newdataset)[1], na.rm=TRUE),length=100)
  #predict.grid.cox <- seq(quantile(newdataset$prediction,probs=10/dim(newdataset)[2], na.rm = TRUE), quantile(newdataset$prediction,probs=(dim(newdataset)[2]-10)/dim(newdataset)[2], na.rm=TRUE),length=100)
  #predict.grid.cox <- seq(0.01,0.99,0.01)
  prediction.cll <- log(-log(1-predict.grid.cox))
  prediction.cll[is.nan(prediction.cll)] <- 0
  #prediction.cll <- prediction.cll[complete.cases(prediction.cll)]
  
  predict.grid.cox.cll.df <- data.frame(prediction.cll)
  
  fit.pred <- predictCox(calibrate.cox, newdata=predict.grid.cox.cll.df, times=year_prediction, type = "survival", se = TRUE, iid = TRUE, band = TRUE)
  predict.calibrate.cox <- 1 - fit.pred$survival
  band.cox.up <- 1 - fit.pred$survival.lower
  band.cox.low <- 1 - fit.pred$survival.upper
  
  calibration <- data.frame(predict.grid = predict.grid.cox, predict.calibrate = predict.calibrate.cox,band.low = band.cox.low, band.up = band.cox.up)
  calibration <- calibration %>%  filter(!row_number() %in% which(prediction.cll == 0))
  # return(list(calibration = calibration,raw_prediction = newdataset))
  return(calibration)
}

