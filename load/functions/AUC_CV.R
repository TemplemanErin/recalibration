
library(survival)
library(rms)
library(pec)
library(polspline)
library(riskRegression)


## Functions include:
# - AUC_CV
# - AUC_CV_control
# - AUC_CV_val
# - AUC_CV_recalibrate
# - AUC_CV_external


AUC_CV <- function(variables, outcome_name, time_name,
                   horizon_time_v, k, dev_data, repk = 10) {
  # repk = 3 for repeated cross-validation. Changes everytime so take average
  set.seed(1234)
  
  # Import data
  dev_data <- dev_data
  
  formula <- as.formula(paste0("Surv(",time_name,",",outcome_name,") ~",
                               paste(variables, collapse = "+")))
  
  n_time_end <- length(horizon_time_v)
  
  # to store the results
  AUC_m_k <- matrix(nrow = k*repk, ncol = n_time_end)
  
  for(j in 1:repk){
    #Randomly shuffle the data
    dev_data <- dev_data[sample(nrow(dev_data)), ]
    #Create k equally size folds
    #folds <- cut(seq(1,nrow(dev_data)),breaks = k,labels = FALSE)
    folds <- createFolds(factor(dev_data$t1d), k = k, list = FALSE)
    
    #Perform 10 fold cross validation
    for (i in 1:k) {
      #Segment data by fold
      testIndexes <- which(folds == i,arr.ind = TRUE) 
      # ^^ returns the position or the index of the value satisfying the given condition
      testData <- dev_data[testIndexes, ]
      trainData <- dev_data[-testIndexes, ]

      #compute Cox model
      res.cox <- coxph(formula, data = trainData, x=TRUE, y=TRUE)

      ##compute AUC score with score achieve with Cox model
      delta <- testData[[outcome_name]]
      AUC <- c()

      for(h in 1:length(horizon_time_v)){
        skip_to_next <- FALSE
        # you need to put the good way to compute stratified cox model at the good horizon time
        year_prediction <- (day+ horizon_time_v[h])/365.25
        tryCatch(1-predictSurvProb(res.cox, newdata=testData, times=year_prediction),
                 error = function (e){skip_to_next <<- TRUE})
        if (skip_to_next == FALSE){
          marker_score <- 1-predictSurvProb(res.cox, newdata=testData, times=year_prediction)
          # check model is fitted in years
          tryCatch(timeROC(T = testData[[time_name]],
                           delta = delta,
                           marker = marker_score,
                           cause = 1,
                           times = year_prediction), error = function(e){skip_to_next <<- TRUE})
          }
        if (skip_to_next) {AUC <- c(AUC, NA)}
        else {
            ROC.T <- timeROC(T = testData[[time_name]],
                             delta = delta,
                             marker = marker_score,
                             cause = 1,
                             times = year_prediction)
            AUC <- c(AUC, ROC.T$AUC[[2]])}
        }
      AUC_m_k[i+(j-1)*k,] <- AUC
    }
    }
  AUC_m_k <- data.frame(AUC_m_k)
  colnames(AUC_m_k) <- horizon_time_v/365.25
  return(AUC_m_k)
} 

AUC_CV_control <- function(outcome_name, time_name,
                   horizon_time_v, k, dev_data, repk = 10) {
  # repk = 3 for repeated cross-validation. Changes everytime so take average
  set.seed(1234)
  
  # Import data
  dev_data <- dev_data
  
  formula <- as.formula(paste0("Surv(",time_name,",",outcome_name,") ~",1))
  
  n_time_end <- length(horizon_time_v)
  
  # to store the results
  AUC_m_k <- matrix(nrow = k*repk, ncol = n_time_end)
  
  for(j in 1:repk){
    #Randomly shuffle the data
    dev_data <- dev_data[sample(nrow(dev_data)), ]
    #Create k equally size folds
    #folds <- cut(seq(1,nrow(dev_data)),breaks = k,labels = FALSE)
    folds <- createFolds(factor(dev_data$t1d), k = k, list = FALSE)
    
    #Perform 10 fold cross validation
    for (i in 1:k) {
      #Segment data by fold
      testIndexes <- which(folds == i,arr.ind = TRUE) 
      # ^^ returns the position or the index of the value satisfying the given condition
      testData <- dev_data[testIndexes, ]
      trainData <- dev_data[-testIndexes, ]
      
      #compute Cox model
      res.cox <- coxph(formula, data = trainData, x=TRUE, y=TRUE)
      
      ##compute AUC score with score achieve with Cox model
      delta <- testData[[outcome_name]]
      AUC <- c()
      
      for(h in 1:length(horizon_time_v)){
        skip_to_next <- FALSE
        # you need to put the good way to compute stratified cox model at the good horizon time
        year_prediction <- (day+ horizon_time_v[h])/365.25
        tryCatch(1-predictSurvProb(res.cox, newdata=testData, times=year_prediction),
                 error = function (e){skip_to_next <<- TRUE})
        if (skip_to_next == FALSE){
          marker_score <- 1-predictSurvProb(res.cox, newdata=testData, times=year_prediction)
          # check model is fitted in years
          tryCatch(timeROC(T = testData[[time_name]],
                           delta = delta,
                           marker = marker_score,
                           cause = 1,
                           times = year_prediction), error = function(e){skip_to_next <<- TRUE})
        }
        if (skip_to_next) {AUC <- c(AUC, NA)}
        else {
          ROC.T <- timeROC(T = testData[[time_name]],
                           delta = delta,
                           marker = marker_score,
                           cause = 1,
                           times = year_prediction)
          AUC <- c(AUC, ROC.T$AUC[[2]])}
      }
      AUC_m_k[i+(j-1)*k,] <- AUC
    }
  }
  AUC_m_k <- data.frame(AUC_m_k)
  colnames(AUC_m_k) <- horizon_time_v/365.25
  return(AUC_m_k)
} 

AUC_CV_val <- function(variables, outcome_name, time_name,
                   horizon_time_v, k, dev_data, repk = 10, test_boundaries = NULL) {
  # repk = 3 for repeated cross-validation. Changes everytime so take average
  set.seed(1234)
  
  # Import data
  dev_data <- dev_data
  
  formula <- as.formula(paste0("Surv(",time_name,",",outcome_name,") ~",
                               paste(variables, collapse = "+")))
  
  n_time_end <- length(horizon_time_v)
  
  # to store the results
  AUC_m_k <- matrix(nrow = k*repk, ncol = n_time_end)
  
  for(j in 1:repk){
    #Randomly shuffle the data
    dev_data <- dev_data[sample(nrow(dev_data)), ]
    #Create k equally size folds
    #folds <- cut(seq(1,nrow(dev_data)),breaks = k,labels = FALSE)
    folds <- createFolds(factor(dev_data$t1d), k = k, list = FALSE)
    
    #Perform 10 fold cross validation
    for (i in 1:k) {
      #Segment data by fold
      testIndexes <- which(folds == i,arr.ind = TRUE) 
      # ^^ returns the position or the index of the value satisfying the given condition
      testData <- dev_data[testIndexes, ]
      trainData <- dev_data[-testIndexes, ]
      
      if(!is.null(test_boundaries)){
        testData <- testData %>% filter(age_group == test_boundaries)
      }
      
      #compute Cox model
      res.cox <- coxph(formula, data = trainData, x=TRUE, y=TRUE)
      
      ##compute AUC score with score achieve with Cox model
      delta <- testData[[outcome_name]]
      AUC <- c()
      
      for(h in 1:length(horizon_time_v)){
        skip_to_next <- FALSE
        # you need to put the good way to compute stratified cox model at the good horizon time
        year_prediction <- (day+ horizon_time_v[h])/365.25
        tryCatch(1-predictSurvProb(res.cox, newdata=testData, times=year_prediction),
                 error = function (e){skip_to_next <<- TRUE})
        if (skip_to_next == FALSE){
          marker_score <- 1-predictSurvProb(res.cox, newdata=testData, times=year_prediction)
          # check model is fitted in years
          tryCatch(timeROC(T = testData[[time_name]],
                           delta = delta,
                           marker = marker_score,
                           cause = 1,
                           times = year_prediction), error = function(e){skip_to_next <<- TRUE})
        }
        if (skip_to_next) {AUC <- c(AUC, NA)}
        else {
          ROC.T <- timeROC(T = testData[[time_name]],
                           delta = delta,
                           marker = marker_score,
                           cause = 1,
                           times = year_prediction)
          AUC <- c(AUC, ROC.T$AUC[[2]])}
      }
      AUC_m_k[i+(j-1)*k,] <- AUC
    }
  }
  AUC_m_k <- data.frame(AUC_m_k)
  colnames(AUC_m_k) <- horizon_time_v/365.25
  return(AUC_m_k)
} 



AUC_CV_recalibrate <- function(dev_data, time_name, outcome_name, old_model,
                               k, horizon_time_v, repk = 10, nAB_ammend = NULL) {
  # repk = 3 for repeated cross-validation. Changes everytime so take average
  set.seed(1234)
  
  n_time_end <- length(horizon_time_v)
  
  # to store the results
  AUC_m_k <- matrix(nrow = k*repk, ncol = n_time_end)
  
  for(j in 1:repk){
    #Randomly shuffle the data
    dev_data <- dev_data[sample(nrow(dev_data)), ]
    #Create k equally size folds
    #folds <- cut(seq(1,nrow(dev_data)),breaks = k,labels = FALSE)
    folds <- createFolds(factor(dev_data$t1d), k = k, list = FALSE)
    
    #Perform 10 fold cross validation
    for (i in 1:k) {
      #Segment data by fold
      testIndexes <- which(folds == i,arr.ind = TRUE) 
      # ^^ returns the position or the index of the value satisfying the given condition
      testData <- dev_data[testIndexes, ]
      trainData <- dev_data[-testIndexes, ]
      
      if(!is.null(nAB_ammend)){
        testData <- testData %>%
          mutate(number_autoantibody = ifelse(number_autoantibody == 3, 2, number_autoantibody),
                 number_autoantibody = as.factor(number_autoantibody - 1))
      }


      ##compute AUC score with score achieve with Cox model
      AUC <- c()
      
      for(h in 1:length(horizon_time_v)){
        year_prediction = horizon_time_v[h]
        
        ### recalibrate model
        
        trialnet_lp<- predict(old_model, type = "lp", newdata=trainData)
        trainData <- trainData %>% mutate(lp = trialnet_lp) %>% filter(!is.na(lp))
        
        new_trialnet_lp<- predict(old_model, type = "lp", newdata=testData)
        testData <- testData %>% mutate(lp = new_trialnet_lp) %>% filter(!is.na(lp))

        new_model <- coxph(Surv(time,t1d) ~ strata(number_autoantibody)*(lp),
                                 data= trainData, x=T, y=T)

        marker_score <- 1-predictSurvProb(new_model, newdata=testData, times=year_prediction)
        
        delta <- testData[[outcome_name]]
        
        ROC.T <- timeROC(T = testData[[time_name]],
                delta = delta,
                marker = marker_score,
                cause = 1,
                times = year_prediction)
        
        AUC <- c(AUC, ROC.T$AUC[[2]])
      }
      AUC_m_k[i+(j-1)*k,] <- AUC
    }
  }
  AUC_m_k <- data.frame(AUC_m_k)
  colnames(AUC_m_k) <- horizon_time_v
  return(AUC_m_k)
} 


AUC_CV_external <- function(old_model, old_model_prediction_year, outcome_name, time_name, 
                            val_data, k, horizon_time_v, repk = 10) {
  # repk = 3 for repeated cross-validation. Changes everytime so take average
  set.seed(1234)
  
  n_time_end <- length(horizon_time_v)
  
  # to store the results
  AUC_m_k <- matrix(nrow = k*repk, ncol = n_time_end)
  
  for(j in 1:repk){
    #Randomly shuffle the data
    val_data <- val_data[sample(nrow(val_data)), ]
    #Create k equally size folds
    folds <- createFolds(factor(val_data$t1d), k = k, list = FALSE)
    
    #Perform 10 fold cross validation
    for (i in 1:k) {
      #Segment data by fold
      testIndexes <- which(folds == i,arr.ind = TRUE) 
      # ^^ returns the position or the index of the value satisfying the given condition
      testData <- val_data[testIndexes, ]
      trainData <- val_data[-testIndexes, ]
      
      ##compute AUC score with score achieve with Cox model
      AUC <- c()
      
      for(h in 1:length(horizon_time_v)){
        year_prediction = horizon_time_v[h] + old_model_prediction_year
      
        marker_score <- 1-predictSurvProb(old_model, newdata=testData, times=year_prediction)
        
        delta <- testData[[outcome_name]]
        
        ROC.T <- timeROC(T = testData[[time_name]],
                         delta = delta,
                         marker = marker_score,
                         cause = 1,
                         times = year_prediction)
        
        AUC <- c(AUC, ROC.T$AUC[[2]])
      }
      AUC_m_k[i+(j-1)*k,] <- AUC
    }
  }
  AUC_m_k <- data.frame(AUC_m_k)
  colnames(AUC_m_k) <- horizon_time_v
  return(AUC_m_k)
} 
