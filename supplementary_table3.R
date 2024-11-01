#### individualised risk prediction #####

################################################################
# supplementary figure
################################################################
predicted_horizon <- 5
result_time_serie_proba <- list()

fdr <- rep(c(1,1,1), 6)
age <- rep(c(4,15,20), 6)
GRS2 <- rep(c(11.68, 11.68, 11.68, 15.615689,15.615689,15.615689), 3)
number_autoantibody <- rep(c(1,2,3),each = 6)
peopleID <- 1:18

dataset_ml <- data.frame(peopleID,age, GRS2, number_autoantibody, fdr) %>% mutate(fdr = as.factor(fdr), number_autoantibody = factor(number_autoantibody))


dataset_ml_under7 <- dataset_ml %>% filter(age <= 7)
dataset_ml_over7 <- dataset_ml %>% filter(age > 7 & age < 18) 
dataset_ml_over18 <- dataset_ml %>% filter(age >= 18) %>%
  mutate(number_autoantibody = ifelse(number_autoantibody == 3, 2, number_autoantibody),
         number_autoantibody = as.factor(number_autoantibody))

levels(dataset_ml_over7$number_autoantibody)
levels(dataset_ml_over18$number_autoantibody)

lp_under7 <- predict(teddyage4_res.cox, type = "lp", newdata=dataset_ml_under7)
lp_over7 <- predict(teddyage7_res.cox, type = "lp", newdata=dataset_ml_over7)
lp_over18 <- predict(teddyage7_res.cox, type = "lp", newdata=dataset_ml_over18)

dataset_ml_under7 <- dataset_ml_under7 %>% mutate(lp = lp_under7)
dataset_ml_over7 <- dataset_ml_over7 %>% mutate(lp = lp_over7)
dataset_ml_over18 <- dataset_ml_over18 %>% mutate(lp = lp_over18)

dataset_ml <- rbind(dataset_ml_under7, dataset_ml_over7, dataset_ml_over18)
dataset_ml <- arrange(dataset_ml, peopleID)

load("~/Dropbox/ErinPhDclass/trialnet/models/app_models/TrialNet_strata_model_1-7.RData")
tn_res.cox1 <- res.cox
load("~/Dropbox/ErinPhDclass/trialnet/models/app_models/TrialNet_strata_model_8-17.RData")
tn_res.cox2 <- res.cox
load("~/Dropbox/ErinPhDclass/trialnet/models/app_models/TrialNet_strata_model_18+.RData")
tn_res.cox3 <- res.cox


colnames <- c("time", "n.risk", "n.event","survival","std.err","lower","upper")
dataframe <- data.frame(matrix(nrow = length(peopleID), ncol = length(colnames)))
k <- 1
# compute the probability to develop T1D
for (people in peopleID) {
  personID <- people
  # collect the coefficient for each landmark point
  if (dim(dataset_ml[dataset_ml$peopleID == personID,])[1] != 0) {
    personID_age <- dataset_ml[dataset_ml$peopleID == personID,"age"]
    if(personID_age <= 7){
      prediction_i <- summary(survfit(tn_res.cox1,dataset_ml[dataset_ml$peopleID == personID,]), times = predicted_horizon, extend= TRUE) # https://kkoehler.public.iastate.edu/stat565/coxph.4page.pdf
      print(paste0("1",personID_age))
    } else if(personID_age > 7 & personID_age < 18){
      prediction_i <- summary(survfit(tn_res.cox2,dataset_ml[dataset_ml$peopleID == personID,]), times = predicted_horizon, extend= TRUE) # https://kkoehler.public.iastate.edu/stat565/coxph.4page.pdf
      print(paste0("2",personID_age))
    } else{
      prediction_i <- summary(survfit(tn_res.cox3,dataset_ml[dataset_ml$peopleID == personID,]), times = predicted_horizon, extend= TRUE) # https://kkoehler.public.iastate.edu/stat565/coxph.4page.pdf
      print(paste0("3",personID_age))
    }
    #prediction_df[k,] <- c(prediction_i$time,prediction_i$n.risk,prediction_i$n.event,prediction_i$surv,prediction_i$std.err,prediction_i$lower,prediction_i$upper)
    #k <- k + 1
    prediction <- c(prediction_i$time,prediction_i$n.risk,prediction_i$n.event,prediction_i$surv,prediction_i$std.err,prediction_i$lower,prediction_i$upper)
  }
  dataframe[k,] <- prediction
  #prediction_df <- data.frame(prediction_df)
  colnames(dataframe) <- c("time", "n.risk", "n.event","survival","std.err","lower","upper")
  #prediction_df$ID <- personID
  # confidence interval formula log transformation  eq 4.3.2 from Klein, John P.  Moeschberger, Melvin L. 2003
  # A Note on Confidence Intervals and Bands for the Survival Function Based on Transformations Borgan, Ørnulf Liestøl, Knut
  #result_time_serie_proba[[personID]] <- prediction_df
  k <- k + 1
}

dataframe <- dataframe %>% mutate(personID = peopleID, proba = 1 - survival)

proba_times_series3 <- dataframe %>% 
  mutate(t1d = 0,
         proba = 1 - survival) %>% 
  select(personID,time,t1d,proba)


table_proba <- dataframe %>% mutate( display_P = paste0(round(proba*10,1)/10," [", round((1-upper)*10,2)/10," - ",round((1-lower)*10,2)/10,"]")) %>% select(time, personID, display_P)

fdr <- rep(c("Yes","Yes","Yes"), 6)
age <- rep(c("Between 1 and 7", "Between 7 and 18", "Over 18"), 6)

GRS2 <- rep(c("++","++","++","++++","++++","++++"),3) # 90 and 99% percentile
number_autoantibody <- rep(c("1","2","3"),each = 6)
peopleID <- 1:18
dataset_label <- data.frame(peopleID,age, GRS2, number_autoantibody, fdr)
label <- dataset_label
table_proba_prediction <- left_join(label,table_proba, by = c("peopleID"="personID")) %>% select(number_autoantibody,age,GRS2,display_P)
names(table_proba_prediction) <- c("Autoantibody status","Age at Screening","Genetic risk", "5 year horizon")
View(table_proba_prediction)

table_proba_prediction <- table_proba_prediction %>% arrange(`Age at Screening`) %>% arrange(`Genetic risk`)


table_proba_prediction_5years <- table_proba_prediction[, c(3, 2, 1, 4)]
View(table_proba_prediction_5years)
