#### TrialNet Recalibration Model ROC AUC #####

################################################################
# pre-requisites / dependencies
################################################################

source(here("R/recalibration_paper/load_data.R"))
source(here("R/recalibration_paper/recalibration.R"))
age1_7_recalibrated <- load(here(paste0(pathSaveTNModels, "paper_models/age1_7_recalibrated.RData")))
age8_17_recalibrated <- load(here(paste0(pathSaveTNModels, "paper_models/age8_17_recalibrated.RData")))
age18_recalibrated <- load(here(paste0(pathSaveTNModels, "paper_models/age18_recalibrated.RData")))


################################################################
# Old stratified AUC
################################################################

original_auc1_7 <- AUC_CV_external(teddyage4_original_res.cox, 
                              old_model_prediction_year = four_year_model, 
                              outcome_name = "t1d", time_name = "time", 
                              val_data = trialnet_age1_7, k = 3, 
                              horizon_time_v = c(1,3,5))

original_auc7_18 <- AUC_CV_external(teddyage7_original_res.cox, 
                               old_model_prediction_year = seven_year_model, 
                               outcome_name = "t1d", time_name = "time",
                               val_data = trialnet_age7_18, k = 3, 
                               horizon_time_v = c(1,3,5))

original_auc18 <- AUC_CV_external(teddyage7_original_res.cox, 
                             old_model_prediction_year = seven_year_model, 
                             outcome_name = "t1d", time_name = "time", 
                             val_data = trialnet_age18, k = 3, 
                             horizon_time_v = c(1,3,5))


age <- rep(c("Age 1-7", "Age 8-17", "Age 18+"), each = 3)
horizon <- rep(c(1,3,5), times = 3)
mean_original_auc <- c(mean(original_auc1_7[,1], na.rm = TRUE),
                  mean(original_auc1_7[,2], na.rm = TRUE),
                  mean(original_auc1_7[,3], na.rm = TRUE),
                  mean(original_auc7_18[,1], na.rm = TRUE),
                  mean(original_auc7_18[,2], na.rm = TRUE),
                  mean(original_auc7_18[,3], na.rm = TRUE),
                  mean(original_auc18[,1], na.rm = TRUE),
                  mean(original_auc18[,2], na.rm = TRUE),
                  mean(original_auc18[,3], na.rm = TRUE))
lower <- c(t.test(original_auc1_7[,1], na.rm=T)$"conf.int"[1],
           t.test(original_auc1_7[,2], na.rm=T)$"conf.int"[1],
           t.test(original_auc1_7[,3], na.rm=T)$"conf.int"[1],
           t.test(original_auc7_18[,1], na.rm=T)$"conf.int"[1],
           t.test(original_auc7_18[,2], na.rm=T)$"conf.int"[1],
           t.test(original_auc7_18[,3], na.rm=T)$"conf.int"[1],
           t.test(original_auc18[,1], na.rm=T)$"conf.int"[1],
           t.test(original_auc18[,2], na.rm=T)$"conf.int"[1],
           t.test(original_auc18[,3], na.rm=T)$"conf.int"[1])
upper <- c(t.test(original_auc1_7[,1], na.rm=T)$"conf.int"[2],
           t.test(original_auc1_7[,2], na.rm=T)$"conf.int"[2],
           t.test(original_auc1_7[,3], na.rm=T)$"conf.int"[2],
           t.test(original_auc7_18[,1], na.rm=T)$"conf.int"[2],
           t.test(original_auc7_18[,2], na.rm=T)$"conf.int"[2],
           t.test(original_auc7_18[,3], na.rm=T)$"conf.int"[2],
           t.test(original_auc18[,1], na.rm=T)$"conf.int"[2],
           t.test(original_auc18[,2], na.rm=T)$"conf.int"[2],
           t.test(original_auc18[,3], na.rm=T)$"conf.int"[2])

original_auc_df <- data.frame(age, horizon, aucs  = mean_original_auc, lower, upper, model = "original")




################################################################
# Old stratified AUC
################################################################

old_auc1_7 <- AUC_CV_external(teddyage4_res.cox, 
                              old_model_prediction_year = four_year_model, 
                              outcome_name = "t1d", time_name = "time", 
                              val_data = trialnet_age1_7, k = 3, 
                              horizon_time_v = c(1,3,5))

old_auc7_18 <- AUC_CV_external(teddyage7_res.cox, 
                               old_model_prediction_year = seven_year_model, 
                               outcome_name = "t1d", time_name = "time",
                               val_data = trialnet_age7_18, k = 3, 
                               horizon_time_v = c(1,3,5))

old_auc18 <- AUC_CV_external(teddyage7_res.cox, 
                              old_model_prediction_year = seven_year_model, 
                              outcome_name = "t1d", time_name = "time", 
                              val_data = trialnet_age18, k = 3, 
                              horizon_time_v = c(1,3,5))

control_auc1_7 <- AUC_CV_control(1, outcome_name = "t1d", time_name = "time", 
                                 dev_data = trialnet_age1_7, k = 3, 
                                 horizon_time_v = c(1,3,5))
control_auc7_18 <- AUC_CV_control(1, outcome_name = "t1d", time_name = "time", 
                                  dev_data = trialnet_age7_18, k = 3, 
                                  horizon_time_v = c(1,3,5))
control_auc18 <- AUC_CV_control(1, outcome_name = "t1d", time_name = "time", 
                                dev_data = trialnet_age18, k = 3, 
                                horizon_time_v = c(1,3,5))

age <- rep(c("Age 1-7", "Age 8-17", "Age 18+"), each = 3)
horizon <- rep(c(1,3,5), times = 3)
mean_old_auc <- c(mean(old_auc1_7[,1], na.rm = TRUE),
                  mean(old_auc1_7[,2], na.rm = TRUE),
                  mean(old_auc1_7[,3], na.rm = TRUE),
                  mean(old_auc7_18[,1], na.rm = TRUE),
                  mean(old_auc7_18[,2], na.rm = TRUE),
                  mean(old_auc7_18[,3], na.rm = TRUE),
                  mean(old_auc18[,1], na.rm = TRUE),
                  mean(old_auc18[,2], na.rm = TRUE),
                  mean(old_auc18[,3], na.rm = TRUE))
lower <- c(t.test(old_auc1_7[,1], na.rm=T)$"conf.int"[1],
           t.test(old_auc1_7[,2], na.rm=T)$"conf.int"[1],
           t.test(old_auc1_7[,3], na.rm=T)$"conf.int"[1],
           t.test(old_auc7_18[,1], na.rm=T)$"conf.int"[1],
           t.test(old_auc7_18[,2], na.rm=T)$"conf.int"[1],
           t.test(old_auc7_18[,3], na.rm=T)$"conf.int"[1],
           t.test(old_auc18[,1], na.rm=T)$"conf.int"[1],
           t.test(old_auc18[,2], na.rm=T)$"conf.int"[1],
           t.test(old_auc18[,3], na.rm=T)$"conf.int"[1])
upper <- c(t.test(old_auc1_7[,1], na.rm=T)$"conf.int"[2],
           t.test(old_auc1_7[,2], na.rm=T)$"conf.int"[2],
           t.test(old_auc1_7[,3], na.rm=T)$"conf.int"[2],
           t.test(old_auc7_18[,1], na.rm=T)$"conf.int"[2],
           t.test(old_auc7_18[,2], na.rm=T)$"conf.int"[2],
           t.test(old_auc7_18[,3], na.rm=T)$"conf.int"[2],
           t.test(old_auc18[,1], na.rm=T)$"conf.int"[2],
           t.test(old_auc18[,2], na.rm=T)$"conf.int"[2],
           t.test(old_auc18[,3], na.rm=T)$"conf.int"[2])

old_auc_df <- data.frame(age, horizon, aucs  = mean_old_auc, lower, upper, model = "stratified")

control_auc_df <- data.frame(age, horizon, aucs = 0.5,lower = 0.5, upper = 0.5,  model = "control")
################################################################
# Recalibrated Model AUC
################################################################

auc1_7 <- AUC_CV_recalibrate(new_trialnet_1_7,"time","t1d", 
                             old_model = teddyage4_original_res.cox,
                             k = 3, horizon_time_v = c(1,3,5))

auc8_17 <- AUC_CV_recalibrate(new_trialnet_8_17,"time","t1d",
                              old_model = teddyage7_original_res.cox,
                             k = 3, horizon_time_v = c(1,3,5))

auc18 <- AUC_CV_recalibrate(new_trialnet_18,"time","t1d", 
                            old_model = teddyage7_original_res.cox,
                              k = 3, horizon_time_v = c(1,3,5), nAB_ammend = "2AB")


age <- rep(c("Age 1-7", "Age 8-17", "Age 18+"), each = 3)
horizon <- rep(c(1,3,5), times = 3)
mean_auc <- c(mean(auc1_7[,1], na.rm = TRUE),
                  mean(auc1_7[,2], na.rm = TRUE),
                  mean(auc1_7[,3], na.rm = TRUE),
                  mean(auc8_17[,1], na.rm = TRUE),
                  mean(auc8_17[,2], na.rm = TRUE),
                  mean(auc8_17[,3], na.rm = TRUE),
                  mean(auc18[,1], na.rm = TRUE),
                  mean(auc18[,2], na.rm = TRUE),
                  mean(auc18[,3], na.rm = TRUE))
lower <- c(t.test(auc1_7[,1], na.rm=T)$"conf.int"[1],
           t.test(auc1_7[,2], na.rm=T)$"conf.int"[1],
           t.test(auc1_7[,3], na.rm=T)$"conf.int"[1],
           t.test(auc8_17[,1], na.rm=T)$"conf.int"[1],
           t.test(auc8_17[,2], na.rm=T)$"conf.int"[1],
           t.test(auc8_17[,3], na.rm=T)$"conf.int"[1],
           t.test(auc18[,1], na.rm=T)$"conf.int"[1],
           t.test(auc18[,2], na.rm=T)$"conf.int"[1],
           t.test(auc18[,3], na.rm=T)$"conf.int"[1])
upper <- c(t.test(auc1_7[,1], na.rm=T)$"conf.int"[2],
           t.test(auc1_7[,2], na.rm=T)$"conf.int"[2],
           t.test(auc1_7[,3], na.rm=T)$"conf.int"[2],
           t.test(auc8_17[,1], na.rm=T)$"conf.int"[2],
           t.test(auc8_17[,2], na.rm=T)$"conf.int"[2],
           t.test(auc8_17[,3], na.rm=T)$"conf.int"[2],
           t.test(auc18[,1], na.rm=T)$"conf.int"[2],
           t.test(auc18[,2], na.rm=T)$"conf.int"[2],
           t.test(auc18[,3], na.rm=T)$"conf.int"[2])

auc_df <- data.frame(age, horizon, aucs  = mean_auc, lower, upper, model = "recalibrated")


################################################################
# combined dfs
################################################################
all_aucs <- rbind(original_auc_df, old_auc_df, auc_df)

all_aucs$age <- factor(all_aucs$age, levels = c("Age 1-7", "Age 8-17", "Age 18+"))

ggplot(all_aucs, aes(x=age, y = aucs, color = model))+
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(0.05))+
  facet_wrap(~horizon)


################################################################
# p-values
################################################################
pval_1_7 <- c(t.test(auc1_7[,1], old_auc1_7[,1], na.rm=T)$"p.val",
             t.test(auc1_7[,2], old_auc1_7[,2], na.rm=T)$"p.val",
             t.test(auc1_7[,3], old_auc1_7[,3], na.rm=T)$"p.val")
pval_1_7

pval_8_17 <- c(t.test(auc8_17[,1], old_auc7_18[,1], na.rm=T)$"p.val",
             t.test(auc8_17[,2], old_auc7_18[,2], na.rm=T)$"p.val",
             t.test(auc8_17[,3], old_auc7_18[,3], na.rm=T)$"p.val")
pval_8_17

pval_18 <- c(t.test(auc18[,1], old_auc18[,1], na.rm=T)$"p.val",
             t.test(auc18[,2], old_auc18[,2], na.rm=T)$"p.val",
             t.test(auc18[,3], old_auc18[,3], na.rm=T)$"p.val")
pval_18

### p-value on control
pval_1_7 <- c(t.test(control_auc1_7[,1], old_auc1_7[,1], na.rm=T)$"p.val",
              t.test(control_auc1_7[,2], old_auc1_7[,2], na.rm=T)$"p.val",
              t.test(control_auc1_7[,3], old_auc1_7[,3], na.rm=T)$"p.val")
pval_1_7

pval_8_17 <- c(t.test(control_auc7_18[,1], old_auc7_18[,1], na.rm=T)$"p.val",
               t.test(control_auc7_18[,2], old_auc7_18[,2], na.rm=T)$"p.val",
               t.test(control_auc7_18[,3], old_auc7_18[,3], na.rm=T)$"p.val")
pval_8_17

pval_18 <- c(t.test(control_auc18[,1], old_auc18[,1], na.rm=T)$"p.val",
             t.test(control_auc18[,2], old_auc18[,2], na.rm=T)$"p.val",
             t.test(control_auc18[,3], old_auc18[,3], na.rm=T)$"p.val")
pval_18
