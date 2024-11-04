#### table1 #####

################################################################
# pre-requisites / dependencies
################################################################

# source(here("R/recalibration_paper/load_data.R"))

TN_path_data <- here("data/saved_datasets/june_2023/")
trialnet <- as.data.frame(read.csv(here(paste0(TN_path_data, "data_demographics_OGTT2.csv"))))
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
  # select("Mask.ID","time", "t1d", "T1D.Indicator", "number_autoantibody", "GRS2", "fdr", "age", "study") %>%
  filter(number_autoantibody != 0) %>%
  filter(!(is.na(fdr))) %>%
  filter(!(is.na(GRS2))) %>%
  filter(!(is.na(age))) %>%
  filter(time > 0)

################################################################
# table 1
################################################################

table1_df  <- trialnet %>% 
  mutate(T1D.Indicator = as.factor(T1D.Indicator),
         GRS2 = as.numeric(GRS2),
         fdr = as.factor(fdr),
         IA2A = ifelse(IA2 == 1, "IA2", "IA2 free"),
         mIAA = ifelse(IAA == 1, "IAA", "IAA free"),
         GADA = ifelse(GAD == 1, "GAD", "GAD free")) %>%
  mutate(age_group = ifelse(age <= 7, "Age birth to 7", ifelse(age > 7 & age < 18, "Age 8 to 17", "Age 18 and over"))) %>%
  mutate(Participant.Race2 = ifelse(Participant.Race == "More than One Race" | 
                                      Participant.Race == "Refused" |
                                      Participant.Race == "Unknown or not reported" |
                                      is.na(Participant.Race), "Unknown",
                                    ifelse(Participant.Race == "American Indian or Alaska Native" |
                                             Participant.Race == "Native Hawaiian or Other Pacific Islander", "Native North American", as.character(Participant.Race)))) %>%
  mutate(race_ethnicity = ifelse(Participant.Ethnicity == "Hispanic or Latino", "Hispanic, regardless of race",
                                 ifelse(Participant.Race2 == "White", "White Non-Hispanic",
                                        ifelse(Participant.Race2 == "Black or African American", "African American", 
                                               ifelse(Participant.Race2 == "Unknown" | is.na(Participant.Race2), "Unknown", "All other races (non-hispanic)")))),
         race_ethnicity = ifelse(is.na(race_ethnicity), "Unknown", as.character(race_ethnicity))) 
table1_df$age_group <- factor(table1_df$age_group, levels = c("Age birth to 7", "Age 8 to 17", "Age 18 and over"))
table1_df$race_ethnicity <- factor(table1_df$race_ethnicity, levels = c("White Non-Hispanic", "Hispanic, regardless of race", "African American", "All other races (non-hispanic)", "Unknown"))



table1(~ Sex + fdr + T1D.Indicator + GRS2 + GADA + IA2A + mIAA + time| age_group, data=table1_df, overall = F, extra.col=list(`P-value`=pvalue))
table1(~ Sex + fdr + T1D.Indicator + GRS2 + m_AB_at_some_point + time + race_ethnicity| age_group, data=table1_df, overall = F)

table1(~ Sex + fdr + as.factor(t1d) + GRS2 + multiple_autoantibody_all_time + time + race_ethnicity|exclude, data=teddy_all, overall = F)

table(teddy_all$race_ethnicity)

################################################################
# follow-up
################################################################
# median follow-up with 95% confidence limits and inter quartile range (IQR):
teddy_all$t1d <- as.numeric(teddy_all$t1d)
table1_df$T1D.Indicator <- as.numeric(table1_df$T1D.Indicator)
quantile(prodlim(Hist(time, t1d)~1, data=teddy_all, reverse=TRUE))
quantile(prodlim(Hist(time, T1D.Indicator)~age_group, data=table1_df, reverse=TRUE))


table1_df_1 <-table1_df %>% filter(age_group == "Age birth to 7")
table1_df_2 <-table1_df %>% filter(age_group == "Age 8 to 17")
table1_df_3 <-table1_df %>% filter(age_group == "Age 18 and over")

l.model <- lm(GRS2 ~ 1, table1_df_1)
sample.mean <- mean(table1_df_1$GRS2)
sample.mean
confint(l.model, level=0.95)

l.model <- lm(GRS2 ~ 1, table1_df_2)
sample.mean <- mean(table1_df_2$GRS2)
sample.mean
confint(l.model, level=0.95)

l.model <- lm(GRS2 ~ 1, table1_df_3)
sample.mean <- mean(table1_df_3$GRS2)
sample.mean
confint(l.model, level=0.95)

