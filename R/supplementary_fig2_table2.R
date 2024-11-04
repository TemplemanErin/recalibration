
#### Difference in Survival between discovery and validation cohorts
#### Discovery: TEDDY
#### Validation: TrialNet
#### Survival stratified by AB number #####

################################################################
# supplementary figure 2
################################################################

# Pre-requisites ---------
library(pec)
library(gtsummary)
# load_data.R

# Load and prepare data for analysis ---------
# Load Teddy Data for age 7
day <- (4*365.25) + 45
dayend = Inf
source(here("../teddy/R/Extract_information_per_date.R"),local = TRUE)
teddyage4 <- finaldata %>%
  mutate(AB = as.factor(number_autoantibody),
         time = time - (day/365.25),
         age = 4,
         study = "teddy") %>% 
  select("time", "t1d", "AB", "GRS2", "fdr","age", "study")



# Load TrialNet data
trialnet <- trialnet %>%
  mutate(AB = as.factor(number_autoantibody),
         study = "trialnet") %>%
  select("time", "t1d", "AB", "GRS2", "fdr", "age", "study") %>%
  filter(AB != 0)

# Join teddy and trialnet data 
trialnet <- trialnet %>% 
  select(colnames(teddyage7))
joined_data <- rbind(teddyage7, trialnet)
single_joined_data <- joined_data %>%
  filter(AB == 1)
double_joined_data <- joined_data %>%
  filter(AB == 2)
triple_joined_data <- joined_data %>%
  filter(AB == 3)


## Plot survival Curves for TEDDY and TrialNet -------
survival <- Surv(time = joined_data$time, event = joined_data$t1d == 1)
survival_fit <- survfit(survival ~ study + AB, data=joined_data)
add_p(tbl_survfit(survival_fit, times = 5))
ggsurvplot(survival_fit, joined_data, conf.int = TRUE, risk.table=TRUE, 
           xlim = c(0,8), break.x.by = 1, legend = "right")


## separate curves for number of ab present at screening:
survival <- Surv(time = single_joined_data$time, single_joined_data$t1d == 1)
survival_fit_single <- survfit(survival ~ study, data=single_joined_data)
add_p(tbl_survfit(survival_fit_single, times = 5))
ggsurvplot(survival_fit_single, single_joined_data, conf.int = TRUE, 
           xlim = c(0,8), break.x.by = 1, legend = "right")


survival <- Surv(time = double_joined_data$time, double_joined_data$t1d == 1)
survival_fit_double <- survfit(survival ~ study, data=double_joined_data)
add_p(tbl_survfit(survival_fit_double, times = 5))
ggsurvplot(survival_fit_double, double_joined_data, conf.int = TRUE, 
           xlim = c(0,8), break.x.by = 1, legend = "right")


survival <- Surv(time = triple_joined_data$time, triple_joined_data$t1d == 1)
survival_fit_triple <- survfit(survival ~ study, data=triple_joined_data)
add_p(tbl_survfit(survival_fit_triple, times = 5))
ggsurvplot(survival_fit_triple, triple_joined_data, conf.int = TRUE, 
           xlim = c(0,8), break.x.by = 1, legend = "right")



### Survival Graph for Paper: 
survival <- Surv(time = triple_joined_data$time, triple_joined_data$t1d == 1)
survival_fit <- survfit(survival ~ study + AB, data=triple_joined_data)
n_threshold <- 20
df <- surv_summary(survival_fit, data = joined_data) 
df_modified <- df %>% filter(n.risk > n_threshold) 
ggsurvplot(df_modified, joined_data, conf.int = TRUE, 
           xlim = c(0,8), break.x.by = 2, legend = "right")

p1 <- ggsurvplot_df(df_modified, conf.int = TRUE, conf.int.alpha = 0.1,
                    #legend.labs = c("AB=1, study=Teddy", "AB=1, study=TrialNet",
                    #                "AB=2, study=Teddy", "AB=2, study=TrialNet",
                    #                "AB=3, study=Teddy", "AB=3, study=TrialNet"),
                    ggtheme = theme_bw(), xlim = c(0,8), 
                    break.x.by = 2,
                    font.x = 16, font.y = 16,
                    font.tickslab = 16,#font.legend = 16,
                    legend.title = "",
                    palette = c("lightblue", "lightgreen", "royalblue3", "olivedrab4", "darkblue", "darkgreen"),
                    legend = "right",xlab = "Time (years)", risk.table = T)

p1



