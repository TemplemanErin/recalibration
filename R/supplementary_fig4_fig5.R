#### Recalibration AUC panelled figure#####

################################################################
# pre-requisites / dependencies
################################################################

source(here("load/load_data.R"))
source(here("R/recalibration.R"))


################################################################
# trialnet GRS
################################################################

table(trialnet$t1d)

trialnet_grs_AUC <- timeROC(T = trialnet$time,
                     delta = trialnet$t1d,
                     marker = trialnet$GRS2,
                     cause = 1,
                     times = 5, iid=TRUE)
trialnet_grs_AUC
confint(trialnet_grs_AUC)


################################################################
# all teddy individuals
################################################################

load(here("../teddy/output/all_teddy_AUC.RData"))
load(here("../teddy/output/all_teddy_CI.RData"))
all_data_plot <- data_plot
all_data_CI <- data_CI

legend_name = "Future prediction interval (horizon)"

all_teddy_AUC_plot_nolegend <- ggplot(all_data_plot, aes(x =time_from, y =all_data_plot$t_AUC)) +
  geom_point(aes(colour = years, shape = years),size = 5) +
  geom_line(aes(colour = years),size = 0.5) +
  xlab("Years of Age at Prediction Scoring (Landmark)") +
  ylab("ROC AUC") +
  scale_x_continuous(breaks = seq(0,7)) +
  scale_y_continuous(breaks = seq(0.5,1,0.1),limits = c(0.5,1)) +
  scale_colour_manual(name = legend_name,
                      #values = c("#e41a1c","#4daf4a","#377eb8","#984ea3"), #"#e41add", "#e48c1a","#1ae4d7" ),
                      values = c("#F5793A","#A95AA1","#85C0F9","#0F2080"), #"#e41add", "#e48c1a","#1ae4d7" ),
                      breaks = c("X1","X3","X5","X8"),
                      labels = c("1 year","3 years","5 years","8 years")) +
  scale_shape_manual(name = legend_name,
                     values = (c(15,16,17,18)),
                     labels = c("1 year","3 years","5 years","8 years")) +
  theme_bw() +
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 26),
        legend.position = "None",
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 26),
        legend.key.width = unit(0.5,"cm"),
        legend.key.height  = unit(0.5,"cm")) +
  guides(linetype = guide_legend(order = 1)) +
  geom_ribbon(data = all_data_CI, aes(ymin = lower,ymax = upper,fill = years),alpha = 0.1,show.legend = FALSE, colour = "transparent")


all_teddy_AUC_plot_legend <- ggplot(all_data_plot, aes(x =time_from, y =all_data_plot$t_AUC)) +
  geom_point(aes(colour = years, shape = years),size = 5) +
  geom_line(aes(colour = years),size = 0.5) +
  xlab("Years of Age at Prediction Scoring (Landmark)") +
  ylab("ROC AUC") +
  scale_x_continuous(breaks = seq(0,7)) +
  scale_y_continuous(breaks = seq(0.5,1,0.1),limits = c(0.5,1)) +
  scale_colour_manual(name = legend_name,
                      #values = c("#e41a1c","#4daf4a","#377eb8","#984ea3"), #"#e41add", "#e48c1a","#1ae4d7" ),
                      values = c("#F5793A","#A95AA1","#85C0F9","#0F2080"), #"#e41add", "#e48c1a","#1ae4d7" ),
                      breaks = c("X1","X3","X5","X8"),
                      labels = c("1 year","3 years","5 years","8 years")) +
  scale_shape_manual(name = legend_name,
                     values = (c(15,16,17,18)),
                     labels = c("1 year","3 years","5 years","8 years")) +
  theme_bw() +
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 26),
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 26),
        legend.key.width = unit(0.5,"cm"),
        legend.key.height  = unit(0.5,"cm")) +
  #geom_vline(xintercept = 2,linetype = "dashed") +
  guides(linetype = guide_legend(order = 1)) +
  geom_ribbon(data = all_data_CI, aes(ymin = lower,ymax = upper,fill = years),alpha = 0.1,show.legend = FALSE, colour = "transparent")


################################################################
# ab positive teddy individuals
################################################################

load(here("../teddy/output/abpos_teddy_AUC.RData"))
load(here("../teddy/output/abpos_teddy_CI.RData"))
abpos_data_plot <- data_plot
abpos_data_plot <- abpos_data_plot %>% 
  filter(time_from > 0.1)
abpos_data_CI <- data_CI
abpos_data_CI <- abpos_data_CI %>% 
  filter(time_from > 0.1)


abpos_teddy_AUC_plot <- ggplot(abpos_data_plot, aes(x =time_from, y =abpos_data_plot$t_AUC)) +
  geom_point(aes(colour = years, shape = years),size = 5) +
  geom_line(aes(colour = years),size = 0.5) +
  xlab("Years of Age at Prediction Scoring (Landmark)") +
  ylab("ROC AUC") +
  scale_x_continuous(breaks = seq(1,7)) +
  scale_y_continuous(breaks = seq(0.5,1,0.1),limits = c(0.5,1)) +
  scale_colour_manual(name = legend_name,
                      values = c("#F5793A","#A95AA1","#85C0F9","#0F2080"),
                      breaks = c("X1","X3","X5","X8"),
                      labels = c("1 year","3 years","5 years","8 years")) +
  scale_shape_manual(name = legend_name,
                     values = (c(15,16,17,18)),
                     labels = c("1 year","3 years","5 years","8 years")) +
  theme_bw() +
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 26),
        legend.position = "right",
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 26),
        legend.key.width = unit(0.5,"cm"),
        legend.key.height  = unit(0.5,"cm")) +
  guides(linetype = guide_legend(order = 1)) +
  geom_ribbon(data = abpos_data_CI, aes(ymin = lower,ymax = upper,fill = years),alpha = 0.1,show.legend = FALSE, colour = "transparent")



################################################################
# trialnet - teddy style
################################################################


landmarks <- c(1,2,3,4,5,6,7,8)
models <- list(teddyage1_res.cox, teddyage2_res.cox,teddyage3_res.cox,teddyage4_res.cox,
               teddyage5_res.cox, teddyage6_res.cox, teddyage7_res.cox)
datasets <- list(trialnet1, trialnet2, trialnet3, trialnet4, trialnet5, trialnet6, 
                 trialnet7, trialnet_over7)
horizon_times <- c(1,3,5)

data_plot_tn <- matrix(nrow = length(landmarks)*length(horizon_times), ncol = 3)
data_ci_tn <- matrix(nrow = length(landmarks)*length(horizon_times), ncol = 4)

for (i in 1:length(landmarks)){
  dataset <- datasets[[i]]
  if(i < 8){ 
    model <- models[[i]] 
    prediction_year <- i
  }else{
    model <- models[[7]] 
    prediction_year <- 7}
  res <- AUC_CV_external(model, old_model_prediction_year = prediction_year, 
                         outcome_name = "t1d", time_name = "time", 
                         val_data = dataset, k = 3, horizon_time_v = horizon_times)
  
  for (j in 1:length(horizon_times)){
    horizon <- paste0("X", horizon_times[j])
    sub_res <- res[,j]
    
    mean_AUC <- mean(sub_res, na.rm = T)
    summary <- c(landmarks[i], round(mean_AUC,3), horizon)
    data_plot_tn[8*(j-1) + i,] <- summary
    
    lower_quartile <- quantile(sub_res, probs = c(0.05, 0.95), na.rm=T)[[1]]
    upper_quartile <- quantile(sub_res, probs = c(0.05, 0.95), na.rm=T)[[2]]
    summary_quantile <- c(landmarks[i], round(lower_quartile,3), round(upper_quartile,3), horizon)
    data_ci_tn[8*(j-1) + i, ] <- summary_quantile
    
  } 
}


# Format DataFrames
data_plot_tn <- data.frame(data_plot_tn)
colnames(data_plot_tn) <- c("time_from", "t_AUC", "years")
data_plot_tn$time_from <- as.numeric(data_plot_tn$time_from)
data_plot_tn$t_AUC <- as.numeric(data_plot_tn$t_AUC)
data_plot_tn$years <- as.factor(data_plot_tn$years)

data_ci_tn <- data.frame(data_ci_tn)
colnames(data_ci_tn) <- c("time_from", "lower", "upper", "years")
data_ci_tn$time_from <- as.numeric(data_ci_tn$time_from)
data_ci_tn$lower <- as.numeric(data_ci_tn$lower)
data_ci_tn$upper <- as.numeric(data_ci_tn$upper)
data_ci_tn$years <- as.factor(data_ci_tn$years)

data_plot_tn <- data_plot_tn %>% filter(!is.na(time_from))
data_ci_tn <- data_ci_tn %>% filter(!is.na(time_from))

## Create Graph
legend_name = "Future prediction interval (horizon)"
trialnet_auc_nolegend <- ggplot(data_plot_tn, aes(x=time_from, y=data_plot_tn$t_AUC)) +
  geom_point(aes(colour = years, shape = years),size = 5) +
  geom_line(aes(colour = years),size = 0.5) +
  xlab("Years of Age at Prediction Scoring (Landmark)") +
  ylab("ROC AUC") +
  scale_x_continuous(breaks = seq(0,8), labels = c('0','1','2','3','4','5','6','7','Over 7')) + 
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1)) +
  scale_colour_manual(name = legend_name,
                      values = c("#F5793A","#A95AA1","#85C0F9"),
                      breaks = c("X1","X3","X5"),
                      labels = c("1 year","3 years","5 years")) +
  scale_shape_manual(name = legend_name,
                     values = (c(15,16,17)),
                     labels = c("1 year","3 years","5 years")) +
  theme_bw() +
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 26),
        legend.position = "None",
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 26),
        legend.key.width = unit(0.5,"cm"),
        legend.key.height  = unit(0.5,"cm")) +
  guides(linetype = guide_legend(order = 1)) +
  geom_ribbon(data = data_ci_tn, aes(ymin = lower,ymax = upper,fill = years),alpha = 0.1,show.legend = FALSE, colour = "transparent")


trialnet_auc_legend <- ggplot(data_plot_tn, aes(x =time_from, y =data_plot_tn$t_AUC)) +
  geom_point(aes(colour = years, shape = years),size = 5) +
  geom_line(aes(colour = years),size = 0.5) +
  xlab("Years of Age at Prediction Scoring (Landmark)") +
  ylab("ROC AUC") +
  scale_x_continuous(breaks = seq(0,8), labels = c('0','1','2','3','4','5','6','7','Over 7')) + 
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1)) +
  scale_colour_manual(name = legend_name,
                      values = c("#F5793A","#A95AA1","#85C0F9"),
                      breaks = c("X1","X3","X5"),
                      labels = c("1 year","3 years","5 years")) +
  scale_shape_manual(name = legend_name,
                     values = (c(15,16,17)),
                     labels = c("1 year","3 years","5 years")) +
  theme_bw() +
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 26),
        legend.position = "right",
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 26),
        legend.key.width = unit(0.5,"cm"),
        legend.key.height  = unit(0.5,"cm")) +
  guides(linetype = guide_legend(order = 1)) +
  geom_ribbon(data = data_ci_tn, aes(ymin = lower,ymax = upper,fill = years),alpha = 0.1,show.legend = FALSE, colour = "transparent")



################################################################
# trialnet - grouped
# supplementary figure 6
################################################################

landmarks <- c(4, 7, 18, 4, 7, 18, 4, 7, 18)
model_type <- c("original", "strata", "recalibrated")

# models <- list(teddyage4_original_res.cox, teddyage7_original_res.cox,
#                teddyage7_original_res.cox, teddyage4_res.cox,
#                teddyage7_res.cox,teddyage7_res.cox,
#                age1_7_new_model_5years, age7_18_new_model_5years, 
#                age18_new_model_5years)

models <- list(teddyage4_original_res.cox, teddyage7_original_res.cox,
               teddyage7_original_res.cox, teddyage4_res.cox,
               teddyage7_res.cox,teddyage7_res.cox,
               teddyage4_res.cox, teddyage7_res.cox, 
               teddyage7_res.cox)

datasets <- list(trialnet_age1_7, trialnet_age7_18, trialnet_age18,
                 trialnet_age1_7, trialnet_age7_18, trialnet_age18,
                 new_trialnet_1_7, new_trialnet_8_17, new_trialnet_18)
horizon_times <- c(3)

data_plot <- matrix(nrow = length(landmarks)*length(horizon_times), ncol = 4)
data_CI <- matrix(nrow = length(landmarks)*length(horizon_times), ncol = 5)

for (i in 1:length(landmarks)){
    # dataset = 1-7, 7-18, 18+
    dataset <- datasets[[i]]
    
    if(i < 7){ 
      model <- models[[i]] 
      
      if(landmarks[i]==18){prediction_year = 7}else{prediction_year <- landmarks[i]}
      
      if(i < 4){model_type = "original"} else{model_type = "strata"}
      
      res <- AUC_CV_external(model, old_model_prediction_year = prediction_year, 
                             outcome_name = "t1d", time_name = "time", 
                             val_data = dataset, k = 3, horizon_time_v = horizon_times)
      
      for (j in 1:length(horizon_times)){
        horizon <- paste0("X", horizon_times[j])
        sub_res <- res[,j]
        
        mean_AUC <- mean(sub_res, na.rm = T)
        summary <- c(landmarks[i], round(mean_AUC,3), horizon, model_type)
        print(summary)
        data_plot[9*(j-1) + i,] <- summary
        
        lower_quartile <- quantile(sub_res, probs = c(0.05, 0.95), na.rm=T)[[1]]
        upper_quartile <- quantile(sub_res, probs = c(0.05, 0.95), na.rm=T)[[2]]
        summary_quantile <- c(landmarks[i], round(lower_quartile,3), 
                              round(upper_quartile,3), horizon, model_type)
        data_CI[9*(j-1) + i, ] <- summary_quantile
      } 
      
    }
    else{
      #model <- models[[7]] 
      prediction_year <- 7
      
      model_type <- "recalibrated"
      
      if(landmarks[i] == 18){
        res <- AUC_CV_recalibrate(dataset,"time","t1d", 
                                  old_model = model,
                                  k = 3, horizon_time_v = horizon_times,
                                  nAB_ammend = "2AB")
      } else{
        res <- AUC_CV_recalibrate(dataset,"time","t1d", 
                                  old_model = model,
                                  k = 3, horizon_time_v = horizon_times)
      }
      
      for (j in 1:length(horizon_times)){
        horizon <- paste0("X", horizon_times[j])
        sub_res <- res[,j]
        
        mean_AUC <- mean(sub_res, na.rm = T)
        summary <- c(landmarks[i], round(mean_AUC,3), horizon, model_type)
        print(summary)
        data_plot[9*(j-1) + i,] <- summary
        
        lower_quartile <- quantile(sub_res, probs = c(0.05, 0.95), na.rm=T)[[1]]
        upper_quartile <- quantile(sub_res, probs = c(0.05, 0.95), na.rm=T)[[2]]
        summary_quantile <- c(landmarks[i], round(lower_quartile,3), round(upper_quartile,3), horizon, model_type)
        data_CI[9*(j-1) + i, ] <- summary_quantile
      }
      
    }
}

    
# Format DataFrames
data_plot <- data.frame(data_plot)
colnames(data_plot) <- c("time_from", "t_AUC", "years", "model_type")
data_plot$time_from <- c("Under 7", "8 - 17" ,"18 and over",
                         "Under 7", "8 - 17" ,"18 and over",
                         "Under 7", "8 - 17" ,"18 and over")
data_plot$t_AUC <- as.numeric(data_plot$t_AUC)
data_plot$years <- as.factor(data_plot$years)
data_plot$model_type <- factor(data_plot$model_type, levels = c("original", "strata", "recalibrated"))

data_CI <- data.frame(data_CI)
colnames(data_CI) <- c("time_from", "lower", "upper", "years", "model_type")
data_CI$time_from <- c("Under 7", "8 - 17" ,"18 and over",
                       "Under 7", "8 - 17" ,"18 and over",
                       "Under 7", "8 - 17" ,"18 and over")
data_CI$lower <- as.numeric(data_CI$lower)
data_CI$upper <- as.numeric(data_CI$upper)
data_CI$years <- as.factor(data_CI$years)
data_CI$model_type <- as.factor(data_CI$model_type)
data_CI$model_type <- factor(data_CI$model_type, levels = c("original", "strata", "recalibrated"))

data_plot <- data_plot %>% filter(!is.na(time_from))
data_CI <- data_CI %>% filter(!is.na(time_from))

## Create Graph
legend_name_grouped = "Model Type"
grouped <- ggplot(data_plot, aes(x =time_from, y =data_plot$t_AUC)) +
  geom_point(aes(colour = model_type, shape = model_type),size = 7) +
  geom_errorbar(data = data_CI, aes(ymin = lower,ymax = upper,colour = model_type),width = 0.6, size=1, alpha = 0.7,show.legend = FALSE) + 
  xlab("TrialNet Age Group") +
  ylab("ROC AUC") +
  scale_x_discrete(labels=c("Under 7", "8 - 17" ,"18 and over")) + 
  scale_y_continuous(breaks = seq(0.5,1,0.1),limits = c(0.5,1)) +
  scale_colour_manual(name = legend_name_grouped,
                      values = c("#e41a1c","#4daf4a","#377eb8"), #"#e41add", "#e48c1a","#1ae4d7" ),
                      #values = c("#F5793A","#A95AA1","#85C0F9"),
                      breaks = c("original", "strata", "recalibrated"),
                      labels = c("Original","Stratified","Recalibrated")) +
  scale_shape_manual(name = legend_name_grouped,
                     values = (c(15,16,17)),
                     labels = c("Original","Stratified","Recalibrated")) +
  theme_bw() +
  theme(axis.text = element_text(size = 22),
        axis.title = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        legend.key.width = unit(0.5,"cm"),
        legend.key.height  = unit(0.5,"cm"))





library(cowplot)
# plot_grid(all_teddy_AUC_plot,abpos_teddy_AUC_plot,trialnet_auc, grouped, nrow = 2, ncol = 2, scale = 0.7)
# all_teddy_AUC_plot
# abpos_teddy_AUC_plot
# grouped
# trialnet_auc
# grouped



################################################################
# supplementary figure 4
################################################################

legend <- get_legend(all_teddy_AUC_plot_legend)
legend2 <- get_legend(trialnet_auc_legend)

as_ggplot(legend)

grid1 <- plot_grid(all_teddy_AUC_plot_nolegend,abpos_teddy_AUC_plot, legend, "blank", nrow = 1, ncol = 4)
grid2 <- plot_grid("blank",trialnet_auc_nolegend,legend2, grouped, nrow = 1, ncol = 4)
grid2
all_grid <- plot_grid(grid1, grid2, nrow = 2, ncol = 1)
all_grid
ggsave(here("figures/new_figures/auc_cv_all.JPEG"), dpi = 300, width = 30, height = 20)


teddy_grid1 <- plot_grid(all_teddy_AUC_plot_nolegend,abpos_teddy_AUC_plot, legend, nrow = 1, ncol = 3)
trialnet_grid2 <- plot_grid(trialnet_auc_nolegend,legend2, grouped, nrow = 1, ncol = 3)
all_grid <- plot_grid(teddy_grid1, trialnet_grid2, nrow = 2, ncol = 1)
ggsave(here("figures/new_figures/auc_cv_all_noblanks.JPEG"), dpi = 300, width = 30, height = 20)


onlyauc_grid1 <- plot_grid(all_teddy_AUC_plot_nolegend,NULL,abpos_teddy_AUC_plot, nrow = 1, ncol = 3, labels = c('A', '','B'), rel_widths = c(1.5,0.1, 2), label_size = 30)
onlyauc_grid1
onlyauc_grid2 <- plot_grid(trialnet_auc_legend, nrow = 1, ncol = , labels = c('C'))
all_grid <- plot_grid(onlyauc_grid1, NULL, trialnet_auc_legend, nrow = 3, ncol = 1,labels = c('', '','C'),label_size = 30, rel_heights = c(1,0.1,1))
all_grid
ggsave(here("figures/new_figures/only_auc_cv_all_noblanks.JPEG"), dpi = 1000, width = 30, height = 20)


grouped
ggsave(here("figures/new_figures/grouped_auc.JPEG"), dpi = 500, width = 20, height = 15)



##### Compare 3 year horizons
allteddy_3year <- all_data_plot %>% filter(years =="X3") %>% mutate(model = "All TEDDY")
allteddy_ci_3year <- all_data_CI %>% filter(years =="X3") %>% mutate(model = "All TEDDY")
abposteddy_3year <- abpos_data_plot %>% filter(years =="X3") %>% mutate(model = "AB positive TEDDY")
abposteddy_ci_3year <- abpos_data_CI %>% filter(years =="X3") %>% mutate(model = "AB positive TEDDY")
data_plot_tn_3year <- data_plot_tn %>% filter(years =="X3") %>% mutate(model = "TrialNet")
data_ci_tn_3year <- data_ci_tn %>% filter(years =="X3") %>% mutate(model = "TrialNet")
data_plot_tn_3year$time_from <- c(1.1,2.1,3.1,4.1,5.1,6.1,7.1,8)
data_ci_tn_3year$time_from <- c(1.1,2.1,3.1,4.1,5.1,6.1,7.1,8)

newdata <- rbind(allteddy_3year, abposteddy_3year, data_plot_tn_3year)
newdata$model <- factor(newdata$model, levels = c("AB positive TEDDY","All TEDDY","TrialNet"))
newdata_ci <- rbind(allteddy_ci_3year, abposteddy_ci_3year, data_ci_tn_3year)
newdata_ci$model <- factor(newdata_ci$model, levels = c("AB positive TEDDY","All TEDDY","TrialNet"))

ggplot(newdata, aes(x =time_from, y =newdata$t_AUC)) +
  geom_point(aes(colour = model, shape = model),size = 5) +
  geom_line(aes(colour = model),size = 0.5) +
  xlab("Years of Age at Prediction Scoring (Landmark)") +
  ylab("ROC AUC") +
  scale_x_continuous(breaks = seq(0,8), labels = c('0','1','2','3','4','5','6','7','Over 7')) + 
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1)) +
  scale_colour_manual(name = "Model",
                      values = c("#F5793A","#A95AA1","#85C0F9"),
                      breaks = c("AB positive TEDDY","All TEDDY","TrialNet"),
                      labels = c("AB positive TEDDY","All TEDDY","TrialNet")) +
  scale_shape_manual(name = "Model",
                     values = (c(15,16,17)),
                     labels = c("AB positive TEDDY","All TEDDY","TrialNet")) +
  theme_bw() +
  theme(axis.text = element_text(size = 26),
        axis.title = element_text(size = 26),
        legend.position = "right",
        legend.text = element_text(size = 26),
        legend.title = element_text(size = 26),
        legend.key.width = unit(0.5,"cm"),
        legend.key.height  = unit(0.5,"cm")) +
  guides(linetype = guide_legend(order = 1)) +
  geom_ribbon(data = newdata_ci, aes(ymin = lower,ymax = upper,fill = model),alpha = 0.1,show.legend = FALSE, colour = "transparent")

ggsave(here("figures/new_figures/3year_auc.JPEG"), dpi = 500, width = 20, height = 15)




