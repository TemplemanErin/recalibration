# machine learning
library(caret)
library(My.stepwise)

# handling data
library(dplyr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(naniar)
# ROC analyses
library(pROC)
library(plotROC)
library(timeROC)

# plot tools
library(plotly)
library(ggplot2)
library(cowplot)
library(GGally)
library(ggpubr)

# export to word office
library(readxl)
library(officer)
library(rvg)
library(flextable)

#Cox model survival analyse
library(survival)
library(survey)
library(survminer)
library(rms)
library(pec)
library(riskRegression)
# utilitary
library(reshape2)
library(haven)
library(readr)
library(finalfit)
library(pracma) # to compute AUC (integrale with trapeze)
library(RColorBrewer)
library(ggrepel)

library(gt)
library(SurvMetrics)

######### function to compute AUC despite missing values
trapz_LF <- function(x = NULL,y){
  res <- NA
  
  ny <- length(y)
  if( is.null(x)){
    x <- seq(1:ny)
  }
  nx <- length(x)
  if(!any_na(c(x[1],x[nx],y[1],y[ny])) & nx == ny){
    # remove in both vectors when there is a na in one
    
    x <- x[!is.na(y)]
    y <- y[!is.na(y)]
    y <- y[!is.na(x)]
    x <- x[!is.na(x)]
    
    # compute trapez with cleaned 
    res <- trapz(x, y) 
  }
  return(res)
}
trapz_LF


trapz_LF2 <- function(...){
  y <- c(...)
  res <- trapz_LF(y=y)
  return(res)
}

library(here)
here()

pathdata <- ""

TN_path_data <- paste0(pathdata,"/Release_2023_02_14/")
TN_path_load_data <- paste0(pathdata,"/Release_2023_05_15/")
TN_fdr_path_load_data <- paste0(pathdata,"/Release_2023_06_21/")
TN_path_save_data <- paste0(pathdata,"/saved_datasets/")
TN_GRS_path_data <- paste0("/Release_2023_02_02/")

path_figures <-  here("figures/")
path_tables <-  here("tables/")
pathSaveTNModels <- here("models/")
