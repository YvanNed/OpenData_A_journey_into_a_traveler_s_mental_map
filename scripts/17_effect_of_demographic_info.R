  ################################################################################
  ############## Compute and plot effect of demographic information ##############
  ################################################################################
  
  library(ggplot2)
  library(fitdistrplus)
  library(gridExtra)
  library(ggpubr)
  library(rstatix)
  library(plyr) 
  library(tidyverse)
  library(car)
  library(lmerTest)
  library(nortest)
  library(lmPerm)
  library(summarytools)
  library(forestmodel)
  library(gridExtra)
  library(mcr)
  library(bestNormalize)
  library(performance)
  library("report")
  library(emmeans)
  
  rm(list=ls())     # to clear current workspace
  
  ################################################################################
  # load data
  ################################################################################
  
  setwd("C:\\Users\\yvann\\OneDrive\\2022_QTEV_Analysis_YN\\")
  df <- read.csv("dataframe_ready2use_score.csv")
  
  df$laterality <- as.factor(df$laterality)
  df$sex <- as.factor(df$sex)
  
  # descriptive stats
  df_dur <- df %>% filter(type == "dur")
  table(df_dur$sex, useNA = "ifany")
  table(df_dur$laterality, useNA = "ifany")
  table(df_dur$orientation, useNA = "ifany")
  table(df_dur$ep_est, useNA = "ifany")
  # Calcul de la moyenne ed l'age
  moyenne <- mean(df_dur$age, na.rm = TRUE)
  # Calcul de l'écart type de l'age
  ecart_type <- sd(df_dur$age, na.rm = TRUE)
  # Calcul du range (min et max) de l'age
  min_value <- min(df_dur$age, na.rm = TRUE)
  max_value <- max(df_dur$age, na.rm = TRUE)
  # Calcul du nombre d'observations manquantes
  observations_manquantes <- sum(is.na(df_dur$age))
  # Affichage des résultats
  cat("Moyenne :", moyenne, "\n")
  cat("Écart type :", ecart_type, "\n")
  cat("Min :", min_value, "\n")
  cat("Max :", max_value, "\n")
  cat("Nombre d'observations manquantes :", observations_manquantes, "\n")
  

  # age_________________________________________________________________________
  
  dur_age <- lm(RME ~ 1 + age, data = df_dur)
  performance::check_model(dur_age)
  summary(dur_age)
  report(dur_age)
  # perform anova of coef for easier clarity
  anova(dur_age)
  report(anova(dur_age))
  
  # no effect of age
  
  # cloud dot plot of duration ~ age
  distrib_age_RMEdur <- ggplot() +
    geom_point(data = df_dur, aes(age, RME), color = '#66CCEE',) +
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme_classic() +
    theme(legend.position = c(0.7,0.8),  text = element_text(size = 20))+  xlab("Age")+
    ylab("Estimated relative duration")+
    guides(color = "none", fill = "none", alpha = "none")
  
  # geom_line(data=df_dur_score,aes(x=obj_est,y=obj_est,)) # add the identity line
  distrib_age_RMEdur
  
  # sex_________________________________________________________________________
  
  dur_sex <- lm(RME ~ 1 + sex, data = df_dur)
  performance::check_model(dur_sex)
  summary(dur_sex)
  report(dur_sex)
  # perform anova of coef for easier clarity
  anova(dur_sex)
  report(anova(dur_sex))
  
  # no effect of age
  
  bp_sex_dur = ggplot(df_dur, aes(x=sex, y = RME), na.rm = FALSE) +
    geom_boxplot(na.rm = FALSE, aes(fill = sex), fill = '#66CCEE') +
    # geom_point(aes(color = knwYes No_est) +
    theme_classic()+ 
    xlab("Sex")+
    ylab("Relative estimated duration")+
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme( text = element_text(size = 20)) +
    guides(fill = "none", alpha = "none")
  bp_sex_dur

  # laterality__________________________________________________________________
  
  dur_laterality <- lm(RME ~ 1 + laterality, data = df_dur)
  performance::check_model(dur_laterality)
  summary(dur_laterality)
  report(dur_laterality)
  # perform anova of coef for easier clarity
  anova(dur_laterality)
  report(anova(dur_laterality))
  
  # no effect of age
  
  bp_laterality_dur = ggplot(df_dur, aes(x=laterality, y = RME), na.rm = FALSE) +
    geom_boxplot(na.rm = FALSE, aes(fill = laterality), fill = '#66CCEE') +
    # geom_point(aes(color = knwYes No_est) +
    theme_classic()+ 
    xlab("laterality")+
    ylab("Relative estimated duration")+
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme( text = element_text(size = 20)) +
    guides(fill = "none", alpha = "none")
  bp_laterality_dur

  # Interaction?________________________________________________________________  
  dur_interaction <- lm(RME ~ 1 + age*sex*laterality, data = df_dur)
  performance::check_model(dur_interaction)
  summary(dur_interaction)
  report(dur_interaction)
  # perform anova of coef for easier clarity
  anova(dur_interaction)
  report(anova(dur_interaction))
  
  # nothing is significant
 