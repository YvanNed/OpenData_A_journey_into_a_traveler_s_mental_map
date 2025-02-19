  ################################################################################
  ################### Compute and plot effect of train factors ###################
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
  
  df$ep_est[df$ep_est == "acc"] <- "positive"
  df$ep_est[df$ep_est == "cst"] <- "null"
  df$ep_est[df$ep_est == "dec"] <- "negative"
  unique(df$ep_est)
  
  
  df_new <- df # create another dataframe for later
  
  # Ajoutez une colonne alpha personnalisée en fonction des catégories
  df <- df %>%
    mutate(alpha = case_when(
      ep_est == "positive" ~ 1,
      ep_est == "null" ~ 0.5,
      ep_est == "negative" ~ 0.1,
      TRUE ~ 1  # Valeur alpha par défaut pour les autres catégories
    ))
  
  df$ep_est <- as.factor(df$ep_est)
  df$orientation <- as.factor(df$orientation)
  
  # display current order of metacog
  print(levels(df$ep_est))
  # create a new order
  new_order_ep <- c("positive", "null", "negative")
  # add the new order
  df$ep_est <- factor(df$ep_est, levels = new_order_ep)
  
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
  

  # duration____________________________________________________________________
  # check colinearity between the two factors
  dur_trainfactors_col <- lm(RME ~ 1 + ep_est + orientation, data = df_dur)
  performance::check_model(dur_trainfactors_col) # all good
  
  dur_trainfactors_lm <- lm(RME ~ 1 + ep_est * orientation, data = df_dur)
  performance::check_model(dur_trainfactors_lm)
  summary(dur_trainfactors_lm)
  report(dur_trainfactors_lm)
  # perform anova of coef for easier clarity
  anova(dur_trainfactors_lm)
  report(anova(dur_trainfactors_lm))
  
  # no main effect
  
  # Utilisez ggplot avec scale_fill_alpha pour spécifier l'alpha
  bp_speed_ep_dur <- ggplot(df_dur, aes(x = ep_est, y = RME)) +
    geom_boxplot(aes(fill = ep_est, alpha = alpha), fill = '#66CCEE') +
    theme_classic() + 
    xlab("Acceleration") +
    ylab("Relative estimated duration") +
    geom_hline(aes(yintercept = 1), linetype = "solid", size = 1, color = "grey") +
    theme(text = element_text(size = 20)) +
    labs(fill = "Acceleration") +
    guides(fill = "none", alpha = "none") +  # Masquer les légendes pour la couleur et l'alpha
    scale_alpha_identity()  # Permet de conserver les valeurs alpha que vous avez spécifiées
  
  bp_speed_ep_dur
  
  # duration
  distrib_across_travel_RMEdur <- ggplot() +
    geom_point(data = df_dur, aes(obj_est, RME, alpha = alpha), color = '#66CCEE',) +
    # add regression line with outliers
    # geom_smooth(data = df_dur_score, aes(obj_est, c(ME),),  method = lm, se = TRUE) +
    # stat_regline_equation(label.y = 325, data = df_dur_score, aes(obj_est, c(ME), label = ..eq.label..), color = "black") + # the parameter might not be good, it would be better to get the values from the model 
    # stat_regline_equation(label.y = 310, data = df_dur_score, aes(obj_est, c(ME), label = ..rr.label..), color = "black") +  # the parameter might not be good, it would be better to get the values from the model
    # add regression line without outliers
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme_classic() +
    labs(color = "Acceleration episodes:", size = 30) +
    theme(legend.position = c(0.7,0.8),  text = element_text(size = 20))+  xlab("Objective duration (min)")+
    ylab("Estimated relative duration")+
    guides(color = "none", fill = "none", alpha = "none")
  
  # geom_line(data=df_dur_score,aes(x=obj_est,y=obj_est,)) # add the identity line
  distrib_across_travel_RMEdur
  
  # distance______________________________________________________________________
  df_dist <- df %>% filter(type == "dist")
  dist_trainfactors_lm <- lm(RME ~ 1 + ep_est * orientation, data = df_dist)
  performance::check_model(dist_trainfactors_lm)
  summary(dist_trainfactors_lm)
  report(dist_trainfactors_lm)
  # main effect of episode, estimations are overestimated when the rate of change is changing (in acc and dec compared to cst)  
  
  # perform anova of coef for easier clarity
  anova(dist_trainfactors_lm)
  report(anova(dist_trainfactors_lm))
  # perform pairwise comparison
  # compute simpler model with only the significant term
  dist_comparaison <- emmeans(dist_trainfactors_lm, ~ ep_est)  
  pairs(dist_comparaison)
  
  
  bp_speed_ep_dist = ggplot(df_dist, aes(x=ep_est, y = RME), na.rm = FALSE) +
    geom_boxplot(na.rm = FALSE, aes(fill = ep_est, alpha = alpha), fill = '#EE6677') +
    # geom_point(aes(color = knwYes No_est) +
    theme_classic()+ 
    xlab("Acceleration")+
    ylab("Relative estimated distance")+
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme( text = element_text(size = 20)) +
    labs(fill = "Acceleration") +
    guides(fill = "none", alpha = "none")
  bp_speed_ep_dist
  
  # distance
  distrib_across_travel_RMEdist <- ggplot() +
    geom_point(data = df_dist, aes(obj_est, RME, color = ep_est, alpha = alpha), color = '#EE6677' ) +
    # add regression line with outliers
    # geom_smooth(data = df_dur_score, aes(obj_est, c(ME),),  method = lm, se = TRUE) +
    # stat_regline_equation(label.y = 325, data = df_dur_score, aes(obj_est, c(ME), label = ..eq.label..), color = "black") + # the parameter might not be good, it would be better to get the values from the model 
    # stat_regline_equation(label.y = 310, data = df_dur_score, aes(obj_est, c(ME), label = ..rr.label..), color = "black") +  # the parameter might not be good, it would be better to get the values from the model
    # add regression line without outliers
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme_classic() +
    labs(color = "Acceleration episodes:", size = 30) +
    theme(legend.position = c(0.7,0.8),  text = element_text(size = 20))+  xlab("Objective distance (km)")+
    ylab("Estimated relative distance")+
    guides(color = "none", fill = "none", alpha = "none")
  
  # geom_line(data=df_dur_score,aes(x=obj_est,y=obj_est,)) # add the identity line
  distrib_across_travel_RMEdist
  
  
  # instantaneous speed___________________________________________________________
  df_spdinst <- df %>% filter(type == "spd_inst")
  
  # check if no colinearity when ther is no interaction
  spdinst_trainfactors_lm <- lm(RME ~ 1 + ep_est + orientation, data = df_spdinst)
  performance::check_model(spdinst_trainfactors_lm)
  # all good
  
  spdinst_trainfactors_lm <- lm(RME ~ 1 + ep_est * orientation, data = df_spdinst)
  performance::check_model(spdinst_trainfactors_lm)
  summary(spdinst_trainfactors_lm) # significant model but no main effect
  report(spdinst_trainfactors_lm)
  
  # perform anova of coef for easier clarity
  anova(spdinst_trainfactors_lm)
  report(anova(spdinst_trainfactors_lm))
  # perform pairwise comparison
  # compute simpler model with only the significant term
  comparaison_spdinst <- emmeans(spdinst_trainfactors_lm, ~ ep_est)  
  pairs(comparaison_spdinst)

  bp_speed_ep_spdinst = ggplot(df_spdinst, aes(x=ep_est, y = RME), na.rm = FALSE) +
    geom_boxplot(na.rm = FALSE, aes(fill = ep_est, alpha = alpha), fill = '#228833') +
    # geom_point(aes(color = knwYes No_est) +
    theme_classic()+ 
    xlab("Acceleration")+
    ylab("Relative estimated inst. speed")+
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme( text = element_text(size = 20))+
    labs(fill = "Acceleration") +
    guides(fill = "none", alpha = "none")
  bp_speed_ep_spdinst
  
  
  distrib_across_travel_RMEspdinst <- ggplot() +
    geom_point(data = df_spdinst, aes(obj_est, RME, color = ep_est, alpha = alpha), color = '#228833') +
    # add regression line with outliers
    # geom_smooth(data = df_dur_score, aes(obj_est, c(ME),),  method = lm, se = TRUE) +
    # stat_regline_equation(label.y = 325, data = df_dur_score, aes(obj_est, c(ME), label = ..eq.label..), color = "black") + # the parameter might not be good, it would be better to get the values from the model 
    # stat_regline_equation(label.y = 310, data = df_dur_score, aes(obj_est, c(ME), label = ..rr.label..), color = "black") +  # the parameter might not be good, it would be better to get the values from the model
    # add regression line without outliers
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme_classic() +
    labs(color = "Acceleration episodes:", size = 30) +
    theme(legend.position = c(0.7,0.8),  text = element_text(size = 20))+  xlab("Objective inst. speed (km/h)")+
    ylab("Estimated relative inst. speed")+
    guides(color = "none", fill = "none", alpha = "none")
  
  # geom_line(data=df_dur_score,aes(x=obj_est,y=obj_est,)) # add the identity line
  distrib_across_travel_RMEspdinst
  
  # average speed_________________________________________________________________
  df_spdave <- df %>% filter(type == "spd_mean")
  # check colinearity
  spdave_trainfactors_col <- lm(RME ~ 1 + ep_est + orientation, data = df_spdave)
  performance::check_model(spdave_trainfactors_col)
  # all good
  
  spdave_trainfactors_lm <- lm(RME ~ 1 + ep_est * orientation, data = df_spdave)
  performance::check_model(spdave_trainfactors_lm)
  summary(spdave_trainfactors_lm) # main effect of speed episode
  report(spdave_trainfactors_lm)
  
  # perform anova of coef for easier clarity
  anova(spdave_trainfactors_lm)
  report(anova(spdave_trainfactors_lm))
  # perform pairwise comparison
  comparaison_spdave <- emmeans(spdave_trainfactors_lm, ~ ep_est)  
  pairs(comparaison_spdave)
  
  bp_speed_ep_spdave = ggplot(df_spdave, aes(x=ep_est, y = RME), na.rm = FALSE) +
    geom_boxplot(na.rm = FALSE, aes(fill = ep_est, alpha = alpha), fill = '#CCBB44') +
    # geom_point(aes(color = knwYes No_est) +
    theme_classic()+ 
    xlab("Acceleration")+
    ylab("Relative estimated average speed")+
    labs(color = "Acceleration :", size = 30) +
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme( text = element_text(size = 20))+
    labs(fill = "Acceleration") +
    guides(fill = "none", alpha = "none")
  bp_speed_ep_spdave
  
  distrib_across_travel_RMEspdave <- ggplot() +
    geom_point(data = df_spdave, aes(obj_est, RME, color = ep_est, alpha = alpha), color = '#CCBB44') +
    # add regression line with outliers
    # geom_smooth(data = df_dur_score, aes(obj_est, c(ME),),  method = lm, se = TRUE) +
    # stat_regline_equation(label.y = 325, data = df_dur_score, aes(obj_est, c(ME), label = ..eq.label..), color = "black") + # the parameter might not be good, it would be better to get the values from the model 
    # stat_regline_equation(label.y = 310, data = df_dur_score, aes(obj_est, c(ME), label = ..rr.label..), color = "black") +  # the parameter might not be good, it would be better to get the values from the model
    # add regression line without outliers
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme_classic() +
    labs(color = "Acceleration:", size = 30) +
    theme(legend.position = c(0.7,0.8),  text = element_text(size = 20))+  xlab("Objective average speed (km/h)")+
    ylab("Estimated relative average speed")+
    guides(color = "none", fill = "none", alpha= "none")
  # geom_line(data=df_dur_score,aes(x=obj_est,y=obj_est,)) # add the identity line
  distrib_across_travel_RMEspdave
  
  ################################################################################
  boxplot_allmag_epest <- gridExtra::grid.arrange(bp_speed_ep_dur, bp_speed_ep_dist,bp_speed_ep_spdinst, bp_speed_ep_spdave, ncol = 2, nrow = 2)
  #ggsave(file = "D:\\2022_QTEV_Analysis_YN\\effect_of_speedepisode.pdf", plot = boxplot_allmag, width = 10, height = 10, dpi = 300, )
  
  distrib_allmag <- gridExtra::grid.arrange(distrib_across_travel_RMEdur, distrib_across_travel_RMEdist, distrib_across_travel_RMEspdinst, distrib_across_travel_RMEspdave)
  
  # figure orientation 
  bp_ori_dur = ggplot(df_dur %>% filter(!(is.na(orientation))), aes(x=orientation, y = RME), na.rm = FALSE) +
    geom_boxplot(na.rm = FALSE, alpha = 1, fill = '#66CCEE') +
    # geom_point(aes(color = knwYes No_est) +
    theme_classic()+ 
    xlab("Orientation")+
    ylab("Relative estimated duration")+
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme( text = element_text(size = 20)) +
    labs(fill = "Orientation") +
    guides(fill = "none")
  bp_ori_dur
  
  bp_ori_dist = ggplot(df_dist %>% filter(!(is.na(orientation))), aes(x=orientation, y = RME), na.rm = FALSE) +
    geom_boxplot(na.rm = FALSE, alpha = 1, fill = '#EE6677') +
    # geom_point(aes(color = knwYes No_est) +
    theme_classic()+ 
    xlab("Orientation")+
    ylab("Relative estimated distance")+
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme( text = element_text(size = 20)) +
    labs(fill = "Orientation") +
    guides(fill = "none")
  bp_ori_dist
  
  bp_ori_spdinst = ggplot(df_spdinst %>% filter(!(is.na(orientation))), aes(x=orientation, y = RME), na.rm = FALSE) +
    geom_boxplot(na.rm = FALSE, alpha = 1, fill = '#228833') +
    # geom_point(aes(color = knwYes No_est) +
    theme_classic()+ 
    xlab("Orientation")+
    ylab("Relative estimated inst. speed")+
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme( text = element_text(size = 20)) +
    labs(fill = "Orientation") +
    guides(fill = "none")
  bp_ori_spdinst
  
  bp_ori_spdave = ggplot(df_spdave %>% filter(!(is.na(orientation))), aes(x=orientation, y = RME), na.rm = FALSE) +
    geom_boxplot(na.rm = FALSE, alpha = 1, fill = '#CCBB44') +
    # geom_point(aes(color = knwYes No_est) +
    theme_classic()+ 
    xlab("Orientation")+
    ylab("Relative estimated average speed")+
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme( text = element_text(size = 20)) +
    labs(fill = "Orientation") +
    guides(fill = "none")
  bp_ori_spdave
  
  boxplot_allmag_ori <- gridExtra::grid.arrange(bp_ori_dur, bp_ori_dist, bp_ori_spdinst, bp_ori_spdave)
  
  ##############################################################################
  # effect of acceleration if we consider that rate of change impact our estimation of duration
  
  df_new$ep_est[df_new$ep_est == "acc"] <- "rate"
  df_new$ep_est[df_new$ep_est == "dec"] <- "rate"
  df_new$ep_est[df_new$ep_est == "cst"] <- "norate"
  
  df_new$ep_est <- as.factor(df_new$ep_est)
  
  df_new$ERM <- abs(df_new$ERM)
  
  # Ajoutez une colonne alpha personnalisée en fonction des catégories
  df_new <- df_new %>%
    mutate(alpha = case_when(
      ep_est == "rate" ~ 1,
      ep_est == "norate" ~ 0.3,
      TRUE ~ 1  # Valeur alpha par défaut pour les autres catégories
    ))
  
 # descriptive stats
  df_new_dur <- df_new %>% filter(type == "dur")
 # look at the new repartition of acceleration episodes
  table(df_new_dur$ep_est, useNA = "ifany")
  
  
  # duration______________________________________________________________________
  dur_trainfactors_lm <- lm(ERM ~ 0 + ep_est * orientation, data = df_new_dur)
  performance::check_model(dur_trainfactors_lm)
  summary(dur_trainfactors_lm)
  report(dur_trainfactors_lm)
  # no effect
  
  # Utilisez ggplot avec scale_fill_alpha pour spécifier l'alpha
  bp_speed_ep_dur <- ggplot(df_new_dur, aes(x = ep_est, y = ERM)) +
    geom_boxplot(aes(fill = ep_est, alpha = alpha), fill = '#66CCEE') +
    theme_classic() + 
    xlab("Acceleration episodes") +
    ylab("Relative duration error") +
    # geom_hline(aes(yintercept = 1), linetype = "solid", size = 1, color = "grey") +
    theme(text = element_text(size = 20)) +
    labs(fill = "Acceleration episode") +
    guides(fill = "none", alpha = "none") +  # Masquer les légendes pour la couleur et l'alpha
    scale_alpha_identity()  # Permet de conserver les valeurs alpha que vous avez spécifiées
  bp_speed_ep_dur
  
  
  # distance______________________________________________________________________
  df_new_dist <- df_new %>% filter(type == "dist")
  dist_trainfactors_lm <- lm(ERM ~ 0 + ep_est * orientation, data = df_new_dist)
  performance::check_model(dist_trainfactors_lm)
  summary(dist_trainfactors_lm)
  report(dist_trainfactors_lm)
  # main effect of episode, estimations are overestimated when the rate of change is changing (in acc and dec compared to cst)  
  
  bp_speed_ep_dist = ggplot(df_new_dist, aes(x=ep_est, y = ERM), na.rm = FALSE) +
    geom_boxplot(na.rm = FALSE, aes(fill = ep_est, alpha = alpha), fill = '#EE6677') +
    # geom_point(aes(color = knwYes No_est) +
    theme_classic()+ 
    xlab("Acceleration episodes")+
    ylab("Relative  distance error")+
    # geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme( text = element_text(size = 20)) +
    labs(fill = "Acceleration episode") +
    guides(fill = "none", alpha = "none")
  bp_speed_ep_dist
  
  
  # instantaneous speed___________________________________________________________
  df_new_spdinst <- df_new %>% filter(type == "spd_inst")
  spdinst_trainfactors_lm <- lm(ERM ~ 0 + ep_est * orientation, data = df_new_spdinst)
  performance::check_model(spdinst_trainfactors_lm)
  summary(spdinst_trainfactors_lm) # significant model 
  report(spdinst_trainfactors_lm)
  # main effect of rate
  
  bp_speed_ep_spdinst = ggplot(df_new_spdinst, aes(x=ep_est, y = ERM), na.rm = FALSE) +
    geom_boxplot(na.rm = FALSE, aes(fill = ep_est, alpha = alpha), fill = '#228833') +
    # geom_point(aes(color = knwYes No_est) +
    theme_classic()+ 
    xlab("Acceleration episodes")+
    ylab("Relative inst. speed error")+
    # geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme( text = element_text(size = 20))+
    labs(fill = "Acceleration episode") +
    guides(fill = "none", alpha = "none")
  bp_speed_ep_spdinst
  # speed is overestimated (and closer to one) when rate is changing
  
  # average speed_________________________________________________________________
  df_new_spdave <- df_new %>% filter(type == "spd_mean")
  spdave_trainfactors_lm <- lm(ERM ~ 0 + ep_est * orientation, data = df_new_spdave)
  performance::check_model(spdave_trainfactors_lm)
  summary(spdave_trainfactors_lm) 
  report(spdave_trainfactors_lm)
  # main effect of rate
  
  bp_speed_ep_spdave = ggplot(df_new_spdave, aes(x=ep_est, y = ERM), na.rm = FALSE) +
    geom_boxplot(na.rm = FALSE, aes(fill = ep_est, alpha = alpha), fill = '#CCBB44') +
    # geom_point(aes(color = knwYes No_est) +
    theme_classic()+ 
    xlab("Acceleration episodes")+
    ylab("Relative average speed error")+
    labs(color = "Acceleration episodes:", size = 30) +
    # geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme( text = element_text(size = 20))+
    labs(fill = "Acceleration episode") +
    guides(fill = "none", alpha = "none")
  bp_speed_ep_spdave
  # average speed is overestimated when speed is changing
  
  ################################################################################
  boxplot_allmag_epest <- gridExtra::grid.arrange(bp_speed_ep_dur, bp_speed_ep_dist,bp_speed_ep_spdinst, bp_speed_ep_spdave, ncol = 2, nrow = 2)
  #ggsave(file = "D:\\2022_QTEV_Analysis_YN\\effect_of_speedepisode.pdf_new", plot = boxplot_allmag, width = 10, height = 10, dpi = 300, )
  
  distrib_allmag <- gridExtra::grid.arrange(distrib_across_travel_RMEdur, distrib_across_travel_RMEdist, distrib_across_travel_RMEspdinst, distrib_across_travel_RMEspdave)
  
  # figure orientation 
  bp_ori_dur = ggplot(df_new_dur %>% filter(!(is.na(orientation))), aes(x=orientation, y = RME), na.rm = FALSE) +
    geom_boxplot(na.rm = FALSE, alpha = 1, fill = '#66CCEE') +
    # geom_point(aes(color = knwYes No_est) +
    theme_classic()+ 
    xlab("Orientation")+
    ylab("Relative estimated duration")+
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme( text = element_text(size = 20)) +
    labs(fill = "Orientation") +
    guides(fill = "none")
  bp_ori_dur
  
  bp_ori_dist = ggplot(df_new_dist %>% filter(!(is.na(orientation))), aes(x=orientation, y = RME), na.rm = FALSE) +
    geom_boxplot(na.rm = FALSE, alpha = 1, fill = '#EE6677') +
    # geom_point(aes(color = knwYes No_est) +
    theme_classic()+ 
    xlab("Orientation")+
    ylab("Relative estimated distance")+
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme( text = element_text(size = 20)) +
    labs(fill = "Orientation") +
    guides(fill = "none")
  bp_ori_dist
  
  bp_ori_spdinst = ggplot(df_new_spdinst %>% filter(!(is.na(orientation))), aes(x=orientation, y = RME), na.rm = FALSE) +
    geom_boxplot(na.rm = FALSE, alpha = 1, fill = '#228833') +
    # geom_point(aes(color = knwYes No_est) +
    theme_classic()+ 
    xlab("Orientation")+
    ylab("Relative estimated inst. speed")+
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme( text = element_text(size = 20)) +
    labs(fill = "Orientation") +
    guides(fill = "none")
  bp_ori_spdinst
  
  bp_ori_spdave = ggplot(df_new_spdave %>% filter(!(is.na(orientation))), aes(x=orientation, y = RME), na.rm = FALSE) +
    geom_boxplot(na.rm = FALSE, alpha = 1, fill = '#CCBB44') +
    # geom_point(aes(color = knwYes No_est) +
    theme_classic()+ 
    xlab("Orientation")+
    ylab("Relative estimated average speed")+
    geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
    theme( text = element_text(size = 20)) +
    labs(fill = "Orientation") +
    guides(fill = "none")
  bp_ori_spdave
  
  boxplot_allmag_ori <- gridExtra::grid.arrange(bp_ori_dur, bp_ori_dist, bp_ori_spdinst, bp_ori_spdave)
  
  
  