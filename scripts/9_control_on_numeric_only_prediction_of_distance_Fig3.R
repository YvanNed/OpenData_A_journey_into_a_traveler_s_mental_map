################################################################################
################### How did participant estimated distance? ####################
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

rm(list=ls())     # to clear current workspace

################################################################################
# load data
################################################################################

# load old data that were not scored
setwd("C:\\Users\\yvann\\OneDrive\\2022_QTEV_Analysis_YN\\")
df <- read.csv("dataframe_ready2use.csv")

# keep only numeric estimation
df <- df %>% filter(format == "numeric")

# select dataframe per type of estimation in order to remove outliers
df_dur <- df %>% filter(type == "dur")
df_dist <- df %>% filter(type == "dist")  
df_spdinst <- df %>% filter(type == "spd_inst")  
df_spdmean <- df %>% filter(type == "spd_mean")  


# remove outliers based on blursday and fuat's paper____________________________
# duration______________________________________________________________________
# first, based on objective value
dur_min_thres <- min(df_dur$obj_est)*0.2
dur_max_thres <- max(df_dur$obj_est)*5

which(df_dur$est < dur_min_thres)
which(df_dur$est > dur_max_thres)
# none are removed

# second, based on relative duration errors beyond the central 95th percentile
# compute percentile:
dur_lower_percentile <- quantile(df_dur$ER_est, 0.025)
dur_upper_percentile <- quantile(df_dur$ER_est, 0.975)

# detect which is lower and upper
which(df_dur$ER_est < dur_lower_percentile)
which(df_dur$ER_est > dur_upper_percentile)
df_dur_score_clean <- subset(df_dur, ER_est >= dur_lower_percentile & ER_est <=dur_upper_percentile)
# 5.6% removed participants

# distance______________________________________________________________________
# first, based on objective value
dist_min_thres <- min(df_dist$obj_est)*0.2
dist_max_thres <- max(df_dist$obj_est)*5

which(df_dist$est < dist_min_thres)
which(df_dist$est > dist_max_thres)
# 1 is detected, and removed by quantile

# second, based on relative duration errors beyond the central 95th percentile
# compute percentile:
dist_lower_percentile <- quantile(df_dist$ER_est, 0.025, na.rm = TRUE)
dist_upper_percentile <- quantile(df_dist$ER_est, 0.975, na.rm = TRUE)

# detect which is lower and upper
which(df_dist$ER_est <= dist_lower_percentile)
which(df_dist$ER_est >= dist_upper_percentile)
df_dist_score_clean <- subset(df_dist, ER_est >= dist_lower_percentile & ER_est <=dist_upper_percentile)
# 7.2% removed participants

# instantaneous speed___________________________________________________________
# first, based on objective value
spdinst_min_thres <- min(df_spdinst$obj_est)*0.2
spdinst_max_thres <- max(df_spdinst$obj_est)*5

which(df_spdinst$est < spdinst_min_thres)
which(df_spdinst$est > spdinst_max_thres)
# none is removed 

# second, based on relative duration errors beyond the central 95th percentile
# compute percentile:
spdinst_lower_percentile <- quantile(df_spdinst$ER_est, 0.025, na.rm = TRUE)
spdinst_upper_percentile <- quantile(df_spdinst$ER_est, 0.975, na.rm = TRUE)

# detect which is lower and upper
which(df_spdinst$ER_est <= spdinst_lower_percentile)
which(df_spdinst$ER_est >= spdinst_upper_percentile)
df_spdinst_score_clean <- subset(df_spdinst, ER_est >= spdinst_lower_percentile & ER_est <=spdinst_upper_percentile)
# 6.9% removed participants

# average speed_________________________________________________________________
# first, based on objective value
spdave_min_thres <- min(df_spdmean$obj_est)*0.2
spdave_max_thres <- max(df_spdmean$obj_est)*5

which(df_spdmean$est < spdave_min_thres)
which(df_spdmean$est > spdave_max_thres)
# none is removed

# second, based on relative duration errors beyond the central 95th percentile
# compute percentile:
spdave_lower_percentile <- quantile(df_spdmean$ER_est, 0.025, na.rm = TRUE)
spdave_upper_percentile <- quantile(df_spdmean$ER_est, 0.975, na.rm = TRUE)

# detect which is lower and upper
which(df_spdmean$ER_est <= spdave_lower_percentile)
which(df_spdmean$ER_est >= spdave_upper_percentile)
df_spdave_score_clean <- subset(df_spdmean, ER_est >= spdave_lower_percentile & ER_est <=spdave_upper_percentile)
# 7.3% removed participants
#_______________________________________________________________________________


df <- rbind(df_dur_score_clean, df_dist_score_clean, df_spdinst_score_clean, df_spdave_score_clean)

df$ME <- df$est

# Extract dataframe______________________________________________________________________
df_dur_raw <- df %>% filter(type == "dur")

df_dist_raw <- df %>% filter(type == "dist")

df_spd_raw <- df %>% filter(type == "spd_mean")

# select participant 
df_list <- list(df_dur_raw, df_dist_raw, df_spd_raw)

common_participants <- Reduce(intersect, lapply(df_list, function(df) df$participants))

shared_dfs <- lapply(df_list, function(df) subset(df, participants %in% common_participants))

df_clean <- do.call(rbind, shared_dfs)

df_dur <- df_clean %>% filter(type == "dur")

df_dist <- df_clean %>% filter(type == "dist") # why there is not the same number of participant in this dataframe ?

df_spd <- df_clean %>% filter(type == "spd_mean")

# ==> 16 participants are removed when I combined all the outliers 
################################################################################


# 1- participant rely on their knowledge about the total distance and they simply do a produit en croix with the elapsed duration
df_dist$pred_dist_knw <- (df_dur$ME / df_dur$knw_est)*df_dist$knw_est

lm_dist_knw <- lm(ME ~ pred_dist_knw, data = df_dist)
performance::check_model(lm_dist_knw)
summary(lm_dist_knw)
report(lm_dist_knw)

# 2- participant rely on their known maximal speed, doing the simplification that the train mostly goes to its maximal speed

df_dist$pred_dist_spd <- df_spd$knw_est * (df_dur$ME/60)

lm_dist_durknwspd <- lm(ME ~ pred_dist_spd, data = df_dist)
performance::check_model(lm_dist_durknwspd)
summary(lm_dist_durknwspd)
report(lm_dist_durknwspd)

# 2- participant use a fixed speed of 320 km/h

# df_dist$pred_dist_spdfixed <- 320 * (df_dur$ME/60)
# 
# lm_distspdfixed <- lm(ME ~ pred_dist_spdfixed, data = df_dist)
# performance::check_model(lm_distspdfixed)
# summary(lm_distspdfixed)
# report(lm_distspdfixed)

#  old 4- participant just use duration to estimate distance
# # this is essentially the same as 3 because the only difference is a coefficient
# df_dist$dur <- df_dur$ME
# lm_distonlydur <- lm(ME ~ dur, data = df_dist)
# performance::check_model(lm_distonlydur)
# summary(lm_distonlydur)

# 3- they use average speed
df_dist$preddist_duravespd <- df_spd$ME*(df_dur$ME/60)
lm_dist_duravespd <- lm(ME ~ preddist_duravespd, data = df_dist)
performance::check_model(lm_dist_duravespd)
summary(lm_dist_duravespd)
report(lm_dist_duravespd)
# It seems that participant are using their knowledge about the total distance !!!
# adjusted r² are as ranked as followed:
# dur_ratio*distknw > dur_only = fixedspd*dur > ave_spd*dur > maxspd*dur

# selection of the best model with AIC (it does not make any  sense since it is not an incrementation of model)
# performance::compare_performance(lm_distknw, lm_distspdfixed, lm_dist_duravespd) # new way. I prefer this because I understand better the parameters that are displayed
# # trying other order since it is not an incrementation of model
# performance::compare_performance(lm_distspdfixed, lm_dist_duravespd, lm_distknw) # new way. I prefer this because I understand better the parameters that are displayed
# performance::compare_performance(lm_dist_duravespd, lm_distknw, lm_distspdfixed) # new way. I prefer this because I understand better the parameters that are displayed
# performance::compare_performance(lm_distknw, lm_dist_duravespd,  lm_distspdfixed) # new way. I prefer this because I understand better the parameters that are displayed
# performance::compare_performance(lm_distspdfixed, lm_distknw, lm_dist_duravespd) # new way. I prefer this because I understand better the parameters that are displayed
# performance::compare_performance(lm_dist_duravespd, lm_distspdfixed, lm_distknw) # new way. I prefer this because I understand better the parameters that are displayed
# # the order doesn't matter

# now I need to do the figure

# plot the regression curve
rawdist_as_preddist <- ggplot() +
  # add the predicted distance based on duration ratio and distance knowledge
  geom_point(data = df_dist, aes(pred_dist_knw, c(ME)), color = "#c51b7d", alpha = 0.3) +
  # add regression line and equation
  geom_smooth(data = df_dist, aes(pred_dist_knw, c(ME),),  method = lm, se = TRUE,  color = "#c51b7d", fill ="#c51b7d") +
  # stat_regline_equation(label.x = 20, label.y = 600, data = df_dist, aes(pred_dist_knw, c(ME), label = ..eq.label..), color = "#66c2a5") + # the parameter might not be good, it would be better to get the values from the model 
  # stat_regline_equation(label.x = 20, label.y = 580, data = df_dist, aes(pred_dist_knw, c(ME), label = ..rr.label..), color = "#c51b7d", size=6) +  # the parameter might not be good, it would be better to get the values from the model
  annotate("text", x = 130, y = 500, label = "Knowledge: R² = 0.88", col = "#c51b7d", size = 4)+
  
  theme_classic() +
  
  # add the predicted distance based on duration and an arbitrary speed
  geom_point(data = df_dist, aes(pred_dist_spd, c(ME)), color = "#b8e186", alpha = 0.3) +
  # add regression line and equation
  geom_smooth(data = df_dist, aes(pred_dist_spd, c(ME),),  method = lm, se = TRUE, color = "#b8e186", fill ="#b8e186") +
  annotate("text", x = 150, y = 540, label = "Maximum speed: R² = 0.67", col = "#b8e186", size = 4)+
  
  # stat_regline_equation(label.x = 20, label.y = 500, data = df_dist, aes(pred_dist_spdfixed, c(ME), label = ..eq.label..), color = "#fc8d62") + # the parameter might not be good, it would be better to get the values from the model 
  # stat_regline_equation(label.x = 20, label.y = 530, data = df_dist, aes(pred_dist_spd, c(ME), label = ..rr.label..), color = "#b8e186", size=6) +  # the parameter might not be good, it would be better to get the values from the model
  
  # add the predicted distance based on averaged speed
  geom_point(data = df_dist, aes(preddist_duravespd, c(ME)), color = "grey", alpha = 0.3) +
  # add regression line and equation
  geom_smooth(data = df_dist, aes(preddist_duravespd, c(ME),),  method = lm, se = TRUE, color = "grey", fill = "grey") +
  annotate("text", x = 150, y = 580, label = "Estimated speed: R² = 0.69", col = "grey", size=4)+
  
  # stat_regline_equation(label.x = 20, label.y = 400, data = df_dist, aes(preddist_duravespd, c(ME), label = ..eq.label..), color = "#8da0cb") + # the parameter might not be good, it would be better to get the values from the model 
  # stat_regline_equation(label.x = 20, label.y = 480, data = df_dist, aes(preddist_duravespd, c(ME), label = ..rr.label..), color = "grey", size=6) +  # the parameter might not be good, it would be better to get the values from the model
  
  
  # add the point of subjective equality
  # geom_segment(aes(x = 0, y = dur_point_of_intersect, xend = dur_point_of_intersect , yend = dur_point_of_intersect), linetype = "dashed", color = "red") +
  # geom_segment(aes(x = dur_point_of_intersect, y = 0, xend = dur_point_of_intersect , yend = dur_point_of_intersect), linetype = "dashed", color = "red") +
  # annotate("text", x = 7, y = dur_point_of_intersect+3, label = "79.8", col = "red")+
  # annotate("text", x = dur_point_of_intersect+7, y = 3, label = "79.8", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(breaks=seq(0,max(df_dist$pred_dist_knw[!(is.na(df_dist$pred_dist_knw))]),100), limits = c(0, max(df_dist$pred_dist_knw[!(is.na(df_dist$pred_dist_knw))])), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,max(df_dist$pred_dist_knw[!(is.na(df_dist$pred_dist_knw))]),100), limits = c(0, max(df_dist$pred_dist_knw[!(is.na(df_dist$pred_dist_knw))])), expand = c(0,0)) +
  theme( text = element_text(size = 20))+
  xlab("Modeled distance (km)")+
  ylab("Estimated distance (km)")+
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)
rawdist_as_preddist

################################################################################
############ Does estimated duration can be predicted by distance ##############
################################################################################

# 1- participant rely on their knowledge about the total duration and they simply do a product en croix with the elapsed distance
df_dur$pred_dur_knw <- (df_dist$ME / df_dist$knw_est)*df_dur$knw_est

lm_dur_knw <- lm(ME ~ pred_dur_knw, data = df_dur)
performance::check_model(lm_dur_knw)
summary(lm_dur_knw)
report(lm_dur_knw)

# 2- participant rely on their known maximal speed
# 
df_dur$pred_dur_spd <- df_dist$ME / (df_spd$knw_est/60)

lm_dur_distknwspd <- lm(ME ~ pred_dur_spd, data = df_dur)
performance::check_model(lm_dur_distknwspd)
summary(lm_dur_distknwspd)
report(lm_dur_distknwspd)

# 2- participant use a fixed speed of 320 km/h
# 
# df_dur$pred_dur_spdfixed <- df_dist$ME / (320/60)
# 
# lm_durspdfixed <- lm(ME ~ pred_dur_spdfixed, data = df_dur)
# performance::check_model(lm_durspdfixed)
# summary(lm_durspdfixed)
# report(lm_durspdfixed)

# old 4- participant just use duration to estimate distance
# df_dur$dist <- df_dist$ME
# lm_duronlydist <- lm(ME ~ dist, data = df_dur)
# performance::check_model(lm_duronlydist)
# summary(lm_duronlydist)

# 3- they use average speed
df_dur$preddur_distavespd <- df_dist$ME / (df_spd$ME/60)
lm_dur_distavespd <- lm(ME ~ preddur_distavespd, data = df_dur)
performance::check_model(lm_dur_distavespd)
summary(lm_dur_distavespd)
report(lm_dur_distavespd)

# model comparison
# performance::compare_performance(lm_dur_distavespd, lm_durspdfixed, lm_durknw)

# => the pattern is different for duration if only numeric estimation are used 

# adjusted r² are as ranked as followed:
# dist_only = dist/fixedspd ~= dist_ratio*durknw >  maxspd*dur > ave_spd*dur 

rawdur_as_preddur <- ggplot() +
  # add the predicted distance based on duration ratio and distance knowledge
  geom_point(data = df_dur, aes(pred_dur_knw, c(ME)), color = "#c51b7d", alpha = 0.3) +
  # add regression line and equation
  geom_smooth(data = df_dur, aes(pred_dur_knw, c(ME),),  method = lm, se = TRUE, color = "#c51b7d", fill = "#c51b7d") +
  # stat_regline_equation(label.x = 10, label.y = 170, data = df_dur, aes(pred_dur_knw, c(ME), label = ..eq.label..), color = "#66c2a5") + # the parameter might not be good, it would be better to get the values from the model
  # stat_regline_equation(label.x = 10, label.y = 160, data = df_dur, aes(pred_dur_knw, c(ME), label = ..rr.label..), color = "#c51b7d", size=6) +  # the parameter might not be good, it would be better to get the values from the model
  annotate("text", x = 35, y = 140, label = "Knowledge: R² = 0.77", col = "#c51b7d", size = 4)+
  
  theme_classic() +
  
  # add the predicted distance based on duration and an arbitrary speed
  geom_point(data = df_dur, aes(pred_dur_spd, c(ME)), color = "#b8e186", alpha = 0.3) +
  # add regression line and equation
  geom_smooth(data = df_dur, aes(pred_dur_spd, c(ME),),  method = lm, se = TRUE, color = "#b8e186", fill = "#b8e186") +
  # stat_regline_equation(label.x = 10, label.y = 150, data = df_dur, aes(pred_dur_spdfixed, c(ME), label = ..eq.label..), color = "#fc8d62") + # the parameter might not be good, it would be better to get the values from the model 
  # stat_regline_equation(label.x = 10, label.y = 145, data = df_dur, aes(pred_dur_spd, c(ME), label = ..rr.label..), color = "#b8e186", size=6) +  # the parameter might not be good, it would be better to get the values from the model
  annotate("text", x = 45, y = 150, label = "Maximum speed: R² = 0.68", col = "#b8e186", size = 4)+
  
  # add the predicted distance based on averaged speed
  geom_point(data = df_dur, aes(preddur_distavespd, c(ME)), color = "grey", alpha = 0.3) +
  # add regression line and equation
  geom_smooth(data = df_dur, aes(preddur_distavespd, c(ME),),  method = lm, se = TRUE, color = "grey", fill ="grey") +
  # stat_regline_equation(label.x = 10, label.y = 130, data = df_dur, aes(preddur_distavespd, c(ME), label = ..eq.label..), color = "#8da0cb") + # the parameter might not be good, it would be better to get the values from the model 
  # stat_regline_equation(label.x = 10, label.y = 130, data = df_dur, aes(preddur_distavespd, c(ME), label = ..rr.label..), color = "grey", size=6) +  # the parameter might not be good, it would be better to get the values from the model
  annotate("text", x = 45, y = 160, label = "Estimated speed: R² = 0.69", col = "grey", size = 4)+
  
  
  # add the point of subjective equality
  # geom_segment(aes(x = 0, y = dur_point_of_intersect, xend = dur_point_of_intersect , yend = dur_point_of_intersect), linetype = "dashed", color = "red") +
  # geom_segment(aes(x = dur_point_of_intersect, y = 0, xend = dur_point_of_intersect , yend = dur_point_of_intersect), linetype = "dashed", color = "red") +
  # annotate("text", x = 7, y = dur_point_of_intersect+3, label = "79.8", col = "red")+
  # annotate("text", x = dur_point_of_intersect+7, y = 3, label = "79.8", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(breaks=seq(0,175,25), limits = c(0, 175), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,175,25), limits = c(0, 175), expand = c(0,0)) +
  theme( text = element_text(size = 20))+
  xlab("Modeled duration (min)")+
  ylab("Estimated duration (min)")+
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)
rawdur_as_preddur

################################################################################
############# Does estimated speed correspond to distance/duration #############
################################################################################

df_spd$pred_spd <- df_dist$ME/(df_dur$ME/60)

lm_spdpred <- lm(ME~pred_spd, data = df_spd)
performance::check_model(lm_spdpred)
summary(lm_spdpred)
report(lm_spdpred)

# look at the relationship between the two
pred_spd_as_est_spd <- ggplot() +
  geom_point(data = df_spd, aes(pred_spd, ME), color = "grey", alpha = 0.3) +
  # add regression line
  geom_smooth(data = df_spd, aes(pred_spd, ME,),  method = lm, se = TRUE, color = "grey", fill = "grey") +
  # stat_regline_equation(label.x = 10, label.y = 325, data = df_spd, aes(pred_spd, ME, label = ..eq.label..), color = "blue") + # the parameter might not be good, it would be better to get the values from the model
  # stat_regline_equation(label.x = 10, label.y = 310, data = df_spd, aes(pred_spd, ME, label = ..rr.label..), color = "blue") +  # the parameter might not be good, it would be better to get the values from the model

  # add regression line manually because when I set limits to the plot it affect the coefficients of the regression curves
  # geom_abline(intercept = 205, slope = 3.09e-03, color = "blue")+
  annotate("text", x = 150, y = 450, label = "y = 205 + 3.09e-03 x", col = "grey", size = 6)+
  annotate("text", x = 180, y = 415, label = "R² = 2.85e-05 (p = 0.934)", col = "grey", size = 6)+
  # 
  # add the point of subjective equality
  # geom_segment(aes(x = 0, y = spd_mean_point_of_intersect, xend = spd_mean_point_of_intersect , yend = spd_mean_point_of_intersect), linetype = "dashed", color = "red") +
  # geom_segment(aes(x = spd_mean_point_of_intersect, y = 0, xend = spd_mean_point_of_intersect , yend = spd_mean_point_of_intersect), linetype = "dashed", color = "red") +
  # annotate("text", x = 20, y = spd_mean_point_of_intersect+10, label = "212", col = "red")+
  # annotate("text", x = spd_mean_point_of_intersect+20, y = 10, label = "212", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  # scale_x_continuous(breaks=seq(0,max(df_spd_est$ME_pred[!(is.na(df_spd_est$ME_pred))]),50), limits = c(0, max(df_spd_est$ME_pred[!(is.na(df_spd_est$ME_pred))])), expand = c(0,0)) +
  # scale_y_continuous(breaks=seq(0,max(df_spd_est$ME_pred[!(is.na(df_spd_est$ME_pred))]),50), limits = c(0, max(df_spd_est$ME_pred[!(is.na(df_spd_est$ME_pred))])), expand = c(0,0)) +
  # 
  scale_x_continuous(breaks=seq(0,500,50), limits = c(0, 500), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,500,50), limits = c(0, 500), expand = c(0,0)) +
  
  
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)+
  
  # add regression line without outliers
  theme_classic() +
  theme( text = element_text(size = 20))+
  xlab("Modeled average speed (km/h)")+
  ylab("Estimated average speed (km/h)")
pred_spd_as_est_spd

# looking at the relationship between pred/est spdmean and objective spdmean to investigate whether there is a different bias in duration and distance
lm_pred_obj <- lm(pred_spd~obj_est, data = df_spd)
performance::check_model(lm_pred_obj)
summary(lm_pred_obj)
report(lm_pred_obj)

lm_ME_obj <- lm(ME~obj_est, data = df_spd)
performance::check_model(lm_ME_obj)
summary(lm_ME_obj)
report(lm_ME_obj)

# create a dataframe with a specific column spd_type
df_spd_pred <- df_spd
df_spd_pred$ME <- df_spd_pred$pred_spd

df_spd$spd_type <- "estimated"
df_spd_pred$spd_type <- "modeled"

df_spd_plot <- rbind(df_spd, df_spd_pred)

spd_type_as_obj_spd <- ggplot() +
  geom_point(data = df_spd_plot, aes(obj_est, c(ME), color = spd_type)) +
  # add regression line with outliers
  # geom_smooth(data = df_spd, aes(obj_est, c(ME), fill = spd_type),  method = lm, se = TRUE, fullrange = TRUE) +
  # stat_regline_equation(label.x = 10, label.y = 325, data = df_spd_mean_score, aes(obj_est, c(ME), label = ..eq.label..), color = "black") + # the parameter might not be good, it would be better to get the values from the model 
  # stat_regline_equation(label.x = 10, label.y = 310, data = df_spd_mean_score, aes(obj_est, c(ME), label = ..rr.label..), color = "black") +  # the parameter might not be good, it would be better to get the values from the model
  
  # add regression line manually because when I set limits to the plot it affect the coefficients of the regression curves
  # estimated
  geom_abline(intercept = 162, slope = 0.25, color = "#F8766D", size=0.8)+
  annotate("text", x = 125, y = 50, label = "y = 167 + 0.25 x", col = "#F8766D", size = 8)+
  annotate("text", x = 125, y = 30, label = "R² = 0.062 (p < 0.05)", col = "#F8766D", size = 8)+
  # predicted
  geom_abline(intercept = 211, slope = 0.067, color = "#00BFC4", size=0.8)+
  annotate("text", x = 300, y = 50, label = "y = 211 + 0.067 x", col = "#00BFC4", size = 8)+
  annotate("text", x = 300, y = 30, label = "R² = 0.0015 (p = 0.55)", col = "#00BFC4", size = 8)+
  
  # add the point of subjective equality
  # geom_segment(aes(x = 0, y = spd_mean_point_of_intersect, xend = spd_mean_point_of_intersect , yend = spd_mean_point_of_intersect), linetype = "dashed", color = "red") +
  # geom_segment(aes(x = spd_mean_point_of_intersect, y = 0, xend = spd_mean_point_of_intersect , yend = spd_mean_point_of_intersect), linetype = "dashed", color = "red") +
  # annotate("text", x = 20, y = spd_mean_point_of_intersect+10, label = "212", col = "red")+
  # annotate("text", x = spd_mean_point_of_intersect+20, y = 10, label = "212", col = "red")+
  # 
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(breaks=seq(0,500,50), limits = c(0, 400), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,500,50), limits = c(0, 400), expand = c(0,0)) +
  
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8)+
  
  # add regression line without outliers
  theme_classic() +
  labs(color = "Type:", size = 40) +
  theme(legend.position = c(0.8,0.4), text = element_text(size = 30))+
  xlab("Objective average speed (km/h)")+
  ylab("Estimated & modeled average speed (km/h)")
spd_type_as_obj_spd

################################################################################
#################### simple correlation duration ~ distance ####################
################################################################################


df_dur$RME_dist <- df_dist$RME
cor_res <- cor.test(df_dur$RME, df_dur$RME_dist)
report(cor_res)
# bad correlation of 0.36

cor_rme_distdur <- ggplot()+
  geom_point(data = df_dur, aes(RME, RME_dist), color = "grey", alpha = 0.3)+
  # if I want to add a regression line, uncomment the line below
  # geom_smooth(data = df_dur, aes(RME, RME_dist), method = "lm", color = "black", se = TRUE, fullrange = TRUE) +
  geom_hline(aes(yintercept = 1),linetype = "dashed", size = 1, color = "black")+
  geom_vline(aes(xintercept = 1),linetype = "dashed", size = 1, color = "black")+
  scale_x_continuous(breaks=seq(0,5,1), limits = c(0, 5), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(1,12,2), limits = c(0, 12), expand = c(0,0)) +
  annotate("text", x = 0.2, y = 0.5, label = "51", col = "red", size = 6)+
  annotate("text", x = 2.4, y = 5, label = "n = 77", col = "red", size = 6)+
  annotate("text", x = 2.2, y = 0.5, label = "36", col = "red", size = 6)+
  annotate("text", x = 0.2, y = 5, label = "45", col = "red", size = 6)+
  annotate("text", x = 2.51, y = 8, label = "r = 0.36", col = "black", size = 6)+
  xlab("relative estimated duration")+
  ylab("relative estimated distance")+
  theme_classic()+
  theme(legend.position = c(0.8,0.4), text = element_text(size = 20))
cor_rme_distdur

# count the number of participant that underestimate both
# remove na
count_both_below_1 <- sum(df_dur$RME < 1 & df_dur$RME_dist < 1, na.rm = TRUE)

# Comptez le nombre de lignes où les deux colonnes sont au-dessus de 1
count_both_above_1 <- sum(df_dur$RME > 1 & df_dur$RME_dist > 1, na.rm = TRUE)

# Comptez le nombre de lignes où une colonne est plus grande que 1 et l'autre plus petite
count_one_above_one_below <- sum((df_dur$RME > 1 & df_dur$RME_dist < 1), na.rm = TRUE)

# Comptez le nombre de lignes où une colonne est plus grande que 1 et l'autre plus petite
count_one_below_one_above <- sum((df_dur$RME < 1 & df_dur$RME_dist > 1), na.rm = TRUE)

# Affichez les résultats
cat("Nombre de participant sousestimant durée et distance :", count_both_below_1, "\n")
cat("Nombre de participant surestimant durée et distance :", count_both_above_1, "\n")
cat("Nombre de participant surestimant la durée et sousestimant la distance :", count_one_above_one_below, "\n")
cat("Nombre de participant sousestimant la durée et surestimant la distance :", count_one_below_one_above, "\n")


gridExtra::grid.arrange(pred_spd_as_est_spd, cor_rme_distdur, rawdist_as_preddist, rawdur_as_preddur,  nrow = 2, ncol = 2)

