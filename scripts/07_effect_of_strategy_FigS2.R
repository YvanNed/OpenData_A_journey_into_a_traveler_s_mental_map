################################################################################
###################### Control for the effect of strategy ######################
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

setwd("C:\\Users\\yvann\\OneDrive\\2022_QTEV_Analysis_YN")
df <- read.csv("dataframe_ready2use_score.csv")
df$est_strat <- as.factor(df$est_strat)

# duration______________________________________________________________________
df_dur <- df %>% filter(type == "dur")

table(df_dur$est_strat, useNA = "ifany")
table(df_dur$ep_est, useNA = "ifany")


dur_strategy_lm <- lm(RME ~ 1 + est_strat, data = df_dur)
performance::check_model(dur_strategy_lm)
summary(dur_strategy_lm)
report(dur_strategy_lm)
# use camill way to inspect stats
anova(dur_strategy_lm)

descdist(df_dur$RME[df_dur$est_strat == 1], discrete = FALSE, boot = 1000, method = "unbiased",graph = TRUE, obs.col = "darkblue", obs.pch = 16, boot.col = "orange")
descdist(df_dur$RME[df_dur$est_strat == 2], discrete = FALSE, boot = 1000, method = "unbiased",graph = TRUE, obs.col = "darkblue", obs.pch = 16, boot.col = "orange")
descdist(df_dur$RME[df_dur$est_strat == 3], discrete = FALSE, boot = 1000, method = "unbiased",graph = TRUE, obs.col = "darkblue", obs.pch = 16, boot.col = "orange")
descdist(df_dur$RME[df_dur$est_strat == 4], discrete = FALSE, boot = 1000, method = "unbiased",graph = TRUE, obs.col = "darkblue", obs.pch = 16, boot.col = "orange")
descdist(df_dur$RME[df_dur$est_strat == 5], discrete = FALSE, boot = 1000, method = "unbiased",graph = TRUE, obs.col = "darkblue", obs.pch = 16, boot.col = "orange")
descdist(df_dur$RME[df_dur$est_strat == 6], discrete = FALSE, boot = 1000, method = "unbiased",graph = TRUE, obs.col = "darkblue", obs.pch = 16, boot.col = "orange")

# no effect of strategy

bp_strat_dur = ggplot(df_dur, aes(x=est_strat, y = RME), na.rm = FALSE) +
  geom_boxplot(na.rm = FALSE, alpha = 0.2, aes(fill = est_strat, color = est_strat)) +
  # geom_point(aes(color = knwYes No_est) +
  theme_classic()+ 
  xlab("Strategy")+
  ylab("Relative estimated duration")+
  geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
  # ylim(0, 5)+
  # scale_fill_manual(breaks = c("acc", "cst", "dec"), values = c("#8dd3c7", "#ffffb3", "#bebada")) +
  # labs(fill = "Speed episodes:", size = 35) +
  theme( text = element_text(size = 20))+
  guides(fill = "none", color = "none")
bp_strat_dur

# distance______________________________________________________________________
df_dist <- df %>% filter(type == "dist")

dist_strategy_lm <- lm(RME ~ 1 + est_strat, data = df_dist)
performance::check_model(dist_strategy_lm)
summary(dist_strategy_lm)
report(dist_strategy_lm)
# no effect of strategy
# use camill way to inspect stats
anova(dist_strategy_lm)

# distance no effect
bp_strat_dist = ggplot(df_dist %>% filter(est_strat != 0), aes(x=est_strat, y = RE_est), na.rm = FALSE) +
  geom_boxplot(na.rm = FALSE, alpha = 0.2, aes(fill = est_strat, color = est_strat)) +
  # geom_point(aes(color = knwYes No_est) +
  theme_classic()+ 
  xlab("Strategy")+
  ylab("Relative estimated distance")+
  geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
  # ylim(0, 10)+
  # scale_fill_manual(breaks = c("acc", "cst", "dec"), values = c("#8dd3c7", "#ffffb3", "#bebada")) +
  # labs(fill = "Speed episodes:", size = 35) +
  theme( text = element_text(size = 20))+
  guides(fill = "none", color = "none")
bp_strat_dist


# instantaneous speed___________________________________________________________
df_spdinst <- df %>% filter(type == "spd_inst")

spdinst_strategy_lm <- lm(RME ~ 1 + est_strat, data = df_spdinst)
performance::check_model(spdinst_strategy_lm)
summary(spdinst_strategy_lm)
report(spdinst_strategy_lm)
# no effect of strategy on speedinst

# use camill way to inspect stats
anova(spdinst_strategy_lm)

bp_strat_spdinst = ggplot(df_spdinst, aes(x=est_strat, y = RE_est), na.rm = FALSE) +
  geom_boxplot(na.rm = FALSE, alpha = 0.2, aes(fill = est_strat, color = est_strat)) +
  # geom_point(aes(color = knwYes No_est) +
  theme_classic()+ 
  xlab("Strategy")+
  ylab("Relative instantaneous speed")+
  geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
  # ylim(0, 5)+
  # scale_fill_manual(breaks = c("acc", "cst", "dec"), values = c("#8dd3c7", "#ffffb3", "#bebada")) +
  # labs(fill = "Speed episodes:", size = 35) +
  theme( text = element_text(size = 20))+
  guides(fill = "none", color = "none")
bp_strat_spdinst


# average speed_________________________________________________________________
df_spdave <- df %>% filter(type == "spd_mean")

spdave_strategy_lm <- lm(RME ~ 1 + est_strat, data = df_spdave)
performance::check_model(spdave_strategy_lm)
summary(spdave_strategy_lm)
report(spdave_strategy_lm)
# no effect of strategy

# use camill way to inspect stats
anova(spdave_strategy_lm)

bp_strat_spdmean = ggplot(df_spdave, aes(x=est_strat, y = RE_est), na.rm = FALSE) +
  geom_boxplot(na.rm = FALSE, alpha = 0.2, aes(fill = est_strat, color = est_strat)) +
  # geom_point(aes(color = knwYes No_est) +
  theme_classic()+ 
  xlab("Strategy")+
  ylab("Relative average speed")+
  geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
  # ylim(0, 4)+
  # scale_fill_manual(breaks = c("acc", "cst", "dec"), values = c("#8dd3c7", "#ffffb3", "#bebada")) +
  # labs(fill = "Speed episodes:", size = 35) +
  theme( text = element_text(size = 20))+
  guides(fill = "none", color = "none")
bp_strat_spdmean

# full fig 
gridExtra::grid.arrange(bp_strat_dur, bp_strat_dist, bp_strat_spdinst, bp_strat_spdmean)

################################################################################
# plot the distribution across the travel
# dur_density = ggplot() +
#   geom_density(data = df_dur, mapping = aes(x = obj_est, fill = est_strat), alpha = 0.2)+
# 
# # to plot hist + density
# #geom_histogram(aes(y = ..density..), alpha = 0.2, position = "identity") +
# # geom_density(adjust = 1, alpha = 0.2) +
#   xlab("Objective duration") +
#   ylab("Density") +
#   # ylim(0.0, 1.6) +
#   # xlim(0, 3) +
#   theme_classic() +
#   theme(legend.position = c(0.8,0.6), text = element_text(size = 15))+
#   guides(fill = "none")
# 
# dur_density 
# 
# dist_density = ggplot() +
#   geom_density(data = df_dist, mapping = aes(x = obj_est, fill = est_strat), alpha = 0.2)+
#   
#   # to plot hist + density
#   #geom_histogram(aes(y = ..density..), alpha = 0.2, position = "identity") +
#   # geom_density(adjust = 1, alpha = 0.2) +
#   xlab("Objective distance") +
#   ylab("Density") +
#   # ylim(0.0, 1.6) +
#   # xlim(0, 3) +
#   theme_classic() +
#   theme(legend.position = c(0.8,0.6), text = element_text(size = 15))+
#   guides(fill = "none")
# 
# dist_density 
# 
# spdinst_density = ggplot() +
#   geom_density(data = df_spdinst, mapping = aes(x = obj_est, fill = est_strat), alpha = 0.2)+
#   
#   # to plot hist + density
#   #geom_histogram(aes(y = ..density..), alpha = 0.2, position = "identity") +
#   # geom_density(adjust = 1, alpha = 0.2) +
#   xlab("Objective instantaneous speed") +
#   ylab("Density") +
#   # ylim(0.0, 1.6) +
#   # xlim(0, 3) +
#   theme_classic() +
#   theme(legend.position = c(0.8,0.6), text = element_text(size = 15))+
#   guides(fill = "none")
# 
# spdinst_density 
# 
# spdave_density = ggplot() +
#   geom_density(data = df_spdave, mapping = aes(x = obj_est, fill = est_strat), alpha = 0.2)+
#   
#   # to plot hist + density
#   #geom_histogram(aes(y = ..density..), alpha = 0.2, position = "identity") +
#   # geom_density(adjust = 1, alpha = 0.2) +
#   xlab("Objective average speed") +
#   ylab("Density") +
#   # ylim(0.0, 1.6) +
#   # xlim(0, 3) +
#   theme_classic() +
#   theme(legend.position = c(0.8,0.6), text = element_text(size = 15))+
#   guides(fill = "none")
# 
# spdave_density
# 
# gridExtra::grid.arrange(dur_density, dist_density, spdinst_density, spdave_density)

# this figure is not that illustrative so I will go back to the old idea use the plot of the estimation across the travel
#_______________________________________________________________________________
# duration
distrib_strat_dur <- ggplot() +
  geom_point(data = df_dur, aes(obj_est, c(ME), color = est_strat, alpha = 0.2)) +
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(breaks=seq(0,max(df_dur$est),25), limits = c(0, max(df_dur$est)), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,max(df_dur$est),25), limits = c(0, max(df_dur$est)), expand = c(0,0)) +
  theme_classic() +
  labs(color = "Strategy used:", size = 20) +
  theme( text = element_text(size = 20))+
  xlab("Objective duration (min)")+
  ylab("Estimated duration (min)")+
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)+
  guides(fill = "none", color = "none", alpha = "none")
distrib_strat_dur

# distance
distrib_strat_dist <- ggplot() +
  geom_point(data = df_dist %>% filter(est_strat !=0), aes(obj_est, c(ME), color = est_strat, alpha = 0.2)) +
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(breaks=seq(0,max(df_dist$est[!(is.na(df_dist$est))]),100), limits = c(0, max(df_dist$est[!(is.na(df_dist$est))])), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,max(df_dist$est[!(is.na(df_dist$est))]),100), limits = c(0, max(df_dist$est[!(is.na(df_dist$est))])), expand = c(0,0)) +
  
  # add regression line without outliers
  labs(color = "Strategy used:", size = 20) +
  theme_classic() +
  theme( text = element_text(size = 20))+
  xlab("Objective distance (km)")+
  ylab("Estimated distance (km)")+
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)+
  guides(fill = "none", color = "none", alpha = "none")
distrib_strat_dist

# spd inst
distrib_strat_spd_inst <- ggplot() +
  geom_point(data = df_spdinst, aes(obj_est, c(ME), color = est_strat, alpha = 0.2)) +
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(breaks=seq(0,max(df_spdinst$est[!(is.na(df_spdinst$est))]),50), limits = c(0, max(df_spdinst$est[!(is.na(df_spdinst$est))])), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,max(df_spdinst$est[!(is.na(df_spdinst$est))]),50), limits = c(0, max(df_spdinst$est[!(is.na(df_spdinst$est))])), expand = c(0,0)) +
  
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)+
  
  # add regression line without outliers
  labs(color = "Strategy used:", size = 20) +
  theme_classic() +
  theme( text = element_text(size = 20))+
  xlab("Objective inst. speed (km/h)")+
  ylab("Estimated inst. speed (km/h)")+
  guides(fill = "none", color = "none", alpha = "none")
distrib_strat_spd_inst

# spd mean
distrib_strat_spd_mean <- ggplot() +
  geom_point(data = df_spdave, aes(obj_est, c(ME), color = est_strat, alpha = 0.2)) +
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(breaks=seq(0,max(df_spdave$est[!(is.na(df_spdave$est))]),50), limits = c(0, max(df_spdave$est[!(is.na(df_spdave$est))])), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,max(df_spdave$est[!(is.na(df_spdave$est))]),50), limits = c(0, max(df_spdave$est[!(is.na(df_spdave$est))])), expand = c(0,0)) +
  
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)+
  
  # add regression line without outliers
  labs(color = "Strategy used:", size = 20) +
  theme_classic() +
  theme( text = element_text(size = 20))+
  xlab("Objective average speed (km/h)")+
  ylab("Estimated average speed (km/h)")+
  guides(fill = "none", color = "none", alpha = "none")
distrib_strat_spd_mean

# main figure
gridExtra::grid.arrange(distrib_strat_dur, distrib_strat_dist, distrib_strat_spd_inst, distrib_strat_spd_mean,  ncol = 2, nrow = 2)

