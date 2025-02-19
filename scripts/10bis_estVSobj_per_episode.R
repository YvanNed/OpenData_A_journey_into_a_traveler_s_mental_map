################################################################################
### analyse the relationship subjective vs objective per acceleration episode ##
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

setwd("C:\\Users\\yvann\\OneDrive\\2022_QTEV_Analysis_YN\\")
df <- read.csv("dataframe_ready2use_score.csv")

# add a column to change the alpha with acceleration episodes
df <- df %>%
  mutate(alpha = case_when(
    ep_est == "acc" ~ 1,
    ep_est == "cst" ~ 0.5,
    ep_est == "dec" ~ 0.1,
    TRUE ~ 1  # Valeur alpha par défaut pour les autres catégories
  ))

################################################################################
# Working hypothesis:
# sensory information refine participants estimation i.e. they are more accurate 

# Currently I see two ways of doing it
# 1- group data per acceleration episode and compare the R² between episode
# => this ask in which episode the subjective estimation are more accurate
# + seems to tackle exactly what I want, 
# - implies to do a lot of model (4 estimation types * 3 acceleration episode)
# - seems weird to compare models with not the same number of datapoints
#
# 2- do a linear model with objective value and acceleration episodes as predictors
# => we will have to look at the interaction, but I don't know what to expect in those results 
# + seems the correct way for statistics
# - not sure how to interpret the results (compare the slope of the interactions ?)

# Theo penses que la methode 2 est plus propre, et qu'il faut reporter les valeurs de colinéarités ou de variance qui augmente avec la valeur objective

################################################################################

# duration______________________________________________________________________
# split dataframe
df_dur <- df %>% filter(type == "dur")

df_dur_acc <- df_dur %>% filter(ep_est == "acc")

df_dur_cst <- df_dur %>% filter(ep_est == "cst")

df_dur_dec <- df_dur %>% filter(ep_est == "dec")

# compute linear regression
lm_dur_1 <- lm(ME~ep_est, data = df_dur)
lm_dur_2 <- lm(ME~ep_est+obj_est, data = df_dur)
lm_dur_3 <- lm(ME~ep_est*obj_est, data = df_dur)
performance::check_model(lm_dur_1)
performance::check_model(lm_dur_2)
performance::check_model(lm_dur_3)
# look at model matrix to better understand what is computed by the models
head(model.matrix(lm_dur_1), 20)
head(model.matrix(lm_dur_2), 20)
head(model.matrix(lm_dur_3), 20)

# check content of models
summary(lm_dur_1)
# significant
summary(lm_dur_2)
# both are significant
summary(lm_dur_3)
# none is significant anymore

# compare models
anova(lm_dur_1, lm_dur_2, lm_dur_3)
performance::compare_performance(lm_dur_1, lm_dur_2, lm_dur_3)
# best model is 2, without the interaction i.e. the interaction is capturing random noise in our data (I'm not sure about that statement, since if also remove the significance of one factor) )

# acc
lm_dur_acc <- lm(ME~obj_est, data = df_dur_acc)
performance::check_model(lm_dur_acc)
summary(lm_dur_acc)
report(lm_dur_acc)

# cst
lm_dur_cst <- lm(ME~obj_est, data = df_dur_cst)
performance::check_model(lm_dur_cst)
summary(lm_dur_cst)
report(lm_dur_cst)

# dec
lm_dur_dec <- lm(ME~obj_est, data = df_dur_dec)
performance::check_model(lm_dur_dec)
summary(lm_dur_dec)
report(lm_dur_dec)

# plot for dur, color code is disgusting right now...
rawdur_as_obj_per_epi <- ggplot() +
  # add the predicted distance based on duration ratio and distance knowledge
  geom_point(data = df_dur_acc, aes(obj_est, c(ME),), color = '#66CCEE', alpha = 1) +
  # add regression line and equation
  geom_smooth(data = df_dur_acc, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#66CCEE', fill = '#66CCEE', alpha = 1) +
  stat_regline_equation(label.x = 10, label.y = 130, data = df_dur_acc, aes(obj_est, c(ME), label = ..rr.label..), color = '#66CCEE', size=6, alpha = 1) +  # the parameter might not be good, it would be better to get the values from the model
  theme_classic() +
  
  # add the predicted distance based on duration and an arbitrary speed
  geom_point(data = df_dur_cst, aes(obj_est, c(ME)), color = '#66CCEE', alpha = 0.5) +
  # add regression line and equation
  geom_smooth(data = df_dur_cst, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#66CCEE', fill = '#66CCEE', alpha = 0.5) +
  stat_regline_equation(label.x = 10, label.y = 120, data = df_dur_cst, aes(obj_est, c(ME), label = ..rr.label..), color = '#66CCEE', size=6, alpha = 0.5) +  # the parameter might not be good, it would be better to get the values from the model
  
  # add the predicted distance based on averaged speed
  geom_point(data = df_dur_dec, aes(obj_est, c(ME)), color = '#66CCEE', alpha = 0.1) +
  # add regression line and equation
  geom_smooth(data = df_dur_dec, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#66CCEE', fill = '#66CCEE', alpha = 0.1) +
  stat_regline_equation(label.x = 10, label.y = 110, data = df_dur_dec, aes(obj_est, c(ME), label = ..rr.label..), color = '#66CCEE', size=6, alpha = 0.1) +  # the parameter might not be good, it would be better to get the values from the model
  
  
  # add the point of subjective equality
  # geom_segment(aes(x = 0, y = dur_point_of_intersect, xend = dur_point_of_intersect , yend = dur_point_of_intersect), linetype = "dashed", color = "red") +
  # geom_segment(aes(x = dur_point_of_intersect, y = 0, xend = dur_point_of_intersect , yend = dur_point_of_intersect), linetype = "dashed", color = "red") +
  # annotate("text", x = 7, y = dur_point_of_intersect+3, label = "79.8", col = "red")+
  # annotate("text", x = dur_point_of_intersect+7, y = 3, label = "79.8", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(limits = c(min(df_dur$obj_est), max(df_dur$ME)), expand = c(0,0)) +
  scale_y_continuous(limits = c(min(df_dur$obj_est), max(df_dur$ME)), expand = c(0,0)) +
  theme( text = element_text(size = 20))+
  xlab("Objective duration (min)")+
  ylab("Estimated duration (min)")+
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)
rawdur_as_obj_per_epi
#_______________________________________________________________________________

# distance______________________________________________________________________
# split dataframe
df_dist <- df %>% filter(type == "dist")

df_dist_acc <- df_dist %>% filter(ep_est == "acc")

df_dist_cst <- df_dist %>% filter(ep_est == "cst")

df_dist_dec <- df_dist %>% filter(ep_est == "dec")

# compute linear regression
lm_dist_1 <- lm(ME~ep_est, data = df_dist)
lm_dist_2 <- lm(ME~ep_est+obj_est, data = df_dist)
lm_dist_3 <- lm(ME~ep_est*obj_est, data = df_dist)
performance::check_model(lm_dist_1)
performance::check_model(lm_dist_2)
performance::check_model(lm_dist_3)

# check model matrix
head(model.matrix(lm_dist_1), 20)
head(model.matrix(lm_dist_2), 20)
head(model.matrix(lm_dist_3), 20)

# check model content
summary(lm_dist_1)
# significant
summary(lm_dist_2)
# only obj_est is significant now
summary(lm_dist_3)
# only interaction  ep_est dec is significant

# compare models
anova(lm_dist_1, lm_dist_2, lm_dist_3)
performance::compare_performance(lm_dist_1, lm_dist_2, lm_dist_3)
# best model is 3

# interaction ep_est/deceleration is significant only

# acc
lm_dist_acc <- lm(ME~obj_est, data = df_dist_acc)
performance::check_model(lm_dist_acc)
summary(lm_dist_acc)
report(lm_dist_acc)

# cst
lm_dist_cst <- lm(ME~obj_est, data = df_dist_cst)
performance::check_model(lm_dist_cst)
summary(lm_dist_cst)
report(lm_dist_cst)

# dec
lm_dist_dec <- lm(ME~obj_est, data = df_dist_dec)
performance::check_model(lm_dist_dec)
summary(lm_dist_dec)
report(lm_dist_dec)

# plot for dist, color code is disgusting right now...
rawdist_as_obj_per_epi <- ggplot() +
  # add the predicted distance based on distation ratio and distance knowledge
  geom_point(data = df_dist_acc, aes(obj_est, c(ME),), color = '#EE6677', alpha = 1) +
  # add regression line and equation
  geom_smooth(data = df_dist_acc, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#EE6677', fill = '#EE6677', alpha = 1) +
  stat_regline_equation(label.x = 10, label.y = 550, data = df_dist_acc, aes(obj_est, c(ME), label = ..rr.label..), color = '#EE6677', size=6, alpha = 1) +  # the parameter might not be good, it would be better to get the values from the model
  theme_classic() +
  
  # add the predicted distance based on distation and an arbitrary speed
  geom_point(data = df_dist_cst, aes(obj_est, c(ME)), color = '#EE6677', alpha = 0.5) +
  # add regression line and equation
  geom_smooth(data = df_dist_cst, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#EE6677', fill = '#EE6677', alpha = 0.5) +
  stat_regline_equation(label.x = 10, label.y = 500, data = df_dist_cst, aes(obj_est, c(ME), label = ..rr.label..), color = '#EE6677', size=6, alpha = 0.5) +  # the parameter might not be good, it would be better to get the values from the model
  
  # add the predicted distance based on averaged speed
  geom_point(data = df_dist_dec, aes(obj_est, c(ME)), color = '#EE6677', alpha = 0.1) +
  # add regression line and equation
  geom_smooth(data = df_dist_dec, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#EE6677', fill = '#EE6677', alpha = 0.1) +
  stat_regline_equation(label.x = 10, label.y = 450, data = df_dist_dec, aes(obj_est, c(ME), label = ..rr.label..), color = '#EE6677', size=6, alpha = 0.1) +  # the parameter might not be good, it would be better to get the values from the model
  
  
  # add the point of subjective equality
  # geom_segment(aes(x = 0, y = dist_point_of_intersect, xend = dist_point_of_intersect , yend = dist_point_of_intersect), linetype = "dashed", color = "red") +
  # geom_segment(aes(x = dist_point_of_intersect, y = 0, xend = dist_point_of_intersect , yend = dist_point_of_intersect), linetype = "dashed", color = "red") +
  # annotate("text", x = 7, y = dist_point_of_intersect+3, label = "79.8", col = "red")+
  # annotate("text", x = dist_point_of_intersect+7, y = 3, label = "79.8", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(limits = c(min(df_dist$obj_est), max(df_dist$ME[!(is.na(df_dist$ME))])), expand = c(0,0)) +
  scale_y_continuous(limits = c(min(df_dist$obj_est), max(df_dist$ME[!(is.na(df_dist$ME))])), expand = c(0,0)) +
  theme( text = element_text(size = 20))+
  xlab("Objective distance (km)")+
  ylab("Estimated distance (km)")+
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)
rawdist_as_obj_per_epi
#_______________________________________________________________________________

# instantaneous speed___________________________________________________________
# split dataframe
df_spd_inst <- df %>% filter(type == "spd_inst")

df_spd_inst_acc <- df_spd_inst %>% filter(ep_est == "acc")

df_spd_inst_cst <- df_spd_inst %>% filter(ep_est == "cst")

df_spd_inst_dec <- df_spd_inst %>% filter(ep_est == "dec")

# compute linear regression
lm_spd_inst_1 <- lm(ME~ep_est, data = df_spd_inst)
lm_spd_inst_2 <- lm(ME~ep_est+obj_est, data = df_spd_inst)
lm_spd_inst_3 <- lm(ME~ep_est*obj_est, data = df_spd_inst)
# adding the model with only obj_est
lm_spd_inst_4 <- lm(ME~obj_est, data = df_spd_inst)

performance::check_model(lm_spd_inst_1)
performance::check_model(lm_spd_inst_2)
performance::check_model(lm_spd_inst_3)
performance::check_model(lm_spd_inst_4)

# check model matrix 
head(model.matrix(lm_spd_inst_1))
head(model.matrix(lm_spd_inst_2))
head(model.matrix(lm_spd_inst_3))

# check model content
summary(lm_spd_inst_1)
# intercept (acc?) and cst is significant
summary(lm_spd_inst_2)
# intercept (acc?) and objective value are significant
summary(lm_spd_inst_3)
# intercept and objective value are significant
summary(lm_spd_inst_4)
# obj_est significant 

# compare models
anova(lm_spd_inst_1, lm_spd_inst_2, lm_spd_inst_3, lm_spd_inst_4)
# playing with the order in the anova function
anova(lm_spd_inst_4, lm_spd_inst_2, lm_spd_inst_1, lm_spd_inst_3)
# the order have an impact on which model is significant are not
performance::compare_performance(lm_spd_inst_1, lm_spd_inst_2, lm_spd_inst_3, lm_spd_inst_4)
# best model is only objective est, but with incrementation approach, best model is without interaction

# only objective value driving the effect

# acc
lm_spd_inst_acc <- lm(ME~obj_est, data = df_spd_inst_acc)
performance::check_model(lm_spd_inst_acc)
summary(lm_spd_inst_acc)
report(lm_spd_inst_acc)

# cst
lm_spd_inst_cst <- lm(ME~obj_est, data = df_spd_inst_cst)
performance::check_model(lm_spd_inst_cst)
summary(lm_spd_inst_cst)
report(lm_spd_inst_cst)

# dec
lm_spd_inst_dec <- lm(ME~obj_est, data = df_spd_inst_dec)
performance::check_model(lm_spd_inst_dec)
summary(lm_spd_inst_dec)
report(lm_spd_inst_dec)

# plot for spd_inst, color code is disgusting right now...
rawspd_inst_as_obj_per_epi <- ggplot() +
  # add the predicted spd_instance based on spd_instation ratio and spd_instance knowledge
  geom_point(data = df_spd_inst_acc, aes(obj_est, c(ME),), color = '#228833', alpha = 1) +
  # add regression line and equation
  geom_smooth(data = df_spd_inst_acc, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#228833', fill = '#228833', alpha = 1) +
  stat_regline_equation(label.x = 50, label.y = 350, data = df_spd_inst_acc, aes(obj_est, c(ME), label = ..rr.label..), color = '#228833', size=6, alpha = 1) +  # the parameter might not be good, it would be better to get the values from the model
  theme_classic() +
  
  # add the predicted spd_instance based on spd_instation and an arbitrary speed
  geom_point(data = df_spd_inst_cst, aes(obj_est, c(ME)), color = '#228833', alpha = 0.5) +
  # add regression line and equation
  geom_smooth(data = df_spd_inst_cst, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#228833', fill = '#228833', alpha = 0.5) +
  stat_regline_equation(label.x = 50, label.y = 330, data = df_spd_inst_cst, aes(obj_est, c(ME), label = ..rr.label..), color = '#228833', size=6, alpha = 0.5) +  # the parameter might not be good, it would be better to get the values from the model
  
  # add the predicted spd_instance based on averaged speed
  geom_point(data = df_spd_inst_dec, aes(obj_est, c(ME)), color = '#228833', alpha = 0.1) +
  # add regression line and equation
  geom_smooth(data = df_spd_inst_dec, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#228833', fill = '#228833', alpha = 0.1) +
  stat_regline_equation(label.x = 50, label.y = 310, data = df_spd_inst_dec, aes(obj_est, c(ME), label = ..rr.label..), color = '#228833', size=6, alpha = 0.1) +  # the parameter might not be good, it would be better to get the values from the model
  
  
  # add the point of subjective equality
  # geom_segment(aes(x = 0, y = spd_inst_point_of_intersect, xend = spd_inst_point_of_intersect , yend = spd_inst_point_of_intersect), linetype = "dashed", color = "red") +
  # geom_segment(aes(x = spd_inst_point_of_intersect, y = 0, xend = spd_inst_point_of_intersect , yend = spd_inst_point_of_intersect), linetype = "dashed", color = "red") +
  # annotate("text", x = 7, y = spd_inst_point_of_intersect+3, label = "79.8", col = "red")+
  # annotate("text", x = spd_inst_point_of_intersect+7, y = 3, label = "79.8", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(limits = c(min(df_spd_inst$obj_est), max(df_spd_inst$ME[!(is.na(df_spd_inst$ME))])), expand = c(0,0)) +
  scale_y_continuous(limits = c(min(df_spd_inst$obj_est), max(df_spd_inst$ME[!(is.na(df_spd_inst$ME))])), expand = c(0,0)) +
  theme( text = element_text(size = 20))+
  xlab("Objective inst. speed (km/h)")+
  ylab("Estimated inst. speed (km/h)")+
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)
rawspd_inst_as_obj_per_epi
#_______________________________________________________________________________

# average speed___________________________________________________________
# split dataframe
df_spd_mean <- df %>% filter(type == "spd_mean")

df_spd_mean_acc <- df_spd_mean %>% filter(ep_est == "acc")

df_spd_mean_cst <- df_spd_mean %>% filter(ep_est == "cst")

df_spd_mean_dec <- df_spd_mean %>% filter(ep_est == "dec")

# compute linear regression
# compute linear regression
lm_spd_mean_1 <- lm(ME~ep_est, data = df_spd_mean)
lm_spd_mean_2 <- lm(ME~ep_est+obj_est, data = df_spd_mean)
lm_spd_mean_3 <- lm(ME~ep_est*obj_est, data = df_spd_mean)
lm_spd_mean_4 <- lm(ME~obj_est, data = df_spd_mean)

performance::check_model(lm_spd_mean_1)
performance::check_model(lm_spd_mean_2)
performance::check_model(lm_spd_mean_3)
performance::check_model(lm_spd_mean_4)

# check model content
summary(lm_spd_mean_1)
# all are significant
summary(lm_spd_mean_2)
# intercept and obj_est significant
summary(lm_spd_mean_3)
# only intercept significant
summary(lm_spd_mean_4)
# obje_est significant

# compare model
anova(lm_spd_mean_1, lm_spd_mean_2, lm_spd_mean_3)
# model 2 is better
anova(lm_spd_mean_1, lm_spd_mean_4, lm_spd_mean_2, lm_spd_mean_3)
# model 2 is not significant compared to model 4

performance::compare_performance(lm_spd_mean_1, lm_spd_mean_2, lm_spd_mean_3)
# best model is 2 (lowest AIC)

performance::compare_performance(lm_spd_mean_1, lm_spd_mean_2, lm_spd_mean_3,lm_spd_mean_4)
# best model is 4

# acc
lm_spd_mean_acc <- lm(ME~obj_est, data = df_spd_mean_acc)
performance::check_model(lm_spd_mean_acc)
summary(lm_spd_mean_acc)
report(lm_spd_mean_acc)

# cst
lm_spd_mean_cst <- lm(ME~obj_est, data = df_spd_mean_cst)
performance::check_model(lm_spd_mean_cst)
summary(lm_spd_mean_cst)
report(lm_spd_mean_cst)

# dec
lm_spd_mean_dec <- lm(ME~obj_est, data = df_spd_mean_dec)
performance::check_model(lm_spd_mean_dec)
summary(lm_spd_mean_dec)
report(lm_spd_mean_dec)

# plot for spd_mean, color code is disgusting right now...
rawspd_mean_as_obj_per_epi <- ggplot() +
  # add the predicted spd_meanance based on spd_meanation ratio and spd_meanance knowledge
  geom_point(data = df_spd_mean_acc, aes(obj_est, c(ME),), color = '#CCBB44', alpha = 1) +
  # add regression line and equation
  geom_smooth(data = df_spd_mean_acc, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#CCBB44', fill = '#CCBB44', alpha = 1) +
  stat_regline_equation(label.x = 50, label.y = 350, data = df_spd_mean_acc, aes(obj_est, c(ME), label = ..rr.label..), color = '#CCBB44', size=6, alpha = 1) +  # the parameter might not be good, it would be better to get the values from the model
  theme_classic() +
  
  # add the predicted spd_meanance based on spd_meanation and an arbitrary speed
  geom_point(data = df_spd_mean_cst, aes(obj_est, c(ME)), color = '#CCBB44', alpha = 0.5) +
  # add regression line and equation
  geom_smooth(data = df_spd_mean_cst, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#CCBB44', fill = '#CCBB44', alpha = 0.5) +
  stat_regline_equation(label.x = 50, label.y = 330, data = df_spd_mean_cst, aes(obj_est, c(ME), label = ..rr.label..), color = '#CCBB44', size=6, alpha = 0.5) +  # the parameter might not be good, it would be better to get the values from the model
  
  # add the predicted spd_meanance based on averaged speed
  geom_point(data = df_spd_mean_dec, aes(obj_est, c(ME)), color = '#CCBB44', alpha = 0.1) +
  # add regression line and equation
  geom_smooth(data = df_spd_mean_dec, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#CCBB44', fill = '#CCBB44', alpha = 0.1) +
  stat_regline_equation(label.x = 50, label.y = 310, data = df_spd_mean_dec, aes(obj_est, c(ME), label = ..rr.label..), color = '#CCBB44', size=6, alpha = 0.1) +  # the parameter might not be good, it would be better to get the values from the model
  
  
  # add the point of subjective equality
  # geom_segment(aes(x = 0, y = spd_mean_point_of_intersect, xend = spd_mean_point_of_intersect , yend = spd_mean_point_of_intersect), linetype = "dashed", color = "red") +
  # geom_segment(aes(x = spd_mean_point_of_intersect, y = 0, xend = spd_mean_point_of_intersect , yend = spd_mean_point_of_intersect), linetype = "dashed", color = "red") +
  # annotate("text", x = 7, y = spd_mean_point_of_intersect+3, label = "79.8", col = "red")+
  # annotate("text", x = spd_mean_point_of_intersect+7, y = 3, label = "79.8", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(limits = c(min(df_spd_mean$obj_est), max(df_spd_mean$ME[!(is.na(df_spd_mean$ME))])), expand = c(0,0)) +
  scale_y_continuous(limits = c(min(df_spd_mean$obj_est), max(df_spd_mean$ME[!(is.na(df_spd_mean$ME))])), expand = c(0,0)) +
  theme( text = element_text(size = 20))+
  xlab("Objective average speed (km/h)")+
  ylab("Estimated average speed (km/h)")+
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)
rawspd_mean_as_obj_per_epi
#_______________________________________________________________________________

# trying the same analysis for instantaneous speed, but restricting the range (to compare apples to apples)
# instantaneous speed___________________________________________________________
# restrict the range of the dataset
df_spd_inst_restricted <- subset(df_spd_inst, ME >= 100)
# split dataframe

df_spd_inst_acc_restricted <- df_spd_inst_restricted %>% filter(ep_est == "acc")

df_spd_inst_cst_restricted <- df_spd_inst_restricted %>% filter(ep_est == "cst")

df_spd_inst_dec_restricted <- df_spd_inst_restricted %>% filter(ep_est == "dec")

# compute linear regression
lm_spd_inst_1 <- lm(ME~ep_est, data = df_spd_inst_restricted)
lm_spd_inst_2 <- lm(ME~ep_est+obj_est, data = df_spd_inst_restricted)
lm_spd_inst_3 <- lm(ME~ep_est*obj_est, data = df_spd_inst_restricted)
# adding the model with only obj_est
lm_spd_inst_4 <- lm(ME~obj_est, data = df_spd_inst_restricted)

performance::check_model(lm_spd_inst_1)
performance::check_model(lm_spd_inst_2)
performance::check_model(lm_spd_inst_3)
performance::check_model(lm_spd_inst_4)

# check model matrix 
head(model.matrix(lm_spd_inst_1))
head(model.matrix(lm_spd_inst_2))
head(model.matrix(lm_spd_inst_3))

# check model content
summary(lm_spd_inst_1)
# intercept (acc?) and cst is significant
summary(lm_spd_inst_2)
# intercept (acc?) and objective value are significant
summary(lm_spd_inst_3)
# intercept and objective value are significant
summary(lm_spd_inst_4)
# obj_est significant 

# compare models
anova(lm_spd_inst_1, lm_spd_inst_2, lm_spd_inst_3, lm_spd_inst_4)
# playing with the order in the anova function
anova(lm_spd_inst_1, lm_spd_inst_4, lm_spd_inst_2, lm_spd_inst_3)
# the order have an impact on which model is significant are not
performance::compare_performance(lm_spd_inst_1, lm_spd_inst_2, lm_spd_inst_3, lm_spd_inst_4)
# best model is only objective est, but with incrementation approach, best model is without interaction

# only objective value driving the effect

# acc
lm_spd_inst_acc <- lm(ME~obj_est, data = df_spd_inst_acc_restricted)
performance::check_model(lm_spd_inst_acc)
summary(lm_spd_inst_acc)
report(lm_spd_inst_acc)

# cst
lm_spd_inst_cst <- lm(ME~obj_est, data = df_spd_inst_cst_restricted)
performance::check_model(lm_spd_inst_cst)
summary(lm_spd_inst_cst)
report(lm_spd_inst_cst)

# dec
lm_spd_inst_dec <- lm(ME~obj_est, data = df_spd_inst_dec_restricted)
performance::check_model(lm_spd_inst_dec)
summary(lm_spd_inst_dec)
report(lm_spd_inst_dec)

# plot for spd_inst, color code is disgusting right now...
rawspd_inst_as_obj_per_epi <- ggplot() +
  # add the predicted spd_instance based on spd_instation ratio and spd_instance knowledge
  geom_point(data = df_spd_inst_acc_restricted, aes(obj_est, c(ME),), color = '#228833', alpha = 1) +
  # add regression line and equation
  geom_smooth(data = df_spd_inst_acc_restricted, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#228833', fill = '#228833', alpha = 1) +
  stat_regline_equation(label.x = 102, label.y = 350, data = df_spd_inst_acc_restricted, aes(obj_est, c(ME), label = ..rr.label..), color = '#228833', size=6, alpha = 1) +  # the parameter might not be good, it would be better to get the values from the model
  theme_classic() +
  
  # add the predicted spd_instance based on spd_instation and an arbitrary speed
  geom_point(data = df_spd_inst_cst_restricted, aes(obj_est, c(ME)), color = '#228833', alpha = 0.5) +
  # add regression line and equation
  geom_smooth(data = df_spd_inst_cst_restricted, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#228833', fill = '#228833', alpha = 0.5) +
  stat_regline_equation(label.x = 102, label.y = 330, data = df_spd_inst_cst_restricted, aes(obj_est, c(ME), label = ..rr.label..), color = '#228833', size=6, alpha = 0.5) +  # the parameter might not be good, it would be better to get the values from the model
  
  # add the predicted spd_instance based on averaged speed
  geom_point(data = df_spd_inst_dec_restricted, aes(obj_est, c(ME)), color = '#228833', alpha = 0.1) +
  # add regression line and equation
  geom_smooth(data = df_spd_inst_dec_restricted, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#228833', fill = '#228833', alpha = 0.1) +
  stat_regline_equation(label.x = 102, label.y = 310, data = df_spd_inst_dec_restricted, aes(obj_est, c(ME), label = ..rr.label..), color = '#228833', size=6, alpha = 0.1) +  # the parameter might not be good, it would be better to get the values from the model
  
  
  # add the point of subjective equality
  # geom_segment(aes(x = 0, y = spd_inst_point_of_intersect, xend = spd_inst_point_of_intersect , yend = spd_inst_point_of_intersect), linetype = "dashed", color = "red") +
  # geom_segment(aes(x = spd_inst_point_of_intersect, y = 0, xend = spd_inst_point_of_intersect , yend = spd_inst_point_of_intersect), linetype = "dashed", color = "red") +
  # annotate("text", x = 7, y = spd_inst_point_of_intersect+3, label = "79.8", col = "red")+
  # annotate("text", x = spd_inst_point_of_intersect+7, y = 3, label = "79.8", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(limits = c(min(df_spd_inst_restricted$obj_est), max(df_spd_inst_restricted$ME[!(is.na(df_spd_inst_restricted$ME))])), expand = c(0,0)) +
  scale_y_continuous(limits = c(min(df_spd_inst_restricted$obj_est), max(df_spd_inst_restricted$ME[!(is.na(df_spd_inst_restricted$ME))])), expand = c(0,0)) +
  theme( text = element_text(size = 20))+
  xlab("Objective inst. speed (km/h)")+
  ylab("Estimated inst. speed (km/h)")+
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)
rawspd_inst_as_obj_per_epi
#_______________________________________________________________________________



# trying now to reduce the degree of freedom in the cst speed episode
# instantaneous speed___________________________________________________________
df_spd_inst_cst_reduced <- df_spd_inst_cst_restricted[sample(nrow(df_spd_inst_cst_restricted), 20), ]

# trying a sort of permutation/bootstrap to be more representative than a random selection of 20 samples
# Nombre de répétitions
nombre_repetitions <- 1000

# Initialisation du dataframe final
df_spd_inst_cst_reduced_final <- NULL

# Boucle pour répéter l'opération 1000 fois
for (i in 1:nombre_repetitions) {
  # Sélection aléatoire de 20 lignes du dataframe resultat_specifique
  echantillon_aleatoire <- df_spd_inst_cst_restricted[sample(nrow(df_spd_inst_cst_restricted), 20), ]
  
  # Ajout de l'échantillon à dataframe_final
  df_spd_inst_cst_reduced_final <- rbind(df_spd_inst_cst_reduced_final, colMeans(echantillon_aleatoire))
}
 # ça ne correspond pas tout à fait à ce que je voudrais faire. Peut être plutôt repeter 1000 fois le test et faire la moyenne des résultats stats ?


# create a more balanced dataframe
df_spd_inst_reduced <- rbind(df_spd_inst_acc_restricted, df_spd_inst_cst_reduced, df_spd_inst_dec_restricted)

# compute linear regression
lm_spd_inst_1 <- lm(ME~ep_est, data = df_spd_inst_reduced)
lm_spd_inst_2 <- lm(ME~ep_est+obj_est, data = df_spd_inst_reduced)
lm_spd_inst_3 <- lm(ME~ep_est*obj_est, data = df_spd_inst_reduced)
# adding the model with only obj_est
lm_spd_inst_4 <- lm(ME~obj_est, data = df_spd_inst_reduced)

performance::check_model(lm_spd_inst_1)
performance::check_model(lm_spd_inst_2)
performance::check_model(lm_spd_inst_3)
performance::check_model(lm_spd_inst_4)

# check model content
summary(lm_spd_inst_1)
# intercept (acc?) and cst is significant
summary(lm_spd_inst_2)
# intercept (acc?) and objective value are significant
summary(lm_spd_inst_3)
# intercept and objective value are significant
summary(lm_spd_inst_4)
# obj_est significant 

# compare models
anova(lm_spd_inst_1, lm_spd_inst_2, lm_spd_inst_3, lm_spd_inst_4)
# model 2 is the best
# playing with the order in the anova function
anova(lm_spd_inst_1, lm_spd_inst_4, lm_spd_inst_2, lm_spd_inst_3)
# the order have an impact on which model is significant are not
performance::compare_performance(lm_spd_inst_1, lm_spd_inst_2, lm_spd_inst_3, lm_spd_inst_4)
# best model is only objective est, but with incrementation approach, best model is without interaction

# acc
lm_spd_inst_acc <- lm(ME~obj_est, data = df_spd_inst_acc_restricted)
performance::check_model(lm_spd_inst_acc)
summary(lm_spd_inst_acc)
report(lm_spd_inst_acc)

# cst
lm_spd_inst_cst <- lm(ME~obj_est, data = df_spd_inst_cst_reduced)
performance::check_model(lm_spd_inst_cst)
summary(lm_spd_inst_cst)
report(lm_spd_inst_cst)

# dec
lm_spd_inst_dec <- lm(ME~obj_est, data = df_spd_inst_dec_restricted)
performance::check_model(lm_spd_inst_dec)
summary(lm_spd_inst_dec)
report(lm_spd_inst_dec)

# plot for spd_inst, color code is disgusting right now...
rawspd_inst_as_obj_per_epi <- ggplot() +
  # add the predicted spd_instance based on spd_instation ratio and spd_instance knowledge
  geom_point(data = df_spd_inst_acc_restricted, aes(obj_est, c(ME),), color = '#228833', alpha = 1) +
  # add regression line and equation
  geom_smooth(data = df_spd_inst_acc_restricted, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#228833', fill = '#228833', alpha = 1) +
  stat_regline_equation(label.x = 102, label.y = 350, data = df_spd_inst_acc_restricted, aes(obj_est, c(ME), label = ..rr.label..), color = '#228833', size=6, alpha = 1) +  # the parameter might not be good, it would be better to get the values from the model
  theme_classic() +
  
  # add the predicted spd_instance based on spd_instation and an arbitrary speed
  geom_point(data = df_spd_inst_cst_reduced, aes(obj_est, c(ME)), color = '#228833', alpha = 0.5) +
  # add regression line and equation
  geom_smooth(data = df_spd_inst_cst_reduced, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#228833', fill = '#228833', alpha = 0.5) +
  stat_regline_equation(label.x = 102, label.y = 330, data = df_spd_inst_cst_reduced, aes(obj_est, c(ME), label = ..rr.label..), color = '#228833', size=6, alpha = 0.5) +  # the parameter might not be good, it would be better to get the values from the model
  
  # add the predicted spd_instance based on averaged speed
  geom_point(data = df_spd_inst_dec_restricted, aes(obj_est, c(ME)), color = '#228833', alpha = 0.1) +
  # add regression line and equation
  geom_smooth(data = df_spd_inst_dec_restricted, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#228833', fill = '#228833', alpha = 0.1) +
  stat_regline_equation(label.x = 102, label.y = 310, data = df_spd_inst_dec_restricted, aes(obj_est, c(ME), label = ..rr.label..), color = '#228833', size=6, alpha = 0.1) +  # the parameter might not be good, it would be better to get the values from the model
  
  
  # add the point of subjective equality
  # geom_segment(aes(x = 0, y = spd_inst_point_of_intersect, xend = spd_inst_point_of_intersect , yend = spd_inst_point_of_intersect), linetype = "dashed", color = "red") +
  # geom_segment(aes(x = spd_inst_point_of_intersect, y = 0, xend = spd_inst_point_of_intersect , yend = spd_inst_point_of_intersect), linetype = "dashed", color = "red") +
  # annotate("text", x = 7, y = spd_inst_point_of_intersect+3, label = "79.8", col = "red")+
  # annotate("text", x = spd_inst_point_of_intersect+7, y = 3, label = "79.8", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(limits = c(min(df_spd_inst_restricted$obj_est), max(df_spd_inst_restricted$ME[!(is.na(df_spd_inst_restricted$ME))])), expand = c(0,0)) +
  scale_y_continuous(limits = c(min(df_spd_inst_restricted$obj_est), max(df_spd_inst_restricted$ME[!(is.na(df_spd_inst_restricted$ME))])), expand = c(0,0)) +
  theme( text = element_text(size = 20))+
  xlab("Objective inst. speed (km/h)")+
  ylab("Estimated inst. speed (km/h)")+
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)
rawspd_inst_as_obj_per_epi
#_______________________________________________________________________________


