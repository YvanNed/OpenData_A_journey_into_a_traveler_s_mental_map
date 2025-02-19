################################################################################
################ Compute and plot effect of objective magnitude ################
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

intersection <- function(l1, l2){
  x <- (l2[1] - l1[1]) / (l1[2] - l2[2])
  y <- l1[1] + l1[2] * x
  return(xy=c(x, y))
}

################################################################################
# load data
################################################################################

setwd("C:\\Users\\yvann\\OneDrive\\2022_QTEV_Analysis_YN\\")
df <- read.csv("dataframe_ready2use_score.csv")

# Duration______________________________________________________________________
df_dur <- df %>% filter(type == "dur")

# looking at the distribution
dur_density = ggplot() +
  geom_density(data = df_dur, mapping = aes(x = ME), alpha = 0.2)+
  xlab("Estimated duration") +
  ylab("Density") +
  theme_classic() +
  theme(legend.position = c(0.8,0.6), text = element_text(size = 15))
dur_density

# trying to normalize the data
BN_dur_obj <- bestNormalize(df_dur$ME, allow_orderNorm = FALSE, allow_lamber_s = TRUE)
plot(BN_dur_obj, leg_loc = "bottomright")
df_dur$norm_ME <- predict(BN_dur_obj)

# Compute log of estimated and objective value
df_dur$log_ME <- log10(df_dur$ME)
df_dur$log_obj <- log10(df_dur$obj_est)

# look at the resulting distribution
dur_density = ggplot() +
  geom_density(data = df_dur, mapping = aes(x = log_ME), alpha = 0.2)+
  xlab("Log(duration)") +
  ylab("Density") +
  theme_classic() +
  theme(legend.position = c(0.8,0.6), text = element_text(size = 15))
dur_density

# using the log yield very similar results in terms of variance explained, and do not make particular sense for other estimations (moreover model were correct already)

# compute model
dur_lm <- lm(ME ~ obj_est, data = df_dur)
performance::check_model(dur_lm)
summary(dur_lm)
report(dur_lm)

# compute model for log log
log_dur_lm <- lm(log_ME ~ log_obj, data = df_dur)
performance::check_model(log_dur_lm)
summary(log_dur_lm)
report(log_dur_lm)

# use Camille way of reporting stats. This is the same thing reported by the model stat (because there is only one factor so I don't need it)
anova(log_dur_lm)
report(anova(log_dur_lm))

# compute the point of intersection between the regression
dur_reg_line <- c(3.52900,  0.94900) # reg_line[1] = intercept ; reg_line[2] = slope
dur_unity_line <- c(0,1) # unity_line[1] = intercept (0) ; unity_line[2] = slope (1)

dur_point_of_intersect <- intersection(dur_reg_line, dur_unity_line)

# plot the regression curve
rawdur_as_obj <- ggplot() +
  geom_point(data = df_dur, aes(obj_est, c(ME))) +
  # add regression line and equation
  geom_smooth(data = df_dur, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#66CCEE', fill = '#66CCEE') +
  # stat_regline_equation(label.x = 5, label.y = 130, data = df_dur, aes(obj_est, c(ME), label = ..eq.label..), color = '#66CCEE', size = 6) + # the parameter might not be good, it would be better to get the values from the model 
  # stat_regline_equation(label.x = 5, label.y = 122, data = df_dur, aes(obj_est, c(ME), label = ..rr.label..), color = '#66CCEE', size = 6) +  # the parameter might not be good, it would be better to get the values from the model
  theme_classic() +
  
  # add the point of subjective equality
  geom_segment(aes(x = 0, y = dur_point_of_intersect, xend = dur_point_of_intersect , yend = dur_point_of_intersect), linetype = "dashed", color = "red") +
  geom_segment(aes(x = dur_point_of_intersect, y = 0, xend = dur_point_of_intersect , yend = dur_point_of_intersect), linetype = "dashed", color = "red") +
  annotate("text", x = 7, y = dur_point_of_intersect+3, label = "69.2", col = "red")+
  annotate("text", x = dur_point_of_intersect+7, y = 3, label = "69.2", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(breaks=seq(0,max(df_dur$ME),25), limits = c(0, max(df_dur$ME)), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,max(df_dur$ME),25), limits = c(0, max(df_dur$ME)), expand = c(0,0)) +
  theme( text = element_text(size = 20))+
  xlab("Objective duration (min)")+
  ylab("Estimated duration (min)")+
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)+
  guides(color = "none")
rawdur_as_obj

# plot with axis in log log representation
logdur_as_logobj <- ggplot() +
  geom_point(data = df_dur, aes(obj_est, c(ME))) +
  # add regression line and equation
  geom_smooth(data = df_dur, aes(obj_est, c(ME),),  method = lm, se = TRUE, color = '#66CCEE', fill = '#66CCEE') +
  # annotate("text", x = 10.25, y = dur_point_of_intersect-10, label = "log(y) = 0.11 + 0.94*log(x)", col = "#66CCEE")+
  # annotate("text", x = 6.5, y = dur_point_of_intersect-20, label = "R² = 0.91", col = "#66CCEE")+
  theme_classic() +
  
  # add the point of subjective equality
  geom_segment(aes(x = 0, y = dur_point_of_intersect, xend = dur_point_of_intersect , yend = dur_point_of_intersect), linetype = "dashed", color = "red") +
  geom_segment(aes(x = dur_point_of_intersect, y = 0, xend = dur_point_of_intersect , yend = dur_point_of_intersect), linetype = "dashed", color = "red") +
  annotate("text", x = 5.5, y = dur_point_of_intersect+6, label = "69.2", col = "red")+
  annotate("text", x = dur_point_of_intersect+12, y = 5.2, label = "69.2", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(trans = "log10", limits = c(min(df_dur$obj_est), max(df_dur$ME)), expand = c(0,0)) +
  scale_y_continuous(trans = "log10", limits = c(min(df_dur$obj_est), max(df_dur$ME)), expand = c(0,0)) +
  theme(text = element_text(size = 20), axis.title.x = element_blank(), axis.title.y = element_blank())+
  xlab("Objective duration (min)")+
  ylab("Estimated duration (min)")+
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)+
  guides(color = "none")
logdur_as_logobj


# distance______________________________________________________________________
df_dist <- df %>% filter(type == "dist")

# looking at the distribution
dist_density = ggplot() +
  geom_density(data = df_dist, mapping = aes(x = ME), alpha = 0.2)+
  xlab("Estimated distance") +
  ylab("Density") +
  theme_classic() +
  theme(legend.position = c(0.8,0.6), text = element_text(size = 15))
dist_density

# trying to normalize the data
BN_dist_obj <- bestNormalize(df_dist$ME, allow_orderNorm = FALSE, allow_lamber_s = TRUE)
plot(BN_dist_obj, leg_loc = "bottomright")
df_dist$norm_ME <- predict(BN_dist_obj)

# compute log of estimate and obj
df_dist$log_ME <- log10(df_dist$ME)
df_dist$log_obj <- log10(df_dist$obj_est)

# look at the resulting distribution
dist_density = ggplot() +
  geom_density(data = df_dist, mapping = aes(x = log_ME), alpha = 0.2)+
  xlab("Log(distance)") +
  ylab("Density") +
  theme_classic() +
  theme(legend.position = c(0.8,0.6), text = element_text(size = 15))
dist_density

# compute model
dist_lm <- lm(ME ~ obj_est, data = df_dist)
performance::check_model(dist_lm)
summary(dist_lm)
report(dist_lm)

# compute model for log
dist_log_lm <- lm(log_ME ~ log_obj, data = df_dist)
performance::check_model(dist_log_lm)
summary(dist_log_lm)
report(dist_log_lm)

# use Camille way of reporting stats. This is the same thing reported by the model stat (because there is only one factor so I don't need it)
anova(dist_log_lm)
report(anova(dist_log_lm))

# compute the point of intersection between the regression
dist_reg_line <- c(39.99823, 0.84913) # reg_line[1] = intercept ; reg_line[2] = slope
dist_unity_line <- c(0,1) # unity_line[1] = intercept (0) ; unity_line[2] = slope (1)

dist_point_of_intersect <- intersection(dist_reg_line, dist_unity_line)

# estimated distance as objective distance (mixing format)
rawdist_as_obj <- ggplot() +
  geom_point(data = df_dist, aes(obj_est, c(ME))) +
  # add regression line with outliers
  geom_smooth(data = df_dist, aes(obj_est, c(ME),),  method = lm, se = TRUE, color =  '#EE6677', fill =  '#EE6677') +
  # stat_regline_equation(label.x = 15, label.y = 540, data = df_dist, aes(obj_est, c(ME), label = ..eq.label..), color = '#EE6677', size = 6) + # the parameter might not be good, it would be better to get the values from the model 
  # stat_regline_equation(label.x = 15, label.y = 510, data = df_dist, aes(obj_est, c(ME), label = ..rr.label..), color = '#EE6677', size = 6) +  # the parameter might not be good, it would be better to get the values from the model
  
  # add the point of subjective equality
  geom_segment(aes(x = 0, y = dist_point_of_intersect, xend = dist_point_of_intersect , yend = dist_point_of_intersect), linetype = "dashed", color = "red") +
  geom_segment(aes(x = dist_point_of_intersect, y = 0, xend = dist_point_of_intersect , yend = dist_point_of_intersect), linetype = "dashed", color = "red") +
  annotate("text", x = 30, y = dist_point_of_intersect+15, label = "226", col = "red")+
  annotate("text", x = dist_point_of_intersect+30, y = 15, label = "226", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(breaks=seq(0,max(df_dist$ME[!(is.na(df_dist$ME))]),100), limits = c(0, max(df_dist$ME[!(is.na(df_dist$ME))])), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,max(df_dist$ME[!(is.na(df_dist$ME))]),100), limits = c(0, max(df_dist$ME[!(is.na(df_dist$ME))])), expand = c(0,0)) +
  
  # add regression line without outliers
  theme_classic() +
  theme( text = element_text(size = 20))+
  xlab("Objective distance (km)")+
  ylab("Estimated distance (km)")+
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)+
  guides(color = "none")
rawdist_as_obj

# plot for log
logdist_as_obj <- ggplot() +
  geom_point(data = df_dist, aes(obj_est, c(ME))) +
  # add regression line with outliers
  geom_smooth(data = df_dist, aes(obj_est, c(ME),),  method = lm, se = TRUE, color =  '#EE6677', fill =  '#EE6677') +
  stat_regline_equation(label.x = 15, label.y = 540, data = df_dist, aes(obj_est, c(ME), label = ..eq.label..), color = '#EE6677', size = 6) + # the parameter might not be good, it would be better to get the values from the model 
  stat_regline_equation(label.x = 15, label.y = 510, data = df_dist, aes(obj_est, c(ME), label = ..rr.label..), color = '#EE6677', size = 6) +  # the parameter might not be good, it would be better to get the values from the model
  # annotate("text", x = 9, y = dist_point_of_intersect-50, label = "log(y) = 0.6 + 0.75*log(x)", col = "#EE6677")+
  # annotate("text", x = 4.7, y = dist_point_of_intersect-95, label = "R² = 0.85", col = "#EE6677")+
  
  
  # add the point of subjective equality
  geom_segment(aes(x = 0, y = dist_point_of_intersect, xend = dist_point_of_intersect , yend = dist_point_of_intersect), linetype = "dashed", color = "red") +
  geom_segment(aes(x = dist_point_of_intersect, y = 0, xend = dist_point_of_intersect , yend = dist_point_of_intersect), linetype = "dashed", color = "red") +
  annotate("text", x = 3.6, y = dist_point_of_intersect+30, label = "226", col = "red")+
  annotate("text", x = dist_point_of_intersect+50, y = 3.3, label = "226", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(trans = "log10", limits = c(min(df_dist$obj_est), max(df_dist$ME[!(is.na(df_dist$ME))])), expand = c(0,0)) +
  scale_y_continuous(trans = "log10", limits = c(min(df_dist$obj_est), max(df_dist$ME[!(is.na(df_dist$ME))])), expand = c(0,0)) +
  
  # add regression line without outliers
  theme_classic() +
  theme(text = element_text(size = 20), axis.title.x = element_blank(), axis.title.y = element_blank())+
  xlab("Objective distance (km)")+
  ylab("Estimated distance (km)")+
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)+
  guides(color = "none")
logdist_as_obj

# instantaneous speed___________________________________________________________
df_spdinst <- df %>% filter(type == "spd_inst")

# looking at the distribution
spdinst_density = ggplot() +
  geom_density(data = df_spdinst, mapping = aes(x = ME), alpha = 0.2)+
  xlab("Estimated instantaneous speed") +
  ylab("Density") +
  theme_classic() +
  theme(legend.position = c(0.8,0.6), text = element_text(size = 15))
spdinst_density

# trying to normalize the data
BN_spdinst_obj <- bestNormalize(df_spdinst$ME, allow_orderNorm = FALSE, allow_lamber_s = TRUE)
plot(BN_spdinst_obj, leg_loc = "bottomright")
df_spdinst$norm_ME <- predict(BN_spdinst_obj)

# compute log
df_spdinst$log_ME <- log10(df_spdinst$ME)
df_spdinst$log_obj <- log10(df_spdinst$obj_est)

# look at the resulting distribution
spdinst_density = ggplot() +
  geom_density(data = df_spdinst, mapping = aes(x = log_ME), alpha = 0.2)+
  xlab("Log(spdinst)") +
  ylab("Density") +
  theme_classic() +
  theme(legend.position = c(0.8,0.6), text = element_text(size = 15))
spdinst_density

# compute model
spdinst_lm <- lm(ME ~ obj_est, data = df_spdinst)
performance::check_model(spdinst_lm)
summary(spdinst_lm)
report(spdinst_lm)

# compute model for log
spdinst_log_lm <- lm(log_ME ~ log_obj, data = df_spdinst)
performance::check_model(spdinst_log_lm)
summary(spdinst_log_lm)
report(spdinst_log_lm)

# use Camille way of reporting stats. This is the same thing reported by the model stat (because there is only one factor so I don't need it)
anova(spdinst_log_lm)
report(anova(spdinst_log_lm))

# compute the point of intersection between the regression
spd_inst_reg_line <- c(46.00323, 0.71970) # reg_line[1] = intercept ; reg_line[2] = slope
spd_inst_unity_line <- c(0,1) # unity_line[1] = intercept (0) ; unity_line[2] = slope (1)

spd_inst_point_of_intersect <- intersection(spd_inst_reg_line, spd_inst_unity_line)

# estimated spd_instance as objective spd_instance (mixing format)
rawspd_inst_as_obj <- ggplot() +
  geom_point(data = df_spdinst, aes(obj_est, c(ME))) +
  # add regression line with outliers
  geom_smooth(data = df_spdinst, aes(obj_est, c(ME)),  method = lm, se = TRUE, color =  '#228833', fill =  '#228833') +
  # stat_regline_equation(label.x = 10, label.y = 340, data = df_spdinst, aes(obj_est, c(ME), label = ..eq.label..), color = '#228833', size = 6) + # the parameter might not be good, it would be better to get the values from the model 
  # stat_regline_equation(label.x = 10, label.y = 320, data = df_spdinst, aes(obj_est, c(ME), label = ..rr.label..), color = '#228833', size = 6) +  # the parameter might not be good, it would be better to get the values from the model
  
  # add the point of subjective equality
  geom_segment(aes(x = 0, y = spd_inst_point_of_intersect, xend = spd_inst_point_of_intersect , yend = spd_inst_point_of_intersect), linetype = "dashed", color = "red") +
  geom_segment(aes(x = spd_inst_point_of_intersect, y = 0, xend = spd_inst_point_of_intersect , yend = spd_inst_point_of_intersect), linetype = "dashed", color = "red") +
  annotate("text", x = 20, y = spd_inst_point_of_intersect+10, label = "178", col = "red")+
  annotate("text", x = spd_inst_point_of_intersect+20, y = 10, label = "178", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(breaks=seq(0,max(df_spdinst$ME[!(is.na(df_spdinst$ME))]),50), limits = c(0, max(df_spdinst$ME[!(is.na(df_spdinst$ME))])), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,max(df_spdinst$ME[!(is.na(df_spdinst$ME))]),50), limits = c(0, max(df_spdinst$ME[!(is.na(df_spdinst$ME))])), expand = c(0,0)) +
  
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)+
  
  # add regression line without outliers
  theme_classic() +
  theme( text = element_text(size = 20))+
  xlab("Objective inst. speed (km/h)")+
  ylab("Estimated inst. speed (km/h)")+
  guides(color = "none")
rawspd_inst_as_obj

# log
logspd_inst_as_obj <- ggplot() +
  geom_point(data = df_spdinst, aes(obj_est, c(ME))) +
  # add regression line with outliers
  geom_smooth(data = df_spdinst, aes(obj_est, c(ME)),  method = lm, se = TRUE, color =  '#228833', fill =  '#228833') +
  stat_regline_equation(label.x = 10, label.y = 340, data = df_spdinst, aes(obj_est, c(ME), label = ..eq.label..), color = '#228833', size = 6) + # the parameter might not be good, it would be better to get the values from the model 
  stat_regline_equation(label.x = 10, label.y = 320, data = df_spdinst, aes(obj_est, c(ME), label = ..rr.label..), color = '#228833', size = 6) +  # the parameter might not be good, it would be better to get the values from the model
  # annotate("text", x = 64, y = 300, label = "log(y) = 0.53 + 0.76*log(x)", col = "#228833")+
  # annotate("text", x = 47, y = 270, label = "R² = 0.64", col = "#228833")+
  
  # add the point of subjective equality
  geom_segment(aes(x = 0, y = spd_inst_point_of_intersect, xend = spd_inst_point_of_intersect , yend = spd_inst_point_of_intersect), linetype = "dashed", color = "red") +
  geom_segment(aes(x = spd_inst_point_of_intersect, y = 0, xend = spd_inst_point_of_intersect , yend = spd_inst_point_of_intersect), linetype = "dashed", color = "red") +
  annotate("text", x = 41, y = spd_inst_point_of_intersect+10, label = "178", col = "red")+
  annotate("text", x = spd_inst_point_of_intersect+13, y = 40, label = "178", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(trans = "log10", limits = c(min(df_spdinst$obj_est), max(df_spdinst$ME[!(is.na(df_spdinst$ME))])), expand = c(0,0)) +
  scale_y_continuous(trans = "log10", limits = c(min(df_spdinst$obj_est), max(df_spdinst$ME[!(is.na(df_spdinst$ME))])), expand = c(0,0)) +
  
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)+
  
  # add regression line without outliers
  theme_classic() +
  theme( text = element_text(size = 20), axis.title.x = element_blank(), axis.title.y = element_blank())+
  xlab("Objective inst. speed (km/h)")+
  ylab("Estimated inst. speed (km/h)")+
  guides(color = "none")
logspd_inst_as_obj

# average speed_________________________________________________________________
df_spdave <- df %>% filter(type == "spd_mean")

# looking at the distribution
spdave_density = ggplot() +
  geom_density(data = df_spdave, mapping = aes(x = ME), alpha = 0.2)+
  xlab("Estimated average speed") +
  ylab("Density") +
  theme_classic() +
  theme(legend.position = c(0.8,0.6), text = element_text(size = 15))
spdave_density

# trying to normalize the data
BN_spdave_obj <- bestNormalize(df_spdave$ME, allow_orderNorm = FALSE, allow_lamber_s = TRUE)
plot(BN_spdave_obj, leg_loc = "bottomright")
df_spdave$norm_ME <- predict(BN_spdave_obj)

# compute log
df_spdave$log_ME <- log10(df_spdave$ME)
df_spdave$log_obj <- log10(df_spdave$obj_est)


# look at the resulting distribution
spdave_density = ggplot() +
  geom_density(data = df_spdave, mapping = aes(x = log_ME), alpha = 0.2)+
  xlab("Log(spdave)") +
  ylab("Density") +
  theme_classic() +
  theme(legend.position = c(0.8,0.6), text = element_text(size = 15))
spdave_density

# compute model
spdave_lm <- lm(ME ~ obj_est, data = df_spdave)
performance::check_model(spdave_lm)
summary(spdave_lm)
report(spdave_lm)

# compute model for log
spdave_log_lm <- lm(log_ME ~ log_obj, data = df_spdave)
performance::check_model(spdave_log_lm)
summary(spdave_log_lm)
report(spdave_log_lm)

# use Camille way of reporting stats. This is the same thing reported by the model stat (because there is only one factor so I don't need it)
anova(spdave_log_lm)
report(anova(spdave_log_lm))

# compute the point of intersection between the regression
spd_mean_reg_line <- c(141.12842, 0.37154) # reg_line[1] = intercept ; reg_line[2] = slope
spd_mean_unity_line <- c(0,1) # unity_line[1] = intercept (0) ; unity_line[2] = slope (1)

spd_mean_point_of_intersect <- intersection(spd_mean_reg_line, spd_mean_unity_line) # 212

rawspd_mean_as_obj <- ggplot() +
  geom_point(data = df_spdave, aes(obj_est, c(ME))) +
  # add regression line with outliers
  geom_smooth(data = df_spdave, aes(obj_est, c(ME)),  method = lm, se = TRUE, color = '#CCBB44', fill ='#CCBB44') +
  # stat_regline_equation(label.x = 10, label.y = 340, data = df_spdave, aes(obj_est, c(ME), label = ..eq.label..), color = '#CCBB44', size = 6) + # the parameter might not be good, it would be better to get the values from the model 
  # stat_regline_equation(label.x = 10, label.y = 320, data = df_spdave, aes(obj_est, c(ME), label = ..rr.label..), color = '#CCBB44', size = 6) +  # the parameter might not be good, it would be better to get the values from the model
  
  # add the point of subjective equality
  geom_segment(aes(x = 0, y = spd_mean_point_of_intersect, xend = spd_mean_point_of_intersect , yend = spd_mean_point_of_intersect), linetype = "dashed", color = "red") +
  geom_segment(aes(x = spd_mean_point_of_intersect, y = 0, xend = spd_mean_point_of_intersect , yend = spd_mean_point_of_intersect), linetype = "dashed", color = "red") +
  annotate("text", x = 20, y = spd_mean_point_of_intersect+10, label = "216", col = "red")+
  annotate("text", x = spd_mean_point_of_intersect+20, y = 10, label = "216", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(breaks=seq(0,max(df_spdave$ME[!(is.na(df_spdave$ME))]),50), limits = c(0, max(df_spdave$ME[!(is.na(df_spdave$ME))])), expand = c(0,0)) +
  scale_y_continuous(breaks=seq(0,max(df_spdave$ME[!(is.na(df_spdave$ME))]),50), limits = c(0, max(df_spdave$ME[!(is.na(df_spdave$ME))])), expand = c(0,0)) +
  
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)+
  
  # add regression line without outliers
  theme_classic() +
  theme( text = element_text(size = 20))+
  xlab("Objective average speed (km/h)")+
  ylab("Estimated average speed (km/h)")+
  guides(color = "none")
rawspd_mean_as_obj
# we can see the vierdort law (can be debated), however the weber law is not so clear, as we see that the sd start to reduce by the end of the table (effect of cognitive map?) 

# plot for log
logspd_mean_as_obj <- ggplot() +
  geom_point(data = df_spdave, aes(obj_est, c(ME))) +
  # add regression line with outliers
  geom_smooth(data = df_spdave, aes(obj_est, c(ME)),  method = lm, se = TRUE, color = '#CCBB44', fill ='#CCBB44') +
  stat_regline_equation(label.x = 10, label.y = 420, data = df_spdave, aes(obj_est, c(ME), label = ..eq.label..), color = '#CCBB44', size = 6) + # the parameter might not be good, it would be better to get the values from the model 
  stat_regline_equation(label.x = 10, label.y = 320, data = df_spdave, aes(obj_est, c(ME), label = ..rr.label..), color = '#CCBB44', size = 6) +  # the parameter might not be good, it would be better to get the values from the model
  # annotate("text", x = 69, y = 340, label = "log(y) = 1.65 + 0.29*log(x)", col = "#CCBB44")+
  # annotate("text", x = 52, y = 300, label = "R² = 0.17", col = "#CCBB44")+
  
  
  
  # add the point of subjective equality
  geom_segment(aes(x = 0, y = spd_mean_point_of_intersect, xend = spd_mean_point_of_intersect , yend = spd_mean_point_of_intersect), linetype = "dashed", color = "red") +
  geom_segment(aes(x = spd_mean_point_of_intersect, y = 0, xend = spd_mean_point_of_intersect , yend = spd_mean_point_of_intersect), linetype = "dashed", color = "red") +
  annotate("text", x = 46.5, y = spd_mean_point_of_intersect+10, label = "216", col = "red")+
  annotate("text", x = spd_mean_point_of_intersect+20, y = 45.2, label = "216", col = "red")+
  
  # change ticks in x and y and the scale. I want talso to set the size but not sure it here that I should do it 
  scale_x_continuous(trans= "log10", limits = c(min(df_spdave$obj_est), max(df_spdave$ME[!(is.na(df_spdave$ME))])), expand = c(0,0)) +
  scale_y_continuous(trans= "log10", limits = c(min(df_spdave$obj_est), max(df_spdave$ME[!(is.na(df_spdave$ME))])), expand = c(0,0)) +
  
  # add unity line
  geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8, alpha = 0.5)+
  
  # add regression line without outliers
  theme_classic() +
  theme(text = element_text(size = 20), axis.title.x = element_blank(), axis.title.y = element_blank())+
  xlab("Objective average speed (km/h)")+
  ylab("Estimated average speed (km/h)")+
  guides(color = "none")
logspd_mean_as_obj


# full fig
ME_as_objMag_allmag <- gridExtra::grid.arrange(rawdur_as_obj,rawdist_as_obj,rawspd_inst_as_obj,rawspd_mean_as_obj)

logME_as_objMag_allmag <- gridExtra::grid.arrange(logdur_as_logobj, logdist_as_obj, logspd_inst_as_obj, logspd_mean_as_obj)
