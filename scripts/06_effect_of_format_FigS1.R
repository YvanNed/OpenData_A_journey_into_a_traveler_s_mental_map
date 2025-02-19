################################################################################
# check correlation between format and a possible interaction with train factors #
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
library(RColorBrewer)

rm(list=ls())     # to clear current workspace

################################################################################
################################# sub fonction #################################
################################################################################

cook.influential_index <- function(x, y, my_title="")
  # this function compute the cook distance of each data point  and return the idx of the column of outliers 
{
  mod <- lm(x ~ y)
  cooksd <- cooks.distance(mod)
  sample_size <- length(y)
  plot(cooksd, pch=23, bg= "orange", cex=2, main=my_title)
  thres = 4
  abline(h = 4 / sample_size, lwd=2)
  # text(x = 1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd > 4 / sample_size, names(cooksd), ""))
  text(x = 1:length(cooksd)+1, y=cooksd, labels=names(cooksd))
  influential <- as.numeric(names(cooksd)[(cooksd > 4/sample_size)])
  print(paste( "Threshold:", 4/sample_size))
  return(influential)
}

add_outliers <- function(df, out_list, knw, comment = ""){
  
  for (o in 1:length(out_list)){
    if (knw){
      print(paste("Knowledge: ", df$knw_est[df$participants == out_list[o]], " ; participant: ", df$participants[df$participants == out_list[o]], sep="") )
    }else{
      print(paste("Participant: ", df$participants[df$participants == out_list[o]], " ; distance (est-obj): ", df$est[df$participants == out_list[o]] - df$obj_est[df$participants == out_list[o]], " ; relative estimation: ", df$RE_est[df$participants == out_list[o]], sep = "" ))
    }
    if (is.na(df$outliers[df$participants == out_list[o]])){
      df$outliers[df$participants == out_list[o]] <- comment
    }else{
      df$outliers[df$participants == out_list[o]] <- paste(df$outliers[df$participants == out_list[o]], paste("_", comment,sep=""), sep = "")
    }
  }
  return(df)
}

intersection <- function(l1, l2){
  x <- (l2[1] - l1[1]) / (l1[2] - l2[2])
  y <- l1[1] + l1[2] * x
  return(xy=c(x, y))
}

################################################################################
# load data
################################################################################

setwd("C:\\Users\\yvann\\OneDrive\\2022_QTEV_Analysis_YN\\")
df <- read.csv("dataframe_ready2use.csv")

# check that the correct dataframe is loaded. There should be only 247 participants
df %>% group_by(type, format) %>% count(type)

# remove graphic estimation based on objective values (it was a curiosity and I don't have time to focus on it now)
df <- df %>% filter(format != "graphic (obj)")
# rename graphic estimation for simplicity 
df$format[df$format == "graphic (knw)"] <- "graphic"

df$format <- as.factor(df$format)
df$ep_est <- as.factor(df$ep_est)
df$orientation <- as.factor(df$orientation)


################################################################################
################## Compute correlation between the two format ##################
################################################################################

# for duration__________________________________________________________________
# extract one df for dur estimation, and remove the graphic based on objective duration (curiosity that I abandoned)
df_dur <- df %>% filter(type == "dur")  

# check correlation between the two format
# check normality
shapiro.test(df_dur$est)
qqnorm(df_dur$est)
qqline(df_dur$est)
hist(df_dur$est)
# reject null hyp (so not following a normal distribution..)
# QQ plot seems fine anyway

shapiro.test(df_dur$est[df_dur$format == "numeric"])
qqnorm(df_dur$est[df_dur$format == "numeric"])
qqline(df_dur$est[df_dur$format == "numeric"])
# reject null hyp (so not following a normal distribution..)
# QQ plot is not so bad

shapiro.test(df_dur$est[df_dur$format == "graphic"])
qqnorm(df_dur$est[df_dur$format == "graphic"])
qqline(df_dur$est[df_dur$format == "graphic"])
# reject null hyp (so not following a normal distribution..)
# QQ plot is not so bad

cor_dur_pears <- cor.test(df_dur$est[df_dur$format == "numeric"], df_dur$est[df_dur$format == "graphic"])
report(cor_dur_pears)
# ==> cor. coef. 0.91

cor_dur_spear <- cor.test(df_dur$est[df_dur$format == "numeric"], df_dur$est[df_dur$format == "graphic"], method = "spearman")
report(cor_dur_spear)
# spearman is also significant, but p-values are not exact because there is ties 
# ==> rho = 0.93

# plot the scatterplot of the correlation
dur_num <- df_dur$est[df_dur$format == "numeric"] # extract numerical duration estimation
dur_graph <- df_dur$est[df_dur$format == "graphic"] # extract graphical duration estimation
df_scatter_dur <- data.frame(numeric = dur_num, graphic = dur_graph) # create a dataframe to use ggplot2 
dur_scatter <- ggplot(df_scatter_dur, aes(x = numeric, y = graphic)) +
  geom_point(color = "black", size = 2, shape = 16) +
  geom_smooth(method = "lm", color = "black", se = TRUE, fullrange = FALSE) +
  annotate("text", x = 35, y = 170, label = "pearson's r = 0.91", col = "black")+
  annotate("text", x = 35, y = 160, label = "pvalue < 2.2e-16", col = "black")+
  scale_x_continuous(breaks=seq(0,max(df_scatter_dur$graphic[!(is.na(df_scatter_dur$graphic))]),25), limits = c(0, max(df_scatter_dur$graphic[!(is.na(df_scatter_dur$graphic))]), expand = c(0,0))) +
  scale_y_continuous(breaks=seq(0,max(df_scatter_dur$graphic[!(is.na(df_scatter_dur$graphic))]),25), limits = c(0, max(df_scatter_dur$graphic[!(is.na(df_scatter_dur$graphic))]), expand = c(0,0))) +
  theme_classic()+
  theme(axis.title = element_text(size = 15))+
  # theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "grey"), axis.title = element_text(size = 15)) +
  labs(x = "Numerical estimated duration (min)", y = "Graphical estimated duration (min)")
dur_scatter

# plot the linear regression between est and objective est for each format
# this plot is not necessary, because there is no interaction
# dur_as_objest_format <- ggplot(data = df_dur) +
#   geom_point(aes(obj_est, c(est), fill = format, color = format)) +
#   # add regression line with outliers
#   geom_smooth(aes(obj_est, c(est), fill = format, color = format), method = lm, se = TRUE, fullrange = FALSE) +
#   theme_classic() +
#   scale_x_continuous(breaks=seq(0,max(df_dur$est),25), limits = c(0, max(df_dur$est)), expand = c(0,0)) +
#   scale_y_continuous(breaks=seq(0,max(df_dur$est),25), limits = c(0, max(df_dur$est)), expand = c(0,0)) +
#   ylab("Estimated duration (min)")+
#   xlab("Objective duration (min)")+
#   theme(legend.position = c(0.2,0.8),text = element_text(size = 15))+ # pour supprimers les nombre sur l'axe des x il faut rajouter l'argument , axis.text.x=element_blank()
#   # add unity line
#   geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8)
# dur_as_objest_format
# ==> the two regression lines overlap perfectly (as suggested by their very high correlation)
# ==> Note for later: the regression point seems not at the middle of the travel, but it could be highly influenced by the outliers
# I am gonna check the regression point with the outliers and remove them if needed (either cook distance, or arbitrary threshold "à la" Balci)
# Should I do this after I checked the effect of format ? maybe yes

# checking the effect of format on relative estimation, and its interaction with train travel factors (speed episodes, orientation, objective magnitude)
dur_lm <- lm(RE_est ~ 1 + format + orientation + ep_est + format:orientation + format:ep_est, data = df_dur)
performance::check_model(dur_lm) # all good
summary(dur_lm)
# only main effect of acceleration episode !
# there is no interaction between format and train factors

# use Camille method to investigate results
anova(dur_lm) # only main effect of acceleration ep


# Since there is no effect of format, I compute a score between the two
df_dur_score <- df_dur %>% filter(format == "graphic")
df_dur_num <- df_dur %>% filter(format == "numeric")

df_dur_score$ME <- NA # mean of the Magnitude Estimations in graphic and numeric 
df_dur_score$RME <- NA # mean of the Relative Magnitude Estimations in graphic and numeric 
df_dur_score$ERM <- NA # mean of the relative error

for(i in 1:length(df_dur_score$participants)){
  
  if (!(is.na(df_dur_score$est[i])) & !(is.na(df_dur_num$est[i]))){ # if none is missing then take the mean
    df_dur_score$ME[i] <- (df_dur_score$est[i] + df_dur_num$est[i])/2
    df_dur_score$RME[i] <- (df_dur_score$RE_est[i] + df_dur_num$RE_est[i])/2
    df_dur_score$ERM[i] <- (df_dur_score$ER_est[i] + df_dur_num$ER_est[i])/2
  }else if (is.na(df_dur_score$est[i]) & !(is.na(df_dur_num$est[i]))){ # if graph is missing then take only num
    df_dur_score$ME[i] <- df_dur_num$est[i]
    df_dur_score$RME[i] <- df_dur_num$RE_est[i]
    df_dur_score$ERM[i] <- df_dur_num$ER_est[i]
  }else if (!(is.na(df_dur_score$est[i])) & is.na(df_dur_num$est[i])){ # if num is missing then take only graph
    df_dur_score$ME[i] <- df_dur_score$est[i] 
    df_dur_score$RME[i] <- df_dur_score$RE_est[i]
    df_dur_score$ERM[i] <- df_dur_score$ER_est[i]
  }
}

# test with anova
dur_aov <- aov(RE_est ~ format * orientation * ep_est, data = df_dur)
summary(dur_aov)
report(dur_aov) %>% as.data.frame() # comment the last part to have a written summary
# check assumptions
# Vérifier l'indépendance des observations
durbinWatsonTest(dur_aov)

# Vérifier la normalité des résidus
shapiro.test(residuals(dur_aov))
# ==> les résidus sont pas normals

# Vérifier l'homogénéité des variances
car::leveneTest(residuals(dur_aov) ~ format * orientation * ep_est, data = na.omit(df_dur))
df_dur$combined_factor <- interaction(df_dur$format, df_dur$orientation, df_dur$ep_est)
bartlett.test(RE_est ~ combined_factor, data = df_dur)
# ==> l'homogénéité des variances n'est pas respecté...
# je vais rester sur le modèle linéaire pcq les hypothèses sont pas respecté pour l'anova



# for distance__________________________________________________________________
# extract one df for dist estimation, and remove the graphic based on objective duration (curiosity that I abandoned)
df_dist <- df %>% filter(type == "dist")  

# check normality
shapiro.test(df_dist$est)
qqnorm(df_dist$est)
qqline(df_dist$est)
# not normally distributed
# but QQplot not so bad

shapiro.test(df_dist$est[df_dist$format == "numeric"])
qqnorm(df_dist$est[df_dist$format == "numeric"])
qqline(df_dist$est[df_dist$format == "numeric"])
# not normally distributed
# but QQplot not so bad

shapiro.test(df_dist$est[df_dist$format == "graphic"])
qqnorm(df_dist$est[df_dist$format == "graphic"])
qqline(df_dist$est[df_dist$format == "graphic"])
# not normally distributed
# but QQplot not so bad

# check correlation between the two format
cor_dist_pears <- cor.test(df_dist$est[df_dist$format == "numeric"], df_dist$est[df_dist$format == "graphic"], method = "pearson")
report(cor_dist_pears)
# ==> cor. coef. 0.92 + significant
cor_dist_spear <- cor.test(df_dist$est[df_dist$format == "numeric"], df_dist$est[df_dist$format == "graphic"], method = "spearman")
report(cor_dist_spear)
# ==> =spearman is also significant and positive and very large
# plot the scatterplot of the correlation
dist_num <- df_dist$est[df_dist$format == "numeric"] # extract numerical duration estimation
dist_graph <- df_dist$est[df_dist$format == "graphic"] # extract graphical duration estimation
df_scatter_dist <- data.frame(numeric = dist_num, graphic = dist_graph) # create a dataframe to use ggplot2 
dist_scatter <- ggplot(df_scatter_dist, aes(x = numeric, y = graphic)) +
  geom_point(color = "black", size = 2, shape = 16) +
  geom_smooth(method = "lm", color = "black", se = TRUE, fullrange = FALSE) +
  annotate("text", x = 100, y = 580, label = "pearson's r = 0.92", col = "black")+
  annotate("text", x = 100, y = 550, label = "pvalue < 2.2e-16", col = "black")+
  scale_x_continuous(breaks=seq(0,max(df_scatter_dist$graphic[!(is.na(df_scatter_dist$graphic))]),100), limits = c(0, max(df_scatter_dist$graphic[!(is.na(df_scatter_dist$graphic))]), expand = c(0,0))) +
  scale_y_continuous(breaks=seq(0,max(df_scatter_dist$graphic[!(is.na(df_scatter_dist$graphic))]),100), limits = c(0, max(df_scatter_dist$graphic[!(is.na(df_scatter_dist$graphic))]), expand = c(0,0))) +
  theme_classic()+
  theme(axis.title = element_text(size = 15))+
  # theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "grey"), axis.title = element_text(size = 15)) +
  labs(x = "Numerical estimated distance (km)", y = "Graphical estimated distance (km)")
dist_scatter

# checking the effect of format on relative estimation, and its interaction with train travel factors (speed episodes, orientation, objective magnitude)
dist_lm <- lm(RE_est ~ 1 + format + orientation + ep_est + format:orientation + format:ep_est, data = df_dist)
performance::check_model(dist_lm) # all good
summary(dist_lm)
# main effect of format (underestimation of numeric compared to graphic).., acceleration episode!
# interaction format & acceleration episode

# chek Camille way of reporting results
anova(dist_lm)

# test with anova
dist_aov <- aov(RE_est ~ format * orientation * ep_est, data = df_dist)
summary(dist_aov)
report(dist_aov) %>% as.data.frame() 


# plot main effect of format
bp_dist_format = ggplot(df_dist, aes(x=format, y = RE_est), na.rm = FALSE) +
  geom_boxplot(na.rm = FALSE, fill = '#EE6677') +
  # geom_point(aes(color = knwYes No_est) +
  theme_classic()+ 
  xlab("Format")+
  ylab("Relative estimated distance")+
  geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
  theme( text = element_text(size = 20)) +
  labs(fill = "Acceleration") +
  guides(fill = "none", alpha = "none")
bp_dist_format
# numeric estimation are underestimated compared to graphic estimation

# plot the interaction
bp_dist_format = ggplot(df_dist, aes(x=ep_est, y = RE_est), na.rm = FALSE) +
  geom_boxplot(na.rm = FALSE, aes(fill = format)) +
  # geom_point(aes(color = knwYes No_est) +
  theme_classic()+ 
  xlab("Format")+
  ylab("Relative estimated distance")+
  geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
  theme( text = element_text(size = 20)) +
  labs(fill = "Acceleration") +
  guides(fill = "none", alpha = "none")
bp_dist_format
# there is a diff of format in the acceleartion episode, but not in the constant

# plot the linear regression between est and objective est for each format
# this plot is not necessary, because there is no interaction between obj_est and format
# dist_as_objest_format <- ggplot(data = df_dist) +
#   geom_point(aes(obj_est, c(est), fill = format, color = format)) +
#   # add regression line with outliers
#   geom_smooth(aes(obj_est, c(est), fill = format, color = format), method = lm, se = TRUE, fullrange = FALSE) +
#   theme_classic() +
#   # scale_x_continuous(breaks=seq(0,max(df_dist$est),25), limits = c(0, max(df_dist$est)), expand = c(0,0)) +
#   # scale_y_continuous(breaks=seq(0,max(df_dist$est),25), limits = c(0, max(df_dist$est)), expand = c(0,0)) +
#   ylab("Estimated distance (km)")+
#   xlab("Objective distance (km)")+
#   theme(legend.position = c(0.2,0.8),text = element_text(size = 15))+ # pour supprimers les nombre sur l'axe des x il faut rajouter l'argument , axis.text.x=element_blank()
#   # add unity line
#   geom_abline(intercept = 0, slope = 1, color = "black", linetype="solid", size=0.8)
# dist_as_objest_format

# Since there is such a high correlation between graphic and numeric, I compute a score between the two
df_dist_score <- df_dist %>% filter(format == "graphic")
df_dist_num <- df_dist %>% filter(format == "numeric")

df_dist_score$ME <- NA # mean of the Magnitude Estimations in graphic and numeric 
df_dist_score$RME <- NA # mean of the Relative Magnitude Estimations in graphic and numeric 
df_dist_score$ERM <- NA # mean of the relative error

for(i in 1:length(df_dist_score$participants)){
  
  if (!(is.na(df_dist_score$est[i])) & !(is.na(df_dist_num$est[i]))){ # if none is missing then take the mean
    df_dist_score$ME[i] <- (df_dist_score$est[i] + df_dist_num$est[i])/2
    df_dist_score$RME[i] <- (df_dist_score$RE_est[i] + df_dist_num$RE_est[i])/2
    df_dist_score$ERM[i] <- (df_dist_score$ER_est[i] + df_dist_num$ER_est[i])/2
  }else if (is.na(df_dist_score$est[i]) & !(is.na(df_dist_num$est[i]))){ # if graph is missing then take only num
    df_dist_score$ME[i] <- df_dist_num$est[i]
    df_dist_score$RME[i] <- df_dist_num$RE_est[i]
    df_dist_score$ERM[i] <- df_dist_num$ER_est[i]
  }else if (!(is.na(df_dist_score$est[i])) & is.na(df_dist_num$est[i])){ # if num is missing then take only graph
    df_dist_score$ME[i] <- df_dist_score$est[i] 
    df_dist_score$RME[i] <- df_dist_score$RE_est[i]
    df_dist_score$ERM[i] <- df_dist_score$ER_est[i]
  }
}


# for instantaneous speed__________________________________________________________________
# extract one df for dist estimation, and remove the graphic based on objective duration (curiosity that I abandoned)
df_spdinst <- df %>% filter(type == "spd_inst")  

# check normality
shapiro.test(df_spdinst$est)
qqnorm(df_spdinst$est)
qqline(df_spdinst$est)
# not normal, qqplot is okish
shapiro.test(df_spdinst$est[df_spdinst$format == "numeric"])
qqnorm(df_spdinst$est[df_spdinst$format == "numeric"])
qqline(df_spdinst$est[df_spdinst$format == "numeric"])
# not normal, qqplot okish
shapiro.test(df_spdinst$est[df_spdinst$format == "graphic"])
qqnorm(df_spdinst$est[df_spdinst$format == "graphic"])
qqline(df_spdinst$est[df_spdinst$format == "graphic"])
# not normal, qqplot is good

# check correlation between the two format
cor_spdinst_pears <- cor.test(df_spdinst$est[df_spdinst$format == "numeric"], df_spdinst$est[df_spdinst$format == "graphic"], method = "pearson")
report(cor_spdinst_pears)
# ==> cor. coef. 0.84 + significant
cor_spdinst_spear <- cor.test(df_spdinst$est[df_spdinst$format == "numeric"], df_spdinst$est[df_spdinst$format == "graphic"], method = "spearman")
report(cor_spdinst_spear)
# spear is significant also

# plot the scatterplot of the correlation
spdinst_num <- df_spdinst$est[df_spdinst$format == "numeric"] # extract numerical duration estimation
spdinst_graph <- df_spdinst$est[df_spdinst$format == "graphic"] # extract graphical duration estimation
df_scatter_spdinst <- data.frame(numeric = spdinst_num, graphic = spdinst_graph) # create a dataframe to use ggplot2 
spdinst_scatter <- ggplot(df_scatter_spdinst, aes(x = numeric, y = graphic)) +
  geom_point(color = "black", size = 2, shape = 16) +
  geom_smooth(method = "lm", color = "black", se = TRUE, fullrange = FALSE) +
  annotate("text", x = 60, y = 340, label = "pearson's r = 0.84", col = "black")+
  annotate("text", x = 60, y = 325, label = "pvalue < 2.2e-16", col = "black")+
  scale_x_continuous(breaks=seq(0,max(df_scatter_spdinst$graphic[!(is.na(df_scatter_spdinst$graphic))]),50), limits = c(0, max(df_scatter_spdinst$graphic[!(is.na(df_scatter_spdinst$graphic))]), expand = c(0,0))) +
  scale_y_continuous(breaks=seq(0,max(df_scatter_spdinst$graphic[!(is.na(df_scatter_spdinst$graphic))]),50), limits = c(0, max(df_scatter_spdinst$graphic[!(is.na(df_scatter_spdinst$graphic))]), expand = c(0,0))) +
  theme_classic()+
  theme(axis.title = element_text(size = 15))+
  # theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "grey"), axis.title = element_text(size = 15)) +
  labs(x = "Numerical instantaneous speed (km/h)", y = "Graphical instantaneous speed (km/h)")
spdinst_scatter

# checking the effect of format on relative estimation, and its interaction with train travel factors (speed episodes, orientation, objective magnitude)
spdinst_lm <- lm(RE_est ~ 1 + format + orientation + ep_est + format:orientation + format:ep_est, data = df_spdinst)
performance::check_model(spdinst_lm) # all good
summary(spdinst_lm)
# main effect of speed episode !

# use camille way of reporting stats
anova(spdinst_lm)

# check with anova
spdinst_aov <- aov(RE_est ~ format + orientation + ep_est + format:orientation + format:ep_est, data = df_spdinst)
summary(spdinst_aov)

# Since there is no effect of format, I compute a score between the two
df_spdinst_score <- df_spdinst %>% filter(format == "graphic")
df_spdinst_num <- df_spdinst %>% filter(format == "numeric")

df_spdinst_score$ME <- NA # mean of the Magnitude Estimations in graphic and numeric 
df_spdinst_score$RME <- NA # mean of the Relative Magnitude Estimations in graphic and numeric 
df_spdinst_score$ERM <- NA # mean of the relative error

for(i in 1:length(df_spdinst_score$participants)){
  
  if (!(is.na(df_spdinst_score$est[i])) & !(is.na(df_spdinst_num$est[i]))){ # if none is missing then take the mean
    df_spdinst_score$ME[i] <- (df_spdinst_score$est[i] + df_spdinst_num$est[i])/2
    df_spdinst_score$RME[i] <- (df_spdinst_score$RE_est[i] + df_spdinst_num$RE_est[i])/2
    df_spdinst_score$ERM[i] <- (df_spdinst_score$ER_est[i] + df_spdinst_num$ER_est[i])/2
  }else if (is.na(df_spdinst_score$est[i]) & !(is.na(df_spdinst_num$est[i]))){ # if graph is missing then take only num
    df_spdinst_score$ME[i] <- df_spdinst_num$est[i]
    df_spdinst_score$RME[i] <- df_spdinst_num$RE_est[i]
    df_spdinst_score$ERM[i] <- df_spdinst_num$ER_est[i]
  }else if (!(is.na(df_spdinst_score$est[i])) & is.na(df_spdinst_num$est[i])){ # if num is missing then take only graph
    df_spdinst_score$ME[i] <- df_spdinst_score$est[i] 
    df_spdinst_score$RME[i] <- df_spdinst_score$RE_est[i]
    df_spdinst_score$ERM[i] <- df_spdinst_score$ER_est[i]
  }
}


# for average speed__________________________________________________________________
# extract one df for dist estimation, and remove the graphic based on objective duration (curiosity that I abandoned)
df_spdave <- df %>% filter(type == "spd_mean")  

# check normality
shapiro.test(df_spdave$est)
qqnorm(df_spdave$est)
qqline(df_spdave$est)
# not normal but qqplot seems fine

shapiro.test(df_spdave$est[df_spdave$format == "numeric"])
qqnorm(df_spdave$est[df_spdave$format == "numeric"])
qqline(df_spdave$est[df_spdave$format == "numeric"])
# not normal but qqplot seems fine
shapiro.test(df_spdave$est[df_spdave$format == "graphic"])
qqnorm(df_spdave$est[df_spdave$format == "graphic"])
qqline(df_spdave$est[df_spdave$format == "graphic"])
# not normal but qqplot seems fine (only few outliers ?)

# check correlation between the two format
cor_spdave_pears <- cor.test(df_spdave$est[df_spdave$format == "numeric"], df_spdave$est[df_spdave$format == "graphic"])
report(cor_spdave_pears)
# ==> cor. coef. 0.71 + significant
cor_spdave_spear <- cor.test(df_spdave$est[df_spdave$format == "numeric"], df_spdave$est[df_spdave$format == "graphic"], method = "spearman")
report(cor_spdave_spear)
# spearman is also significant

# plot the scatterplot of the correlation
spdave_num <- df_spdave$est[df_spdave$format == "numeric"] # extract numerical duration estimation
spdave_graph <- df_spdave$est[df_spdave$format == "graphic"] # extract graphical duration estimation
df_scatter_spdave <- data.frame(numeric = spdave_num, graphic = spdave_graph) # create a dataframe to use ggplot2 
spdave_scatter <- ggplot(df_scatter_spdave, aes(x = numeric, y = graphic)) +
  geom_point(color = "black", size = 2, shape = 16) +
  geom_smooth(method = "lm", color = "black", se = TRUE, fullrange = FALSE) +
  annotate("text", x = 80, y = 445, label = "pearson's r = 0.71", col = "black")+
  annotate("text", x = 80, y = 425, label = "pvalue < 2.2e-16", col = "black")+
  scale_x_continuous(breaks=seq(0,max(df_scatter_spdave$graphic[!(is.na(df_scatter_spdave$graphic))]),50), limits = c(0, max(df_scatter_spdave$graphic[!(is.na(df_scatter_spdave$graphic))]), expand = c(0,0))) +
  scale_y_continuous(breaks=seq(0,max(df_scatter_spdave$graphic[!(is.na(df_scatter_spdave$graphic))]),50), limits = c(0, max(df_scatter_spdave$graphic[!(is.na(df_scatter_spdave$graphic))]), expand = c(0,0))) +
  theme_classic()+
  theme(axis.title = element_text(size = 15))+
  # theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(color = "grey"), axis.title = element_text(size = 15)) +
  labs(x = "Numerical average speed (km/h)", y = "Graphical average speed (km/h)")
spdave_scatter

# checking the effect of format on relative estimation, and its interaction with train travel factors (speed episodes, orientation, objective magnitude)
spdave_lm <- lm(RE_est ~ 1  + format + orientation + ep_est + format:orientation + format:ep_est, data = df_spdave)
performance::check_model(spdave_lm) # all good
summary(spdave_lm)
# main effect of format.. and obj_est
# interaction between format and acceleration is close to be significant (and is significant if the obj_est value is added)

# use Camille way of reporing results
anova(spdave_lm)

# plot main effect of format
bp_spdave_format = ggplot(df_spdave, aes(x=format, y = RE_est), na.rm = FALSE) +
  geom_boxplot(na.rm = FALSE, fill = '#CCBB44') +
  # geom_point(aes(color = knwYes No_est) +
  theme_classic()+ 
  xlab("Format")+
  ylab("Relative estimated average speed")+
  labs(color = "Acceleration :", size = 30) +
  geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
  theme( text = element_text(size = 20))+
  guides(fill = "none", alpha = "none")
bp_spdave_format
# numeric estimation are overestimated compared to graphic

# plot the interaction
bp_spdave_format = ggplot(df_spdave, aes(x=ep_est, y = RE_est), na.rm = FALSE) +
  geom_boxplot(na.rm = FALSE, aes(fill = format)) +
  # geom_point(aes(color = knwYes No_est) +
  theme_classic()+ 
  xlab("acceleration")+
  ylab("Relative estimated average speed")+
  geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")+
  theme( text = element_text(size = 20)) +
  labs(fill = "format") +
  guides(fill = "none", alpha = "none")
bp_spdave_format
# the interaction is that the overestimation of numeric is higher in acceleration than in constant and deceleration
# Since only average  speed have a main effect of format, I compute a score between the two even for average speed to be more consistent
df_spdave_score <- df_spdave %>% filter(format == "graphic")
df_spdave_num <- df_spdave %>% filter(format == "numeric")

df_spdave_score$ME <- NA # mean of the Magnitude Estimations in graphic and numeric 
df_spdave_score$RME <- NA # mean of the Relative Magnitude Estimations in graphic and numeric 
df_spdave_score$ERM <- NA # mean of the relative error

for(i in 1:length(df_spdave_score$participants)){
  
  if (!(is.na(df_spdave_score$est[i])) & !(is.na(df_spdave_num$est[i]))){ # if none is missing then take the mean
    df_spdave_score$ME[i] <- (df_spdave_score$est[i] + df_spdave_num$est[i])/2
    df_spdave_score$RME[i] <- (df_spdave_score$RE_est[i] + df_spdave_num$RE_est[i])/2
    df_spdave_score$ERM[i] <- (df_spdave_score$ER_est[i] + df_spdave_num$ER_est[i])/2
  }else if (is.na(df_spdave_score$est[i]) & !(is.na(df_spdave_num$est[i]))){ # if graph is missing then take only num
    df_spdave_score$ME[i] <- df_spdave_num$est[i]
    df_spdave_score$RME[i] <- df_spdave_num$RE_est[i]
    df_spdave_score$ERM[i] <- df_spdave_num$ER_est[i]
  }else if (!(is.na(df_spdave_score$est[i])) & is.na(df_spdave_num$est[i])){ # if num is missing then take only graph
    df_spdave_score$ME[i] <- df_spdave_score$est[i] 
    df_spdave_score$RME[i] <- df_spdave_score$RE_est[i]
    df_spdave_score$ERM[i] <- df_spdave_score$ER_est[i]
  }
}

# make the full fig
correlation_full_fig <- gridExtra::grid.arrange(dur_scatter, dist_scatter, spdinst_scatter, spdave_scatter)

# change graphic to analog for more consistency with the manuscript

# going for the histograme now
# duration hist
hist_plot_dur = ggplot(df_dur %>% filter(format != "graphic (obj)"), aes(x=est, fill = format)) +
  geom_histogram(binwidth = 2, alpha = 0.5, position = "identity") +
  scale_x_continuous(breaks=seq(0,400,15)) +
  xlab("Estimated duration (min)") +
  ylab("Count") +
  theme_classic() +
  # scale_fill_discrete(labels = c("numeric" = "numeric", "graphic" = "analog")) +
  # scale_fill_manual(values = c("numeric" = "#66C2A5", "analog" = "#FC8D62")) +
  theme(legend.position = c(0.2,0.9), text = element_text(size = 15)) +
  guides(color = "none", fill = "none", alpha = "none")
hist_plot_dur

# distance
hist_plot_dist = ggplot(df_dist %>% filter(format != "graphic (obj)"), aes(x=est, fill = format)) +
  geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") +
  scale_x_continuous(breaks=seq(0,700,50)) +
  xlab("Estimated distance (km)") +
  ylab("Count") +
  theme_classic() +
  # scale_fill_discrete(labels = c("numeric" = "numeric", "graphic" = "analog")) +
  # scale_fill_manual(values = c("numeric" = "#66C2A5", "analog" = "#FC8D62")) +
  theme(legend.position = c(0.8,0.7), text = element_text(size = 15))
hist_plot_dist

# inst. speed
hist_plot_spd_inst = ggplot(df_spdinst %>% filter(format != "graphic (obj)"), aes(x=est, fill = format)) +
  geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") +
  scale_x_continuous(breaks=seq(0,500,50)) +
  xlab("Estimated inst. speed (km/h)") +
  ylab("Count") +
  theme_classic() +
  # scale_fill_discrete(labels = c("numeric" = "numeric", "graphic" = "analog")) +
  # scale_fill_manual(values = c("numeric" = "#66C2A5", "analog" = "#FC8D62")) +
  theme(legend.position = c(0.22,0.8), text = element_text(size = 15)) +
  guides(color = "none", fill = "none", alpha = "none")

hist_plot_spd_inst

# mean speed
hist_plot_spd_mean = ggplot(df_spdave %>% filter(format != "graphic (obj)"), aes(x=est, fill = format)) +
  geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") +
  scale_x_continuous(breaks=seq(0,500,50)) +
  ylim(0,15)+
  xlab("Estimated average speed (km/h)") +
  ylab("Count") +
  theme_classic() +
  # scale_fill_discrete(labels = c("numeric" = "numeric", "graphic" = "analog")) +
  # scale_fill_manual(values = c("numeric" = "#66C2A5", "analog" = "#FC8D62")) +
  theme(legend.position = c(0.8,0.5), text = element_text(size = 15)) +
  guides(color = "none", fill = "none", alpha = "none")

hist_plot_spd_mean

gridExtra::grid.arrange(hist_plot_dur, hist_plot_dist, hist_plot_spd_inst, hist_plot_spd_mean,  ncol = 2, nrow = 2)

# remove outliers based on blursday and fuat's paper____________________________
# duration______________________________________________________________________
# first, based on objective value
dur_min_thres <- min(df_dur_score$obj_est)*0.2
dur_max_thres <- max(df_dur_score$obj_est)*5

which(df_dur_score$ME < dur_min_thres)
which(df_dur_score$ME > dur_max_thres)
# none is removed

# second, based on relative duration errors beyond the central 95th percentile
# compute percentile:
dur_lower_percentile <- quantile(df_dur_score$ERM, 0.025)
dur_upper_percentile <- quantile(df_dur_score$ERM, 0.975)

# detect which is lower and upper
which(df_dur_score$ERM < dur_lower_percentile)
which(df_dur_score$ERM > dur_upper_percentile)
df_dur_score_clean <- subset(df_dur_score, ERM >= dur_lower_percentile & ERM <=dur_upper_percentile)
# 5.7% removed participants

# distance______________________________________________________________________
# first, based on objective value
dist_min_thres <- min(df_dist_score$obj_est)*0.2
dist_max_thres <- max(df_dist_score$obj_est)*5

which(df_dist_score$ME < dist_min_thres)
which(df_dist_score$ME > dist_max_thres)
# none is removed

# second, based on relative duration errors beyond the central 95th percentile
# compute percentile:
dist_lower_percentile <- quantile(df_dist_score$ERM, 0.025, na.rm = TRUE)
dist_upper_percentile <- quantile(df_dist_score$ERM, 0.975, na.rm = TRUE)

# detect which is lower and upper
which(df_dist_score$ERM <= dist_lower_percentile)
which(df_dist_score$ERM >= dist_upper_percentile)
df_dist_score_clean <- subset(df_dist_score, ERM >= dist_lower_percentile & ERM <=dist_upper_percentile)
# 6.5% removed participants

# instantaneous speed___________________________________________________________
# first, based on objective value
spdinst_min_thres <- min(df_spdinst_score$obj_est)*0.2
spdinst_max_thres <- max(df_spdinst_score$obj_est)*5

which(df_spdinst_score$ME < spdinst_min_thres)
which(df_spdinst_score$ME > spdinst_max_thres)
# none is removed 

# second, based on relative duration errors beyond the central 95th percentile
# compute percentile:
spdinst_lower_percentile <- quantile(df_spdinst_score$ERM, 0.025, na.rm = TRUE)
spdinst_upper_percentile <- quantile(df_spdinst_score$ERM, 0.975, na.rm = TRUE)

# detect which is lower and upper
which(df_spdinst_score$ERM <= spdinst_lower_percentile)
which(df_spdinst_score$ERM >= spdinst_upper_percentile)
df_spdinst_score_clean <- subset(df_spdinst_score, ERM >= spdinst_lower_percentile & ERM <=spdinst_upper_percentile)
# 6.5% removed participants

# average speed_________________________________________________________________
# first, based on objective value
spdave_min_thres <- min(df_spdave_score$obj_est)*0.2
spdave_max_thres <- max(df_spdave_score$obj_est)*5

which(df_spdave_score$ME < spdave_min_thres)
which(df_spdave_score$ME > spdave_max_thres)
# none is removed

# second, based on relative duration errors beyond the central 95th percentile
# compute percentile:
spdave_lower_percentile <- quantile(df_spdave_score$ERM, 0.025, na.rm = TRUE)
spdave_upper_percentile <- quantile(df_spdave_score$ERM, 0.975, na.rm = TRUE)

# detect which is lower and upper
which(df_spdave_score$ERM <= spdave_lower_percentile)
which(df_spdave_score$ERM >= spdave_upper_percentile)
df_spdave_score_clean <- subset(df_spdave_score, ERM >= spdave_lower_percentile & ERM <=spdave_upper_percentile)
# 6% removed participants
#_______________________________________________________________________________

# check how many participant are removed in total for duration, distance, and average speed (because some are removed for every estimation)
# and select only the shared leftovers (^^') participants
# create a lis of df:
df_list <- list(df_dur_score_clean, df_dist_score_clean, df_spdave_score_clean)
# get list of shared participants:
common_participants <- Reduce(intersect, lapply(df_list, function(df) df$participants))

# select only shared participants:
shared_dfs <- lapply(df_list, function(df) subset(df, participants %in% common_participants))

# concatenate together:
df_score_clean <- do.call(rbind, shared_dfs)


# create a new dataframe with only the score
df_score <- rbind(df_dur_score_clean, df_dist_score_clean, df_spdinst_score_clean, df_spdave_score_clean)
write.csv(df_score, "C:\\Users\\yvann\\OneDrive\\2022_QTEV_Analysis_YN\\dataframe_ready2use_score.csv")

