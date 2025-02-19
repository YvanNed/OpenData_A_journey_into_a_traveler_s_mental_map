################################################################################
################# Metacognitive process and confidence rating ##################
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
library(MASS)
library(emmeans)
library(multcomp)
library(nnet)


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

df$ep_est <- as.factor(df$ep_est)
df$orientation <- as.factor(df$orientation)

df_dur <- df %>% filter(type == "dur")

# recode variable
class(df_dur$est_meta)
unique(df_dur$est_meta)
table(df_dur$est_meta)
# since there is very few participant that used the extrem of the likert, I am recomputing them in only two categories
df_dur$est_meta[df_dur$est_meta == "1"] <- "underestimated"
df_dur$est_meta[df_dur$est_meta == "2"] <- "underestimated"
df_dur$est_meta[df_dur$est_meta == "3"] <- "overestimated"
df_dur$est_meta[df_dur$est_meta == "4"] <- "overestimated"
df_dur$est_meta[is.na(df_dur$est_meta)] <- "missing"
unique(df_dur$est_meta)

# remove missing data
df_dur <- df_dur %>% filter(est_meta != "missing")
unique(df_dur$est_meta)

# change to factor
df_dur$est_meta <- as.factor(df_dur$est_meta)
class(df_dur$est_meta)

# turn into numerical factor for binomial regression
# df_dur$est_meta <- as.numeric(df_dur$est_meta)
# class(df_dur$est_meta)

# # display current order of metacog
print(levels(df_dur$est_meta))
# create a new order
# not necessary anymore with only 2 categories
# new_order_meta <- c("underestimated","overestimated")

new_order_meta <- c("underestimated", "rather underestimated", "rather overestimated","overestimated")
# add the new order
df_dur$est_meta <- factor(df_dur$est_meta, levels = new_order_meta)
# display new order
print(levels(df_dur$est_meta))

# check if data is well sampled across metacognitive level
table(df_dur$est_meta)
# there is only few participants that reported clear underestimation/overestimation, but good balance between under/overestimated overall

lapply(df_dur[, c("orientation", "ep_est", "est_meta")], table)

ftable(xtabs(~ orientation + est_meta , data = df_dur))

ftable(xtabs(~ ep_est + est_meta , data = df_dur))

ftable(xtabs(~ est_meta + orientation + ep_est, data = df_dur))

ftable(xtabs(~ orientation + ep_est + est_meta, data = df_dur))

# ready to go !
################################################################################
# WH: relative estimation are lower than one for participant that reported they underestimated, and above one for participant that reported overestimated 

meta_lm <- lm(RME ~ 1 + est_meta, data = df_dur)
performance::check_model(meta_lm)
summary(meta_lm)
report(meta_lm)
anova(meta_lm)
report(anova(meta_lm))
# main effect of est_meta
# pairwise comparison
comparaison <- emmeans(meta_lm, ~ est_meta)  
pairs(comparaison)

# with Log
df_dur$log_RME <- log10(df_dur$RME)

logmeta_lm <- lm(log_RME ~ est_meta, data = df_dur)
performance::check_model(logmeta_lm)
summary(logmeta_lm)
report(logmeta_lm)
anova(logmeta_lm)
# main effect of est_meta
# pairwise comparison
comparaison <- emmeans(meta_lm, ~ est_meta)  
pairs(comparaison)
# There is stil an effect of meta_cog 

# trying binomial regression

# Ajuster le modèle de régression logistique
modele_logistique <- glm(est_meta ~ RME, data = df_dur, family = "binomial")

# Résumé du modèle
summary(modele_logistique)
report(modele_logistique)
# same results as OLS linear regression

# fig6E if est_meta is two category
# boxplot of metacog
ggplot(df_dur, aes(x = est_meta, y = RME)) +
  geom_boxplot(size = .75,) +
  geom_jitter(alpha = .5) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), text = element_text(size = 15))+
  xlab("Metacognition direction report")+
  ylab("Relative estimated duration")+
  geom_hline(aes(yintercept = 1),linetype = "solid", size = 1, color = "grey")
################################################################################

# Effect of acceleration and orientation
# WH for orientation: overestimation in forward vs backward
# WH for acceleration: either overestimation for null vs pos and neg or Pos and neg > null

# first looking at one factor by factor
# dur_ori_olm <- polr(est_meta ~ orientation, data = df_dur, Hess=TRUE)
# summary(dur_ori_olm)
# # get p values and display
# (ctable <- coef(summary(dur_ori_olm)))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p))
# # orientation is not significant
# 
# dur_acc_olm <- polr(est_meta ~ ep_est, data = df_dur, Hess=TRUE)
# summary(dur_acc_olm)
# # get p values and display
# (ctable <- coef(summary(dur_acc_olm)))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p))
# # acceleration is not significant

# trying when they are added in the same model
dur_oriacc_olm <- polr(est_meta ~ orientation * ep_est, data = df_dur, Hess=TRUE)
summary(dur_oriacc_olm)

# get p values and display
(ctable <- coef(summary(dur_oriacc_olm)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

anova_object <- Anova(dur_oriacc_olm, type = "III")
anova_object# interaction orientation and ep_est is significant

emm <- emmeans(dur_oriacc_olm, ~ orientation | ep_est) # can change the order to make more sense if needed
posthoc <- pairs(emm)
# if orientation | ep_est ==> Backward - Forward is significative with positive acceleration
# if ep_est | orientation ==> negative - positive acceleration is significative in Forward
#  ==> my understanding is that the diff B - F is positive so meta_cog is higher in backward than in forward only in acc, so participants tend to report that they overestimate in backward compared to forward (only in acceleration)
#  ==> or that the diff negative - positive is positive so meta_cog is higher in negative than positive only for forward, so participant tend to report that they overestimate in deceleration compared to acceleration (only in forward)
# it makes more sense to look at negative - positive since it was the WH.
# The effect of acceleration is significative only in the Forward, this goes in the direction of WH, since forward examplify the effect of acceleration (to the point it becomes significant)
# However, the effect of acceleration is that participant are overestimating in deceleration compared to acceleration, which does not correspond to any prediction we have made.
# Sadly, this doesn't even reflect the actual patern of the effect of acceleration on RME since there was no effect.
# However, if you really want to tire it par les cheveux, it follows the pattern of the median of the duration RME. 
# what does it mean ? if I want to do "le mec" then I can say that metacog direction judgement follows the median of individuals, so maybe more interesting to look at the median thatn the mean, but I don't belive it at all

