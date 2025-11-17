################################################################################
###### RDE ~ emotional states with a single model fro review and sup. mat. #####
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
library(ppcor)
library(dplyr)
library(tibble)
library(purrr)
library(stringr)
library(MBESS)
library(broom)
library(broom.helpers)
library(effectsize)

rm(list=ls())     # to clear current workspace

################################################################################
# load data
################################################################################

setwd("C:\\Users\\yvann\\OneDrive\\2022_QTEV_Analysis_YN")
df <- read.csv("dataframe_ready2use_score.csv")

df$log_RME <- log10(df$RME) 

# prepare dataframe_____________________________________________________________
df_dur <- df %>% filter(type == "dur")

# make sure likert scale are factors
# check column type
class(df_dur$sadness)
class(df_dur$happiness)
class(df_dur$boredom)
class(df_dur$pot)

# check if na
colSums(is.na(df_dur[, c("RME","happiness","sadness","boredom", "pot")]))
# RME happiness   sadness   boredom       pot 
# 0         5         4         1         5 
#compute a clean df without NA
df_clean <- na.omit(df_dur[, c("RME","happiness","sadness","boredom")])
# 7 participants => 3% of 233 (number of participants with a dur RME)

# df_dur without removing NA
df_dur$sadness <- as.factor(df_dur$sadness)
df_dur$happiness <- as.factor(df_dur$happiness)
df_dur$boredom <- as.factor(df_dur$boredom)
df_dur$pot <- as.factor(df_dur$pot)

# to test for linear, quadratic or cubic relationship, uncomment:
df_dur$sadness <- ordered(df_dur$sadness, levels = c("1","2","3","4"))
df_dur$happiness <- ordered(df_dur$happiness, levels = c("1","2","3","4"))
df_dur$boredom <- ordered(df_dur$boredom, levels = c("1","2","3","4"))
df_dur$pot <- ordered(df_dur$pot, levels = c("1","2","3","4"))

# remove the single datapoint in sadness level 4 = participants 127
df_test <- df_dur %>% filter(participants !=127)

# check le contenu des facteurs
table(df_dur$sadness)
table(df_dur$happiness)
table(df_dur$boredom)
table(df_dur$pot)

#_______________________________________________________________________________

# compute single model__________________________________________________________
dur_emo_lm <- lm(RME ~ 1 + sadness+happiness+boredom, data = df_test)
performance::check_model(dur_emo_lm)
summary(dur_emo_lm)
report(dur_emo_lm)
# the model is significant (p=0.044) and explain a weak proportion of variance (r²=0.04)
anova(dur_emo_lm)
car::Anova(dur_emo_lm, type = 2)
report(anova(dur_emo_lm))
# the factor boredom is significant
# compute contrast
comparaison_boredom <- emmeans(dur_emo_lm, ~ boredom)  
pairs(comparaison_boredom)
# the 2nd level of boredom is significant (compare to level 1)
contrast(comparaison_boredom, "poly")
# when testing for linear, quadratic or cubic effect, boredom is not significant

# check des valeurs numériques des Variance Influence Factors (VIFs) pcq a dit max   
vif_tbl <- performance::check_collinearity(dur_emo_lm)
vif_tbl
# all VIF are below 2, which is acceptable

################################################################################
################################################################################
# Quick check for a full full model
class(df_dur$obj_est)
class(df_dur$orientation)
class(df_dur$ep_est)
class(df_dur$pot)

df_dur$orientation <- as.factor(df_dur$orientation)
df_dur$orientation <- factor(df_dur$orientation, levels = c("Forward", "Backward"))

df_dur$ep_est <- as.factor(df_dur$ep_est)

dur_full_lm <- lm(RME ~ 1 + obj_est+ep_est+orientation+sadness+happiness+boredom+pot, data = df_dur)

performance::check_model(dur_full_lm)
summary(dur_full_lm)
report(dur_full_lm)
# The model explains a statistically significant and moderate proportion of variance (R2 = 0.16, F(16, 204) = 2.37, p = 0.003, adj. R2 = 0.09)
anova(dur_full_lm)
car::Anova(dur_full_lm, type = 2)
report(anova(dur_full_lm))

# compute contrast
comp_boredom <- emmeans(dur_full_lm, ~ boredom)  
pairs(comp_boredom)
# the 2nd level of boredom is significant (compare to level 1)
contrast(comp_boredom, "poly")


################################################################################
# test for diff
df_dist <- df %>% filter(type == "dist")

dist_full_lm <- lm(RME ~ 1 + obj_est+ep_est+orientation+sadness+happiness+boredom+pot, data = df_dist)

performance::check_model(dist_full_lm)
summary(dist_full_lm)
report(dist_full_lm)
# The model explains a statistically significant and substantial proportion of variance (R2 = 0.45, F(8, 211) = 21.47, p < .001, adj. R2 = 0.43).
anova(dist_full_lm)
car::Anova(dist_full_lm, type = 2)
report(anova(dist_full_lm))

################################################################################
# Testing the ultimate full
df$type <- as.factor(df$type)
df$orientation <- as.factor(df$orientation)
df$ep_est <- as.factor(df$ep_est)
df$sadness <- as.factor(df$sadness)
df$happiness <- as.factor(df$happiness)
df$boredom <- as.factor(df$boredom)
df$pot <- as.factor(df$pot)

full_lm <- lm(RME ~ 1 + type+obj_est+ep_est+orientation+sadness+happiness+boredom+pot, data = df)

performance::check_model(full_lm)
summary(full_lm)
report(full_lm)
# The model explains a statistically significant and substantial proportion of variance (R2 = 0.38, F(19, 864) = 28.43, p < .001, adj. R2 = 0.37).
anova(full_lm)
car::Anova(full_lm, type = 2)
report(anova(full_lm))