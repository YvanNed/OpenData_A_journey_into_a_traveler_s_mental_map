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

rm(list=ls())     # to clear current workspace

################################################################################
# load data & configure factors
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

# keeping the 4-point likert scale to investigate the effect of ep_est and orientation with an ordinal regression
df_dur$est_meta[df_dur$est_meta == "1"] <- "underestimated"
df_dur$est_meta[df_dur$est_meta == "2"] <- "rather underestimated"
df_dur$est_meta[df_dur$est_meta == "3"] <- "rather overestimated"
df_dur$est_meta[df_dur$est_meta == "4"] <- "overestimated"
df_dur$est_meta[is.na(df_dur$est_meta)] <- "missing"
unique(df_dur$est_meta)

# remove missing data
df_dur <- df_dur %>% filter(est_meta != "missing")
unique(df_dur$est_meta)

# change to factor
df_dur$est_meta <- as.factor(df_dur$est_meta)
class(df_dur$est_meta)

# check if data is well sampled across metacognitive level
table(df_dur$est_meta)
# there is only few participants that reported clear underestimation/overestimation, but good balance between under/overestimated overall

# ready to go !
################################################################################

################################################################################
# Effect of acceleration and orientation on metacognitive direction report ?
################################################################################
# WH for orientation: overestimation in forward vs backward
# WH for acceleration: either overestimation for null vs pos and neg or Pos and neg > null
# WH for interaction: effect of acceleratin is greater in forawer (because congruent) than backward

dur_oriacc_olm <- polr(est_meta ~ orientation * ep_est, data = df_dur, Hess=TRUE)
summary(dur_oriacc_olm)
# get p values and display
(ctable <- coef(summary(dur_oriacc_olm)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

Anova(dur_oriacc_olm, type = "III")
# Only the interaction orientation and acceleration ("ep_est") is significant

# compute post-hoc comparison
# not working for now
contrast_matrix <- matrix(c(
  0, 0, 0, 0, 0,   # Coefficients principaux
  0, 0, 0, 0, 0,   # Coefficients principaux
  0, 0, 0, 0, 0,   # Coefficients principaux
  0, 0, 0, 0, 0,   # Coefficients principaux
  0, 0, 0, 0, 0,   # Coefficients principaux
  0, 0, 0, 0, 0,   # Interaction orientation:ep_est (les deux premières colonnes sont pour les niveaux de orientation, les deux suivantes pour les niveaux de ep_est, et la dernière est l'interaction proprement dite)
  0, 0, 0, 0, 0    # Interaction orientation:ep_est
), ncol = 5, byrow = TRUE)  # Ajuster le nombre de colonnes en fonction du nombre total de coefficients dans votre modèle

posthoc_results <- glht(dur_oriacc_olm,  contrast_matrix)

# Afficher les résultats
summary(posthoc_results)

