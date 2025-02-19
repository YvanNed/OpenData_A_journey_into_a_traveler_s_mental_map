################################################################################
################# Compare dedidacted model versus full model ###################
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


rm(list=ls())     # to clear current workspace

################################################################################
# load data
################################################################################

setwd("C:\\Users\\yvann\\OneDrive\\2022_QTEV_Analysis_YN")
df <- read.csv("dataframe_ready2use_score.csv")

# clarify variables_____________________________________________________________
df$sex <- as.factor(df$sex)
df$laterality <- as.factor(df$laterality)
df$sadness <- as.factor(df$sadness)
df$happiness <- as.factor(df$happiness)
df$boredom <- as.factor(df$boredom)
df$est_strat <- as.factor(df$est_strat)
df$est_conf <- as.factor(df$est_conf)

df$ep_est[df$ep_est == "acc"] <- "positive"
df$ep_est[df$ep_est == "cst"] <- "null"
df$ep_est[df$ep_est == "dec"] <- "negative"
unique(df$ep_est)

df$ep_est <- as.factor(df$ep_est)
df$orientation <- as.factor(df$orientation)

# display current order of ep_est
print(levels(df$ep_est))
# create a new order
new_order_ep <- c("positive", "null", "negative")
# add the new order
df$ep_est <- factor(df$ep_est, levels = new_order_ep)
#_______________________________________________________________________________

# for duration__________________________________________________________________
# there was no significant effect for relative duration whatsoever (except for absolute error and confidence)
# so we expect to find no significant factor as well here




