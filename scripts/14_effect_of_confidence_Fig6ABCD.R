################################################################################
#################### effect of confidence on relative error ####################
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
library(multcomp)
library(nnet)


rm(list=ls())     # to clear current workspace

################################################################################
# load data
################################################################################

setwd("C:\\Users\\yvann\\OneDrive\\2022_QTEV_Analysis_YN\\")
df <- read.csv("dataframe_ready2use_score.csv")

# recode variable
class(df$est_conf)
df$est_conf <- as.character(df$est_conf)
unique(df$est_meta)
df$est_conf[df$est_conf == "1"] <- "not at all confident"
df$est_conf[df$est_conf == "2"] <- "not confident"
df$est_conf[df$est_conf == "3"] <- "rather confident"
df$est_conf[df$est_conf == "4"] <- "very confident"
df$est_conf[is.na(df$est_conf)] <- "missing"
unique(df$est_conf)
# check distribution of responses
table(df$est_conf)
# most of the responses are a-in the middle, and only 4 missing 

df$ep_est[df$ep_est == "acc"] <- "positive"
df$ep_est[df$ep_est == "cst"] <- "null"
df$ep_est[df$ep_est == "dec"] <- "negative"
unique(df$ep_est)

df$ep_est <- as.factor(df$ep_est)
df$orientation <- as.factor(df$orientation)


# remove missing data
df <- df %>% filter(est_conf != "missing")
unique(df$est_conf)

# check factor level
df$est_conf <- as.factor(df$est_conf)
print(levels(df$est_conf))
table(df$est_conf)

df$ERM <- abs(df$ERM)


# ready to go !!
################################################################################
# WH: absolute relative error decreases with confidence, whatever the estimation is to be done
# 
# # create different columns corresponding at the confidence level per estimation type, suggested by Max Chaumon
# # can't do it in fact since, there is one absolute relative error per estimation type.  
# 
# conf_lm <- lm(ERM ~ est_conf, data = df)
# performance::check_model(conf_lm)
# summary(conf_lm)
# report(conf_lm)
# # check stat on coef
# anova(conf_lm) # simple check if the factor is still significant 
# 
# # perform pairwise comparison
# comparaison <- emmeans(conf_lm, ~ est_conf)  
# pairs(comparaison)
# # only not at all confident vs very confident is significative, vs rather confident is very close to being significative
# 
# ggplot(df, aes(x = est_conf, y = ERM)) +
#   geom_boxplot(size = .75) +
#   geom_jitter(alpha = .5) +
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
#   xlab("Confidence report")+
#   ylab("Absolute relative error")
# 
# ################################################################################

################################################################################
# adding the type of estimation to check whether the effect is shared or not

# distributions
table(df$est_conf[df$type == "dur"])
table(df$est_conf[df$type == "dist"])
table(df$est_conf[df$type == "spd_inst"])
table(df$est_conf[df$type == "spd_mean"])

table(df$knwYesNo_est[df$type == "dur"])
table(df$knwYesNo_est[df$type == "dist"])


# check type factor
class(df$type)
df$type <- as.factor(df$type)

# change the level order to use duration as the baseline
new_order_type <- c("dur", "spd_inst", "dist", "spd_mean")
df$type <- factor(df$type, levels = new_order_type)
# display new order
print(levels(df$type))

# build model with interaction
# conf_type_lm <- lm(ERM ~ est_conf * type, data = df)
# performance::check_model(conf_type_lm)
# summary(conf_type_lm)
# # spd_inst is significant, and interaction of conf and dur is significant
# # my interpretation is that duration is mainly driving the effect
# report(conf_type_lm)
# 
# anova(conf_type_lm)
# # main effect of type and est_conf, no interaction
# # compute simpler model with no interaction
# conf_type_lm_simpler <- lm(ERM ~est_conf + type, data = df)
# 
# # perform pairwise comparison, separated to avoid interaction
# comparaison <- emmeans(conf_type_lm_simpler, ~ est_conf)  
# pairs(comparaison)
# 
# comparaison <- emmeans(conf_type_lm_simpler, ~ type)  
# pairs(comparaison)
# # seems like the diff is driven by differences in type and not for the likert

# # plot interaction
# ggplot(df, aes(x = est_conf, y = ERM, fill = type)) +
#   geom_boxplot(size = .75, aes( fill = type)) +
#   # geom_jitter(alpha = .5, aes(color = type)) +
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
#   xlab("Confidence report")+
#   ylab("Absolute relative error")
# 
# # plot separately for each type
# 
# # main effect speed inst ?
# ggplot(df, aes(x = type, y = ERM)) +
#   geom_boxplot(size = .75,) +
#   geom_jitter(alpha = .5) +
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
#   xlab("Estimation type")+
#   ylab("Absolute relative error mean speed")
# 
# # full fig quick
# bp_all_est <- gridExtra::grid.arrange(bp_dur, bp_dist, bp_spdinst, bp_spdave, ncol = 2, nrow = 2)
################################################################################
# I think the best way to look at the results (and to interpret them) to make a model per type of estimation, with the ERM and the train factors as predictors

# looking at the effect of sensory information on dur
df_dur <- df %>% filter(type == "dur")

# plot absolute error duration per level of confid
bp_dur <- ggplot(df %>% filter(type == "dur"), aes(x = est_conf, y = ERM)) +
  geom_boxplot(size = .75, fill = '#66CCEE') +
  geom_jitter(alpha = .5) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), text = element_text(size = 15))+
  xlab("Confidence report")+
  ylab("Absolute relative error duration")
bp_dur

# trying with ordinal linear regression
# dur_conftrain_olm <- polr(est_conf ~ ERM + ep_est*orientation, data = df_dur, Hess=TRUE)
# get p values and display
# (ctable <- coef(summary(dur_conftrain_olm)))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p))
# # no effect of sensory information on duration confidence
# dur_anova_object <- Anova(dur_conftrain_olm, type = "III")
# dur_anova_object 
# # only ERM is significant
# dur_emm <- emmeans(dur_conftrain_olm, ~ ERM ) 
# pairs(dur_emm)

# after the discussion of 23/02/2024, we decided, with VvW, to only investigate the link between confidence level and absolute error estimation, not the train factors
dur_conf_lm <- lm(ERM ~ est_conf, data = df_dur)
summary(dur_conf_lm)

dur_lm_anova_object <- anova(dur_conf_lm)
report(dur_lm_anova_object)

test_dur_emm <- emmeans(dur_conf_lm, ~ est_conf) 
pairs(test_dur_emm)

#_______________________________________________________________________________
# looking at the effect of sensory information on dist
df_dist <- df %>% filter(type == "dist")

# dist ERM ~ conf
bp_dist <- ggplot(df %>% filter(type == "dist"), aes(x = est_conf, y = ERM)) +
  geom_boxplot(size = .75, fill = '#EE6677') +
  geom_jitter(alpha = .5) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), text = element_text(size = 15))+
  xlab("Confidence report")+
  ylab("Absolute relative error distance")
bp_dist

# # trying with ordinal linear regression
# dist_conftrain_olm <- polr(est_conf ~ ERM + ep_est*orientation, data = df_dist, Hess=TRUE)
# # summary(dist_conftrain_olm)
# # get p values and display
# (ctable <- coef(summary(dist_conftrain_olm)))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p))
# # no effect of sensory information on distance confidence ?
# dist_anova_object <- Anova(dist_conftrain_olm, type = "III")
# dist_anova_object 
# no effect of ERM, but effect of ep_est
# dist_emm <- emmeans(dist_conftrain_olm, ~ ep_est ) 
# pairs(dist_emm)
# 
# dist_int_emm <- emmeans(dist_conftrain_olm, ~ ep_est | orientation) 
# pairs(dist_int_emm)
# diff between null and positive, only in backward
# ==> very hard to interpret, not following any prediction

# good way of looking at results, only looking at relationship ERM * confidence
dist_conf_lm <- lm(ERM ~ est_conf, data = df_dist)
summary(dist_conf_lm)

dist_lm_anova_object <- Anova(dist_conf_lm, type = "III")
report(dist_lm_anova_object)

test_dist_emm <- emmeans(dist_conf_lm, ~ est_conf) 
pairs(test_dist_emm)


#_______________________________________________________________________________
# looking at the effect of sensory info on instantaneous speed
df_spd_inst <- df %>% filter(type == "spd_inst")

# instantaneous speed
# bp_spdinst <- ggplot(df %>% filter(type == "spd_inst"), aes(x = est_conf, y = ERM)) +
#   geom_boxplot(size = .75, fill = '#228833') +
#   geom_jitter(alpha = .5) +
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), text = element_text(size = 15))+
#   xlab("Confidence report")+
#   ylab("Absolute relative error inst. speed")
# bp_spdinst
# 
# # trying with ordinal linear regression
# spd_inst_conftrain_olm <- polr(est_conf ~ ERM + ep_est*orientation, data = df_spd_inst, Hess=TRUE)
# # summary(spd_inst_conftrain_olm)
# # get p values and display
# (ctable <- coef(summary(spd_inst_conftrain_olm)))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p))
# # no effect of sensory information on spd_inst confidence
# spdinst_anova_object <- Anova(spd_inst_conftrain_olm, type = "III")
# spdinst_anova_object 
# # Non is significant

# good way of looking at results now
test_lm_spdinst <- lm(ERM ~ est_conf, data = df_spd_inst)
summary(test_lm_spdinst)

spdinst_anovalm_object <- Anova(test_lm_spdinst, type = "III")
report(spdinst_anovalm_object) 

test_spdinst_emm <- emmeans(test_lm_spdinst, ~ est_conf ) 
pairs(test_spdinst_emm)

#_______________________________________________________________________________
# looking at the effect of sensory info on average speed
df_spd_mean <- df %>% filter(type == "spd_mean")

# average speed
bp_spdave <- ggplot(df %>% filter(type == "spd_mean"), aes(x = est_conf, y = ERM)) +
  geom_boxplot(size = .75,fill = '#CCBB44') +
  geom_jitter(alpha = .5) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), text = element_text(size = 15))+
  xlab("Confidence report")+
  ylab("Absolute relative error mean speed")
bp_spdave

# trying with ordinal linear regression
# spd_mean_conftrain_olm <- polr(est_conf ~ ERM + ep_est*orientation, data = df_spd_mean, Hess=TRUE)
# # summary(spd_mean_conftrain_olm)
# # get p values and display
# (ctable <- coef(summary(spd_mean_conftrain_olm)))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p))
# spdave_anova_object <- Anova(spd_mean_conftrain_olm, type = "III")
# spdave_anova_object 
# # only ERM is significant !

# good way of doing result now
test_lm_spdave <- lm(ERM ~ est_conf, data = df_spd_mean)
summary(test_lm_spdave)

spdave_anovalm_object <- Anova(test_lm_spdave, type = "III")
report(spdave_anovalm_object) 

test_spdave_emm <- emmeans(test_lm_spdave, ~ est_conf ) 
pairs(test_spdave_emm)
