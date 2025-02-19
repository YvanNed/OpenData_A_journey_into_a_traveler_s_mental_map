################################################################################
############################### Link DJ/ POTJ ##################################
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
library(ordinal)

rm(list=ls())     # to clear current workspace

################################################################################
# load data
################################################################################

setwd("C:\\Users\\yvann\\OneDrive\\2022_QTEV_Analysis_YN")
df <- read.csv("dataframe_ready2use_score.csv")

# Extract dataframe_____________________________________________________________
df_dur <- df %>% filter(type == "dur")

df_dur$pot[is.na(df_dur$pot)] <- "missing"

# count number of participant per categories
table(df_dur$pot)
# 5 participants are missing

# remove participants that did not provided pot
df_dur <- df_dur %>% filter(pot != "missing")

class(df_dur$pot)

# turn pot as a factor
df_dur$pot <- as.factor(df_dur$pot)
class(df_dur$pot)

# checl levels and change if needed
levels(df_dur$pot)
# rigth now 1 is the first level, so the baseline,  will check if I should change it or not

# test if i recode level and use 2 as a baseline
# new_order_pot <- c("2", "1", "3", "4")
# df_dur$pot <- factor(df_dur$pot, levels = new_order_pot)
# 
# print(levels(df_dur$pot))
# this has no effect on the model!
#_______________________________________________________________________________

# compute model_________________________________________________________________
# lm because the predicted variable is continuous
dur_potj_lm <- lm(RME ~ 1 + pot, data = df_dur)
performance::check_model(dur_potj_lm)
summary(dur_potj_lm)
report(dur_potj_lm)
anova(dur_potj_lm)
# not significant

# do the model on log
df_dur$log_RME <- log10(df_dur$RME)

logdur_potj_lm <- lm(log_RME ~ 1 + pot, data = df_dur)
performance::check_model(logdur_potj_lm)
summary(logdur_potj_lm)
# residuals are indeed more normal
report(logdur_potj_lm)
anova(logdur_potj_lm)
# not significative

# # using ordinal regression. I put pot as the predicted variable in order to use   
# dur_potj_olm <- polr(pot ~ RME, data = df_dur, Hess=TRUE)
# summary(dur_potj_olm)
# # get p values and display
# (ctable <- coef(summary(dur_potj_olm)))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p))
# # no effect of RME !

#_______________________________________________________________________________

# plot boxplot__________________________________________________________________
bp_potj_dur <- ggplot(df_dur, aes(x = pot, y = RME, fill = pot)) +
  geom_boxplot() +
  theme_classic() + 
  xlab("POTJ level ('very slow' to 'very fast')") +
  ylab("Relative estimated duration") +
  geom_hline(aes(yintercept = 1), linetype = "solid", linewidth = 1, color = "grey") +
  theme(text = element_text(size = 15)) +
  annotate("text", x = 1, y = 2.7, label = "n:", col = "black", size = 5)+
  annotate("text", x = 1, y = 2.5, label = "2", col = "black", size = 5)+
  annotate("text", x = 2, y = 2.5, label = "59", col = "black", size = 5)+
  annotate("text", x = 3, y = 2.5, label = "128", col = "black", size = 5)+
  annotate("text", x = 4, y = 2.5, label = "39", col = "black", size = 5)+
  scale_fill_manual(values = c("1" = "#feebe2", "2" = "#fbb4b9", "3" = "#f768a1", "4" = "#ae017e"))+
  scale_alpha_identity()+  # Permet de conserver les valeurs alpha que vous avez spécifiées
  guides(fill = "none")

bp_potj_dur
#_______________________________________________________________________________

# quick test with distribution__________________________________________________
density_potj_dur = ggplot() +
  geom_density(data = df_dur, mapping = aes(x = RME, fill = pot), alpha = 0.6)+
  
  # to plot hist + density
  #geom_histogram(aes(y = ..density..), alpha = 0.2, position = "identity") +
  # geom_density(adjust = 1, alpha = 0.2) +
  xlab("Relative estimated duration") +
  ylab("Density") +
  # ylim(0.0, 1.6) +
  # xlim(0, 3) +
  theme_classic() +
  theme(legend.position = c(0.8,0.6), text = element_text(size = 20))+
  scale_fill_manual(values = c("1" = "#feebe2", "2" = "#fbb4b9", "3" = "#f768a1", "4" = "#ae017e"))+
  guides(fill = "none")+
  scale_x_continuous(limits = c(0, 3), expand = c(0,0))
  

density_potj_dur 
#_______________________________________________________________________________

# full fig______________________________________________________________________
gridExtra::grid.arrange(bp_potj_dur, density_potj_dur, nrow = 1, ncol = 2)
#_______________________________________________________________________________

# Analysis on the magnitude estimation with pot_________________________________

# in Droit-Volet 2018 plos one paper, she looks at the link between duration estimation and potj, not relative duration
# she claims that duration are judged shorter for faster pot because pot has a negative beta
# however, seems it is not relative estimation, we don't know if duration are judged shorter, simply that pot is faster for duration that are shorter (not really respective to whether participants are underestimating or overestimating)
# To me, it seems more close to using objective duration that looking at subjective duration estimation
# I want to look at what it looks like in my data 

# trying first with magnitude estimation
# compute model_________________________________________________________________
dur_ME_potj_lm <- lm(ME ~ pot, data = df_dur)
performance::check_model(dur_ME_potj_lm)
summary(dur_ME_potj_lm)
report(dur_ME_potj_lm)
anova(dur_ME_potj_lm)
# not significant

# olm
# dur_ME_potj_olm <- polr(pot ~ ME, data = df_dur)
# # get p values and display
# (ctable <- coef(summary(dur_ME_potj_olm)))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p))
# # no effect of ME

# plot boxplot__________________________________________________________________
bp_ME_potj_dur <- ggplot(df_dur, aes(x = pot, y = ME, fill = pot)) +
  geom_boxplot() +
  theme_classic() + 
  xlab("POTJ level ('very slow' to 'very fast')") +
  ylab("Estimated duration") +
  theme(text = element_text(size = 20)) +
  # annotate("text", x = 1.5, y = 2.7, label = "number of observation:", col = "black", size = 5)+
  # annotate("text", x = 1, y = 2.5, label = "2", col = "black", size = 5)+
  # annotate("text", x = 2, y = 2.5, label = "59", col = "black", size = 5)+
  # annotate("text", x = 3, y = 2.5, label = "128", col = "black", size = 5)+
  # annotate("text", x = 4, y = 2.5, label = "39", col = "black", size = 5)+
  scale_fill_manual(values = c("1" = "#feebe2", "2" = "#fbb4b9", "3" = "#f768a1", "4" = "#ae017e"))+
  scale_alpha_identity()+  # Permet de conserver les valeurs alpha que vous avez spécifiées
  guides(fill = "none")

bp_ME_potj_dur
#_______________________________________________________________________________

# trying with objective values
dur_obj_potj_lm <- lm(obj_est ~ pot, data = df_dur)
performance::check_model(dur_obj_potj_lm)
summary(dur_obj_potj_lm)
report(dur_obj_potj_lm)
# not significant

# olm
# dur_obj_potj_olm <- polr(pot ~ obj_est, data = df_dur)
# # get p values and display
# (ctable <- coef(summary(dur_obj_potj_olm)))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p))
# # not significant

# plot boxplot__________________________________________________________________
bp_obj_potj_dur <- ggplot(df_dur, aes(x = pot, y = obj_est, fill = pot)) +
  geom_boxplot() +
  theme_classic() + 
  xlab("POTJ level ('very slow' to 'very fast')") +
  ylab("Objective duration") +
  theme(text = element_text(size = 20)) +
  # annotate("text", x = 1.5, y = 2.7, label = "number of observation:", col = "black", size = 5)+
  # annotate("text", x = 1, y = 2.5, label = "2", col = "black", size = 5)+
  # annotate("text", x = 2, y = 2.5, label = "59", col = "black", size = 5)+
  # annotate("text", x = 3, y = 2.5, label = "128", col = "black", size = 5)+
  # annotate("text", x = 4, y = 2.5, label = "39", col = "black", size = 5)+
  scale_fill_manual(values = c("1" = "#feebe2", "2" = "#fbb4b9", "3" = "#f768a1", "4" = "#ae017e"))+
  scale_alpha_identity()+  # Permet de conserver les valeurs alpha que vous avez spécifiées
  guides(fill = "none")

bp_obj_potj_dur




