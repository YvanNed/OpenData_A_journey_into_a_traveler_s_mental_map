################################################################################
####################### effect of emotional state on POTJ ######################
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
library(foreign)
library(Hmisc)
library(reshape2)
library(vcd)
library(vcdExtra)

rm(list=ls())     # to clear current workspace

################################################################################
# load data
################################################################################

setwd("C:\\Users\\yvann\\OneDrive\\2022_QTEV_Analysis_YN")
df <- read.csv("dataframe_ready2use_score.csv")

# Extract dataframe_____________________________________________________________
df_dur <- df %>% filter(type == "dur")
df_dur$pot[is.na(df_dur$pot)] <- "missing" 
# remove missing pot
df_dur_clean <- df_dur %>% filter(pot !="missing")
# # rename pot variables to better understand what is going on
# not needed anymore because there was no label on our likert scale
df_dur_clean$pot[df_dur_clean$pot == "1"] <- "very slow"
df_dur_clean$pot[df_dur_clean$pot == "2"] <- "slow"
df_dur_clean$pot[df_dur_clean$pot == "3"] <- "fast"
df_dur_clean$pot[df_dur_clean$pot == "4"] <- "very fast"
# convert into factor
df_dur_clean$pot <- as.factor(df_dur_clean$pot)

# display current order of pot levels
print(levels(df_dur_clean$pot))
# create a new order
new_order_pot <- c("very slow", "slow", "fast", "very fast")
# add the new order
df_dur_clean$pot <- factor(df_dur_clean$pot, levels = new_order_pot)
# display new order
print(levels(df_dur_clean$pot))

# recode acceleration
df_dur_clean$ep_est[df_dur_clean$ep_est == "acc"] <- "positive"
df_dur_clean$ep_est[df_dur_clean$ep_est == "cst"] <- "null"
df_dur_clean$ep_est[df_dur_clean$ep_est == "dec"] <- "negative"
unique(df_dur_clean$ep_est)

# check factors and orders
class(df_dur_clean$ep_est) # not coded as a factor for now..
class(df_dur_clean$orientation)# not coded as a factor for now..

df_dur_clean$ep_est <- as.factor(df_dur_clean$ep_est)
print(levels(df_dur_clean$ep_est)) # order is not correct
# change order
new_order_ep_est <- c("positive", "null", "negative")
df_dur_clean$ep_est <- factor(df_dur_clean$ep_est, levels = new_order_ep_est)
print(levels(df_dur_clean$ep_est)) # order is not correct


df_dur_clean$orientation <- as.factor(df_dur_clean$orientation)
print(levels(df_dur_clean$orientation)) # order is quite perfect, I pref to have forward vs backward
new_order_ori <- c("Forward", "Backward")
df_dur_clean$orientation <- factor(df_dur_clean$orientation, levels = new_order_ori)

# check if na
is.na(df_dur_clean$ep_est)
is.na(df_dur_clean$orientation)

# check if removing very slow make objective duration significative
# df_dur_clean <- df_dur_clean %>% filter(pot != "very slow")
# it is not

# ==> everything is fine now !
#_______________________________________________________________________________

# PoTJ as function of objective measurement_____________________________________
# count number of PoTJ responses
table(df_dur_clean$pot)
# there is imbalance in the number of responses.
# ==> Should I group the data per slow vs fast ?

# PoTJ ~ obj value of duration
potj_obj_lm <- MASS::polr(pot ~ obj_est, data = df_dur_clean, Hess=TRUE)

## view a summary of the model
summary(potj_obj_lm)

# look a p-value
(ctable <- coef(summary(potj_obj_lm)))
p_obj <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p_obj))

pot_anova_object <- Anova(potj_obj_lm, type = "III")
pot_anova_object
# objective estimation is not significant

# look at the boxplot
ggplot(df_dur_clean, aes(x = obj_est, y = pot)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  xlab("Objective duration (min)")+
  ylab("Passage of time judgement")
#_______________________________________________________________________________

# PoTJ as function of train factors_____________________________________________

# descriptive stats
table(df_dur_clean$pot) # there is more responses for 3 than for the others
# acceleration episodes
ftable(xtabs(~ ep_est + pot, data = df_dur_clean)) # there is not a specific episode associated to a specific rating
# orientation
ftable(xtabs(~ orientation + pot, data = df_dur_clean)) # there is not a specific orientation associated to a specific rating


potj_train_lm <- MASS::polr(pot ~ ep_est * orientation, data = df_dur_clean, Hess=TRUE)
summary(potj_train_lm)

# look at p_value
(ctable <- coef(summary(potj_train_lm)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

pottrain_anova_object <- Anova(potj_train_lm, type = "III")
pottrain_anova_object
# acceleration and orientation is not significant

# try to plot___________________________________________________________________
# acceleration episode
# Création du tableau croisé des fréquences
table_ep <- table(df_dur_clean$pot, df_dur_clean$ep_est)

# Conversion du tableau en dataframe
plot_ep <- as.data.frame.table(table_ep)

# Renommage des colonnes
names(plot_ep) <- c("PoTJ", "episode", "Frequency")

ep_colors <- c("#bdd7e7", "#6baed6", "#2171b5")

ep_plot <- ggplot(plot_ep, aes(x = as.factor(episode), y = as.factor(PoTJ))) +
  geom_point(aes(size = Frequency, color = as.factor(episode))) +  # Ajout de l'esthétique de couleur
  geom_text(aes(label = Frequency), vjust = 1.5) +  # Ajout des valeurs numériques
  scale_size_continuous(range = c(1, 40)) +
  scale_color_manual(values = ep_colors) +  # Utilisation des couleurs définies
  labs(y = "PoTJ level (from 'very slow' to 'very fast')", x = "episode (acc, cst, dec)")+
  theme_classic() + 
  theme(legend.position = "none", text = element_text(size = 15))
ep_plot



# orientation
# Création du tableau croisé des fréquences
table_ori <- table(df_dur_clean$pot, df_dur_clean$orientation)

# Conversion du tableau en dataframe
plot_ori <- as.data.frame.table(table_ori)

# Renommage des colonnes
names(plot_ori) <- c("PoTJ", "orientation", "Frequency")

ori_colors <- c("#cbc9e2", "#6a51a3")

# Plot
ori_plot <- ggplot(plot_ori, aes(x = as.factor(orientation), y = as.factor(PoTJ))) +
  geom_point(aes(size = Frequency, color = as.factor(orientation))) +  # Ajout de l'esthétique de couleur
  geom_text(aes(label = Frequency), vjust = 1.5) +  # Ajout des valeurs numériques
  scale_size_continuous(range = c(1, 40)) +
  scale_color_manual(values = ori_colors) +  # Utilisation des couleurs définies
  labs(y = "PoTJ level (from 'very slow' to 'very fast')", x = "orientation")+
  theme_classic() + 
  theme(legend.position = "none", text = element_text(size = 15))
ori_plot


