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
require(foreign)
require(Hmisc)
require(reshape2)
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
# df_dur_clean$pot[df_dur_clean$pot == "1"] <- "v. slow"
# df_dur_clean$pot[df_dur_clean$pot == "2"] <- "slow"
# df_dur_clean$pot[df_dur_clean$pot == "3"] <- "fast"
# df_dur_clean$pot[df_dur_clean$pot == "4"] <- "v. fast"
# convert into factor
df_dur_clean$pot <- as.factor(df_dur_clean$pot)

# display current order of pot levels
print(levels(df_dur_clean$pot))
# # create a new order
# new_order_pot <- c("v. slow", "slow", "fast", "v. fast")
# # add the new order
# df_dur_clean$pot <- factor(df_dur_clean$pot, levels = new_order_pot)
# # display new order
# print(levels(df_dur_clean$pot))

# prepare emotional factors
df_dur_clean$sadness[is.na(df_dur_clean$sadness)] <- "missing"
df_dur_clean$happiness[is.na(df_dur_clean$happiness)] <- "missing" 
df_dur_clean$boredom[is.na(df_dur_clean$boredom)] <- "missing" 

# remove missing values and prepare one dataframe per ES
df_dur_sad <- df_dur_clean %>% filter(sadness !="missing")
df_dur_hap <- df_dur_clean %>% filter(happiness !="missing")
df_dur_bor <- df_dur_clean %>% filter(boredom !="missing")

# uncomment if you want to analyse all the emotional state in the same model
# df_dur_clean <- df_dur_clean %>% filter(sadness !="missing" & happiness !="missing" & boredom !="missing")

# convert into factor and check levels
df_dur_sad$sadness <- as.factor(df_dur_sad$sadness)
df_dur_hap$happiness <- as.factor(df_dur_hap$happiness)
df_dur_bor$boredom <- as.factor(df_dur_bor$boredom)

# df_dur_clean$sadness <- as.factor(df_dur_clean$sadness)
# df_dur_clean$happiness <- as.factor(df_dur_clean$happiness)
# df_dur_clean$boredom <- as.factor(df_dur_clean$boredom)


levels(df_dur_sad$sadness)
levels(df_dur_hap$happiness)
levels(df_dur_bor$boredom)
#_______________________________________________________________________________

# compute model_________________________________________________________________

# happiness_____________________________________________________________________
ftable(xtabs(~ happiness + pot, data = df_dur_hap)) 

dur_hap_lm <- MASS::polr(pot ~ happiness, data = df_dur_hap, Hess=TRUE)
summary(df_dur_hap)

# look at p_value
(ctable <- coef(summary(dur_hap_lm)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
# only very slow to slow is significant (so uninterpretable)
hap_anova_object <- Anova(dur_hap_lm, type = "III")
hap_anova_object
# not significant

# Création du tableau croisé des fréquences
table_happiness <- table(df_dur_hap$pot, df_dur_hap$happiness)

# Conversion du tableau en dataframe
plot_happiness <- as.data.frame.table(table_happiness)

# Renommage des colonnes
names(plot_happiness) <- c("PoTJ", "happiness", "Frequency")

hap_colors <- c("#eff3ff", "#bdd7e7", "#6baed6", "#2171b5")

# Plot
hap_plot <- ggplot(plot_happiness, aes(x = as.factor(happiness), y = as.factor(PoTJ))) +
  geom_point(aes(size = Frequency, color = as.factor(happiness))) +  # Ajout de l'esthétique de couleur
  geom_text(aes(label = Frequency), vjust = 1.5) +  # Ajout des valeurs numériques
  scale_size_continuous(range = c(1, 40)) +
  scale_color_manual(values = hap_colors) +  # Utilisation des couleurs définies
  labs(y = "PoTJ level (from 'very slow' to 'very fast')", x = "happiness level (from 'not at all' to 'very happy')")+
  theme_classic() + 
  theme(legend.position = "none", text = element_text(size = 15))
hap_plot

#_______________________________________________________________________________

# sadness_______________________________________________________________________
ftable(xtabs(~ sadness + pot, data = df_dur_sad)) 

dur_sad_lm <- MASS::polr(pot ~ sadness, data = df_dur_sad, Hess=TRUE)
summary(dur_sad_lm)

# look at p_value
(ctable <- coef(summary(dur_sad_lm)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
# only very slow to slow is significant (so uninterpretable)
sad_anova_object <- Anova(dur_sad_lm, type = "III")
sad_anova_object
# sadness is significant
# only very slow to slow is significant (so uninterpretable)

# Création du tableau croisé des fréquences
table_sadness <- table(df_dur_sad$pot, df_dur_sad$sadness)

# Conversion du tableau en dataframe
plot_sadness <- as.data.frame.table(table_sadness)

# Renommage des colonnes
names(plot_sadness) <- c("PoTJ", "sadness", "Frequency")

sad_colors <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")

# Plot
sad_plot <- ggplot(plot_sadness, aes(x = as.factor(sadness), y = as.factor(PoTJ))) +
  geom_point(aes(size = Frequency, color = as.factor(sadness))) +  # Ajout de l'esthétique de couleur
  geom_text(aes(label = Frequency), vjust = 1.5) +  # Ajout des valeurs numériques
  scale_size_continuous(range = c(1, 40)) +
  scale_color_manual(values = sad_colors) +  # Utilisation des couleurs définies
  labs(y = "PoTJ level (from 'very slow' to 'very fast')", x = "Sadness level (from 'not at all' to 'very sad')")+
  theme_classic() + 
  theme(legend.position = "none", text = element_text(size = 15))
sad_plot
#_______________________________________________________________________________

# boredom_______________________________________________________________________
ftable(xtabs(~ boredom + pot, data = df_dur_bor)) 

dur_bor_lm <- MASS::polr(pot ~ boredom, data = df_dur_bor, Hess=TRUE)
summary(dur_bor_lm)

# look at p_value
(ctable <- coef(summary(dur_bor_lm)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

bor_anova_object <- Anova(dur_bor_lm, type = "III")
bor_anova_object
# boredom is significant

# post hoc comp
bor_emm <- emmeans(dur_bor_lm, ~ boredom ) 
pairs(bor_emm)
# 

# Création du tableau croisé des fréquences
table_boredom <- table(df_dur_bor$pot, df_dur_bor$boredom)

# Conversion du tableau en dataframe
plot_boredom <- as.data.frame.table(table_boredom)

# Renommage des colonnes
names(plot_boredom) <- c("PoTJ", "Boredom", "Frequency")

bor_colors <- c("#edf8e9", "#bae4b3", "#74c476", "#238b45")

# Plot
bord_plot <- ggplot(plot_boredom, aes(x = as.factor(Boredom), y = as.factor(PoTJ))) +
  geom_point(aes(size = Frequency, color = as.factor(Boredom))) +  # Ajout de l'esthétique de couleur
  geom_text(aes(label = Frequency), vjust = 1.5) +  # Ajout des valeurs numériques
  scale_size_continuous(range = c(1, 40)) +
  scale_color_manual(values = bor_colors) +  # Utilisation des couleurs définies
  labs(y = "PoTJ level (from 'very slow' to 'very fast')", x = "Boredom level (from 'not at all' to 'very much')")+
  theme_classic() + 
  theme(legend.position = "none", text = element_text(size = 15))
bord_plot
#_______________________________________________________________________________

# full model
dur_ES_lm <- MASS::polr(pot ~ happiness + sadness + boredom, data = df_dur_clean, Hess=TRUE)
summary(dur_ES_lm)

(ctable <- coef(summary(dur_ES_lm)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
# ==> only sadness is significant

ES_anova_object <- Anova(dur_ES_lm, type = "III")
ES_anova_object
# boredom is signifiacant

# post hoc comp
bor_ES_emm <- emmeans(dur_ES_lm, ~ boredom ) 
pairs(bor_ES_emm)
# same levels that are different than each other