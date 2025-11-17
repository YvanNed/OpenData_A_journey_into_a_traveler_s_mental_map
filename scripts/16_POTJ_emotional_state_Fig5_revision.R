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
library(psych)
library(rcompanion)
library(lmtest)
library(insight)  # pour format_p()

rm(list=ls())     # to clear current workspace

################################################################################
# load data
################################################################################

setwd("C:\\Users\\yvann\\OneDrive\\2022_QTEV_Analysis_YN")
df <- read.csv("dataframe_ready2use_score.csv")

# Extract dataframe_____________________________________________________________
df_dur <- df %>% filter(type == "dur")

# check if emotional factors are correlated (when they are numerical variables)
cor(df_dur[, c("happiness", "sadness", "boredom")], use = "pairwise.complete.obs")
#            happiness    sadness    boredom
# happiness  1.0000000 -0.3487867 -0.1932729
# sadness   -0.3487867  1.0000000  0.2413893
# boredom   -0.1932729  0.2413893  1.0000000

# convert Likert response (integer) as factors
df_dur$pot <- as.factor(df_dur$pot)
df_dur$happiness <- as.factor(df_dur$happiness)
df_dur$sadness <- as.factor(df_dur$sadness)
df_dur$boredom <- as.factor(df_dur$boredom)

# check if emotional factors are correlated (when they are categorical variables)
psych::polychoric(df_dur[, c("happiness", "sadness", "boredom")])$rho
#            happiness  sadness    boredom
# happiness  1.0000000 -0.5080730 -0.2585275
# sadness   -0.5080730  1.0000000  0.3491411
# boredom   -0.2585275  0.3491411  1.0000000

chisq.test(df_dur$happiness, df_dur$sadness) # p-value = 1.402e-08
chisq.test(df_dur$happiness, df_dur$boredom) # p-value = 0.01912
chisq.test(df_dur$sadness, df_dur$boredom)   # p-value = 5.243e-12
cramerV(df_dur$happiness, df_dur$sadness) # 0.2422 
cramerV(df_dur$happiness, df_dur$boredom) # 0.1458 
cramerV(df_dur$sadness, df_dur$boredom)   # 0.2786

# => globalement, l'ensemble des metriques montrent qu'il y a un lien entre les états émotionnels mais qu'on peut construire un seul model avec tous les facteurs

# if you want to test ordered effect
# df_dur$pot <- ordered(df_dur$pot)
# df_dur$happiness <- ordered(df_dur$happiness)
# df_dur$sadness <- ordered(df_dur$sadness)
# df_dur$boredom <- ordered(df_dur$boredom)
  
# check if na
colSums(is.na(df_dur[, c("pot","happiness","sadness","boredom")]))
# pot happiness   sadness   boredom 
# 5         5         4         1   
#compute a clean df without NA
df_dur_clean <- na.omit(df_dur[, c("pot","happiness","sadness","boredom")])
# 7 participants => 3% of 233 (number of participants with a pot and all emotional states)

# # rename pot variables to better understand what is going on
# not needed anymore because there was no label on our likert scale so it would be confusing for readers
# df_dur_clean$pot[df_dur_clean$pot == "1"] <- "v. slow"
# df_dur_clean$pot[df_dur_clean$pot == "2"] <- "slow"
# df_dur_clean$pot[df_dur_clean$pot == "3"] <- "fast"
# df_dur_clean$pot[df_dur_clean$pot == "4"] <- "v. fast"

# display current order of pot levels
print(levels(df_dur_clean$pot))
# #  if needed, create a new order
# new_order_pot <- c("v. slow", "slow", "fast", "v. fast")
# # add the new order
# df_dur_clean$pot <- factor(df_dur_clean$pot, levels = new_order_pot)
# # display new order
# print(levels(df_dur_clean$pot))

levels(df_dur$sadness)
levels(df_dur$happiness)
levels(df_dur$boredom)


#_______________________________________________________________________________

# compute model_________________________________________________________________
# Il y a des toutes petites différence dans les valeurs du model si j'utilise le df avec le NA ou sans.
# les résultats que je rapporte dans le MS sont ceux du df avec les NA (alors qu'avant je les rétirais explicitement)

# happiness_____________________________________________________________________
ftable(xtabs(~ happiness + pot, data = df_dur)) 

dur_hap_lm <- MASS::polr(pot ~ happiness, data = df_dur, Hess=TRUE)
summary(dur_hap_lm)

# look at p_value
(ctable <- coef(summary(dur_hap_lm)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
# only very slow to slow is significant (so uninterpretable)
hap_anova_object <- Anova(dur_hap_lm, type = "III")
hap_anova_object
# not significant

# Création du tableau croisé des fréquences
table_happiness <- table(df_dur$pot, df_dur$happiness)

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
ftable(xtabs(~ sadness + pot, data = df_dur)) 

dur_sad_lm <- MASS::polr(pot ~ sadness, data = df_dur, Hess=TRUE)
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

# post hoc comp
sad_emm <- emmeans(dur_sad_lm, ~ sadness) 
pairs(sad_emm)

# Création du tableau croisé des fréquences
table_sadness <- table(df_dur$pot, df_dur$sadness)

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
ftable(xtabs(~ boredom + pot, data = df_dur)) 

dur_bor_lm <- MASS::polr(pot ~ boredom, data = df_dur, Hess=TRUE)
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
table_boredom <- table(df_dur$pot, df_dur$boredom)

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

# full model for revision
dur_ES_lm <- MASS::polr(pot ~ happiness + sadness + boredom, data = df_dur, Hess=TRUE)
summary(dur_ES_lm)

# check model
performance::check_model(dur_ES_lm)
vif_tbl <- performance::check_collinearity(dur_ES_lm)
vif_tbl
# VIF are super high so: coeffiecients are unstable, inflation of standard errors, interpretation very coomplicated
# The VIFs were not high with the lm() 
# we can compute the polychoric correlation to check the correlation between our emotional factor (independently from our model)
# Because the emotional state ratings were collected on ordinal Likert scales, we used polychoric correlations to estimate the associations between the latent continuous variables underlying each emotional dimension (happiness, sadness, boredom). This approach provides a more accurate measure of association than Pearson's correlation for ordinal data.
psych::polychoric(df_dur[, c("happiness", "sadness", "boredom")])$rho
# Emotional state ratings were moderately correlated (|r| ??? 0.5), indicating that while partially related, they capture distinct affective dimensions.

(ctable <- coef(summary(dur_ES_lm)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
# ==> only sadness is significant

ES_anova_object <- Anova(dur_ES_lm, type = "III")
ES_anova_object
report(ES_anova_object)
# boredom is significant

# post hoc comp
bor_ES_emm <- emmeans(dur_ES_lm, ~ boredom ) 
pairs(bor_ES_emm)
# same levels that are different than each other