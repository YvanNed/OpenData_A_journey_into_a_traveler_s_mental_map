################################################################################
######### test for an affect of emotional state on duration estimation #########
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

# Extract dataframe_____________________________________________________________
df_dur <- df %>% filter(type == "dur")

df_dur$sadness[is.na(df_dur$sadness)] <- "missing" 
# quick test when we remove sadness = "4", bc I suspect that it is driving the significativity when factors are ordered
# c'est bien lui qui drive la significativité de sadness
df_dur <- df_dur %>% filter(sadness != "4")

df_dur$happiness[is.na(df_dur$happiness)] <- "missing" 

df_dur$boredom[is.na(df_dur$boredom)] <- "missing" 

df_dur$pot[is.na(df_dur$pot)] <- "missing"


table(df_dur$sadness)
table(df_dur$happiness)
table(df_dur$boredom)
table(df_dur$pot)

# remove missing values and make one df per emotional level, and recode factor level if needed
# sadness
df_dur_sad <- df_dur %>% filter(sadness != "missing")
df_dur_sad$sadness <- as.factor(df_dur_sad$sadness)
df_dur_sad$sadness <- ordered(df_dur_sad$sadness, levels = c("1","2","3","4"))
table(df_dur_sad$sadness)
levels(df_dur_sad$sadness)

# happiness
df_dur_hap <- df_dur %>% filter(happiness != "missing")
df_dur_hap$happiness <- as.factor(df_dur_hap$happiness)
df_dur_hap$happiness <- ordered(df_dur_hap$happiness, levels = c("1","2","3","4"))
table(df_dur_hap$happiness)
levels(df_dur_hap$happiness)

# boredom
df_dur_bor <- df_dur %>% filter(boredom != "missing")
df_dur_bor$boredom <- as.factor(df_dur_bor$boredom)
df_dur_bor$boredom <- ordered(df_dur_bor$boredom, levels = c("1","2","3","4"))
table(df_dur_bor$boredom)
levels(df_dur_bor$boredom)

# pot
class(df_dur$pot)
# turn pot as a factor
df_dur$pot <- as.factor(df_dur$pot)
df_dur$pot <- ordered(df_dur$pot, levels = c("1","2","3","4"))
class(df_dur$pot)
# check levels and change if needed
levels(df_dur$pot)

# df_dur_clean
df_dur_clean <- df_dur %>% filter(sadness != "missing" & happiness != "missing" & boredom != "missing" & pot != "missing")
df_dur_clean$sadness <- as.factor(df_dur_clean$sadness)
df_dur_clean$happiness <- as.factor(df_dur_clean$happiness)
df_dur_clean$boredom <- as.factor(df_dur_clean$boredom)
df_dur_clean$pot <- as.factor(df_dur_clean$pot)

df_dur_clean$sadness <- ordered(df_dur_clean$sadness, levels = c("1","2","3","4"))
df_dur_clean$happiness <- ordered(df_dur_clean$happiness, levels = c("1","2","3","4"))
df_dur_clean$boredom <- ordered(df_dur_clean$boredom, levels = c("1","2","3","4"))
df_dur_clean$pot <- ordered(df_dur_clean$pot, levels = c("1","2","3","4"))


# df_dur without removing NA
df_dur$sadness <- as.factor(df_dur$sadness)
df_dur$happiness <- as.factor(df_dur$happiness)
df_dur$boredom <- as.factor(df_dur$boredom)
df_dur$pot <- as.factor(df_dur$pot)

df_dur$sadness <- ordered(df_dur$sadness, levels = c("1","2","3","4"))
df_dur$happiness <- ordered(df_dur$happiness, levels = c("1","2","3","4"))
df_dur$boredom <- ordered(df_dur$boredom, levels = c("1","2","3","4"))
df_dur$pot <- ordered(df_dur$pot, levels = c("1","2","3","4"))

#_______________________________________________________________________________

# checking for Relative magnitude duration______________________________________
# sadness_______________________________________________________________________
dur_sadness_lm <- lm(RME ~ 1 + sadness, data = df_dur_sad)
performance::check_model(dur_sadness_lm)
summary(dur_sadness_lm)
report(dur_sadness_lm)
# model is not significant, level 4 is, but there is only one participant
anova(dur_sadness_lm)
# anova on coef is not significant

# model with log
# sadness_______________________________________________________________________
logdur_sadness_lm <- lm(log_RME ~ 1 + sadness, data = df_dur_sad)
performance::check_model(logdur_sadness_lm)
summary(logdur_sadness_lm)
report(logdur_sadness_lm)
# model is not significant, level 4 is, but there is only one participant
anova(logdur_sadness_lm)
# not significant

# using ordinal regression
# dur_sadness_olm <- polr(sadness ~ RME, data = df_dur_sad, Hess=TRUE)
# summary(dur_sadness_olm)
# # get p values and display
# (ctable <- coef(summary(dur_sadness_olm)))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p))
# # no effect of RME
#_______________________________________________________________________________

# happiness_____________________________________________________________________
dur_happiness_lm <- lm(RME ~ 1 + happiness, data = df_dur_hap)
performance::check_model(dur_happiness_lm)
summary(dur_happiness_lm)
# not significant
anova(dur_happiness_lm)
# anova on coef is not significant

# with log
logdur_happiness_lm <- lm(log_RME ~ 1 + happiness, data = df_dur_hap)
performance::check_model(logdur_happiness_lm)
summary(logdur_happiness_lm)
# not significant
anova(logdur_happiness_lm)
# anova on coef is not significant

# using ordinal regression
# dur_happiness_olm <- polr(happiness ~ RME, data = df_dur_hap, Hess=TRUE)
# summary(dur_happiness_olm)
# # get p values and display
# (ctable <- coef(summary(dur_happiness_olm)))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p))
# # no effect of RME
#_______________________________________________________________________________

# boredom_______________________________________________________________________
dur_boredom_lm <- lm(RME ~ 1 + boredom, data = df_dur_bor)
performance::check_model(dur_boredom_lm)
summary(dur_boredom_lm)
report(dur_boredom_lm)
# model not significant, boredom 2 is
anova(dur_boredom_lm)
# anova on coef is not significant

# with log
logdur_boredom_lm <- lm(log_RME ~ 1 + boredom, data = df_dur_bor)
performance::check_model(logdur_boredom_lm)
summary(logdur_boredom_lm)
report(logdur_boredom_lm)
# model not significant, boredom 2 is
anova(logdur_boredom_lm)
# anova on coef is not significant

# using ordinal regression
# dur_boredom_olm <- polr(boredom ~ RME, data = df_dur_bor, Hess=TRUE)
# summary(dur_boredom_olm)
# # get p values and display
# (ctable <- coef(summary(dur_boredom_olm)))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p))
# # RME close to being significative

#_______________________________________________________________________________

dur_emo_lm <- lm(RME ~ 1 + sadness+happiness+boredom, data = df_dur_clean)
performance::check_model(dur_emo_lm)
summary(dur_emo_lm)
report(dur_emo_lm)
# not significant
anova(dur_emo_lm)
car::Anova(dur_emo_lm, type = 2)
report(anova(dur_emo_lm))
# when all the emotional states are added, the factor bordom is significant
# compute contrast
comparaison_boredom <- emmeans(dur_emo_lm, ~ boredom)  
pairs(comparaison_boredom)
contrast(comparaison_boredom, "poly")

comparaison_sadness <- emmeans(dur_emo_lm, ~ sadness)  
pairs(comparaison_sadness)
contrast(comparaison_sadness, "poly")
# only the boredom level 2 is different than the level 1
# is that a true effect ? How to interpret it
# it could be a threshold effect, meaning that reporting being bored (against not being bored), is sufficient to impact relative duration estimation

# check des valeurs numériques des Variance Influence Factors (VIFs) pcq a dit max   
vif_tbl <- performance::check_collinearity(dur_emo_lm)
vif_tbl


#recompute a simpler model with only bordom
dur_bor_lm <- lm(RME ~ 1 + boredom, data = df_dur_clean)
summary(dur_bor_lm)
anova(dur_bor_lm)
# still significant
# compute contrast
comparaison_boredom <- emmeans(dur_bor_lm, ~ boredom)  
pairs(comparaison_boredom)
# only the boredom level 2 is different than the level 1
# is that a true effect ? How to interpret it
# it could be a threshold effect, meaning that reporting being bored (against not being bored), is sufficient to impact relative duration estimation
# 

# with log
logdur_emo_lm <- lm(log_RME ~ 1 + sadness+happiness+boredom, data = df_dur_clean)
performance::check_model(logdur_emo_lm)
summary(logdur_emo_lm)
report(logdur_emo_lm)
# not significant
anova(logdur_emo_lm)
# when all the emotional states are added, the factor bordom is significant
# compute contrast
comparaison_logdur_emo_lm <- emmeans(logdur_emo_lm, ~ boredom)  
pairs(comparaison_logdur_emo_lm)

#recompute a simpler model with only bordome
log_dur_bor_lm <- lm(log_RME ~ 1 + boredom, data = df_dur_clean)
summary(log_dur_bor_lm)
anova(log_dur_bor_lm)
# still significant
# compute contrast
comparaison_logboredom <- emmeans(log_dur_bor_lm, ~ boredom)  
pairs(comparaison_logboredom)
# only the boredom level 1 is different than the level 2
# is that a true effect ? nope according to max

# test for a large model with random factors
df_dur_clean$orientation <- as.factor(df_dur_clean$orientation)
df_dur_clean$ep_est <- as.factor(df_dur_clean$ep_est)

dur_emo_lm <- lmer(RME ~ 1 + sadness+happiness+boredom+pot + (1|orientation) + (1|ep_est), data = df_dur_clean)
performance::check_model(dur_emo_lm)
summary(dur_emo_lm)
report(dur_emo_lm)
# not significant
anova(dur_emo_lm)
# when all the emotional states are added, the factor bordom is significant
# compute contrast
comparaison_boredom <- emmeans(dur_emo_lm, ~ boredom)  
pairs(comparaison_boredom)

# test for the model without removing NA
dur_emo_lm <- lm(RME ~ 1 + sadness+happiness+boredom, data = df_dur)
performance::check_model(dur_emo_lm)
summary(dur_emo_lm)
report(dur_emo_lm)
# not significant
anova(dur_emo_lm)
report(anova(dur_emo_lm))
# when all the emotional states are added, the factor bordom is significant
# compute contrast
comparaison_boredom <- emmeans(dur_emo_lm, ~ boredom)  
pairs(comparaison_boredom)

#recompute a simpler model with only bordome
dur_bor_lm <- lm(RME ~ 1 + boredom, data = df_dur)
summary(dur_bor_lm)
# level 2 is significant, but the model is not
anova(dur_bor_lm)
# boredom is not significant => exact same result as with df without NA
# there is no point of doing the rest but doing it still..
#compute contrast
comparaison_boredom <- emmeans(dur_bor_lm, ~ boredom)  
pairs(comparaison_boredom)
# boredom level 2 vs level 1 close to be significant (0.057)

# check for monotonic effect of boredom (1 vs 2,3,4)____________________________
df_dur$bor_bin <- ifelse(df_dur$boredom == "1", "none", "some+")
df_dur$bor_bin <- as.factor(df_dur$bor_bin)
levels(df_dur$bor_bin)

dur_boredome_mono_lm <- lm(RME ~ 1 + bor_bin, data = df_dur)
summary(dur_boredome_mono_lm)
# level 2 is significant, but the model is not
anova(dur_boredome_mono_lm)

# test with the full model => not significant
dur_emo_bormono_lm <- lm(RME ~ 1 + sadness+happiness+bor_bin, data = df_dur)
performance::check_model(dur_emo_bormono_lm)
summary(dur_emo_bormono_lm)
report(dur_emo_bormono_lm)
# not significant
anova(dur_emo_bormono_lm)
report(anova(dur_emo_bormono_lm))


#_______________________________________________________________________________

# # checking for magnitude duration_______________________________________________
# durme_sadness_lm <- lm(ME ~ 1 + sadness, data = df_dur_sad)
# performance::check_model(durme_sadness_lm)
# summary(durme_sadness_lm)
# report(durme_sadness_lm)
# # not significant
# 
# durme_happiness_lm <- lm(ME ~ 1 + happiness, data = df_dur_hap)
# performance::check_model(durme_happiness_lm)
# summary(durme_happiness_lm)
# report(durme_happiness_lm)
# # not significant
# 
# durme_boredom_lm <- lm(ME ~ 1 + boredom, data = df_dur_bor)
# performance::check_model(durme_boredom_lm)
# summary(durme_boredom_lm)
# report(durme_boredom_lm)
# # not significant
# 
# #_______________________________________________________________________________
# 
# # checking for relative error  duration_________________________________________
# durerm_sadness_lm <- lm(ERM ~ 1 + sadness, data = df_dur_sad)
# performance::check_model(durerm_sadness_lm)
# summary(durerm_sadness_lm)
# report(durerm_sadness_lm)
# # not significant
# 
# durerm_happiness_lm <- lm(ERM ~ 1 + happiness, data = df_dur_hap)
# performance::check_model(durerm_happiness_lm)
# summary(durerm_happiness_lm)
# report(durerm_happiness_lm)
# # not significant
# 
# durerm_boredom_lm <- lm(ERM ~ 1 + boredom, data = df_dur_bor)
# performance::check_model(durerm_boredom_lm)
# summary(durerm_boredom_lm)
# report(durerm_boredom_lm)
# # not significant
# 
#_______________________________________________________________________________

# plotting boxplot of emotional rating on RME___________________________________

bp_sadness_dur <- ggplot(df_dur_sad, aes(x = sadness, y = RME, fill = sadness)) +
  geom_boxplot() +
  theme_classic() + 
  xlab("sadness level (from 'not sad at all' to 'very sad')") +
  ylab("Relative estimated duration") +
  geom_hline(aes(yintercept = 1), linetype = "solid", size = 1, color = "grey") +
  theme(legend.position = c(0.8,0.6), text = element_text(size = 15))+
  annotate("text", x = 1, y = 2.25, label = "n:", col = "black", size = 5)+
  annotate("text", x = 1, y = 2.15, label = "173", col = "black", size = 5)+
  annotate("text", x = 2, y = 2.15, label = "40", col = "black", size = 5)+
  annotate("text", x = 3, y = 2.15, label = "15", col = "black", size = 5)+
  annotate("text", x = 4, y = 2.15, label = "1", col = "black", size = 5)+
  scale_fill_manual(values = c("1" = "#f2f0f7", "2" = "#cbc9e2", "3" = "#9e9ac8", "4" = "#6a51a3", "missing" = "#DBDBDB"))+

  # labs(fill = "Acceleration episode") +
  # guides(fill = "none", alpha = "none") +  # Masquer les légendes pour la couleur et l'alpha
  scale_alpha_identity()+  # Permet de conserver les valeurs alpha que vous avez spécifiées
  guides(fill = "none", color = "none")
bp_sadness_dur

bp_happiness_dur <- ggplot(df_dur_hap, aes(x = happiness, y = RME, fill = happiness)) +
  geom_boxplot() +
  theme_classic() + 
  xlab("happiness level (from 'not happy at all' to 'very happy')") +
  ylab("Relative estimated duration") +
  geom_hline(aes(yintercept = 1), linetype = "solid", size = 1, color = "grey") +
  theme(legend.position = c(0.8,0.6), text = element_text(size = 15))+
  annotate("text", x = 1, y = 2.25, label = "n:", col = "black", size = 5)+
  annotate("text", x = 1, y = 2.15, label = "10", col = "black", size = 5)+
  annotate("text", x = 2, y = 2.15, label = "56", col = "black", size = 5)+
  annotate("text", x = 3, y = 2.15, label = "134", col = "black", size = 5)+
  annotate("text", x = 4, y = 2.15, label = "28", col = "black", size = 5)+
  # labs(fill = "Acceleration episode") +
  # guides(fill = "none", alpha = "none") +  # Masquer les légendes pour la couleur et l'alpha
  scale_fill_manual(values = c("1" = "#eff3ff", "2" = "#bdd7e7", "3" = "#6baed6", "4" = "#2171b5", "missing" = "#DBDBDB"))+
  scale_alpha_identity()+  # Permet de conserver les valeurs alpha que vous avez spécifiées
  guides(fill = "none", color = "none")
bp_happiness_dur


bp_boredom_dur <- ggplot(df_dur_bor, aes(x = boredom, y = RME, fill = boredom)) +
  geom_boxplot() +
  theme_classic() + 
  xlab("boredom level (from 'not at all' to 'very much')") +
  ylab("Relative estimated duration") +
  geom_hline(aes(yintercept = 1), linetype = "solid", size = 1, color = "grey") +
  theme(legend.position = c(0.8,0.6), text = element_text(size = 15))+
  annotate("text", x = 1, y = 2.25, label = "n:", col = "black", size = 5)+
  annotate("text", x = 1, y = 2.15, label = "127", col = "black", size = 5)+
  annotate("text", x = 2, y = 2.15, label = "59", col = "black", size = 5)+
  annotate("text", x = 3, y = 2.15, label = "42", col = "black", size = 5)+
  annotate("text", x = 4, y = 2.15, label = "4", col = "black", size = 5)+
  # labs(fill = "Acceleration episode") +
  # guides(fill = "none", alpha = "none") +  # Masquer les légendes pour la couleur et l'alpha
  scale_fill_manual(values = c("1" = "#edf8e9", "2" = "#bae4b3", "3" = "#74c476", "4" = "#238b45", "missing" = "#DBDBDB"))+
  scale_alpha_identity()+  # Permet de conserver les valeurs alpha que vous avez spécifiées
  guides(fill = "none", color = "none")
bp_boredom_dur

# check monotonic effect of boredom
bp_boredom_dur <- ggplot(df_dur, aes(x = bor_bin, y = RME, fill = bor_bin)) +
  geom_boxplot() +
  theme_classic() + 
  xlab("boredom level (from 'not at all' to 'very much')") +
  ylab("Relative estimated duration") +
  geom_hline(aes(yintercept = 1), linetype = "solid", linewidth = 1, color = "grey") +
  theme(legend.position = c(0.8,0.6), text = element_text(size = 15))+
  #annotate("text", x = 1, y = 2.25, label = "n:", col = "black", size = 5)+
  #annotate("text", x = 1, y = 2.15, label = "127", col = "black", size = 5)+
  #annotate("text", x = 2, y = 2.15, label = "59", col = "black", size = 5)+
  #annotate("text", x = 3, y = 2.15, label = "42", col = "black", size = 5)+
  #annotate("text", x = 4, y = 2.15, label = "4", col = "black", size = 5)+
  # labs(fill = "Acceleration episode") +
  # guides(fill = "none", alpha = "none") +  # Masquer les légendes pour la couleur et l'alpha
  scale_fill_manual(values = c("none" = "#edf8e9", "some+" = "#238b45"))+
  scale_alpha_identity()+  # Permet de conserver les valeurs alpha que vous avez spécifiées
  guides(fill = "none", color = "none")
bp_boredom_dur

#_______________________________________________________________________________

# quick test with distribution__________________________________________________
# density_sadness_dur = ggplot() +
#   geom_density(data = df_dur, mapping = aes(x = RME, fill = sadness), alpha = 0.6)+
#   
#   # to plot hist + density
#   #geom_histogram(aes(y = ..density..), alpha = 0.2, position = "identity") +
#   # geom_density(adjust = 1, alpha = 0.2) +
#   xlab("Relative estimated duration") +
#   ylab("Density") +
#   # ylim(0.0, 1.6) +
#   # xlim(0, 3) +
#   theme_classic() +
#   theme(legend.position = c(0.8,0.6), text = element_text(size = 15))+
#   scale_fill_manual(values = c("1" = "#f2f0f7", "2" = "#cbc9e2", "3" = "#9e9ac8", "4" = "#6a51a3", "missing" = "#DBDBDB"))+
#   guides(fill = "none")
# 
# density_sadness_dur 
# 
# 
# density_happiness_dur = ggplot() +
#   geom_density(data = df_dur, mapping = aes(x = RME, fill = happiness), alpha = 0.6)+
#   
#   # to plot hist + density
#   #geom_histogram(aes(y = ..density..), alpha = 0.2, position = "identity") +
#   # geom_density(adjust = 1, alpha = 0.2) +
#   xlab("Relative estimated duration") +
#   ylab("Density") +
#   # ylim(0.0, 1.6) +
#   # xlim(0, 3) +
#   theme_classic() +
#   theme(legend.position = c(0.8,0.6), text = element_text(size = 15))+
#   scale_fill_manual(values = c("1" = "#eff3ff", "2" = "#bdd7e7", "3" = "#6baed6", "4" = "#2171b5", "missing" = "#DBDBDB"))+
#   guides(fill = "none")
# 
# density_happiness_dur 
# 
# 
# density_boredom_dur = ggplot() +
#   geom_density(data = df_dur, mapping = aes(x = RME, fill = boredom), alpha = 0.6)+
#   
#   # to plot hist + density
#   #geom_histogram(aes(y = ..density..), alpha = 0.2, position = "identity") +
#   # geom_density(adjust = 1, alpha = 0.2) +
#   xlab("Relative estimated duration") +
#   ylab("Density") +
#   # ylim(0.0, 1.6) +
#   # xlim(0, 3) +
#   theme_classic() +
#   theme(legend.position = c(0.8,0.6), text = element_text(size = 15))+
#   scale_fill_manual(values = c("1" = "#edf8e9", "2" = "#bae4b3", "3" = "#74c476", "4" = "#238b45", "missing" = "#DBDBDB"))+
#   guides(fill = "none")
# 
# density_boredom_dur 
#_______________________________________________________________________________
# make full fig
gridExtra::grid.arrange(bp_sadness_dur, bp_happiness_dur, bp_boredom_dur, nrow = 1, ncol = 3)
gridExtra::grid.arrange(density_sadness_dur, density_happiness_dur, density_boredom_dur, nrow = 1, ncol = 3)
