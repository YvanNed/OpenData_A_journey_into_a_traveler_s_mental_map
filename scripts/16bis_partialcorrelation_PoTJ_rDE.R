# script to compute partial correlation when testing for RDE ~ emotional state

library(dplyr)
library(ppcor)
library(MBESS)
library(broom)

rm(list=ls())     # to clear current workspace

################################################################################
# load data and prep dataframe
################################################################################
setwd("C:\\Users\\yvann\\OneDrive\\2022_QTEV_Analysis_YN")
df <- read.csv("dataframe_ready2use_score.csv")

df_dur <- df %>% filter(type == "dur")

# df_dur_clean
# check if na
colSums(is.na(df_dur[, c("pot","happiness","sadness","boredom")]))
# pot happiness   sadness   boredom 
# 5         5         4         1   
#compute a clean df without NA
df_dur_clean <- na.omit(df_dur[, c("pot","happiness","sadness","boredom")])
# 7 participants => 3% of 233 (number of participants with a pot and all emotional states)

# turn to numeric for partial correlation
# ---- 0) Copie de travail, s'assurer du format numérique des émotions
df_dur <- df_dur %>%
  mutate(
    sadness    = as.numeric(as.character(sadness)),
    happiness  = as.numeric(as.character(happiness)),
    boredom    = as.numeric(as.character(boredom))
  )

df_dur_clean <- df_dur_clean %>%
  mutate(
    sadness    = as.numeric(as.character(sadness)),
    happiness  = as.numeric(as.character(happiness)),
    boredom    = as.numeric(as.character(boredom))
  )

################################################################################

################################################################################
################### define functions and usable (utilitaire) ####################
################################################################################
# IC95% via Fisher z (poignées aux bords)
ci_r_fisher <- function(r, n, conf.level = 0.95) {
  r <- ifelse(abs(r) >= 0.999999, sign(r)*0.999999, r)
  z  <- atanh(r)
  se <- 1/sqrt(n - 3)
  zc <- qnorm(1 - (1 - conf.level)/2)
  c(lo = tanh(z - zc*se), hi = tanh(z + zc*se))
}

run_pcor <- function(df, y, x, controls, method = c("pearson","spearman")){
  method <- match.arg(method)
  cols <- unique(c(y, x, controls))
  
  # 1) vérifier présence des variables
  missing <- setdiff(cols, names(df))
  if (length(missing)) stop("Colonnes manquantes dans df: ", paste(missing, collapse=", "))
  
  # 2) sous-ensemble base R (évite dplyr::select/all_of)
  dat <- df[, cols, drop = FALSE]
  
  # 3) convertir factors/ordered en numériques (Likert) si possible
  for (col in cols) {
    if (is.factor(dat[[col]]) || is.ordered(dat[[col]])) {
      suppressWarnings({
        num_try <- as.numeric(as.character(dat[[col]]))
      })
      if (all(!is.na(num_try))) {
        dat[[col]] <- num_try
      } else {
        # fallback: codes des niveaux (1,2,3,…) si non numériques
        dat[[col]] <- as.numeric(dat[[col]])
      }
    }
  }
  
  # 4) enlever NA listwise
  dat <- stats::na.omit(dat)
  n <- nrow(dat)
  if (n < 4) stop("Trop peu d'observations après NA-omit (n<4).")
  
  # 5) partial correlation
  pc <- ppcor::pcor.test(dat[[x]], dat[[y]], dat[, controls, drop = FALSE], method = method)
  ci <- ci_r_fisher(pc$estimate, n)
  df_true <- n - length(controls) - 2
  
  tibble(
    outcome   = y,
    predictor = x,
    controls  = paste(controls, collapse = " + "),
    method    = method,
    n         = n,
    r_partial = unname(pc$estimate),
    t         = unname(pc$statistic),
    df        = df_true,
    p_value   = unname(pc$p.value),
    ci_low    = ci["lo"],
    ci_high   = ci["hi"]
  )
}
################################################################################

################################################################################
################################# run analysis #################################
################################################################################
# ---- 3) Lancer les 3 partial correlations (chaque émotion contrôlant les 2 autres)
controls_list <- list(
  sadness   = c("happiness","boredom"),
  happiness = c("sadness","boredom"),
  boredom   = c("sadness","happiness")
)

# Pearson
res_p_pearson <- bind_rows(
  run_pcor(df_dur, y="pot", x="sadness",   controls=controls_list$sadness,   method="pearson"),
  run_pcor(df_dur, y="pot", x="happiness", controls=controls_list$happiness, method="pearson"),
  run_pcor(df_dur, y="pot", x="boredom",   controls=controls_list$boredom,   method="pearson")
)

# Spearman (robuste à l’ordinal/monotone)
res_p_spearman <- bind_rows(
  run_pcor(df_dur, y="pot", x="sadness",   controls=controls_list$sadness,   method="spearman"),
  run_pcor(df_dur, y="pot", x="happiness", controls=controls_list$happiness, method="spearman"),
  run_pcor(df_dur, y="pot", x="boredom",   controls=controls_list$boredom,   method="spearman")
)

# ---- 4) Tableaux finaux à reporter
res_p_pearson
res_p_spearman

# (Optionnel) joindre en un seul tableau pour l’annexe
res_all <- bind_rows(res_p_pearson, res_p_spearman)
res_all

#############################################################################
################################################################################
################################################################################
# other solution proposed by Chatty
# to be tested
library(dplyr)
library(car)
library(effectsize)
library(broom)

# ---- 0) Copie de travail, s'assurer que ce sont bien des facteurs ordonnés si c'est ton choix analytique
df_f <- df_dur_clean %>%
  mutate(
    sadness   = if (!is.ordered(sadness)) ordered(sadness) else sadness,
    happiness = if (!is.ordered(happiness)) ordered(happiness) else happiness,
    boredom   = if (!is.ordered(boredom)) ordered(boredom) else boredom
  ) %>% na.omit()

# ---- 1) Modèle avec tous les états
mod_full <- lm(RME ~ sadness + happiness + boredom, data = df_f)

# ---- 2) ANOVA type II (ou III si tu as des interactions ou si tu utilises des contrastes particuliers)
anova_type <- car::Anova(mod_full, type = 2)  # table des sommes des carrés par facteur
anova_type

# ---- 3) η² partiel par prédicteur + conversion en r partiel
eta_tab <- effectsize::eta_squared(anova_type, partial = TRUE, ci = 0.95)
eta_tab

# Conversion en r (positif par défaut car basé sur variance expliquée).
# Si tu veux un signe, tu peux le définir à partir du contraste principal (p.ex. slope linéaire pour un facteur ordonné).
eta_to_r <- function(eta2p) sqrt(pmax(eta2p, 0))

res_eta_r <- eta_tab %>%
  filter(!is.na(Effect), Effect != "Residuals") %>%
  transmute(
    predictor = Effect,
    eta2_partial = Eta2_partial,
    eta2_ci_low  = CI_low,
    eta2_ci_high = CI_high,
    r_partial    = eta_to_r(Eta2_partial)
  )

res_eta_r

# ---- 4) Semi-partial r (sr) via ΔR² entre modèles plein et réduits (on retire un facteur à la fois)
r2_full <- broom::glance(mod_full)$r.squared

drop_and_sr <- function(drop_term){
  form_reduced <- reformulate(
    termlabels = setdiff(c("sadness","happiness","boredom"), drop_term),
    response = "RME"
  )
  mod_red <- lm(form_reduced, data = df_f)
  r2_red  <- broom::glance(mod_red)$r.squared
  sr      <- sqrt(max(r2_full - r2_red, 0))
  tibble(
    predictor = drop_term,
    r2_full   = r2_full,
    r2_reduced= r2_red,
    sr        = sr
  )
}

res_sr <- bind_rows(
  drop_and_sr("sadness"),
  drop_and_sr("happiness"),
  drop_and_sr("boredom")
)

res_sr

