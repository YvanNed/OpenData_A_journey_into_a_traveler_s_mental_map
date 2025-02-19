################################################################################
########### Remove participants from other travel & prepare dataframe ##########
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

rm(list=ls())     # to clear current workspace

################################################################################
################################# sub fonction #################################
################################################################################

cook.influential_index <- function(x, y, my_title="")
  # this function compute the cook distance of each data point  and return the idx of the column of outliers 
{
  mod <- lm(x ~ y)
  cooksd <- cooks.distance(mod)
  sample_size <- length(y)
  plot(cooksd, pch=23, bg= "orange", cex=2, main=my_title)
  thres = 4
  abline(h = 4 / sample_size, lwd=2)
  # text(x = 1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd > 4 / sample_size, names(cooksd), ""))
  text(x = 1:length(cooksd)+1, y=cooksd, labels=names(cooksd))
  influential <- as.numeric(names(cooksd)[(cooksd > 4/sample_size)])
  print(paste( "Threshold:", 4/sample_size))
  return(influential)
}

add_outliers <- function(df, out_list, knw, comment = ""){
  
  for (o in 1:length(out_list)){
    if (knw){
      print(paste("Knowledge: ", df$knw_est[df$participants == out_list[o]], " ; participant: ", df$participants[df$participants == out_list[o]], sep="") )
    }else{
      print(paste("Participant: ", df$participants[df$participants == out_list[o]], " ; distance (est-obj): ", df$est[df$participants == out_list[o]] - df$obj_est[df$participants == out_list[o]], " ; relative estimation: ", df$RE_est[df$participants == out_list[o]], sep = "" ))
    }
    if (is.na(df$outliers[df$participants == out_list[o]])){
      df$outliers[df$participants == out_list[o]] <- comment
    }else{
      df$outliers[df$participants == out_list[o]] <- paste(df$outliers[df$participants == out_list[o]], paste("_", comment,sep=""), sep = "")
    }
  }
  return(df)
}

intersection <- function(l1, l2){
  x <- (l2[1] - l1[1]) / (l1[2] - l2[2])
  y <- l1[1] + l1[2] * x
  return(xy=c(x, y))
}

################################################################################

# load data
setwd("D:\\2022_QTEV_Analysis_YN\\")

df <- read.csv("dataframe_corrected.csv")

################################################################################
#___________ remove bad participants and make few minor adjustement ____________

# remove participant 196 and 277 because they don't have data, and remove participants 26 and 109 because they answered to only 1 question, remove participant 16 because it doesn't have any objective value
df <- subset(df, participants != 196 & participants != 277 & participants != 26 & participants != 109 & participants != 16)

# remove participant that are below 18 but preserve ones with NA
df <- df %>% filter(age >= 18 | is.na(age))

# remove participant getting on/offboard at Le Creusot
df <- df %>% filter(creusot == 0)

# remove participant that responded during acc/dec after/before Le creusot
df <- df %>% filter(accdec_creusot == "no")

# change the negative value of participant 119 into 0 (the guy put a cross behind Lyon..)
which(df$est < 0) # this returns only one value corresponding to the index of participants with a negative estimation
df$est[which(df$est < 0)] <- 0
df$RE_est[which(df$RE_est < 0)] <- 0

# change the na in the strategy to number 6 (corresponding to no Strategy)
df$est_strat[is.na(df$est_strat)] <- 6

# check the number of particpants
df %>% group_by(type, format) %>% count(type)

# putting variable into factor to avoid doing it for each plot
col_notfactor <- c("participants", "age", "knw_est", "est", "RE_est", "ER_est", "obj_est", "rep_est", "away_est", "outliers", "theo_dur")
col_asfactor <- colnames(df)[! colnames(df) %in% col_notfactor]

df[col_asfactor] <- lapply(df[col_asfactor], factor)

# remove unnecessary column 
df <- df %>% select(-one_of(c("accdec_creusot", "travel", "creusot", "rep_est", "away_est")))

# Annotate participants that might come from another travel detected on the knowledge they provided and whether they knew it or not (Yes/No)
out_knw_manual <- list(107, 139, 205, 2, 37, 54, 103, 108, 158, 176, 181, 203, 228, 230, 243, 275)

df <- add_outliers(df = df, out_list=out_knw_manual, knw = TRUE, comment = "knwdur") # the printing is degueulasse

# descriptive ?
df %>% filter(is.na(outliers)) %>% group_by(type, format) %>% count(type)
# there is 247 participants after removing participants coming from another travel

# remove the participants from the other travel
df_clean <- df %>% filter(is.na(outliers))
df_clean %>% group_by(type, format) %>% count(type)

# save the dataframe with the correct info
write.csv(df_clean, "D:\\2022_QTEV_Analysis_YN\\dataframe_ready2use.csv", row.names = FALSE)










