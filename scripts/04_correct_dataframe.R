################################################################################
########################## Correct the dataframe ###############################
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

setwd("D:\\2022_QTEV_Analysis_YN\\")

df_final <- read.csv("survey_byfactors.csv")

#_______________________few minor adjustment of the df__________________________
# rename the column direction into orientation
df_final <- dplyr::rename(df_final, orientation = direction) # i'm using dplyr:: before calling rename because I want specifically the function from dplyr and it seems that other packages loaded have also a rename function so I was having an error

# rename format 
df_final$format[df_final$format == "graphic"] <- "graphic (knw)"
df_final$format[df_final$format == "total"] <- "graphic (obj)"

# add a column for whether participants are in acc or dec before/after le creusot (identify with the identify speed episode script)
df_final$accdec_creusot <- NA
df_final$accdec_creusot[df_final$participants == 184 | df_final$participants == 185 | df_final$participants == 186 | df_final$participants == 68 ] <- "yes"
df_final$accdec_creusot[is.na(df_final$accdec_creusot)] <- "no"

# one participant had a wrong sex 
df_final$sex[df_final$sex == 3] = 'Woman'

# add a column for outliers
df_final$outliers <- NA

# add a column with the theoretical duration of the travel (can be found here https://www.sncf-connect.com/train/horaires/paris/lyon)
df_final$theo_dur <- NA
# travel 6609 = 1h56 (116min)
df_final$theo_dur[grepl("6609", df_final$travel, fixed = TRUE)] <- 116
# travel 6611 = 1h56 (116min)
df_final$theo_dur[grepl("6611", df_final$travel, fixed = TRUE)] <- 116
# travel 6613 = 2h04 (124min)
df_final$theo_dur[grepl("6613", df_final$travel, fixed = TRUE)] <- 124
# travel 6617 = 2h04 (124min)
df_final$theo_dur[grepl("6617", df_final$travel, fixed = TRUE)] <- 124
# travel 6624 = 2h04 (116min)
df_final$theo_dur[grepl("6624", df_final$travel, fixed = TRUE)] <- 124
# travel 6626 = 1h56 (116min)
df_final$theo_dur[grepl("6626", df_final$travel, fixed = TRUE)] <- 116

write.csv(df_final, "D:\\2022_QTEV_Analysis_YN\\dataframe_corrected.csv", row.names = FALSE)
