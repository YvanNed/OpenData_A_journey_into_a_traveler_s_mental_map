##################################################################################################################
# this script allows to open the csv files containing raphaël train data and reformat them (so that it will fit the rest of my code)
##################################################################################################################

library(RColorBrewer)
library("ggplot2")
library(readxl)
library(tidyverse)
library(plyr); library(dplyr)
library(tidyr)
library(DBI)
library("ggplot2")
library(ggpubr)
library("qqplotr")
library("DescTools")
library("PMCMRplus")
library(ggsignif)
library("readr")
library("chron")

rm(list=ls())

str_times_to_dec <- function(times, format = "%H:%M:%S", units = "hours") {
  # convert vector of string times in the specified format to decimal units
  # default output is decimal hours
  time_diff <- difftime(
    strptime(times, format = format),
    strptime("00:00:00", format = format),
    units = units
  )
  times <- as.numeric(time_diff, units = units)
  
  times
}

setwd("D:\\2022_QTEV_Analysis_YN\\GPS_data\\")

filenames <- list.files(pattern="*.csv", full.names=TRUE)

start_error <- seq(1,32,1)

for (k in 1:length(filenames)) {
  
  name <- filenames[k]

  df_sncf <- read.csv(filenames[k])
  
  #put timing information in the good format
  df_sncf$Heure.franchissement <- str_times_to_dec(df_sncf$reconstructed_times_str)
  df_sncf$Depart.theo.SAT <- str_times_to_dec(df_sncf$Depart.theo.SAT)
  df_sncf$Depart.real.SAT <- str_times_to_dec(df_sncf$Depart.real.SAT)

  # check if the error on the start of the journey (this will work only for the travel from Paris to Lyon)
  start_error[k] <- df_sncf$Depart.real.SAT[1] - df_sncf$Heure.franchissement[1]

  ## add a column to say whether the point has been interpolated or not
  df_sncf['Interpolated'] <- NA
  df_sncf$Interpolated[is.na(df_sncf$timestamps_secs)] <- "Yes" # add Yes if the time-stamp is missing
  df_sncf$Interpolated[!(is.na(df_sncf$timestamps_secs))] <- "No" # add No if the the time-stamp is NOT missing
  
  # Convert time-stamps into elapsed duration 
  Duration <- df_sncf$Heure.franchissement - df_sncf$Heure.franchissement[1] # duration is calculated by the difference between the time of the data and the time of the first point (we can't use the real start because for the travels Lyon-Paris, it correspond to the start from Perrache)
  df_sncf["Duration"] <- Duration*60 # data is converted in minutes

  # Convert position into traveled distance in km
  df_sncf["CumDist"] <- c(0,cumsum(diff(df_sncf$Distance)/1000)) # data is converted in km
  
  # Convert speed (m/s) in (km/h)
  df_sncf$Vitesse <- df_sncf$Vitesse*3.6
  
  #f <- data.frame(mean(data$Vitesse),max(data$Vitesse),filenames[k])
  flush.console()
  
  # re-order data for easier inspection
  df_sncf<-df_sncf[, c("Jour.circulation","Depart.theo.SAT","Depart.real.SAT", "Heure.franchissement", "CumDist", "Duration",  "Vitesse", "Interpolated" )]
  
  write.csv(df_sncf, paste("D:\\2022_QTEV_Analysis_YN\\GPS_data\\clean\\", name, sep = ""), sep = ',')
  # test <- read.csv(paste("D:\\Yvan_PhD\\PROJECTS\\QTEV\\Analysis_YN\\GPS_data\\CSV_CLEAN\\", name, sep = ''), sep = ",") # use read_csv to open the file after
}




