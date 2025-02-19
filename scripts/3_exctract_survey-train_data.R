library(RColorBrewer)
library(ggplot2)
library(readxl)
library(plyr)
library(tidyverse)
library(DBI)
#library(ggpubr)
#library("qqplotr")
library("DescTools")  # can't manage to install it, hope it is not useful
library("PMCMRplus")  # can't manage to install it, hope it is not useful
library(ggsignif)
library("readr")
library("chron")
library(lattice)
#library(car)
#library(gdata)
library(lsr)
library(prettyR)
library(lmerTest)
library(contrast)
library(reshape2)
#library(multcomp) make my variable column name as function so if I don't need it, I will not add it for now

rm(list=ls())     # to clear current workspace

################################################################################
################################ define functions ##############################
################################################################################

find_obj_value <- function(sub_data, train_data) {
  # this function will find the index of the closest train data point (objective value) to the response of the participant
  # sub_data must be a single value for the participant data
  # train data must be a column containing the time point of the travel 
  # both should be in decimal hour format
  # it will return the index of the data and the distance between the two points to check whether there was a missing values at the participants response
  
  train_data <- data.frame(train_data)
  
  idx <- which.min(dist(rbind(sub_data, train_data))[1:nrow(train_data)])
  dist <- (dist(rbind(sub_data, train_data))[idx])*60                           # converted into minutes   
  
  return(c(idx, dist))
}

find_spd_ep <- function(obj_val, acc_end, dec_str) {
  # add the corresponding speed episode by checking whether the objective value response is lower or greater than acc and dec ending and starting
  
  if (!is.na(obj_val)) { # check if the objective value of duration linked to speed is not NA esle if condition will fail
    
    if (obj_val < acc_end){
      spd_ep = "acc"
    } else if (obj_val >= acc_end &&obj_val < dec_str){   
      spd_ep = "cst"
    } else if (obj_val >= dec_str){
      spd_ep = "dec"
    }
  }
  
  spd_ep
  
}

################################################################################
################################## main code ###################################
################################################################################

# This code will extract objective values (from train data) corresponding to the participant responses
# Then reshape the format of the data to be use in factors (each column is a factor)

setwd("D:\\2022_QTEV_Analysis_YN\\")

df_survey <- read.csv2("DATA_survey_raw.csv") # using test because participants 11 had wrong timing response so I had to changed it manually and was afraid that I could not read correctly the csv file anymore

#df_survey_test <- read.csv2("Formulaires_csv.csv") 

#all_equal(df_survey, df_survey_test)

# intialyse a dict with the value of speed episodes identified visually with the first part of the script 2_Identify_speed_episodes.R
dict <- list("./6609_2021-07-15_full_journey.csv" = c(15.75, 99),
             "./6609_2021-07-21_full_journey.csv" = c(14.5, 102),
             "./6611_2021-06-01_full_journey.csv" = c(15,91),
             "./6611_2021-07-01_full_journey.csv" = c(17,104),
             "./6611_2021-06-02_full_journey.csv" = c(15,96.5),
             "./6611_2021-06-03_full_journey.csv" = c(14.75,90.5),
             "./6611_2021-06-08_full_journey.csv" = c(15.5,97),
             "./6611_2021-06-09_full_journey.csv" = c(13.5,89),
             "./6611_2021-06-10_full_journey.csv" = c(14,97.5),
             "./6611_2021-04-22_full_journey.csv" = c(14.5,96),
             "./6611_2021-06-24_full_journey.csv" = c(15.25,98),
             "./6611_2021-05-27_full_journey.csv" = c(14.5,92),
             "./6611_2021-06-30_full_journey.csv" = c(16,96.5),
             "./6613_2021-07-07_full_journey.csv" = c(17.5,105),
             "./6613_2021-07-08_full_journey.csv" = c(16,107.5),
             "./6617_2021-04-14_full_journey.csv" = c(30,111.5),
             "./6624_2021-06-01_full_journey.csv" = c(20,106.5),
             "./6624_2021-07-01_full_journey.csv" = c(19,105.5),
             "./6624_2021-06-02_full_journey.csv" = c(19,108.5),
             "./6624_2021-06-03_full_journey.csv" = c(20,106.5),
             "./6624_2021-06-08_full_journey.csv" = c(18.5,108),
             "./6624_2021-06-09_full_journey.csv" = c(17.5,106.75),
             "./6624_2021-06-10_full_journey.csv" = c(22.75,107.25),
             "./6624_2021-07-15_full_journey.csv" = c(15,107.25),
             "./6624_2021-07-21_full_journey.csv" = c(19.5,113.25),
             "./6624_2021-04-22_full_journey.csv" = c(16,106),
             "./6624_2021-06-24_full_journey.csv" = c(14.5,96.75),
             "./6624_2021-05-27_full_journey.csv" = c(21,105.5),
             "./6624_2021-06-30_full_journey.csv" = c(16,98),
             "./6626_2021-07-07_full_journey.csv" = c(17,98.5),
             "./6626_2021-07-08_full_journey.csv" = c(15.75,89.75),
             "./6626_2021-04-14_full_journey.csv" = c(13,92.5)
)

#testing the loop to get the speed_episode delimitation
#name = "./,EXP_6617_14-04_2021_20210512151158.csv"

# I can just call the dictionnary key to retrieve the delimitation
#acc_end = dict[[name]][1]
#dec_str = dict[[name]][2]
#print(paste("acceleration stops after ", acc_end, " min"))
#print(paste("deceleration starts after ", dec_str, " min"))

# reformat global data
df_survey$departure <- df_survey$Departure_hour + (df_survey$Departure_min)/60 # timing of the response is converted in hours decimals to match train data and therefore identify speed episodes

df_survey$order = df_survey$Order

df_survey$direction[df_survey$Direction == 1] <- "Forward"                      # convert direction into categorical data, Forward
df_survey$direction[df_survey$Direction == 2] <- "Backward"                     # convert direction into categorical data, Backward

df_survey$laterality[df_survey$Laterality == 1] <- "Right-handed"
df_survey$laterality[df_survey$Laterality == 2] <- "Left-handed"

# knowledge
df_survey$rep_knw <- df_survey$Hour_1 + (df_survey$min_1)/60                    # convert timing of response to a hour decimals format

df_survey$knowledge_distance[df_survey$Knowledge_distance == 0] <- "No"
df_survey$knowledge_distance[df_survey$Knowledge_distance == 1] <- "Yes"
df_survey$knwYesNo_dist <- df_survey$knowledge_distance                           # whether participant know the distance
df_survey$knw_dist <- df_survey$Distance

df_survey$knowledge_time[df_survey$Knowledge_time == 0] <- "No"
df_survey$knowledge_time[df_survey$Knowledge_time == 1] <- "Yes"
df_survey$knwYesNo_dur <- df_survey$knowledge_time
df_survey$knw_dur <- (df_survey$Time_hour*60) + df_survey$Time_min              # convert duration in minutes to match the estimation perfomd

df_survey$knwYesNo_spd <- NA                                                       # create an empty column to match the other otherwise the code might crash when I will reformat the data        
df_survey$knw_spd <- df_survey$Max_speed

# Speed
df_survey$rep_spd <- df_survey$Hour_2 + (df_survey$min_2)/60     
df_survey$spd_inst_allo <- (df_survey$Inst_Speed_graphic/180)*df_survey$knw_spd
df_survey$spd_inst_ego <- df_survey$Inst_Speed_num
df_survey$spd_inst_conf <- df_survey$Inst_Speed_conf
df_survey$spd_inst_meta <- NA

df_survey$spd_mean_allo = (df_survey$Av_speed_graphic/180)*df_survey$knw_spd
df_survey$spd_mean_ego = df_survey$Av_speed_num
df_survey$spd_mean_conf = df_survey$Av_speed_confidence
df_survey$spd_mean_meta <- NA

# Duration
df_survey$rep_dur = df_survey$Hour_3 + (df_survey$min_3/60)
df_survey$dur_allo = (df_survey$Time_graphic/360)*df_survey$knw_dur
df_survey$dur_ego = (df_survey$Time_hour_num*60) + df_survey$Time_min_num       # duration estimation is converted in minutes to match the duration elapsed onboard
df_survey$dur_conf = df_survey$Time_conf
df_survey$dur_meta = df_survey$Over_or_Underestimation

# distance
df_survey$rep_dist = df_survey$Hour_4 + (df_survey$min_4/60)
df_survey$dist_allo = (df_survey$Dist_graph/6.2)*df_survey$knw_dist             # Diane measured 6.3 as the total length on the graph but after verification I measured 6.2
df_survey$dist_ego = df_survey$Dist_num
df_survey$dist_conf = df_survey$Dist_conf
df_survey$dist_meta <- NA

# Debrief
df_survey$rep_debrief = df_survey$Hour_5 + (df_survey$min_5/60)
df_survey$pot = df_survey$Speed_of_time
df_survey$homogeneity_pot = df_survey$Homogeneity
df_survey$sadness = df_survey$Sadness
df_survey$happiness = df_survey$Hapiness
df_survey$boredom = df_survey$Boredom
df_survey$dist_strat = df_survey$Dist_Strat
df_survey$dur_strat = df_survey$Time_Strat
df_survey$spd_strat = df_survey$Speed_Strat

# demographic
df_survey$participants = df_survey$Participants
df_survey$age = df_survey$Age
df_survey$Sex[df_survey$Sex == 1] <- "Man"
df_survey$Sex[df_survey$Sex == 2] <- "Woman"
df_survey$sex = df_survey$Sex

# instantiate objective value column
df_survey$obj_dur = NA
df_survey$obj_dist = NA
df_survey$obj_spd_inst = NA
df_survey$obj_spd_mean = NA

# instantiate distance between the participant response and the objective value
df_survey$away_dur = NA
df_survey$away_dist = NA
df_survey$away_spd_inst = NA
df_survey$away_spd_mean = NA


# instantiate speed episode
df_survey$ep_dur = NA
df_survey$ep_dist = NA
df_survey$ep_spd = NA

# instantiate morning/afternoon
df_survey$paris_lyon = NA

# I should probably add the objective value of travel (max distance, total duration, and maximum speed) for each participants
# however I'm still not sure whether I should use the theoretical value or the actual one. It is computed in the Identify_speed_episodes.R  
#df_survey$obj_knw_dist = NA
#df_survey$obj_knw_dur = NA
#df_survey$obj_knw_speed = NA

# add a column for the travel name
df_survey$travel = NA

# add a column for the total objectiv duration and distance
df_survey$tot_dur = NA
df_survey$tot_dist = 429.12
df_survey$tot_speed = 320

# loop over train data files to get the objective values

setwd("D:\\2022_QTEV_Analysis_YN\\GPS_data\\clean\\")   # go into train data directory
filenames <- list.files(pattern="*.csv", full.names=TRUE)	                      # get the list of files in the directory

################################################################################
for (i in 1:nrow(df_survey)) { # loop over the number of participants 
  
  for (k in 1:length(filenames)) { # loop over the number of train data file
    
    name <- filenames[k]
    df_sncf <- read_csv(name)
    
    if (df_survey$Date[i]==df_sncf$Jour.circulation[1]){ # Select the train data file with the corresponding date 
      
      if (floor(df_sncf$Depart.theo.SAT[1]) < 14 && floor(df_survey$departure[i]) < 14) { # select the train data file with the corresponding time (by using the "partie entière" of the hour)
        
        acc_end = dict[[name]][1]
        dec_str = dict[[name]][2]
        df_survey$travel[i] = name
        df_survey$paris_lyon = "PL"
        df_survey$tot_dur = max(df_sncf$Duration)
        
        # extract objective duration
        df_survey$obj_dur[i] <- df_sncf$Duration[find_obj_value(df_survey$rep_dur[i], df_sncf$Heure.franchissement)[1]] # find_obj_value will return the index of the time point corresponding to the response
        df_survey$away_dur[i] <- find_obj_value(df_survey$rep_dur[i], df_sncf$Heure.franchissement)[2]
        
        # add the corresponding speed episode
        if (!is.na(df_survey$rep_dur[i])) {
          df_survey$ep_dur[i] <- find_spd_ep(df_survey$obj_dur[i], acc_end, dec_str)
        }
        
        # extract objective distance
        df_survey$obj_dist[i] <- df_sncf$CumDist[find_obj_value(df_survey$rep_dist[i], df_sncf$Heure.franchissement)[1]] # find_obj_value will return the index of the time point corresponding to the response
        df_survey$away_dist[i] <- find_obj_value(df_survey$rep_dist[i], df_sncf$Heure.franchissement)[2]
        
        # add the corresponding speed episode
        if (!is.na(df_survey$rep_dist[i])){
          df_survey$ep_dist[i] <- find_spd_ep(df_sncf$Duration[find_obj_value(df_survey$rep_dist[i], df_sncf$Heure.franchissement)[1]], acc_end, dec_str)
        }
        
        # extract objective speed
        df_survey$obj_spd_inst[i] <- df_sncf$Vitesse[find_obj_value(df_survey$rep_spd[i], df_sncf$Heure.franchissement)[1]] # find_obj_value will return the index of the time point corresponding to the response
        df_survey$away_spd_inst[i] <- find_obj_value(df_survey$rep_spd[i], df_sncf$Heure.franchissement)[2]
        
        if (!is.na(df_survey$rep_spd[i]))
        df_survey$obj_spd_mean[i] <- mean(df_sncf[["Vitesse"]][1:find_obj_value(df_survey$rep_spd[i], df_sncf$Heure.franchissement)[1]]) 
        
        # add the corresponding speed episode
        if (!is.na(df_survey$rep_spd[i])){
          df_survey$ep_spd[i] <- find_spd_ep(df_sncf$Duration[find_obj_value(df_survey$rep_spd[i], df_sncf$Heure.franchissement)[1]], acc_end, dec_str)
        }
        
      } else if (floor(df_sncf$Depart.theo.SAT[1]) > 14 && floor(df_survey$departure[i]) > 14) { # select the train data file with the corresponding time (by using the "partie entière" of the hour and to contrast the morning vs afternoon)
        
        acc_end = dict[[name]][1]
        dec_str = dict[[name]][2]
        df_survey$travel[i] = name
        df_survey$paris_lyon = "LP"
        df_survey$tot_dur = max(df_sncf$Duration)
        
        # extract objective duration
        df_survey$obj_dur[i] <- df_sncf$Duration[find_obj_value(df_survey$rep_dur[i], df_sncf$Heure.franchissement)[1]] # find_obj_value will return the index of the time point corresponding to the response
        df_survey$away_dur[i] <- find_obj_value(df_survey$rep_dur[i], df_sncf$Heure.franchissement)[2]
        
        # add the corresponding speed episode
        if (!is.na(df_survey$rep_dur[i])) {
          df_survey$ep_dur[i] <- find_spd_ep(df_survey$obj_dur[i], acc_end, dec_str)
        }
        
        # extract objective distance
        df_survey$obj_dist[i] <- df_sncf$CumDist[find_obj_value(df_survey$rep_dist[i], df_sncf$Heure.franchissement)[1]] # find_obj_value will return the index of the time point corresponding to the response
        df_survey$away_dist[i] <- find_obj_value(df_survey$rep_dist[i], df_sncf$Heure.franchissement)[2]
        
        # add the corresponding speed episode
        if (!is.na(df_survey$rep_dist[i])){
          df_survey$ep_dist[i] <- find_spd_ep(df_sncf$Duration[find_obj_value(df_survey$rep_dist[i], df_sncf$Heure.franchissement)[1]], acc_end, dec_str)
        }
        
        # extract objective speed
        df_survey$obj_spd_inst[i] <- df_sncf$Vitesse[find_obj_value(df_survey$rep_spd[i], df_sncf$Heure.franchissement)[1]] # find_obj_value will return the index of the time point corresponding to the response
        df_survey$away_spd_inst[i] <- find_obj_value(df_survey$rep_spd[i], df_sncf$Heure.franchissement)[2]
        
        if (!is.na(df_survey$rep_spd[i]))
          df_survey$obj_spd_mean[i] <- mean(df_sncf[["Vitesse"]][1:find_obj_value(df_survey$rep_spd[i], df_sncf$Heure.franchissement)[1]]) 
        
        # add the corresponding speed episode
        if (!is.na(df_survey$rep_spd[i])){
          df_survey$ep_spd[i] <- find_spd_ep(df_sncf$Duration[find_obj_value(df_survey$rep_spd[i], df_sncf$Heure.franchissement)[1]], acc_end, dec_str)
        }
      }
    }
  }
}

# away_spd_mean is missing so adding it here
df_survey$away_spd_mean <- df_survey$away_spd_inst

# add graphic estimation based on the objective value of the travel (and not the knowledge of participants)
df_survey$spd_inst_tot <- (df_survey$Inst_Speed_graphic/180)*df_survey$tot_speed
df_survey$spd_mean_tot = (df_survey$Av_speed_graphic/180)*df_survey$tot_speed
df_survey$dur_tot = (df_survey$Time_graphic/360)*df_survey$tot_dur
df_survey$dist_tot = (df_survey$Dist_graph/6.2)*df_survey$tot_dist


# saving the file with all the columns
write.csv(df_survey, "D:\\2022_QTEV_Analysis_YN\\survey-trains_raw.csv", row.names = FALSE)

# to read the csv file if only the last part is needed
#df_survey <- read.csv("D:\\2022_QTEV_Analysis_YN\\Formulaires&Trains_raw_v3.csv")
df_survey$rep_spd_inst <- df_survey$rep_spd
df_survey$rep_spd_mean <- df_survey$rep_spd

# compute relative estimation and relative error
# allo
df_survey$RE_dist_allo = df_survey$dist_allo/df_survey$obj_dist
df_survey$ER_dist_allo = (df_survey$dist_allo - df_survey$obj_dist)/ df_survey$obj_dist

df_survey$RE_dur_allo = df_survey$dur_allo/df_survey$obj_dur
df_survey$ER_dur_allo = (df_survey$dur_allo - df_survey$obj_dur)/ df_survey$obj_dur

df_survey$RE_spd_inst_allo = df_survey$spd_inst_allo/df_survey$obj_spd_inst
df_survey$ER_spd_inst_allo = (df_survey$spd_inst_allo - df_survey$obj_spd_inst)/ df_survey$obj_spd_inst

df_survey$RE_spd_mean_allo = df_survey$spd_mean_allo/df_survey$obj_spd_mean
df_survey$ER_spd_mean_allo = (df_survey$spd_mean_allo - df_survey$obj_spd_mean)/ df_survey$obj_spd_mean

# ego
df_survey$RE_dist_ego = df_survey$dist_ego/df_survey$obj_dist
df_survey$ER_dist_ego = (df_survey$dist_ego - df_survey$obj_dist)/ df_survey$obj_dist

df_survey$RE_dur_ego = df_survey$dur_ego/df_survey$obj_dur
df_survey$ER_dur_ego = (df_survey$dur_ego - df_survey$obj_dur)/ df_survey$obj_dur

df_survey$RE_spd_inst_ego = df_survey$spd_inst_ego/df_survey$obj_spd_inst
df_survey$ER_spd_inst_ego = (df_survey$spd_inst_ego - df_survey$obj_spd_inst)/ df_survey$obj_spd_inst

df_survey$RE_spd_mean_ego = df_survey$spd_mean_ego/df_survey$obj_spd_mean
df_survey$ER_spd_mean_ego = (df_survey$spd_mean_ego - df_survey$obj_spd_mean)/ df_survey$obj_spd_mean

# tot
df_survey$RE_dist_tot = df_survey$dist_tot/df_survey$obj_dist
df_survey$ER_dist_tot = (df_survey$dist_tot - df_survey$obj_dist)/ df_survey$obj_dist

df_survey$RE_dur_tot = df_survey$dur_tot/df_survey$obj_dur
df_survey$ER_dur_tot = (df_survey$dur_tot - df_survey$obj_dur)/ df_survey$obj_dur

df_survey$RE_spd_inst_tot = df_survey$spd_inst_tot/df_survey$obj_spd_inst
df_survey$ER_spd_inst_tot = (df_survey$spd_inst_tot - df_survey$obj_spd_inst)/ df_survey$obj_spd_inst

df_survey$RE_spd_mean_tot = df_survey$spd_mean_tot/df_survey$obj_spd_mean
df_survey$ER_spd_mean_tot = (df_survey$spd_mean_tot - df_survey$obj_spd_mean)/ df_survey$obj_spd_mean


# select only the relevant columns
df_clean = select(df_survey,
                  participants,
                  age,
                  sex,
                  laterality,
                  order,
                  direction,
                  knwYesNo_dist,
                  knw_dist,
                  knwYesNo_dur,
                  knw_dur,
                  knwYesNo_spd,
                  knw_spd,
                  spd_inst_allo,
                  spd_inst_ego,
                  spd_inst_tot,
                  spd_inst_conf,
                  RE_spd_inst_allo,
                  RE_spd_inst_ego,
                  RE_spd_inst_tot,
                  ER_spd_inst_allo,
                  ER_spd_inst_ego,
                  ER_spd_inst_tot,
                  obj_spd_inst,
                  rep_spd_inst,
                  ep_spd,
                  spd_mean_allo,
                  spd_mean_ego,
                  spd_mean_tot,
                  spd_mean_conf,
                  RE_spd_mean_allo,
                  RE_spd_mean_ego,
                  RE_spd_mean_tot,
                  ER_spd_mean_allo,
                  ER_spd_mean_ego,
                  ER_spd_mean_tot,
                  obj_spd_mean,
                  rep_spd_mean,
                  dur_allo,
                  dur_ego,
                  dur_tot,
                  dur_conf,
                  dur_meta, # for now I'm not using dur_meta (participants reported whether the duration was over or underestimated)
                  dist_meta,
                  spd_inst_meta,
                  spd_mean_meta,
                  RE_dur_allo,
                  RE_dur_ego,
                  RE_dur_tot,
                  ER_dur_allo,
                  ER_dur_ego,
                  ER_dur_tot,
                  obj_dur,
                  ep_dur,
                  rep_dur,
                  dist_allo,
                  dist_ego,
                  dist_tot,
                  dist_conf,
                  RE_dist_allo,
                  RE_dist_ego,
                  RE_dist_tot,
                  ER_dist_allo,
                  ER_dist_ego,
                  ER_dist_tot,
                  obj_dist,
                  ep_dist,
                  rep_dist,
                  pot,
                  homogeneity_pot,
                  sadness,
                  happiness,
                  boredom,
                  spd_strat,
                  dur_strat,
                  dist_strat,
                  creusot,
                  away_dur,
                  away_dist,
                  away_spd_inst,
                  away_spd_mean,
                  travel,
)

# reshape the df to have factors
ego <- df_clean %>%
  select(!contains("allo") & !contains("tot")) %>%
  rename_with(function(x){gsub("_ego","",x)}) %>%
  mutate(format = "numeric")

allo <- df_clean %>%
  select(!contains("ego") & !contains("tot")) %>%
  rename_with(function(x){gsub("_allo","",x)}) %>%
  mutate(format = "graphic")

tot <- df_clean %>%
  select(!contains("ego") & !contains("allo")) %>%
  rename_with(function(x){gsub("_tot","",x)}) %>%
  mutate(format = "total")
  

df <- rbind(ego, allo, tot)

dur <- df %>%
  select(!contains(c("dist", "spd"))) %>%   # I use spd only and not spd_inst and spd_mean because it will remove both and also remove the spd_knw (that is shared for spd_inst and mean)
  rename_with(function(x){gsub("dur","est",x)}) %>%
  mutate(type = "dur")

dist <- df %>%
  select(!contains(c("dur", "spd"))) %>%   # I use spd only and not spd_inst and spd_mean because it will remove both and also remove the spd_knw (that is shared for spd_inst and mean)
  rename_with(function(x){gsub("dist","est",x)}) %>%
  mutate(type = "dist")

spd_inst <- df %>%
  select(!contains(c("dur", "dist", "spd_mean"))) %>%   # I use spd only and not spd_inst and spd_mean because it will remove both and also remove the spd_knw (that is shared for spd_inst and mean)
  rename_with(function(x){gsub("spd_inst","est",x)}) %>%
  rename_with(function(x){gsub("spd_","est",x)}) %>%
  rename_with(function(x){gsub("spd","est",x)}) %>%
  rename_with(function(x){gsub("eststrat","est_strat",x)}) %>%
  mutate(type = "spd_inst")

spd_mean <- df %>%
  select(!contains(c("dur", "dist", "spd_inst"))) %>%   # I use spd only and not spd_inst and spd_mean because it will remove both and also remove the spd_knw (that is shared for spd_inst and mean)
  rename_with(function(x){gsub("spd_mean","est",x)}) %>%
  rename_with(function(x){gsub("spd_","est",x)}) %>%
  rename_with(function(x){gsub("spd","est",x)}) %>%
  rename_with(function(x){gsub("eststrat","est_strat",x)}) %>%
  mutate(type = "spd_mean")

df_final <- rbind(dur, dist, spd_inst, spd_mean)

write.csv(df_final, "D:\\2022_QTEV_Analysis_YN\\survey_byfactors.csv", row.names = FALSE)

