################################################################################
# This is to compute the mean travel speed to identify the different speed episodes
################################################################################
library(Hmisc)
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
library(prettyR)


rm(list=ls()) # clear environment

# subfonction ------------------------------------------------------------------

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
#-------------------------------------------------------------------------------

# setwd("D:\\2022_QTEV_Analysis_YN\\GPS_data\\CSV_CLEAN\\")
setwd("D:\\2022_QTEV_Analysis_YN\\GPS_data\\clean\\")   # go into train data directory
filenames <- list.files(pattern="*.csv", full.names=TRUE)	                      # get the list of files in the directory

# to have access to the value in the dict use: (name is the name of the file)
# acc_end = dict[[name]][1]
# dec_str = dict[[name]][2]

df_sncf = c()
max_dist = c()
max_spd = c()
mean_spd = c()
max_dur = c()
na_num = c()
na_perc = c()
theo_dur = c()
data_point = c()


for (i in 1:length(filenames)) { # loop over the number of train data file
  
  name <- filenames[i]
  
  df_sncf_i <- read_csv(name)
  df_sncf_i = as.data.frame(df_sncf_i)
  df_sncf_i$name <- NA
  df_sncf_i$name <- name
  df_sncf_i = cbind(df_sncf_i,rep(i,dim(df_sncf_i)[1]))
  colnames(df_sncf_i) = c(colnames(df_sncf_i)[1:10],"travel")
  # extract the maximum value for each magnitude
  cur_max_dist = max(df_sncf_i$CumDist)
  cur_max_spd = max(df_sncf_i$Vitesse)
  cur_mean_spd = mean(df_sncf_i$Vitesse)
  cur_max_dur = max(df_sncf_i$Duration)
  cur_na_num = length(df_sncf_i$Interpolated[df_sncf_i$Interpolated == "Yes"])
  cur_na_perc = (length(df_sncf_i$Interpolated[df_sncf_i$Interpolated == "Yes"])*100)/(length(df_sncf_i$Interpolated[df_sncf_i$Interpolated == "Yes"]) + length(df_sncf_i$Interpolated[df_sncf_i$Interpolated == "No"]))
  cur_data_point = length(df_sncf_i$Interpolated)
  
  # add whether the travel is supposed to last 1h56 or 2h04
  if (grepl("6609", df_sncf_i$name[1], fixed = TRUE) | grepl("6611", df_sncf_i$name[1], fixed = TRUE) | grepl("6626", df_sncf_i$name[1], fixed = TRUE)){
    
    cur_theo_dur <- "1h56"
      
  }else if (grepl("6613", df_sncf_i$name[1], fixed = TRUE) | grepl("6617", df_sncf_i$name[1], fixed = TRUE) | grepl("6624", df_sncf_i$name[1], fixed = TRUE)){
    
    cur_theo_dur <- "2h04"
    
  }
  
  # bind the current data to the others
  df_sncf = rbind(df_sncf,df_sncf_i)
  max_dist = rbind(max_dist, cur_max_dist)
  max_spd = rbind(max_spd, cur_max_spd)
  mean_spd = rbind(mean_spd, cur_mean_spd)
  max_dur = rbind(max_dur, cur_max_dur)
  na_num = rbind(na_num, cur_na_num)
  na_perc = rbind(na_perc, cur_na_perc)
  theo_dur = rbind(theo_dur, cur_theo_dur)
  data_point = rbind(data_point, cur_data_point)
  
  #plot(df_sncf$Duration, df_sncf$Vitesse, type = "l", ylab = "Speed (km/h)", xlab = "Duration (min)", main = "Speed across the travel")
    
  #mean_speed <- as.matrix(select(df_sncf,
  #                                 Vitesse,
  #                                 Duration)) 
  
}

travel_dur <- cbind(max_dur, theo_dur)
travel_dur_df <- data.frame(travel_dur)
colnames(travel_dur_df) <- c("empirical_duration", "theoretical_duration")

travel_dist <- cbind(max_dist, theo_dur)
travel_dist_df <- data.frame(travel_dist)
colnames(travel_dist_df) <- c("empirical_distance", "theoretical_duration")

# descriptive stats for the maximum value
describe(data_point, num.desc = c("mean","sd", "max", "min","valid.n"))
describe(na_num, num.desc = c("mean","sd", "max", "min","valid.n"))
describe(na_perc, num.desc = c("mean","sd", "max", "min","valid.n"))
describe(max_dist, num.desc = c("mean","sd", "max", "min","valid.n"))
describe(max_spd, num.desc = c("mean","sd", "max", "min","valid.n"))
describe(mean_spd, num.desc = c("mean","sd", "max", "min","valid.n"))
describe(max_dur, num.desc = c("mean","sd", "max", "min","valid.n"))
describe(as.numeric(travel_dur_df$empirical_duration[travel_dur_df$theoretical_duration == "1h56"]), num.desc = c("mean","sd", "max", "min","valid.n"))
describe(as.numeric(travel_dur_df$empirical_duration[travel_dur_df$theoretical_duration == "2h04"]), num.desc = c("mean","sd", "max", "min","valid.n"))
# distance is the same for the two different travel durations
describe(as.numeric(travel_dist_df$empirical_distance[travel_dist_df$theoretical_duration == "1h56"]), num.desc = c("mean","sd", "max", "min","valid.n"))
describe(as.numeric(travel_dist_df$empirical_distance[travel_dist_df$theoretical_duration == "2h04"]), num.desc = c("mean","sd", "max", "min","valid.n"))


df_sncf$travel = factor(df_sncf$travel)
df_sncf$Interpolated = factor(df_sncf$Interpolated)

# dict with the value delimiting speed episodes
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

################################################################################
###################################### Plot ####################################
################################################################################
# requires to run the script 3_Extract_survey&train_data.R
## to plot individual travel each at a time
fig_path = "D:\\2022_QTEV_Analysis_YN\\figures\\travels\\individual_travel_with_spd_episodes\\"

df_survey <- read.csv("D:\\2022_QTEV_Analysis_YN\\survey_byfactors.csv")

for (t in 1:length(filenames)){
  
  travelId = t # this is just to plot manually each travel if needed
  
  df_sncf_i = df_sncf[which(df_sncf$travel == travelId),]
  
  individual_travel = ggplot(df_sncf_i, aes(x=(Duration), y=Vitesse)) +
    #facet_wrap(. ~ travel) +
    geom_line() +
    geom_point(mapping = aes(color = Interpolated)) +
    scale_color_manual(breaks = c("Yes", "No"), 
                       values=c("red", "black")) +
    xlab("Duration (min)") +
    ylab("Speed (km/h)") +
    labs(color = "Reconstructed", size = 35) +
    ggtitle(paste("individual travel", toString(travelId))) + 
    scale_x_continuous(breaks=seq(0,160,5)) +
    scale_y_continuous(breaks=seq(0,300,50)) +
    geom_vline(aes(xintercept = dict[[df_sncf_i$name[1]]][1]), size = 1) +
    geom_vline(aes(xintercept = dict[[df_sncf_i$name[1]]][2]), size = 1) +
    theme_classic()
  individual_travel
  
  fig_name = paste(fig_path, "travel_", as.character(travelId),".png", sep = "")
  ggsave(fig_name)  
}

# to plot all travels on top of each other
ggplot_test = ggplot(df_sncf, aes(x=Duration, y=Vitesse, color =  travel)) +
  #facet_wrap(. ~ travel) +
  geom_line() +
  xlab("Duration (ms)") +
  ylab("Speed") +
  ggtitle("All travels") + 
  scale_x_continuous(breaks=seq(0,160,5)) +
  theme_classic()

plot(ggplot_test)

# to plot all travels one by one in the same fig
ggplot_test = ggplot(df_sncf, aes(x=Duration, y=Vitesse)) +
  facet_wrap(. ~ travel) +
  geom_line() +
  
  geom_point(size = 0.1,mapping = aes(color = Interpolated)) +
  scale_color_manual(breaks = c("Yes", "No"), 
                     values=c("red", "black")) +
  xlab("Duration (min)") +
  ylab("Speed (km/h)") +
  ggtitle("All travels") + 
  scale_x_continuous(breaks=seq(0,160,30)) +
  theme_classic()

plot(ggplot_test)
alltravel_name = "D:\\2022_QTEV_Analysis_YN\\figures\\travels\\alltravels.png"
ggsave(alltravel_name)


################################################################################
# plot the train data with the participants responses

fig_path = "D:\\2022_QTEV_Analysis_YN\\figures\\travels\\individual_travel_with_participants\\"

for (t in 1:length(filenames)){
  
  travelId = t # select the travel to work on
  df_sncf_i = df_sncf[which(df_sncf$travel == travelId),] 
  
  df_survey_i = df_survey[which(df_survey$travel == df_sncf_i$name[1]),]  # select participants that responded during the selected travel
  df_survey_i$rep_vitesse <- NA # create a new column for the speed associated to paricipants responses
  df_survey_i$rep_dur <- NA # create an empty column to store the duration associated to the participant response
  for (r in 1:length(df_survey_i$rep_est)){
    
    df_survey_i$rep_vitesse[r] <- df_sncf_i$Vitesse[find_obj_value(df_survey_i$rep_est[r], df_sncf_i$Heure.franchissement)[1]] # get
    
    df_survey_i$rep_dur[r] <- df_sncf_i$Duration[find_obj_value(df_survey_i$rep_est[r], df_sncf_i$Heure.franchissement)[1]]
    
  }
  
  individual_travel = ggplot() + 
    geom_line(df_sncf_i,mapping = aes(x=Duration, y=Vitesse)) +
    geom_point(mapping = aes(x=df_survey_i$rep_dur, y = df_survey_i$rep_vitesse,color = factor(df_survey_i$participants)), size = 3) +
    xlab("Duration (min)") +
    ylab("Speed (km/h)") +
    scale_x_continuous(breaks=seq(0,160,5)) +
    labs(color = "Participants", size = 35) +
    ggtitle(paste("Travel", toString(travelId))) +
    geom_vline(aes(xintercept = dict[[df_sncf_i$name[1]]][1]), size = 1) +
    geom_vline(aes(xintercept = dict[[df_sncf_i$name[1]]][2]), size = 1) +
    theme_classic()
  
  individual_travel
  # there is only three point on the curve per participant because rep_est graphic and numeric are the same, and spd_inst and spd_mean is the same
  
  fig_name = paste(fig_path, "travel_", as.character(travelId),".png", sep = "")
  ggsave(fig_name)
}

################################################################################
# plot the train data with the participants responses with speed episode to check whether paticipants were well matched

fig_path = "D:\\2022_QTEV_Analysis_YN\\figures\\travels\\individual_travel_with_spd_episodes\\"

for (t in 1:length(filenames)){
  
  travelId = t # select the travel to work on
  df_sncf_i = df_sncf[which(df_sncf$travel == travelId),] 
  
  df_survey_i = df_survey[which(df_survey$travel == df_sncf_i$name[1]),]  # select participants that responded during the selected travel
  df_survey_i$rep_vitesse <- NA # create a new column for the speed associated to paricipants responses
  df_survey_i$rep_dur <- NA # create an empty column to store the duration associated to the participant response
  for (r in 1:length(df_survey_i$rep_est)){
    
    df_survey_i$rep_vitesse[r] <- df_sncf_i$Vitesse[find_obj_value(df_survey_i$rep_est[r], df_sncf_i$Heure.franchissement)[1]] # get
    
    df_survey_i$rep_dur[r] <- df_sncf_i$Duration[find_obj_value(df_survey_i$rep_est[r], df_sncf_i$Heure.franchissement)[1]]
    
  }
  
  individual_travel = ggplot() + 
    geom_line(df_sncf_i,mapping = aes(x=Duration, y=Vitesse)) +
    geom_point(mapping = aes(x=df_survey_i$rep_dur, y = df_survey_i$rep_vitesse,color = factor(df_survey_i$ep_est)), size = 3) +
    scale_color_manual(breaks = c("acc", "cst", "dec"), 
                       values=c("Sky Blue", "Orange", "Purple")) +
    xlab("Duration (min)") +
    ylab("Speed (km/h)") +
    scale_x_continuous(breaks=seq(0,160,5)) +
    labs(color = "Participants", size = 35) +
    ggtitle(paste("Travel", toString(travelId))) +
    geom_vline(aes(xintercept = dict[[df_sncf_i$name[1]]][1]), size = 1) +
    geom_vline(aes(xintercept = dict[[df_sncf_i$name[1]]][2]), size = 1) +
    theme_classic()
  
  plot(individual_travel)
  # there is only three point on the curve per participant because rep_est graphic and numeric are the same, and spd_inst and spd_mean is the same
  
  fig_name = paste(fig_path, "travel_", as.character(travelId),"_&spd_ep.png", sep = "")
  ggsave(fig_name)
}
################################################################################
############################### figure for methods #############################
################################################################################
# panel A for the figure, example the reconstruction process of time stamps 
travelId_A = 5 # this is just to plot manually each travel if needed

df_sncf_A = df_sncf[which(df_sncf$travel == travelId_A),]

acc_text1<-data.frame(
  x = 4, y = 330,
  label = "positive"
)

acc_text2<-data.frame(
  x = 4, y = 310,
  label = "(a > 0)"
)

cst_text1<-data.frame(
  x = 56, y = 330,
  label = "null"
)
cst_text2<-data.frame(
  x = 56, y = 310,
  label = "(a = 0)"
)

dec_text1<-data.frame(
  x = 107, y = 330,
  label = "negative"
)
dec_text2<-data.frame(
  x = 107, y = 310,
  label = "(a < 0)"
)

panel_A = ggplot(df_sncf_A, aes(x=(Duration), y=Vitesse)) +
  #facet_wrap(. ~ travel) +
  geom_line() +
  geom_point(mapping = aes(color = Interpolated)) +
  scale_color_manual(breaks = c("Yes", "No"), 
                     values=c("red", "black")) +
  xlab("Duration (min)") +
  ylab("Speed (km/h)") +
  labs(color = "Reconstructed:", size = 35) +
  # ggtitle("A.") + 
  scale_x_continuous(breaks=seq(0,160,10)) +
  scale_y_continuous(breaks=seq(0,300,50)) +
  geom_vline(aes(xintercept = dict[[df_sncf_A$name[1]]][1]), size = 1) +
  geom_vline(aes(xintercept = dict[[df_sncf_A$name[1]]][2]), size = 1) +
  theme_classic()+
  # geom_text(data = acc_text1, aes( x=x, y=y, label=label), color="sky blue", size=5)+
  # geom_text(data = acc_text2, aes( x=x, y=y, label=label), color="sky blue", size=5)+
  # geom_text(data = cst_text1, aes( x=x, y=y, label=label), color="orange", size=5)+
  # geom_text(data = cst_text2, aes( x=x, y=y, label=label), color="orange", size=5)+
  # geom_text(data = dec_text1, aes( x=x, y=y, label=label), color="purple", size=5)+
  # geom_text(data = dec_text2, aes( x=x, y=y, label=label), color="purple", size=5)+
  theme(legend.position = c(0.56,0.3), text = element_text(size = 17)) # add ,axis.title.x = element_blank() to remove the x axis title
panel_A


# panel B for figure, speed episodes
# rename acc, cst, dec to positive, null, negative
df_survey$ep_est[df_survey$ep_est == 'acc'] <- 'positive'
df_survey$ep_est[df_survey$ep_est == 'cst'] <- 'null'
df_survey$ep_est[df_survey$ep_est == 'dec'] <- 'negative'

travelId_B = 25 # select the travel to work on
df_sncf_B = df_sncf[which(df_sncf$travel == travelId_B),] 

df_survey_i = df_survey[which(df_survey$travel == df_sncf_B$name[1]),]  # select participants that responded during the selected travel
df_survey_i$rep_vitesse <- NA # create a new column for the speed associated to paricipants responses
df_survey_i$rep_dur <- NA # create an empty column to store the duration associated to the participant response
for (r in 1:length(df_survey_i$rep_est)){
  
  df_survey_i$rep_vitesse[r] <- df_sncf_B$Vitesse[find_obj_value(df_survey_i$rep_est[r], df_sncf_B$Heure.franchissement)[1]] # get
  
  df_survey_i$rep_dur[r] <- df_sncf_B$Duration[find_obj_value(df_survey_i$rep_est[r], df_sncf_B$Heure.franchissement)[1]]
  
}

panel_B = ggplot() + 
  geom_line(df_sncf_B,mapping = aes(x=Duration, y=Vitesse)) +
  geom_point(mapping = aes(x=df_survey_i$rep_dur, y = df_survey_i$rep_vitesse,color = factor(df_survey_i$ep_est)), size = 3) +
  scale_color_manual(breaks = c("positive", "null", "negative"), 
                     values=c("Sky Blue", "Orange", "Purple")) +
  xlab("Duration (min)") +
  ylab("Speed (km/h)") +
  scale_x_continuous(breaks=seq(0,160,10)) +
  scale_y_continuous(breaks=seq(0,300,50)) +
  labs(color = "Acceleration episode:", size = 35) +
  ggtitle("B.") +
  geom_vline(aes(xintercept = dict[[df_sncf_B$name[1]]][1]), size = 1) +
  geom_vline(aes(xintercept = dict[[df_sncf_B$name[1]]][2]), size = 1) +
  theme_classic()+
  theme(legend.position = c(0.56,0.3), text = element_text(size = 17))
panel_B

gridExtra::grid.arrange(panel_A, panel_B, nrow = 2)
