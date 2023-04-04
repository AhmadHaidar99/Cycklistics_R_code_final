library("tidyverse")
install.packages("ggplot2")
library("ggplot2")
library("dplyr")
library("lubridate")
library("hms")
library("data.table")
install.packages("readr")
library("readr")
install.packages("anytime")
library("anytime")
install.packages("parsedate")
library("parsedate")
#renaming the data frames and load the data frames
jan01_df <- read.csv("D:/Data Analysis/Google/G8/Case_study_1/202201-divvy-tripdata/202201-divvy-tripdata.csv")
feb02_df <- read.csv("D:/Data Analysis/Google/G8/Case_study_1/202202-divvy-tripdata/202202-divvy-tripdata.csv")
df1_2 <- rbind(jan01_df,feb02_df)
mar03_df <- read.csv("D:/Data Analysis/Google/G8/Case_study_1/202203-divvy-tripdata/202203-divvy-tripdata.csv")
apr04_df <- read.csv("D:/Data Analysis/Google/G8/Case_study_1/202204-divvy-tripdata/202204-divvy-tripdata.csv")
df3_4 <- rbind(mar03_df,apr04_df)
may05_df <- read.csv("D:/Data Analysis/Google/G8/Case_study_1/202205-divvy-tripdata/202205-divvy-tripdata.csv")
jun06_df <- read.csv("D:/Data Analysis/Google/G8/Case_study_1/202206-divvy-tripdata/202206-divvy-tripdata.csv")
df5_6 <- rbind(may05_df,jun06_df)
jul07_df <- read.csv("D:/Data Analysis/Google/G8/Case_study_1/202207-divvy-tripdata/202207-divvy-tripdata.csv")
aug08_df <- read.csv("D:/Data Analysis/Google/G8/Case_study_1/202208-divvy-tripdata/202208-divvy-tripdata.csv")
df7_8 <- rbind(jul07_df,aug08_df)
sep09_df <- read.csv("D:/Data Analysis/Google/G8/Case_study_1/202209-divvy-tripdata/202209-divvy-publictripdata.csv")
oct10_df <- read.csv("D:/Data Analysis/Google/G8/Case_study_1/202210-divvy-tripdata/202210-divvy-tripdata.csv")
df9_10 <- rbind(sep09_df,oct10_df)
nov11_df <- read.csv("D:/Data Analysis/Google/G8/Case_study_1/202211-divvy-tripdata/202211-divvy-tripdata.csv")
dec12_df <- read.csv("D:/Data Analysis/Google/G8/Case_study_1/202212-divvy-tripdata/202212-divvy-tripdata.csv")
df11_12<- rbind(nov11_df,dec12_df)
#merge all of the data frames into one year view
df1234<- rbind(df1_2,df3_4)
df5678 <- rbind(df5_6,df7_8)
df9101112<- rbind(df9_10,df11_12)
before_final <- rbind(df1234,df5678) 
cyclistic_df <- rbind(before_final,df9101112)
#calculate ride length by subtracting ended_at time from started_at time and converted it to minutes
cyclistic_df$started_at <- as.POSIXct(cyclistic_df$started_at, format = "%Y-%m-%d %H:%M:%S")
cyclistic_df$ended_at <- as.POSIXct(cyclistic_df$ended_at, format = "%Y-%m-%d %H:%M:%S")
cyclistic_df$ride_length <- difftime(cyclistic_df$ended_at, cyclistic_df$started_at, units = "mins")
#calculate the day of the week
cyclistic_df$day_of_week <- wday(cyclistic_df$started_at)
#clean the data
#remove rows with NA values
cyclistic_df <- na.omit(cyclistic_df) 
cyclistic_df <- cyclistic_df[complete.cases(cyclistic_df),]
#remove duplicate rows
cyclistic_df <- distinct(cyclistic_df)
#remove where ride_length is 0 or negative
cyclistic_df <- cyclistic_df[!(cyclistic_df$ride_length <=0),]
#total number of rides
nrow(cyclistic_df)
#Member Type
cyclistic_df %>%
  group_by(member_casual) %>% 
  count(member_casual)
#Type Of Bike
#total rides by member type 
cyclistic_df %>%
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)
#total rides 
cyclistic_df %>%
  group_by(rideable_type) %>% 
  count(rideable_type)
#Average Ride Length
#average of ride_length
cyclistic_avgRide <- mean(cyclistic_df$ride_length)
print(cyclistic_avgRide)
#Member Type
#average ride_length
cyclistic_df %>% group_by( member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
#Type Of Bike
#total rides by member type 
cyclistic_df %>% group_by(member_casual, rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
#average ride_length
cyclistic_df %>% group_by(rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
#Day Of The Week
#average ride_length by member type
cyclistic_df %>% group_by(member_casual, day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
#average ride_length 
cyclistic_df %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))
# Exporting the data
write.csv(cyclistic_df, "D:\\Data Analysis\\Google\\G8\\Case_study_1\\cyclistics_df.csv", row.names=FALSE)
