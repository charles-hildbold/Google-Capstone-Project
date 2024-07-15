setwd("C:/Users/Charl/Desktop/Google Project/Bike Share")
#load libraries 
library(tidyverse)
library(lubridate)
library(hms)
library(data.table)

#Load previous 12 months of Data: May 2023 through April 2024
may23_df <- read_csv("202305-divvy-tripdata.csv") 
jun23_df <- read_csv("202306-divvy-tripdata.csv") 
jul23_df <- read_csv("202307-divvy-tripdata.csv")
aug23_df <- read_csv("202308-divvy-tripdata.csv") 
sep23_df <- read_csv("202309-divvy-tripdata.csv")
oct23_df <- read_csv("202310-divvy-tripdata.csv") 
nov23_df <- read_csv("202311-divvy-tripdata.csv") 
dec23_df <- read_csv("202312-divvy-tripdata.csv")
jan24_df <- read_csv("202401-divvy-tripdata.csv")
feb24_df <- read_csv("202402-divvy-tripdata.csv") 
mar24_df <- read_csv("202403-divvy-tripdata.csv") 
apr24_df <- read_csv("202404-divvy-tripdata.csv") 

#combining all 12 months into a year
bikeshare_df <- rbind(may23_df, jun23_df, jul23_df, aug23_df, sep23_df, oct23_df, nov23_df, dec23_df, jan24_df, feb24_df, mar24_df, apr24_df)

#adding new columns
bikeshare_date <- bikeshare_df

#calculate ride length by subtracting ended_at time from started_at time and converted it to minutes
bikeshare_date$ride_length <- difftime(bikeshare_df$ended_at, bikeshare_df$started_at, units = "mins")
bikeshare_date$ride_length <- round(bikeshare_date$ride_length, digits = 1)

#create new columns for: day of week, month, day, year, time, hour
bikeshare_date$date <- as.Date(bikeshare_date$started_at) #default format is yyyy-mm-dd, use start date
bikeshare_date$day_of_week <- wday(bikeshare_df$started_at) #calculate the day of the week 
bikeshare_date$day_of_week <- format(as.Date(bikeshare_date$date), "%A") #create column for day of week
bikeshare_date$month <- format(as.Date(bikeshare_date$date), "%m")#create column for month
bikeshare_date$day <- format(as.Date(bikeshare_date$date), "%d") #create column for day
bikeshare_date$year <- format(as.Date(bikeshare_date$date), "%Y") #create column for year
bikeshare_date$time <- format(as.Date(bikeshare_date$date), "%H:%M:%S") #format time as HH:MM:SS
bikeshare_date$time <- as_hms((bikeshare_df$started_at)) #create new column for time
bikeshare_date$hour <- hour(bikeshare_date$time) #create new column for hour

#create column for different seasons: Spring, Summer, Fall, Winter
bikeshare_date <-bikeshare_date %>% mutate(season = 
                                             case_when(month == "03" ~ "Spring",
                                                       month == "04" ~ "Spring",
                                                       month == "05" ~ "Spring",
                                                       month == "06"  ~ "Summer",
                                                       month == "07"  ~ "Summer",
                                                       month == "08"  ~ "Summer",
                                                       month == "09" ~ "Fall",
                                                       month == "10" ~ "Fall",
                                                       month == "11" ~ "Fall",
                                                       month == "12" ~ "Winter",
                                                       month == "01" ~ "Winter",
                                                       month == "02" ~ "Winter")
)

#create column for different time_of_day: Night, Morning, Afternoon, Evening
bikeshare_date <-bikeshare_date %>% mutate(time_of_day = 
                                             case_when(hour == "0" ~ "Night",
                                                       hour == "1" ~ "Night",
                                                       hour == "2" ~ "Night",
                                                       hour == "3" ~ "Night",
                                                       hour == "4" ~ "Night",
                                                       hour == "5" ~ "Night",
                                                       hour == "6" ~ "Morning",
                                                       hour == "7" ~ "Morning",
                                                       hour == "8" ~ "Morning",
                                                       hour == "9" ~ "Morning",
                                                       hour == "10" ~ "Morning",
                                                       hour == "11" ~ "Morning",
                                                       hour == "12" ~ "Afternoon",
                                                       hour == "13" ~ "Afternoon",
                                                       hour == "14" ~ "Afternoon",
                                                       hour == "15" ~ "Afternoon",
                                                       hour == "16" ~ "Afternoon",
                                                       hour == "17" ~ "Afternoon",
                                                       hour == "18" ~ "Evening",
                                                       hour == "19" ~ "Evening",
                                                       hour == "20" ~ "Evening",
                                                       hour == "21" ~ "Evening",
                                                       hour == "22" ~ "Evening",
                                                       hour == "23" ~ "Evening")
)


#removing na's and duplicates
bikeshare_date <- na.omit(bikeshare_date) #remove rows with NA values
bikeshare_date <- distinct(bikeshare_date) #remove duplicate rows 
bikeshare_date <- bikeshare_date[!(bikeshare_date$ride_length <=0),] #remove where ride_length is 0 or negative
bikeshare_date <- bikeshare_date %>%  #remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng)) 

#view the final data
View(bikeshare_date)

#-----------------------------------------TOTAL RIDES--------------------------------------

#total number of rides
nrow(bikeshare_date)

#-----------------MEMBER TYPE---------------------
bikeshare_date %>%
  group_by(member_casual) %>% 
  count(member_casual)

#----------------TYPE OF BIKE---------------------

#total rides by member type 
bikeshare_date %>%
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

#total rides 
bikeshare_date %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

#-------------------HOUR--------------------------

#total rides by member type 
bikeshare_date %>%
  group_by(member_casual) %>% 
  count(hour) %>% 
  print

#total rides
bikeshare_date %>%
  count(hour) %>% 
  print

#----------------------TIME OF DAY-----------------------

#-----morning-------
#total rides by member type 
bikeshare_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#total rides
bikeshare_date %>%
  filter(time_of_day == "Morning") %>% 
  count(time_of_day)

#-----afternoon-------
#total rides by member type 
bikeshare_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#total rides 
bikeshare_date %>%
  filter(time_of_day == "Afternoon") %>% 
  count(time_of_day)

#-----evening-------
#total rides by member type
bikeshare_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#total rides
bikeshare_date %>%
  filter(time_of_day == "Evening") %>% 
  count(time_of_day)

#-----night-------
#number of rides by member type
bikeshare_date %>%
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#number of rides 
bikeshare_date %>%
  filter(time_of_day == "Night") %>% 
  count(time_of_day)

#---all times of day----
#total rides by member type 
bikeshare_date %>%
  group_by(member_casual) %>% 
  count(time_of_day)

#number of rides
bikeshare_date %>%
  group_by(time_of_day) %>% 
  count(time_of_day)

#----------------DAY OF THE WEEK------------------

#total rides by member type
bikeshare_date %>%
  group_by(member_casual) %>% 
  count(day_of_week)

#total rides 
bikeshare_date %>%
  count(day_of_week)

#----------------DAY OF THE MONTH-----------------

#total rides by member type
bikeshare_date %>%
  group_by(member_casual) %>% 
  count(day) %>% 
  print(n = 62) #lets you view the entire tibble

#total rides
bikeshare_date %>%
  count(day) %>% 
  print(n = 31) #lets you view the entire tibble

#---------------------MONTH-----------------------

#total rides by member type 
bikeshare_date %>%
  group_by(member_casual) %>% 
  count(month) %>% 
  print(n = 24) #lets you view the entire tibble

#total rides
bikeshare_date %>%
  count(month) 

#--------------------SEASON-----------------------

#-----spring-------

#total rides by member type 
bikeshare_date %>%
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  count(season)

#total rides
bikeshare_date %>%
  filter(season == "Spring") %>% 
  count(season)

#-----summer-------

#total rides by member type
bikeshare_date %>%
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  count(season)

#total rides
bikeshare_date %>%
  filter(season == "Summer") %>% 
  count(season)

#-----fall-------

#total rides by member type
bikeshare_date %>%
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  count(season)

#total rides
bikeshare_date %>%
  filter(season == "Fall") %>% 
  count(season)

#-----winter-------

#total rides by member type
bikeshare_date %>%
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  count(season)

#total rides 
bikeshare_date %>%
  filter(season == "Winter") %>% 
  count(season)

#-----all seasons-------

#total rides by member type
bikeshare_date %>%
  group_by(season, member_casual) %>% 
  count(season)

#total rides
bikeshare_date %>%
  group_by(season) %>% 
  count(season)

#------------------------------------AVERAGE RIDE LENGTH-----------------------------------

#average of ride_length
bikeshare_avgRide <- mean(bikeshare_date$ride_length)
print(bikeshare_avgRide)

#------------------MEMBER TYPE--------------------

#average ride_length
bikeshare_date %>% group_by( member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----------------TYPE OF BIKE---------------------

#total rides by member type 
bikeshare_date %>% group_by(member_casual, rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length
bikeshare_date %>% group_by(rideable_type) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----------------------HOUR-------------------------

#average ride_length by member type
bikeshare_date %>% group_by(hour, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=48) #lets you view entire tibble

#average ride_length
bikeshare_date %>% group_by(hour) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24) #lets you view entire tibble

#--------------------TIME OF DAY---------------------

#----morning----

#average ride length by member type
bikeshare_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
bikeshare_date %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----afternoon----

#average ride length by member type
bikeshare_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
bikeshare_date %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----evening----

#average ride length by member type
bikeshare_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
bikeshare_date %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----night----

#average ride length by member type 
bikeshare_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
bikeshare_date %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#---all times of day---

#average ride length by member type
bikeshare_date %>% 
  group_by(time_of_day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
bikeshare_date %>% 
  group_by(time_of_day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-------------------DAY OF THE WEEK-----------------

#average ride_length by member type
bikeshare_date %>% group_by(member_casual, day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride_length 
bikeshare_date %>% group_by(day_of_week) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----------------DAY OF THE MONTH------------------

#average ride_length by member type
bikeshare_date %>% group_by(day, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=62)  #lets you view entire tibble

#average ride_length
bikeshare_date %>% group_by(day) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=31)  #lets you view entire tibble

#---------------------MONTH--------------------------

#average ride_length by member type
bikeshare_date %>% group_by(month, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean)) %>% 
  print(n=24)  #lets you view entire tibble

#average ride_length
bikeshare_date %>% group_by(month) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----------------------SEASON-------------------------

#-----spring------

#average ride length by member type
bikeshare_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
bikeshare_date %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----summer------

#average ride length by member type for summer 
bikeshare_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length for summer 
bikeshare_date %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----fall------

#average ride length by member type
bikeshare_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
bikeshare_date %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#-----winter-----

#average ride length by member type
bikeshare_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length
bikeshare_date %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#----all seasons----

#average ride length by member type
bikeshare_date %>% 
  group_by(season, member_casual) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

#average ride length 
bikeshare_date %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length),
               list(time = mean))

view(bikeshare_date)

#created a new dataframe to use in Tableau
bikeshare_tableau <- bikeshare_date

#clean the data
bikeshare_tableau <- bikeshare_tableau %>%  #remove columns not needed: start_station_name, end_station_name, time, started_at, ended_at
  select(-c(start_station_name, end_station_name, time, started_at, ended_at))

#download the new data as a .csv file
fwrite(bikeshare_tableau,"bikeshare_data.csv")
