#Install and load the packages we need to job
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)  #helps visualize data
library(readr)
library(lubridate)

# adding a datasets
X2022_01<-read_delim("202201-divvy-tripdata-clean.csv")
colnames(X2022_01)
X2022_02<-read_delim("202202-divvy-tripdata-clean.csv")
colnames(X2022_02)
X2022_03<-read_delim("202203-divvy-tripdata-clean.csv")
colnames(X2022_03)

#review our data frames
str(X2022_01)
str(X2022_02)
str(X2022_03)

# Great one big data frame to analysis
all_2022<-bind_rows(X2022_01,X2022_02,X2022_03)
View(all_2022)

colnames(all_2022)  #List of column names
nrow(all_2022)  #How many rows are in data frame?
dim(all_2022)  #Dimensions of the data frame?
head(all_2022)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_2022)  #See list of columns and data types (numeric, character, etc)

# Calculate ride length for every ride id 
all_2022$ride_length <- difftime(all_2022$ended_at,all_2022$started_at)
#Change a data type to numeric for our case shows time in seconds.
all_2022$ride_length <- as.numeric(as.character(all_2022$ride_length))
is.numeric(all_2022$ride_length)
head(all_2022)
View(all_2022)

#Checking a number of NA values
sum(is.na(all_2022$ride_length))

#data cleaning, remove NA values
all_2022_v2<-na.omit(all_2022)

# Descriptive analysis on ride_length (all figures in seconds)
mean(all_2022_v2$ride_length) #straight average (total ride length / rides)
median(all_2022_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_2022_v2$ride_length) #longest ride
min(all_2022_v2$ride_length) #shortest ride

#  Using summary() to show the specific attribute
summary(all_2022_v2$ride_length)

# Compare members and casual users
aggregate(all_2022_v2$ride_length ~ all_2022_v2$member_casual, FUN = mean)
aggregate(all_2022_v2$ride_length ~ all_2022_v2$member_casual, FUN = median)
aggregate(all_2022_v2$ride_length ~ all_2022_v2$member_casual, FUN = max)
aggregate(all_2022_v2$ride_length ~ all_2022_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users

aggregate(all_2022_v2$ride_length ~ all_2022_v2$member_casual + all_2022_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_2022_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by user type and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_2022_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_2022_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
#Create data frame MyCounts to show results of my analysis 
MyCounts <- aggregate(all_2022_v2$ride_length ~ all_2022_v2$member_casual + all_2022_v2$day_of_week, FUN = mean)

write.csv(MyCounts,file = '~/Documents/avg_ride_length.csv')
write.csv(all_2022_v2,file = '~/Documents/Project Cyclistic/all_2022_v2.csv')
