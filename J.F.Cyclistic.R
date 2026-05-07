#title: "Riding Towards Membership: Analyzing Rider Behavior at Cyclistic Bike-Share"
#author: "Julie Favreau"

##Scenario:
#I'm a data analyst working in the marketing analytics team at a company
#named "Cyclistic", a bike-share company in Chicago. 
#I've joined this team six months ago and have been busy learning about Cyclistic's mission and business 
#goals, as well as how I, as a Data Analyst, can help Cyclistic achieve them. 
#The Director of Marketing believes the company’s future success depends on maximizing the number of annual memberships.

## Characters and Teams :

# Cyclistic: A bike-share fictional company.
# Lily Moreno: The Director of Marketing and my manager.
# Cyclistic Marketing Analytics Team: My team of Data Analysts.
# Cyclistic Executive Team: The detail-oriented executive team.

##About Cyclistic/ Key Points:

#In 2016, Cyclistic launched a successful bike-share offering. Since then, 
#the program has grown to a fleet of 5,824 bicycles that are geo-tracked and 
#locked into a network of 692 stations across Chicago. 
#The bikes can be unlocked from one station and returned to any other station.

#Until now, Cyclistic’s marketing strategy relied on building general 
#awareness and appealing to broad consumer segments.
#One approach that helped make these things possible was the flexibility of its pricing plans: 
#  single-ride passes, full-day passes, and annual memberships. 
#Customers who purchase single-ride or full-day passes are referred to as casual riders. 
#Customers who purchase annual memberships are members.

#*Cyclistic’s finance analysts have concluded that annual members are much 
#more profitable than casual riders. 
#Although the pricing flexibility helps Cyclistic attract more customers, Moreno believes that maximizing the 
#number of annual members will be key to future growth. Rather than creating
#a marketing campaign that targets all-new customers, Moreno believes there 
#is opportunity to convert casual riders into members. 

#Moreno has set a clear goal: Design marketing strategies aimed at converting
#casual riders into annual members. 
#In order to do that, the marketing analyst team needs to understand how annual members 
#and casual riders differ, why casual riders would buy a membership, and how
#digital media could affect their marketing tactics. 
#Management is interested in analyzing the Cyclistic historical bike trip data to identify trends.


##I'll be following these vital steps for the Data Analysis Process:

####PHASE 1: Ask

##In this phase:
  
#I need to answer: How do annual members and 
#casual riders use Cyclistic bikes differently?
  
##Key tasks:
#Identified the business task
#Considered the key stakeholders

##Deliverables:
##Gained insights from the data to solve the business goals.

####PHASE 2: Prepare

##I'll be using Cyclistic’s historical trip data to analyze and identify trends.

##The company has their own licence over the dataset. Besides that, the 
#dataset doesn't have any personal information about the riders. I am 
#comfortable the data isn't biased, and will have full credibility.  

#I downloaded the last 12 months of data, and stored it on my local hard drive.

#I verified the data's integrity by checking all of the files. They have consistent 
#columns and data types.

##Key tasks

#Downloaded data and stored it appropriately.
#Identified how it’s organized.
#Sorted and filtered the data.
#Determined the credibility of the data.


####PHASE 3: Process

##This Dataset is huge. I am using R Programming because it's best suited to handle data of this size.

##INSTALL AND/OR UPDATE PACKAGES

update.packages(ask = FALSE, checkbuilt = TRUE)
if (FALSE) {conflicted_update()}
if (FALSE) {tidyverse_update()}
if (FALSE) {lubridate_update()}
if (FALSE) {janitor_update()}
if (FALSE) {skimr_update()}
if (FALSE) {scales_update()}
if (FALSE) {dplyr_update()}
if (FALSE) {readr_update()}
if (FALSE) {devtools_update()}
if (FALSE) {RQDAassist_update()}
if (FALSE) {gwalkr_update()}
if (FALSE) {duckdb_update()}
if (FALSE) {shinycssloaders_update()}


#LOAD NECESSARY LIBRARIES
#USE THE CONFLICTED PACKAGE TO MANAGE CONFLICTS

library(conflicted)
library(tidyverse)
suppressPackageStartupMessages(library(installr))
library(lubridate)
library(janitor)
library(skimr)
library(scales)
library(dplyr)
library(readr)
library(devtools)
library(usethis)
library(GWalkR)
library(duckdb)
library(shinycssloaders)
library(RSQLite)
library(RQDAassist)

conflicted::conflicts_prefer(dplyr::filter)

rm(list=ls())


##Data Collection: Collect and Import the Dataset
##Downloaded the .csv files from Kaggle's public repository of csv files. Data sets are provided by Motivate International Inc. and uploaded to Kaggle via the following shared data set: "Cyclistic".There are twelve monthly files Date Range: 2020-04-01 through 2021-03-31. 
##I then set the directory to start from my hard drive on my computer. D:/Data/Rstudio/Cyclistic

t1 <- read.csv("D:/RStudio/Cyclistic/Data/202101-divvy-tripdata/202101-divvy-tripdata.csv")
t2 <- read.csv("D:/RStudio/Cyclistic/Data/202102-divvy-tripdata/202102-divvy-tripdata.csv")
t3 <- read.csv("D:/RStudio/Cyclistic/Data/202103-divvy-tripdata/202103-divvy-tripdata.csv")
t4 <- read.csv("D:/RStudio/Cyclistic/Data/202004-divvy-tripdata/202004-divvy-tripdata.csv")
t5 <- read.csv("D:/RStudio/Cyclistic/Data/202005-divvy-tripdata/202005-divvy-tripdata.csv")
t6 <- read.csv("D:/RStudio/Cyclistic/Data/202006-divvy-tripdata/202006-divvy-tripdata.csv")
t7 <- read.csv("D:/RStudio/Cyclistic/Data/202007-divvy-tripdata/202007-divvy-tripdata.csv")
t8 <- read.csv("D:/RStudio/Cyclistic/Data/202008-divvy-tripdata/202008-divvy-tripdata.csv")
t9 <- read.csv("D:/RStudio/Cyclistic/Data/202009-divvy-tripdata/202009-divvy-tripdata.csv")
t10 <- read.csv("D:/RStudio/Cyclistic/Data/202010-divvy-tripdata/202010-divvy-tripdata.csv")
t11 <- read.csv("D:/RStudio/Cyclistic/Data/202011-divvy-tripdata/202011-divvy-tripdata.csv")
t12 <- read.csv("D:/RStudio/Cyclistic/Data/202012-divvy-tripdata/202012-divvy-tripdata.csv")


##Read all csv files in the directory and use rbind to merge them into a single data frame showing the one year view
trips <- rbind(t4, t5, t6, t7, t8, t9, t10, t11, t12, t1, t2, t3)

##Took a quick peek at the data I'm working with
glimpse(trips)

##Reviewed the beginning of the data frame
head(trips)

##Reviewed the end of the data frame
tail(trips)

##Reviewed Summary for the data frame
summary(trips)

##Cross-checked the data integrity
str(trips)

##Saved merged Data :
write.csv(trips, file = "trips.csv")

##Removed "latitude" & "longitude" columns. 
trips <- trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))

##Summary of Data
summary(trips) 

##Checked number of riders as casual and member:
table(trips$member_casual) 

##CLEAN THE DATA

#After reviewing the data, I reached out to Lily Moreno, the Director of Marketing, and raised the following concerns:
#1. Total rows: 3,489,748
#2. 10,552 rows show negative negative trip duration
#3. 122,175 rows have missing starting station names
#4. As many as 1,291 stations are showing two different start station id's

#Lily advised me to ignore rows with trip duration <=0. 
#I could also ignore rows with missing start station names, and missing start station id's.


## Added columns as date, month, day, year and day_of_week for analysis and formatted as date:
trips$date <- as.Date(trips$started_at)
trips$day <- format(as.Date(trips$date), "%D")
trips$month <- format(as.Date(trips$date), "%B")
trips$year <- format(as.Date(trips$date), "%Y")
trips$day_of_week <- format(as.Date(trips$date), "%A")


## Added columns as pick_time & drop_time and formatted as datetime :
trips$pick <- as_datetime(trips$started_at)
trips$pick_time <- format(as_datetime(trips$pick), "%H:%M:")

trips$drop <- as_datetime(trips$ended_at)
trips$drop_time <- format(as_datetime(trips$drop), "%H:%M")

##Took a Peek at the Data:
glimpse(trips)

##Removed the 'pick' & 'drop' columns :
trips <- trips %>% 
  select(-c(pick,drop))

##Checked for Duplicates:
trips<- distinct(trips)

##Took a Peek at the Data:
glimpse(trips)

##Added a column 'ride_length' 
trips$ride_length <- difftime(trips$ended_at,trips$started_at)

##Confirmed data is not categorized as a Factor, and that data is categorized as Numeric:
is.factor(trips$ride_length) 
is.numeric(trips$ride_length) 
trips$ride_length <- as.numeric(as.duration(trips$ride_length))
is.numeric(trips$ride_length)


######PHASE 4 : Analyze
#The "Minimum Ride Duration by Riders" contains negative values. 
#The data contains negative values due to bikes that were taken out of docks for quality checks,  or it may be due to other reasons. 
#Negative data should be removed before analysis.
#A new data frame will be created by removing negative values.


##Created a new data frame copy of the original data:
trips_new <- trips


##Data Summary

##Average Ride Duration
mean(trips_new$ride_length) 

##Median Ride Duration
median(trips_new$ride_length)  

##Maximum Ride Duration
max(trips_new$ride_length) 

##Minimum Ride Duration
min(trips_new$ride_length) 

##Summary of Ride Duration
summary(trips_new$ride_length) 

##Removed the Bad and Negative Data
trips_new <- trips_new[!trips_new$ride_length <1,]


##This cleaning step removed 10,938 rows of bad data. Now the data frame shows 3,478,810 rows, and 17 columns.
#Cleaning the data by removing NA's, and removing empty values reduced the rows to 3,478,810.


##Took a Peek at the Data Again
str(trips_new)  

##Summary of ride duration :
summary(trips_new$ride_length)

##Saved merged Data:
write.csv(trips_new, file = "trips_new.csv")


##COMPARISON OF RIDE DURATION 
##Average
aggregate(trips_new$ride_length~trips_new$member_casual, FUN= mean) 

##Median
aggregate(trips_new$ride_length~trips_new$member_casual, FUN= median) 

##Minimum
aggregate(trips_new$ride_length~trips_new$member_casual, FUN= min) 

##Maximum
aggregate(trips_new$ride_length~trips_new$member_casual, FUN= max)  


##Average Ride Duration for Each Day by Members vs Casual Users:
aggregate(trips_new$ride_length~trips_new$member_casual+
            trips_new$day_of_week, FUN= mean)

##Days of the Week are Not in Order. I Reordered them, Grouping the Weekend Days Together, to Easily Spot Weekday Trends vs. Weekend Trends
trips_new$day_of_week <- ordered(trips_new$day_of_week, 
                                    levels = c("Monday",
                                               "Tuesday","Wednesday",
                                               "Thursday","Friday","Saturday", "Sunday"))

##Average Ride Duration for Each Day by Casual vs. Member users:
aggregate(trips_new$ride_length~trips_new$member_casual+
            trips_new$day_of_week, FUN= mean)

##Took a Peek at the Data:
glimpse(trips_new)



##PHASE 5: Data Visualization

##Created a Separate Dataframe for "Data Visualization 1":
rides_per_day <- trips_new %>% 
  group_by(member_casual, day_of_week, rideable_type) %>% 
  summarise(number_of_rides = n(),
            median_duration = median(ride_length),
            maximum_duration = max(ride_length),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week, rideable_type)


##Converted to Data Frame 
rides_per_day <- as.data.frame(rides_per_day)

##Took a Peek at the Data
glimpse(rides_per_day)


##Used the Newly Converted Dataframe "rides_per_day" 

##Data Visualization 1A: Weekly "Number of Rides" for all users:
ggplot(data = rides_per_day, 
       mapping=  aes(x = day_of_week, y = number_of_rides, fill= member_casual)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = scales::comma)+
  labs( x = "Day of Week", y = "Number of Rides", fill = "Member vs.Casual" , 
        title = "Average Number of Rides Per Day : Member vs Causal",
        subtitle = "High Peak During the Weekend by Casual Riders",
        caption = "Data Analyst   J.Favreau")


##Data Visualization 1B: Weekly "Average Duration" for all users:
ggplot(data= rides_per_day, 
       mapping = aes(x = day_of_week , y = average_duration, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Day of Week", y = "Average Duration", fill = "Member vs. Casual",
       title = " Average Ride Duration Per Day : Member vs Casual", 
       subtitle = "Overall Average of Ride Duration by Casual Riders is High",
       caption = "Data Analyst   J.Favreau")

##Findings: The "Casual Riders" Average Duration for Bike Rides is much higher than for "Members"  

#DATA VISUALIZATION 1C : WEEKLY "MEDIAN DURATION" FOR ALL USERS
ggplot(data= rides_per_day, 
       mapping = aes(x = day_of_week , y = median_duration, fill = member_casual)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Day of Week", y = "Average Duration", fill = "Member vs. Casual",
       title = " Median Ride Duration Per Day : Member vs Casual",
       subtitle = "Overall Average Ride Duration is Higher for Casual Riders",
       caption = "Data Analyst   J.Favreau")

##FINDINGS: CASUAL RIDERS ARE RIDING BIKES OVERALL FOR LONGER PERIODS OF TIME THAN MEMBERS




##Data Visualization 1D: Riders Peak Daily Rate by Bike Type:
ggplot(data = rides_per_day) +
  geom_count(mapping = aes (x = day_of_week , y = number_of_rides, color= rideable_type, shape = member_casual)) +
  scale_y_continuous(labels = scales::comma)

##Findings: The Docked Bike is the most chosen bike type by riders.


##Data Visualization 1E: Average Daily Ride Duration by Bike Type:
ggplot(data= rides_per_day, 
       mapping = aes(x = day_of_week , y = average_duration, color = rideable_type,
                     fill = member_casual, shape = rideable_type)) +
  geom_point() +
  facet_wrap(~member_casual) +
  labs(x = "Day of Week", y = "Average Duration", shape = "Bike Types ", fill = "Member vs. Casual",
       title = "Average Daily Ride Duration by Bike Type: Member vs Casual", 
       subtitle = "The Docked Bike is the Most Chosen Bike Type by Riders.",
       caption = "Data Analyst   J.Favreau")

##Findings: "Casual Riders" prefer to ride "Docked Bikes" for a long amount of time.


##Data Visualization 1F :Peak Daily Rate by Riders:
ggplot(data= rides_per_day, 
       mapping = aes(x = member_casual , y = number_of_rides, fill = day_of_week)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Day of the Week", y = "Number of Rides", fill = "Member vs. Casual",
       title = "Rides Used by Member & Casual", 
       subtitle = "Rides Peak on Weekends for Casual Riders ",
       caption = "Data Analyst   J.Favreau")

##Findings: The number of rides taken by Casual riders is higher than Member riders.  
##Rides Peak on Weekends for Casual Riders 


##To Get Additional Detailed Visualizations, Manipulation of the Original Data was Needed:
trips$drop <- as_datetime(trips$ended_at)
trips$drop_time <- format(as_datetime(trips$drop), "%H:%M")

##Took a Peek at the Data:
glimpse(trips)

##Removed The Unneeded Column(s)
trips <- trips %>% 
  select(-c(drop))

##Now the Original Data is Intact

##Take a Peek at the Data Again
glimpse(trips) 

##Created Another Dataframe for Further Data Visualizations:
trips_new2 <- trips_new

##Manipulation in Newly Created Dataframe for Data Visualization 2:
trips_new2$pick <- as_datetime(trips_new2$started_at)
trips_new2$pick_hour <- format(as_datetime(trips_new2$pick), "%H")

trips_new2$drop <- as_datetime(trips_new2$ended_at)
trips_new2$drop_hour <- format(as_datetime(trips_new2$drop), "%H")

##Removed an Unnecessary Column:
trips_new2 <- trips_new2 %>% 
  select(-c(pick, drop))

##Take a Peek at the Data Again
glimpse(trips_new2) 


##Riders picking up bikes and then returning bikes, based on hour of the day.
  
## Data Visualization 2A: Riders Taking Bikes Peak Hour of Day:
ggplot(data= trips_new2,
       mapping = aes(x = pick_hour, fill = member_casual)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Bikes Taken Hour 00:00 To 23:00", y = "Number of Rides Taken", fill = "Member vs.Casual",
       title = "Beginning Ride Hours by Member & Casual", 
       subtitle = "Highest Peak Rate During the Afternoon",
       caption = "Data Analyst   J.Favreau")


##Data Visualization 2B: Riders Bike Drop Off Hour on time basis:
ggplot(data= trips_new2,
       mapping = aes(x = drop_hour, fill = member_casual)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Bikes' Drop Off Hour 00:00 To 23:00", y = "Number of Rides Taken", fill = "Member vs.Casual",
       title = "Bikes Drop Off Hours by Member & Casual", 
       subtitle = "High Bike Return Rate Also During the Afternoon",
       caption = "Data Analyst   J.Favreau")


##Manipulation in newly created dataframe for Data visualization 3:
trips_new2$month1 <- as.Date(trips_new2$started_at)
trips_new2$month_year <- format(as.Date(trips_new2$month1), "%M %Y")

##Removed The Unneeded Column(s)
trips_new2 <- trips_new2 %>% 
  select(-c(month1))

##Take a Peek at the Data Again
glimpse(trips_new2) 

##Data Visualization 3A: Monthly Rider Engagement:
ggplot(data= trips_new2,
       mapping = aes(x = month_year, fill = member_casual)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Month-Year", y = "Number of Rides Taken", fill = "Member vs. Casual",
       title = " Monthly Rides Taken by Member vs Casual ", 
       subtitle = "Casual Riders Take Less Rides in Winter Season than Members",
       caption = "Data Analyst  J.Favreau")

##Casual Riders" Engagement During Winter is Considerably Low Compared to Members.   

##Top 10 Start Stations for Casual Riders
trips_new2 %>%
  filter(member_casual == "casual" & start_station_name!= "") %>%
  group_by(start_station_name) %>%
  summarize(num_of_rides = n()) %>%
  arrange(-num_of_rides) %>%
  head(10) %>%
  ggplot(aes(x = reorder(start_station_name, num_of_rides), y = num_of_rides, fill = start_station_name)) +
  geom_col(alpha = 0.6) +
  theme(text = element_text(size = 10)) +
  labs(title = "Top 10 Start Stations", subtitle = " Casual Riders Preferences", x = "Start Station") +
  coord_flip()


###### PHASE 6: ACT


##I presented my findings to my manager, Lily Moreno, and our management team. 

##I used data-driven decision-making to determine the following:

##Casual Rider engagement during the winter is much lower than Member Riders, and winter ridership overall is low. 
#This is most likely due to being located in Chicago, which experiences severe winter weather.

#I recommended launching our new membership drives in late spring season. This will encourage casual riders to see the value that membership makes available to them throughout the busy spring, summer and fall seasons.

#I recommended launching promotions via Social Media such as: Instagram and Facebook. Cyclistic can also use casual member's existing user information on file to reach out to casual members via text and email promotions. 
#I recommended launching physical promotional materials at the top 10 most frequented stations by current casual members. These stations will receive the most casual rider traffic.





