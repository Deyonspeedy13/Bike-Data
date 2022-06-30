install.packages("tidyverse") ##Install needed packages and load packages
library(tidyverse)
library(lubridate)
install.packages("ggplot2")
library(ggplot2)

getwd() ##Find out work directory

##Import all files
Mar_2021 <- read_csv("Mar_2021_trip_data.csv")
Apr_2021 <- read_csv("Apr_2021_trip_data.csv")
May_2021 <- read_csv("May_2021_trip_data.csv")
Jun_2021 <- read_csv("Jun_2021_trip_data.csv")
Jul_2021 <- read_csv("Jul_2021_trip_data.csv")
Aug_2021 <- read_csv("Aug_2021_trip_data.csv")
Sept_2021 <-read_csv("Sept_2021_trip_data.csv")
Oct_2021 <- read_csv("Oct_2021_trip_data.csv")
Nov_2021 <- read_csv("Nov_2021_trip_data.csv")
Dec_2021 <- read_csv("Dec_2021_trip_data.csv")
Jan_2022 <- read_csv("Jan_2022_trip_data.csv")
Feb_2022 <- read_csv("Feb_2022_trip_data.csv")

##Look at column names to make sure all are named the same
colnames(Mar_2021)
colnames(Apr_2021)
colnames(May_2021)
colnames(Jun_2021)
colnames(Jul_2021)
colnames(Aug_2021)
colnames(Sept_2021)
colnames(Oct_2021)
colnames(Nov_2021)
colnames(Dec_2021)
colnames(Jan_2022)
colnames(Feb_2022)

##Look at data types for every column 
glimpse(Mar_2021)
glimpse(Apr_2021)
glimpse(May_2021)
glimpse(Jun_2021)
glimpse(Jul_2021)
glimpse(Aug_2021)
glimpse(Sept_2021)
glimpse(Oct_2021)
glimpse(Nov_2021)
glimpse(Dec_2021)
glimpse(Jan_2022)
glimpse(Feb_2022)



##Combine into one data frame
Year_trip_data <-bind_rows(Mar_2021,Apr_2021,May_2021,Jun_2021,Jul_2021,
                           Aug_2021,Sept_2021,Oct_2021,Nov_2021,
                           Dec_2021,Jan_2022,Feb_2022)

##View column names for new data frame and data types 
colnames(Year_trip_data)
glimpse(Year_trip_data) 
##Add date, month, day, and year columns to dataset
Year_trip_data$date <- as.Date(Year_trip_data$started_at, format = "%m/%d/%Y")
Year_trip_data$month <- format(as.Date(Year_trip_data$date), "%B")
Year_trip_data$day <- format(as.Date(Year_trip_data$date), "%d")
Year_trip_data$year <- format(as.Date(Year_trip_data$date), "%Y")
Year_trip_data$day_of_week <- format(as.Date(Year_trip_data$date), "%A")

##delete rows where ride length is less than or equal to 0
Year_trip_data <-Year_trip_data[!(Year_trip_data$ride_length <= 0),] 
View(Year_trip_data)

##convert ride_length to numeric class
Year_trip_data$ride_length <- as.numeric(Year_trip_data$ride_length)
class(Year_trip_data$ride_length) ##check class of ride_length
Year_trip_data$ride_length <- (Year_trip_data$ride_length)/60 ##divide by 60 to get mins
View(Year_trip_data)
summary(Year_trip_data$ride_length) ##view descriptive stats about ride_length

##Compare members and casual riders about ride_length
aggregate(Year_trip_data$ride_length~Year_trip_data$member_casual, FUN = mean)
aggregate(Year_trip_data$ride_length~Year_trip_data$member_casual, FUN = median)
aggregate(Year_trip_data$ride_length~Year_trip_data$member_casual, FUN = max)
aggregate(Year_trip_data$ride_length~Year_trip_data$member_casual, FUN = min)

## since casual members have the higher mean, median, and max >>> Spending more time on trips

aggregate(Year_trip_data$ride_length ~ Year_trip_data$member_casual + Year_trip_data$day_of_week, FUN = mean)
##Order the weekdays
Year_trip_data$day_of_week <- ordered(Year_trip_data$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(Year_trip_data$ride_length ~ Year_trip_data$member_casual + Year_trip_data$day_of_week, FUN = mean)


##Count the number of days in the week
days <-count(Year_trip_data,day_of_week)

##View a tibble of days 1=Sunday, 2=Monday etc.
tibble(days)
days <-rename(days,num_of_trips = n) ##rename 2nd column
View(days)
days$day_of_week <- ordered(days$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
Best_day_for_trips <- max(days$num_of_trips) ##largest trip
Worst_day_for_trips <- min(days$num_of_trips) ##smallest trip


Months <- count(Year_trip_data,month) ##See trips per month
Months <- rename(Months, trips_per_month = n) ##rename 2nd column
view(Months)
##order months to prepare graph 
Months <- mutate(Months, month = factor(month, levels = month.name))

Member <- count(Year_trip_data,member_casual) ##count the number of members
View(Member)
Member <- Member %>% rename(Total_members = n) ##rename 2nd column
View(Members)

ggplot(data = Year_trip_data) +
geom_bar(mapping = aes(x = day_of_week, fill = member_casual)) +
labs(title = "Number of trips per day throughout the year",
     subtitle = "From March 2021 to February 2022",
     x = "Weekday",
     y = "Number of trips")+
annotate("text", x = 5, y = 9e+05,label = "Sat is the best day")


##plot the number of trips per month
ggplot(data = Year_trip_data)+
geom_bar(mapping = aes (x = month, fill = member_casual))+
labs(title = "Total amount of trips throughout the year",
     subtitle = "From March 2021 to February 2022",
     x = "Month",
     y = "Number of trips per month")


##plot members graph
ggplot(data = Year_trip_data) +
geom_bar(mapping = aes (x = member_casual))+
labs(title = "Number of members",
     subtitle = "From March 2021 to February 2022",
     x = "Member/Casual",
     y = "Total")

ride_type <- count(Year_trip_data,rideable_type) ##count number of rideable_type
ride_type <- rename(ride_type,Total = n)
view(ride_type)

##plot rideable_type per member_casual 
ggplot(data = Year_trip_data)+
geom_bar(mapping = aes(x = rideable_type))+
labs(title = "Types of rides amongst members",
     x = "Type of ride",
     y = "Total per ride type")+
facet_wrap(~member_casual)

##Plot Mean between members sorted by day
Year_trip_data%>%
group_by(member_casual,day_of_week)%>%
summarize(duration=mean(ride_length))%>%
ggplot(mapping=aes(x=member_casual,y=duration, fill = day_of_week)) +
geom_bar(position="Dodge",stat = "identity") +
  labs(title = "Average ride length by weekday",
       x = "Member/Casual",
       y = "Duration")+ 
facet_wrap(~day_of_week)


##Plot Mean between members per month
Year_trip_data%>%
group_by(member_casual,month)%>%
summarize(duration=mean(ride_length))%>%
ggplot(mapping=aes(x=member_casual,y=duration, fill = month)) +
geom_bar(position="Dodge",stat = "identity") +
labs(title = "Average ride length by month",
     subtitle = "From March 2021 to February 2022",
      x = "Member/Casual",
      y = "Duration")+
facet_wrap(~month)

counts <- aggregate(Year_trip_data$ride_length ~ Year_trip_data$member_casual + Year_trip_data$day_of_week, FUN = mean)
counts
library(readr)
write_csv(counts,"C:\\Users\\deyon\\OneDrive\\Desktop\\Clean_Bike.csv" )




