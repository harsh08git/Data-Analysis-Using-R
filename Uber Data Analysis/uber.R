install.packages('lubridate')
install.packages('DT')
install.packages('scales')
install.packages('ggthemes')


library(tidyverse)
library(lubridate)
library(scales)
library(DT)
library(ggthemes)

setwd('Uber-dataset')
apr_data <- read_csv('uber-raw-data-apr14.csv')
may_data <- read_csv('uber-raw-data-may14.csv')
jun_data <- read_csv('uber-raw-data-jun14.csv')
jul_data <- read_csv('uber-raw-data-jul14.csv')
aug_data <- read_csv('uber-raw-data-aug14.csv')
sep_data <- read_csv('uber-raw-data-sep14.csv')
View(apr_data)

uber_data <- rbind(apr_data, may_data, jun_data, jul_data, aug_data, sep_data)
uber_data <- rename(uber_data, Date.Time = 'Date/Time')
dim(uber_data)
head(uber_data)

uber_data$Date.Time <- as.POSIXct(uber_data$Date.Time, format="%m/%d/%Y %H:%M:%S")
uber_data$Time <- format(uber_data$Date.Time, "%H:%M:%S")
head(uber_data)


#Date Variables
uber_data$Day <- factor(day(uber_data$Date.Time))
uber_data$Month <- factor(month(uber_data$Date.Time, label = TRUE))
uber_data$Year <- factor(year(uber_data$Date.Time))
uber_data$DayOfWeek <- factor(wday(uber_data$Date.Time, label=TRUE))

#Time Variables
uber_data$second <- factor(second(hms(uber_data$Time)))
uber_data$minute <- factor(minute(hms(uber_data$Time)))
uber_data$hour <- factor(hour(hms(uber_data$Time)))

#Plots

#Plot-1
uber_data %>% 
  group_by(hour) %>% 
  summarise(hourly_trips= n()) %>% 
  ggplot(.) + geom_bar(mapping = aes(x=hour, y=hourly_trips), stat = 'identity') + labs(title = 'Trips Every Hour', subtitle='Aggregated', x='Hour', y='Total Trips') + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) + scale_y_continuous(labels = comma)


#Plot-2
uber_data %>% 
  group_by(hour, Month) %>% 
  summarise(hourly_trips= n()) %>% 
  ggplot(.) + geom_bar(mapping = aes(x=hour, y=hourly_trips, fill = Month), stat = 'identity') + labs(title = 'Trips by Hour & Month', x='Hour', y='Total Trips') + theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) + scale_y_continuous(labels = comma)

#Plot-3
uber_data %>% 
  group_by(DayOfWeek) %>% 
  summarise(trips= n()) %>% 
  ggplot(.) + geom_bar(mapping = aes(x=DayOfWeek, y=trips),fill = '#cc4400' ,stat = 'identity') + labs(title = 'Trips by WeekDay', x='Day of Week', y='Total Trips') + theme(plot.title = element_text(hjust=0.5)) + scale_y_continuous(labels = comma)

#Plot-4
uber_data %>% 
  group_by(DayOfWeek, Month) %>% 
  summarise(trips= n()) %>% 
  ggplot(.) + geom_bar(mapping = aes(x= DayOfWeek, y=trips, fill = Month) ,stat = 'identity', position = 'dodge') + labs(title = 'Trips by WeekDay & Month', x='Day of Week', y='Total Trips') + theme(plot.title = element_text(hjust=0.5), panel.grid = element_blank(), panel.grid.major.y = element_line(size = 0.05, linetype = 2)) + scale_y_continuous(labels = comma) + scale_fill_manual(values = c("#00876c", "#6fab79", "#b9ce91", "#fff1b5", "#f4bb7b", "#e88159"))

#Plot-5
uber_data %>% 
  group_by(Month) %>% 
  summarise(trips= n()) %>% 
  ggplot(.) + geom_bar(mapping = aes(x= Month, y=trips, fill = Month),stat = 'identity') + labs(title = 'Monthly Trips', x='Month', y='Total Trips') + theme(plot.title = element_text(hjust=0.5)) + scale_y_continuous(labels = comma) + scale_fill_manual(values = c("#ff1111", "#ff615a", "#ff9594", "#ffc5c8", "#ff99aa", "#ff6695")) 

#Plot-6
uber_data %>% 
  group_by(Day, hour) %>% 
  summarise(trips = n()) %>% 
  ggplot(.) + geom_tile(mapping = aes(x=Day, y=hour, fill = trips), color ='white') + labs(title= "Heatmap Distribution by Day & Hour",x = 'Day', y='Hour') + scale_fill_gradient(low='#800000', high='#ff9999')


#Plot-7
uber_data %>% 
  group_by(Day, Month) %>% 
  summarise(trips = n()) %>% 
  ggplot(.) + geom_tile(mapping = aes(x=Day, y=Month, fill = trips), color ='white') + labs(title= "Heatmap Distribution by Day & Month",x = 'Day', y='Month') + scale_fill_gradient(low='#00b300', high='#000000')

#Plot-8
uber_data %>% 
  group_by(DayOfWeek, Month) %>% 
  summarise(trips = n()) %>% 
  ggplot(.) + geom_tile(mapping = aes(x=DayOfWeek, y=Month, fill = trips), color ='white') + labs(title= "Heatmap Distribution by WeekDay & Month",x = 'Day of Week', y='Month') + scale_fill_gradient(low='#ffffcc', high='#4d88ff')

#Plot-9
min_lat <- 40 
max_lat <- 40.91
min_long <- -74.15
max_long <- -73.7004

ggplot(data=uber_data) + geom_point(mapping = aes(x=Lon, y=Lat, color = Base), size=1) + scale_x_continuous(limits=c(min_long, max_long)) + scale_y_continuous(limits=c(min_lat, max_lat)) + 
  theme_map() + labs(title = "NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")

