library(stringr)
library(data.table)
library(tidyverse)
library(lubridate)
library(leaflet)


### Transaction each month

my_theme = theme_bw() + theme(panel.grid = element_blank()) 


#fast read to just get the number of rows
files = list.files("data/data",full.names = T)
records = sapply(files, function(file){return(nrow(fread(file,data.table = F)))})
months = sapply(list.files("data/data"), function(x){return(str_split(x,'-')[[1]][1])})
months = paste0(str_sub(months, start = 0, end = 4),"\n",str_sub(months, start = 5))
records.by.month = data.frame(months = months, records = records)
ggplot(records.by.month,aes(x = months, y = records)) + 
  geom_point() +
  geom_line(group=1) +
  labs(x = "Months", y = "Number of records", title = "Number of Bikes Shared 2019-2020") + my_theme


### Data cleaning for October data
oct = fread("data/data/202010-capitalbikeshare-tripdata.csv", header = T)
sum(oct$started_at > oct$ended_at)

oct = oct %>% filter(started_at < ended_at)
start_day = date(oct$started_at)
end_day = date(oct$ended_at)

table(end_day - start_day)

oct$day = wday(start_day, label = TRUE)
oct$date = start_day
oct$week = epiweek(start_day)
oct$hour = hour(oct$started_at)
head(oct)


#1. Average Percentage of Bike Sharing In a Week.

avg_record_day = oct %>% 
  group_by(day, week) %>% 
  count() %>% 
  group_by(day) %>% 
  summarise(avg_rides = mean(n))

## Average Percentage of Bike Sharing In a Week 
ggplot(avg_record_day,aes(x = day, y = avg_rides/sum(avg_rides)*100)) +
  geom_col() + 
  geom_text(aes(label = paste0(round(avg_rides/sum(avg_rides)*100),"%"),
                vjust = -0.1)) +
  labs(x = "Day", y = "Percentage",
       title = "Average Percentage of Bike Sharing In a Week") +
  my_theme

#How the number of bike shared varies for each week:(2020-10-01 is Thursday)

df.date = oct %>% group_by(week, day) %>% count() 
ggplot(df.date) + 
  geom_point(aes(x = day, y = n)) + 
  facet_grid(rows = vars(week)) + 
  labs(x = "Day", y = "Number of Bikes Shared") +
  my_theme


#2. Explore the behavioral differences between members and casual customers.


member_vs_casual.week = oct %>% group_by(member_casual, day, week) %>% count() %>% group_by(member_casual,day) %>% summarise(avg_rides = mean(n)) %>% mutate(avg_rides = avg_rides/sum(avg_rides)*100)

ggplot(member_vs_casual.week,aes(x = day, y = avg_rides)) +
  geom_col() + 
  facet_grid(vars(member_casual)) +
  geom_text(aes(label = paste0(round(avg_rides,1),"%"), vjust = -0.1)) +
  scale_y_continuous(limits=c(0,30))+
  labs(x = "Day", y = "Percentage", title = "Average Percentage of Bike Sharing In a Week") +
  my_theme


### Busy Hours - 3. Explore the behavioral differences between members and casual customers by hour in a day. 

# ALL Days
oct %>% 
filter(started_at < ended_at) %>% 
group_by(day,member_casual, hour) %>% 
count() %>%
summarise(avg_rides = mean(n)) %>% 
mutate(avg_rides = avg_rides/sum(avg_rides)*100) %>% 
ggplot(aes(x = hour, y = avg_rides, group = member_casual, color = member_casual)) +
geom_line() + 
geom_point(size=1) + 
facet_grid(vars(day)) +
my_theme +
scale_y_continuous(limits=c(0,13))+
labs(x = "Hour", y = "Percentage",
title = "Average Percentage of Bike Sharing In a Week", 
color = "Membership")
############################################################
# Saturday only
oct %>% 
filter(day =="Sat") %>% 
group_by(member_casual, hour,date) %>% 
count() %>% 
group_by(member_casual,hour) %>% 
summarise(avg_rides = mean(n)) %>% 
mutate(avg_rides = avg_rides/sum(avg_rides)*100) %>%
ggplot(aes(x = hour, y = avg_rides, group = member_casual, color = member_casual)) + 
geom_line() +
geom_point(size = 1) +
labs(x = "Hour", y = "Percentage", 
title = "Average Percentage of Bike Sharing on Saturdays",
color = "Membership") +
my_theme

############################################################
# Sunday only
oct %>% 
filter(day =="Sun") %>% 
group_by(member_casual, hour,date) %>% 
count() %>% 
group_by(member_casual,hour) %>% 
summarise(avg_rides = mean(n)) %>% 
mutate(avg_rides = avg_rides/sum(avg_rides)*100) %>%
ggplot(aes(x = hour, y = avg_rides, group = member_casual, color = member_casual)) + 
geom_line() +
geom_point(size = 1) +
labs(x = "Hour", y = "Percentage",
title = "Average Percentage of Bike Sharing on Sundays", 
color = "Membership") +
my_theme



# trend of weekdays

# Weekdays
oct %>% 
filter(! day %in% c("Sat","Sun")) %>% 
group_by(member_casual, hour,date) %>% 
count() %>% 
group_by(member_casual,hour) %>% 
summarise(avg_rides = mean(n)) %>% 
mutate(avg_rides = avg_rides/sum(avg_rides)*100) %>%
ggplot(aes(x = hour, y = avg_rides, group = member_casual, color = member_casual)) + 
geom_line() +
geom_point(size = 1) +
labs(x = "Hour", y = "Percentage",
title = "Average Percentage of Bike Sharing on Weekdays", 
color = "Membership") +
my_theme


### Rideable_bike matters? 

bike_type.by.day = oct %>% 
group_by(day) %>% 
summarize(electric_bike = mean(rideable_type == "electric_bike")*100, 
docked_bike = mean(rideable_type == "docked_bike")*100)

ggplot(bike_type.by.day) + 
geom_col(aes(x = day, y = electric_bike)) +
labs(x = "Day", y = "Percentage", title = "Percentage of Electric Bikes Usage In a Week") +
my_theme
ggplot(bike_type.by.day) + 
geom_col(aes(x = day, y = docked_bike)) +
labs(x = "Day", y = "Percentage", title = "Percentage of Docked Bikes Usage In a Week") +
my_theme


ggplot(oct, aes(x = day, fill = rideable_type)) + 
  geom_bar(position = "fill") + 
  facet_grid(vars(member_casual)) + 
  labs(x = "Day", y = "Percentage", title = "Docked Bikes vs Electric Bikes Usage") +
  my_theme

### Duration of trips in hour
duration = oct %>% 
  mutate(dur_min = as.vector(round(difftime(ended_at,started_at, units = "mins")))) %>% 
  mutate(dur_min = round(log(dur_min+1)))

ggplot(duration,aes(x = dur_min)) + 
  geom_histogram(bins = 40) + 
  labs(title = "Log Duration Distribution", x = "Log Duration(min)") +
  scale_x_continuous(breaks = 0:8) +
  my_theme


### Choropleth Map

station_location = oct %>% 
  select(start_station_name, start_lat, start_lng) %>% 
  group_by(start_station_name) %>% 
  summarise(avg_start_lat = mean(start_lat), avg_start_lng = mean(start_lng)) %>% 
  as.data.frame() %>%  #for significant digits
  mutate(avg_start_lat = round(avg_start_lat,2), avg_start_lng = round(avg_start_lng,2))
nrow(station_location)



days.of.week <- c("Mon","Tue","Wed","Thu","Fri", "Sat", "Sun")
get_busy_df <- function(busy_hours, top_n, days_of_week, start_or_end){#may contain empty start_station_name
  df = oct
  if (start_or_end == "end"){
    df$hour = hour(df$ended_at)
  }
  return(df %>% filter(day %in% days_of_week, member_casual == "member") %>% 
           group_by(start_station_name, hour, date) %>% 
           count() %>% 
           group_by(start_station_name,hour) %>% 
           summarise(avg_rides = mean(n)) %>% 
           mutate(avg_rides = avg_rides/sum(avg_rides)*100) %>% 
           filter(hour %in% busy_hours) %>% arrange(desc(avg_rides)) %>% head(top_n) )
}


get_location_info <- function(busy_df){
  df = station_location[station_location$start_station_name %in% busy_df$start_station_name, ]
  return(merge(busy_df,df, by = "start_station_name") %>% arrange(desc(avg_rides)))
}



#Weekdays:8am and 5pm. 

# possible residential/work locations
weekdays.8am = get_busy_df(8, 10, days.of.week[1:5],"start")
weekdays.5pm = get_busy_df(17, 10, days.of.week[1:5],"start")
a = rbind(get_location_info(weekdays.8am),get_location_info(weekdays.5pm))
a$color = "red"

write.csv(a, "a.csv")
leaflet(data = a) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng = ~avg_start_lng , 
                   lat=~avg_start_lat, 
                   popup=~start_station_name, 
                   radius = 5,
                   color = ~color
  )




#Saturdays: 3pm

# possible leisure locations:park, library
weekend.3pm = get_busy_df(15, 20, days.of.week[6],"start")
weekend.3pm = get_location_info(weekend.3pm)

leaflet(data = weekend.3pm ) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng = ~avg_start_lng , 
                   lat=~avg_start_lat, 
                   popup=~start_station_name, 
                   radius = 5,
                   color = "green"
  )




#Sundays: 12-1pm(blue), 4pm(red)

weekend.12_1pm = get_busy_df(c(12,13), 10, days.of.week[7],"start")
weekend.12_1pm = get_location_info(weekend.12_1pm)
weekend.12_1pm$color = "blue"

weekend.4pm = get_location_info(get_busy_df(16, 10, days.of.week[7],"start"))
weekend.4pm$color = "red"


leaflet(data = rbind(weekend.12_1pm, weekend.4pm) ) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng = ~avg_start_lng , 
                   lat = ~avg_start_lat, 
                   popup = ~start_station_name, 
                   radius = 5,
                   color = ~color
  )







