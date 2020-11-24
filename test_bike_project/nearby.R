library(tidyverse)
library(gbfs)


### Predict by Nearest Stations

#1. find the nearest 5 stations, all of those should be within 300m. (Why 300m: https://usa.streetsblog.org/2015/04/29/nacto-if-you-want-bike-share-to-succeed-put-stations-close-together/)

library(geosphere)
url = "https://gbfs.capitalbikeshare.com/gbfs/gbfs.json"
station_info <- 
  get_station_information(url, output = "return") %>% 
  select(station_id, name, lat, lon)

#The geographical distance, 609 stations provided
distance.mat = distm (as.matrix(station_info[,c("lon",'lat')]),fun = distGeo)
rownames(distance.mat) = station_info$station_id
colnames(distance.mat) = station_info$station_id
distance.mat[1:10,1:10]

# input: target id and distance matrix
# output: a data frame of nearby stations and their lat/lon
find_nearest_5 <-function(target_id, distance.mat){
  distance_info = distance.mat[as.character(target_id),]
  within_300 = distance_info[distance_info<300]
  if (length(within_300) <2){
    nearby_id = c()
  }
  else{
    sorted_lst = sort(within_300)[-1]
    nearby_id = names(sorted_lst)
  }
  return (station_info %>% filter(station_id %in% nearby_id))
}

find_nearest_5(1, distance.mat)

# only about 45% percent of the stations has at least one nearby station
# try AB testing in the future
sum(sapply(station_info$station_id, function(x) return(nrow(find_nearest_5(x, distance.mat))))>=1)/length(station_info$station_id)




### Free bikes(only used in Web App)

# ebike not atached to docks, but around the station, has an extra fee of $2
url <- "https://gbfs.capitalbikeshare.com/gbfs/gbfs.json"
free_bike_df <- get_free_bike_status(url, output = "return") %>%
  select(id = bike_id, lon, lat) 

find_nearest_free_bike <-function(target_id){
  target_info = station_info %>% 
    filter(station_id == target_id) %>%
    select(lon, lat)
  combined_df = rbind(target_info,free_bike_df[,-1])
  distance.mat = distm (as.matrix(combined_df), fun = distGeo)
  rownames(distance.mat) = c(target_id, free_bike_df$id)
  return(find_nearest_5(target_id, distance.mat))
}

find_nearest_free_bike(5)
