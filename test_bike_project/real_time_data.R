library(gbfs)
library(tidyverse)

url <- "https://gbfs.capitalbikeshare.com/gbfs/gbfs.json"
station_status <- 
  get_station_status(url, output = "return")

# get num_available_bike
real_time_df <- station_status %>%
  select(station_id,
         num_bikes_available, 
         num_docks_available,
         is_renting,
         last_updated,
         day,
         hour,
         minute) %>%
  mutate(type = "docked")
#head(real_time_df)

#head(real_time_df)

# get lat/lng
station_info <- 
  get_station_information(url, output = "return") %>% 
  select(station_id, name, lat, lon, region_id)

#head(station_info)

merge_df = merge(real_time_df, station_info, by = "station_id") %>% 
  select(station_id,
         name,
         num_bikes_available, 
         lat,
         lon)

#head(merge_df)

#try out plot
'leaflet(data = merge_df) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng = ~lon , 
                   lat=~lat, 
                   popup=~name,  
                   radius = 1,
                   color = "red"
  )'

#merge with clusters

