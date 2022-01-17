# Load required packages
library(tidyverse)
library(ggmap)
library(osmdata)
library(sf)

park_coords <- allPlants_allParks %>% 
  dplyr::select(Park, Latitude_park, Longitude_park) %>% 
  distinct()

ibutton_df <- iButton_summaries %>% 
  mutate(Long = Long * -1) %>% 
  dplyr::select(Park, Lat, Long) %>% 
  distinct()

allParks_bbox <- make_bbox(park_coords$Longitude_park, park_coords$Latitude_park, f = 0.1)
long <- (allParks_bbox['left'] + allParks_bbox['right']) / 2
lat <- (allParks_bbox['top'] + allParks_bbox['bottom']) / 2
allParks_map <- get_googlemap(center=c(long,lat), zoom = 10, maptype = "satellite", source = 'google')

ggmap(allParks_map) +
  geom_point(data = park_coords, aes(x = Longitude_park, y = Latitude_park), 
             size = 4, alpha = 0.9, shape = 21, fill = 'red')


humber_coords <- allPlants_allParks %>% filter(Park == 'Humber')
humber_bbox <- make_bbox(humber_coords$Longitude_plant, humber_coords$Latitude_plant, f = 0.1)
long <- (humber_bbox['left'] + humber_bbox['right']) / 2
lat <- (humber_bbox['top'] + humber_bbox['bottom']) / 2
humber_map <- get_map(humber_bbox, zoom = 15, maptype = "satellite", source = 'google')

ggmap(humber_map) +
  geom_point(data = allPlants_allParks %>% filter(Park == 'Humber'),
             aes(x = Longitude_plant, y = Latitude_plant),
             size = 1, alpha =.5, shape = 21, fill = "darkred") +
  geom_point(data = ibutton_df %>% filter(Park == 'Humber'), 
             aes(x = Long, y = Lat), 
             size = 2, alpha = 1, shape = 22, fill = "purple")


ggplot(data = ibutton_df %>% filter(Park == 'Humber'), aes(x = Long, y = Lat)) + geom_point()
