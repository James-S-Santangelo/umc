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

#### GOOGLE MAPS VERSION ####

allParks_bbox <- make_bbox(park_coords$Longitude_park, park_coords$Latitude_park)
long <- (allParks_bbox['left'] + allParks_bbox['right']) / 2
lat <- (allParks_bbox['top'] + allParks_bbox['bottom']) / 2
allParks_map <- get_googlemap(center=c(long,lat), zoom = 10, maptype = "satellite", source = 'google')

ggmap(allParks_map) +
  geom_point(data = park_coords, aes(x = Longitude_park, y = Latitude_park), 
             size = 3, alpha = 0.65, shape = 21, fill = 'red') +
  xlab("Longitude") + ylab('Latitude') +
  # scale_x_continuous(limits = c(-79.7, -79.1), expand = c(0, 0)) +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15))
  
humber_coords <- allPlants_allParks %>% filter(Park == 'Humber')
humber_bbox <- make_bbox(humber_coords$Longitude_plant, humber_coords$Latitude_plant)
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

#### STAMEN VERSION ####

allParks_bbox <- make_bbox(park_coords$Longitude_park, park_coords$Latitude_park, f = 0.1)
long <- (allParks_bbox['left'] + allParks_bbox['right']) / 2
lat <- (allParks_bbox['top'] + allParks_bbox['bottom']) / 2
allParks_map <- get_map(location = allParks_bbox, zoom = 11, source = 'stamen')

toronto_map <- ggmap(allParks_map) +
  geom_point(data = park_coords, aes(x = Longitude_park, y = Latitude_park), 
             size = 3, alpha = 0.65, shape = 21, fill = 'red') +
  xlab("Longitude") + ylab('Latitude') +
  # scale_x_continuous(limits = c(-79.7, -79.1), expand = c(0, 0)) +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15))

ggsave(filename = 'analysis/toronto_map.png', plot = toronto_map, device = 'png', dpi = 600,
       height = 6, width = 8, units = 'in')



humber_coords <- allPlants_allParks %>% filter(Park == 'Humber')
humber_bbox <- make_bbox(humber_coords$Longitude_plant, humber_coords$Latitude_plant, f = 0.1)
long <- (humber_bbox['left'] + humber_bbox['right']) / 2
lat <- (humber_bbox['top'] + humber_bbox['bottom']) / 2
humber_map <- get_map(humber_bbox, zoom = 15, maptype = "satellite", source = 'google')

humber_map <- ggmap(humber_map) +
  geom_point(data = allPlants_allParks %>% filter(Park == 'Humber'),
             aes(x = Longitude_plant, y = Latitude_plant),
             size = 2, alpha =0.6, shape = 21, fill = "darkred") +
  geom_point(data = ibutton_df %>% filter(Park == 'Humber'), 
             aes(x = Long, y = Lat), 
             size = 3, alpha = 1, shape = 22, fill = "orange") +
  xlab("Longitude") + ylab('Latitude') +
  # scale_x_continuous(limits = c(-79.7, -79.1), expand = c(0, 0)) +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15))

ggsave(filename = 'analysis/humber_map.png', plot = humber_map, device = 'png', dpi = 600,
       height = 4, width = 10, units = 'in')

ggplot(data = ibutton_df %>% filter(Park == 'Humber'), aes(x = Long, y = Lat)) + geom_point()
