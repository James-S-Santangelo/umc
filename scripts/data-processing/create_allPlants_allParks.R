# Script to clean up raw data and create final merged dataset
#
# Author: James S. Santangelo

#### ADD DISTANCE TO ORIGINAL DATASETS ####

# Load in all datasets with relevant columns and remove notes
erindale_data <- read_csv("data-raw/csv/HCN_assays/Erindale_Park.csv") %>% 
  select(Park:Li, -matches("Notes")) %>% 
  na.omit()
high_data <- read_csv("data-raw/csv/HCN_assays/High_Park.csv") %>% 
  select(Park:Li, -matches("Notes")) %>% 
  na.omit()
humber_data <- read_csv("data-raw/csv/HCN_assays/Humber_Park.csv") %>% 
  select(Park:Li, -matches("Notes")) %>% 
  na.omit()
riverdale_data <- read_csv("data-raw/csv/HCN_assays/Riverdale_Park.csv") %>% 
  select(Park:Li, -matches("Notes")) %>% 
  na.omit()
rouge_data <- read_csv("data-raw/csv/HCN_assays/Rouge_Park.csv") %>% 
  select(Park:Li, -matches("Notes")) %>% 
  na.omit()
park_centres <- read_csv("data-raw/Park_centres.csv") %>% 
  separate(Latitude_park, sep = "[ ]", into = c("dir_lat", "dec_deg_lat")) %>% 
  separate(Longitude_park, sep = "[ ]", into = c("dir_long", "dec_deg_long")) %>% 
  mutate(Latitude_park = as.numeric(dec_deg_lat),
         Longitude_park = -1 * as.numeric(dec_deg_long)) %>% 
  select(-matches("dir|dec"))

# Add distance to all park datasets
erindale_modified <- add_distance(erindale_data)
humber_modified <- add_distance(humber_data)
high_modified <- add_distance(high_data)
riverdale_modified <- add_distance(riverdale_data)
rouge_modified <- add_distance(rouge_data)

# Merge all dataframes 
data_merged <- bind_rows(erindale_modified, humber_modified, high_modified,
                         riverdale_modified, rouge_modified)

#### ADD VEGETATION TO DATASETS ####

# Load datasets with % vegetation surrounding each plant
erindale_veg <- read_csv("data-raw/csv/vegetation/Erindale_Park_vegetation.csv") %>% 
  select(Park, Plant, "Vegetation cover m²":"% Asphalt") %>% 
  na.omit()
high_veg <- read_csv("data-raw/csv/vegetation/High_Park_vegetation.csv") %>% 
  select(Park, Plant, "Vegetation cover m²":"% Asphalt") %>% 
  na.omit()
humber_veg <- read_csv("data-raw/csv/vegetation/Humber_Park_vegetation.csv") %>% 
  select(Park, Plant, "Vegetation cover m²":"% Asphalt") %>% 
  na.omit()
riverdale_veg <- read_csv("data-raw/csv/vegetation/Riverdale_Park_vegetation.csv") %>% 
  select(Park, Plant, "Vegetation cover m²":"% Asphalt") %>% 
  na.omit()
rouge_veg <- read_csv("data-raw/csv/vegetation/Rouge_Park_vegetation.csv") %>% 
  select(Park, Plant, "Vegetation cover m²":"% Asphalt") %>% 
  na.omit()

# Rename vegetation columns for all datasets
erindale_veg_modified <- rename_vegetation_cols(erindale_veg)
high_veg_modified <- rename_vegetation_cols(high_veg)
humber_veg_modified <- rename_vegetation_cols(humber_veg)
riverdale_veg_modified <- rename_vegetation_cols(riverdale_veg)
rouge_veg_modified <- rename_vegetation_cols(rouge_veg)

# Merge vegetation data
veg_data_merged <- bind_rows(erindale_veg_modified, high_veg_modified, humber_veg_modified,
                             riverdale_veg_modified, rouge_veg_modified)

# Join vegetation data with phenotype and distance data
all_data_merged <- data_merged %>% 
  left_join(., veg_data_merged, by = c("Park", "Plant"))

#### CREATE COLUMN WITH PARK DISTANCE SET TO 0 ####

all_data_merged <- all_data_merged %>% 
  group_by(Park) %>% 
  mutate(distance_set0 = case_when(
    Plant >= 1 & Plant <= 96 ~ 0,
    TRUE ~ distance
  ))

# Write merged and clean dataframe to disk
write_csv(all_data_merged, "data-clean/allPlants_allParks.csv")
