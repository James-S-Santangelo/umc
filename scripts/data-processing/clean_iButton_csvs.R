# Load in required packages
library(tidyverse)
library(data.table)

# Load in master iButton datasets
Ibutton_master <- read_csv("data-raw/csv/ibuttons.csv") %>% 
  select("Park":"Asphalt_perc") %>% 
  mutate(button = paste0(Park, Name)) %>% 
  filter(button != 'Rouge3P')  # This button is corrupted

# Split the master dataset into a list of separate dataframes, one for each button
Ibuttons_split <- group_split(Ibutton_master, button)

# Clean iButton CSVs for all those listed in the master dataset
purrr::walk(Ibuttons_split, clean_iButton_data, round = 'First')
purrr::walk(Ibuttons_split, clean_iButton_data, round = 'Second')
purrr::walk(Ibuttons_split, clean_iButton_data, round = 'Third')
