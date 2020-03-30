# Load in required packages
library(tidyverse)
library(data.table)

clean_iButton_data <- function(df){

  # Create datetime for when iButton was placed the field
  df <- as.data.frame(df) %>%
    mutate(Installation_time = paste0(str_replace(Installation_time, "h", ":"), ":00")) %>%
    mutate(Installation_Datetime = as.POSIXct(paste(Installation_date, Installation_time)))

  # Get correct path to iButton datasheet, based on name of iButton in master sheet
  button <- df %>% pull(button)
  pattern <- sprintf("^%s_*", button)
  Ibutton_csvs <- "data-raw/submitted_excel_files/Ibuttons/"
  csv_name <- list.files(Ibutton_csvs, pattern = pattern)
  path_to_csv <- paste0(Ibutton_csvs, csv_name)
  
  # Read in iButton data, skipping header (variable length)
  r <- readLines(path_to_csv)
  dt <- grep("Date/Time", r)
  Ibutton_data <- read_csv(path_to_csv, skip = dt - 1)
  
  # Process and clean raw iButton data
  Ibutton_data_mod <- Ibutton_data %>%
    separate("Date/Time", into = c("Date", "Time"), sep = " ") %>%
    
    # Datetime for temperature record
    mutate(Datetime = as.POSIXct(paste(Date, Time), format="%d/%m/%y %H:%M:%S")) %>%
    
    # Keep only observations later than when button was placed in the field
    filter(!(Datetime < df$Installation_Datetime)) %>%
    
    # Add, rename, and select columns
    mutate(Week = strftime(Datetime, format = "%V"),
           Park = df$Park,
           Lat = df$Latitude,
           Long = df$Longitude,
           Percent_asphalt = round(df$Asphalt_perc, 3),
           Location = df$Location,
           Button = button) %>% 
    select(-Unit, -Datetime) %>% 
    rename("Temp" = "Value")
  
  # Write clean dataframe to disk
  outpath <- sprintf("data-clean/iButton_csvs/%s_iButton_clean.csv", button)
  write_csv(Ibutton_data_mod, outpath)
  
}

# Load in master iButton datasets
Ibutton_master <- read_csv("data-raw/csv/ibuttons.csv") %>% 
  select("Park":"Asphalt_perc") %>% 
  mutate(button = paste0(Park, Name)) %>% 
  filter(button != "Rouge2P")

# Split the master dataset into a list of separate dataframes, one for each button
Ibuttons_split <- group_split(Ibutton_master, button)

# Clean iButton CSVs for all those listed in the master dataset
purrr::walk(Ibuttons_split, clean_iButton_data)
     



