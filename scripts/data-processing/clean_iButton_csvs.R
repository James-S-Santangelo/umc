# Load in required packages
library(tidyverse)
library(data.table)

clean_iButton_data <- function(df, round){

  # Create datetime for when iButton was placed the field
  df <- as.data.frame(df) %>%
    mutate(Installation_time = paste0(str_replace(Installation_time, "h", ":"), ":00")) %>%
    mutate(Installation_Datetime = as.POSIXct(paste(Installation_date, Installation_time)))

  # Get correct path to iButton datasheet, based on name of iButton in master sheet
  button <- df %>% pull(button)
  pattern <- sprintf("^%s_*", button)
  Ibutton_csvs <- sprintf("data-raw/submitted_excel_files/Ibuttons/%s_round/", round)
  csv_name <- list.files(Ibutton_csvs, pattern = pattern)
  
  # If CSV file for Ibutton Exists, then clean and write dataframe
  # Some Ibuttons were lost for Second and Third collections so these don't have data
  if(length(csv_name) != 0){
    path_to_csv <- paste0(Ibutton_csvs, csv_name)
    
    if(round == 'First'){

      # Read in iButton data, skipping header (variable length)
      r <- readLines(path_to_csv)
      dt <- grep("Date/Time", r)
      Ibutton_data <- read_delim(path_to_csv, skip = dt, show_col_types = FALSE, delim=',')
      names(Ibutton_data) <- c('Date_Time', 'Unit', 'Ten_val', 'Deci_val')
      buttons_fucked_by_excel <- c("Erindale2W", "Erindale3E", "Erindale4E", "Erindale5E", "Erindale5P", "Erindale5W")
      Ibutton_data <- Ibutton_data %>%
        mutate(Deci_val = ifelse(is.na(Deci_val), 0, Deci_val),
               Value = as.numeric(paste(Ten_val, Deci_val, sep = '.'))) %>%
        dplyr::select(-Ten_val, -Deci_val) %>% 
        mutate(Date_Time = case_when(button %in% buttons_fucked_by_excel ~ paste0(Date_Time, ":00"),
                                     TRUE ~ Date_Time))

      # Process and clean raw iButton data
      Ibutton_data_mod <- Ibutton_data %>%
        separate(Date_Time, into = c("Date", "Time"), sep = " ") %>%
        
        # Convert 2 digit year to 4 digit year
        # https://stackoverflow.com/questions/60581813/is-there-a-way-to-make-a-2-digit-year-into-a-4-digit-year-in-r
        mutate(Date = strftime(as.Date(Date, format="%d/%m/%y"), "%d/%m/%Y")) %>% 
        
        # Datetime for temperature record
        mutate(Datetime = as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M:%S")) %>%

        # Keep only observations later than when button was placed in the field
        filter(!(Datetime < df$Installation_Datetime)) %>%

        # Add, rename, and select columns
        mutate(Year = strftime(Datetime, format = "%Y"),
               Month = strftime(Datetime, format = "%B"),
               Day = strftime(Datetime, format = "%d"),
               Week = strftime(Datetime, format = "%V"),
               Park = df$Park,
               Lat = df$Latitude,
               Long = df$Longitude,
               Percent_asphalt = round(df$Asphalt_perc, 3),
               Location = df$Location,
               Button = button,
               Round = round) %>%
        select(-Unit, -Datetime) %>%
        rename("Temp" = "Value")

      # Write clean dataframe to disk
      dir.create(sprintf('data-clean/iButton_csvs/%s_round', round), showWarnings = FALSE)
      outpath <- sprintf("data-clean/iButton_csvs/%s_round/%s_iButton_%sRound_clean.csv", round, button, round)
      write_csv(Ibutton_data_mod, outpath)

    }else{
      
      # Read in iButton data, skipping header (variable length)
      r <- readLines(path_to_csv)
      dt <- grep("Date/Time", r)
      # Need to manualy set column names for second and third rounds
      Ibutton_data <- read_csv(path_to_csv, skip = dt, show_col_types = FALSE) 
      names(Ibutton_data) <- c('Date', 'Time', 'Unit', 'Value')

      # Process and clean raw iButton data
      Ibutton_data_mod <- Ibutton_data %>%

        # Datetime for temperature record
        mutate(Datetime = as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M:%S")) %>%

        # Keep only observations later than when button was placed in the field
        filter(!(Datetime < df$Installation_Datetime)) %>%

        # Add, rename, and select columns
        mutate(Year = strftime(Datetime, format = "%Y"),
               Month = strftime(Datetime, format = "%B"),
               Day = strftime(Datetime, format = "%d"),
               Week = strftime(Datetime, format = "%V"),
               Park = df$Park,
               Lat = df$Latitude,
               Long = df$Longitude,
               Percent_asphalt = round(df$Asphalt_perc, 3),
               Location = df$Location,
               Button = button,
               Round = round) %>%
        select(-Unit, -Datetime) %>%
        rename("Temp" = "Value")

      # Write clean dataframe to disk
      dir.create(sprintf('data-clean/iButton_csvs/%s_round', round), showWarnings = FALSE)
      outpath <- sprintf("data-clean/iButton_csvs/%s_round/%s_iButton_%sRound_clean.csv", round, button, round)
      write_csv(Ibutton_data_mod, outpath)
    }
  }
}

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
