# Functions used by other scripts

#' Convert degrees to radians
#' 
#' @param deg Degrees
#' 
#' @return Radians
deg2rad <- function(deg) return(deg*pi/180)

# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Haversine formula (hf)
#' Calculates the geodesic distance between two points specified by radian latitude/longitude using 
#'     the Haversine formula (hf)
#'
#' @param long1 Longitude of first point in decimal degrees
#' @param lat1 Latitude of first point in decimal degrees
#' @param long2 Longitude of second point in decimal degrees
#' @param lat2 Latitude of second point in decimal degrees
#' 
#' @return Haversine distance in Kilometers
haversine <- function(long1, lat1, long2, lat2) {
  
  # Ensure Lats and Longs are in radians
  long1 <- deg2rad(long1)
  lat1 <- deg2rad(lat1)
  long2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)
  
  # Calculate geodesic distance based on havesine formala
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  d = R * c
  return(d) # Distance in km
}

#' Formats Lat/Longs and calculates distance of plant
#'     from center of Park
#'     
#' @param df Dataframe containing plant-level data for Park
#' 
#' @return Modified dataframe with formatted Lat/Longs and distance column
add_distance <- function(df){
  
  
  df_modified <- df %>% 
    
    # Convert Degrees, Minutes, Seconds to Decimal Degrees.
    separate(Latitude, sep = "[ |.|°]", into = c("dir_lat", "deg_lat", "dec_lat")) %>% 
    separate(Longitude, sep = "[ |.|°]", into = c("dir_long", "deg_long", "dec_long")) %>% 
    mutate(Latitude_plant = as.numeric(paste(deg_lat, dec_lat, sep = ".")),
           Longitude_plant = -1 * as.numeric(paste(deg_long, dec_long, sep = "."))) %>% 
    select(-matches("dir|deg|dec")) %>% 
    left_join(., park_centres, by = "Park") %>% 
    
    # Calculate distance using haversine formula
    mutate(distance = haversine(Longitude_park, Latitude_park, Longitude_plant, Latitude_plant))
  
  return(df_modified)
}

#' Rename vegetation columns and add total area surveyed
#' 
#' @param df Dataframe containing plant-level vegetation data for Park
#' 
#' @return Dataframe with renamed columns
rename_vegetation_cols <- function(df){
  
  df_modified <- df %>% 
    rename("area_vegetation" = "Vegetation cover m²",
           "area_asphalt" = "Asphalt m²",
           "percent_veg" = "% Vegetation",
           "percent_asphalt" = "% Asphalt") %>% 
    mutate(total_area = area_vegetation + area_asphalt)
  
  return(df_modified)
  
  
}

#' Function to clean Ibutton data and handle edge cases
#' 
#' @param df Ibutton data for specific park
#' @param round Round of Ibutton collection. Either `First`, `Second`, or `Third`
#' 
#' @return None. Write cleaned dataframe to disk
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

#' Function to summarise Ibutton temperature data
#' 
#' @param df Dataframe with cleaned Ibutton data
#' 
#' @return Dataframe with temperature summaries for Ibuttons
summarise_iButtons <- function(df){
  
  df_mod <- df %>% 
    mutate(Week = as.character(Week)) %>% 
    group_by(Week, Year, Month, Day, Location, Lat, Long, Park, Percent_asphalt, Button, Round) %>% 
    summarise(week_date = first(Date),
              minTemp = min(Temp),
              maxTemp = max(Temp),
              meanTemp = mean(Temp),
              .groups = 'drop')
  return(df_mod)
  
}

#' Plots all HCN or gene clines on single plot against `percent_asphalt`
#' 
#' @param df Dataframe containing predicted values from fitted model
#' @param response_var Response variable to be plotted. 
#'     One of either `HCN`, `Ac`, or `Li`
#'     
#'  @return ggplot object
plotClines_Imperv_allParks <- function(mod, response_var, vals){
  
  # Get predicted values from model
  # vals <- seq(from = 0, to = 100, length.out = 1000)
  # print(vals)
  predicted_vals <- ggeffect(mod, terms = c("percent_asphalt [vals]", 'Park')) %>% 
    mutate(sig = ifelse(response_var == 'Ac' & group %in% c('Riverdale', 'High Park'), 'P < 0.05', 'NS'))
  predicted_vals_main <- ggeffect(mod, terms = c("percent_asphalt [vals]")) %>% 
    mutate(sig = ifelse(response_var %in% c('Ac', 'HCN'), 'P < 0.05', 'NS'))
  
  # Plot parameters
  y_axis_title <- case_when(response_var == 'HCN' ~ expression(paste("Presence of ", "HCN")),
                            response_var == 'Ac' ~ expression(paste("Presence of ", italic("Ac"))),
                            response_var == 'Li' ~ expression(paste("Presence of ", italic("Li"))))
  cols <- c('black', met.brewer('Lakota', type = 'discrete', n = 5))

  # Plot
  plot <-  predicted_vals %>% 
    ggplot(., aes(x=x, y=predicted)) +
    geom_line(size = 1.5, aes(color = group, linetype = sig), alpha = 0.75) +
    geom_line(data = predicted_vals_main, size = 1.5, aes(linetype = sig, color = 'All Parks'), 
              show.legend = FALSE, alpha = 1) +
    ylab(y_axis_title) +
    xlab("Percent impervious surface") +
    coord_cartesian(xlim = c(0, 100.5), ylim = c(0.1, 0.85)) +
    scale_y_continuous(breaks = seq(from = 0.1, to = 0.8, by = 0.1)) +
    scale_color_manual("Site", values = cols) +
    scale_linetype_manual("Significance", values = c('twodash', 'solid'),
                          limits = c('NS', 'P < 0.05')) +
    ng1
  
  return(plot)
}

#' Plots all HCN or gene clines on single plot against `Habitat`
#' 
#' @param df Dataframe containing predicted values from fitted model
#' @param response_var Response variable to be plotted. 
#'     One of either `HCN`, `Ac`, or `Li`
#'     
#'  @return ggplot object
plotReactNorm_Hab_allParks <- function(mod, response_var){
  
  # Get predicted values from model
  predicted_vals <- ggeffect(mod, terms = c("Habitat", 'Park')) %>% 
    mutate(x = factor(ifelse(x == 'Park', 'Green space', 'Suburban'),
                         levels = c('Green space', 'Suburban'))) %>% 
    mutate(sig = ifelse(response_var %in% c('Ac', 'HCN') & group %in% c('Riverdale', 'High Park'), 'P < 0.05', 'NS'))
  predicted_vals_main <- ggeffect(mod, terms = c("Habitat")) %>% 
    mutate(x = factor(ifelse(x == 'Park', 'Green space', 'Suburban'),
                         levels = c('Green space', 'Suburban'))) %>% 
    mutate(sig = ifelse(response_var %in% c('Ac', 'HCN', 'Herb', 'maxTemp'), 'P < 0.05', 'NS'))
  
  # Plot parameters
  y_axis_title <- case_when(response_var == 'HCN' ~ expression(paste("Presence of ", "HCN")),
                            response_var == 'Ac' ~ expression(paste("Presence of ", italic("Ac"))),
                            response_var == 'Li' ~ expression(paste("Presence of ", italic("Li"))),
                            response_var == 'Herb' ~ expression(paste("Propotion of ", "leaf area consumed")),
                            response_var == 'maxTemp' ~ expression(paste("Maximum ", "summer temperature (°C)")))
  cols <- c('black', met.brewer('Lakota', type = 'discrete', n = 5))
  dodge <- position_dodge(width = 0.50)
  alpha <- 0.75
  if(response_var == 'Herb'){
    ylim = c(0, 0.15)
    yaxis_scale <- seq(from = 0, to = 0.15, by = 0.05)
  }else if(response_var == 'maxTemp'){
    ylim = c(21, 33)
    yaxis_scale <- seq(from = 21, to = 33, by = 3)
  }else{
    ylim = c(0.1, 0.8)
    yaxis_scale <- seq(from = 0.1, to = 0.8, by = 0.1)
  }
  
  # Plot
  plot <-  predicted_vals %>%
    ggplot(., aes(x=x, y=predicted)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, 
                      color = group, group = group), size = 1, width = 0.25,
                  position = dodge, alpha = alpha, show.legend = FALSE) + 
    geom_line(size = 1.5, aes(color = group, linetype = sig, group = group), position = dodge, alpha = alpha) +
    geom_point(size = 5, shape = 21, aes(group = group, fill = group, color = group), 
               show.legend = FALSE, position = dodge, alpha = alpha) +
    geom_line(data = predicted_vals_main, size = 1.5, aes(linetype = sig, group = group, color = 'All Parks'), 
              show.legend = FALSE, alpha = 1, position = dodge) +
    geom_errorbar(data = predicted_vals_main, aes(ymin = conf.low, ymax = conf.high), size = 1, width = 0.1,
                  position = dodge, alpha = 1, show.legend = FALSE) + 
    geom_point(data = predicted_vals_main, aes(fill = 'All Parks'), size = 4.5, shape = 23, alpha = 1,
               show.legend = FALSE, position = dodge) +
    coord_cartesian(ylim = ylim) +
    scale_y_continuous(breaks = yaxis_scale) +
    ylab(y_axis_title) +
    scale_color_manual("Site", values = cols) +
    scale_fill_manual("Site", values = cols) +
    scale_linetype_manual("Significance", values = c('twodash', 'solid'),
                          limits = c('NS', 'P < 0.05')) +
    ng1 + theme(axis.title.x = element_blank()) 
  
  
  return(plot)
}

#' Plot gene/phenotype frequency against `percent_asphalt` for particular Park
#' 
#' @param mod Model from which predicted values can be estimated
#' @param response_var Response variable to be plotted. 
#'     One of either `HCN`, `Ac`, or `Li`
#' @param vals Values at which predcited values should be estimate
#' @param park Site that should be plotted
#' @param raw_data_df Dataframe with raw phenotype and gene frequency data
#' @param cols_df Dataframe containing colors used for plotting
#'     
#'  @return ggplot object
plotCline_withData <- function(mod, response_var, vals, park, raw_data_df, cols_df){
  
  if(park == 'All Parks'){
    raw_data <- raw_data_df
    col <- 'black'
    predicted_vals <- ggeffect(mod, terms = c("percent_asphalt [vals]")) %>%
      mutate(sig = ifelse(response_var %in% c('Ac', 'HCN'), 'P < 0.05', 'NS'))
  }else{
    raw_data <- raw_data_df %>% filter(Park == park) 
    predicted_vals <- ggeffect(mod, terms = c("percent_asphalt [vals]", 'Park')) %>% 
      mutate(sig = ifelse(response_var == 'Ac' & group %in% c('Riverdale', 'High Park'), 'P < 0.05', 'NS')) %>%
      filter(group == park)
    col <- cols_df %>% filter(Park == park) %>% pull(col)
  }
  
  
  # Plot parameters
  y_axis_title <- case_when(response_var == 'HCN' ~ expression(paste("Presence of ", "HCN")),
                            response_var == 'Ac' ~ expression(paste("Presence of ", italic("Ac"))),
                            response_var == 'Li' ~ expression(paste("Presence of ", italic("Li"))))
  
  # Plot
  plot <-  predicted_vals %>% 
    ggplot(., aes(x=x, y=predicted)) +
    geom_point(data = raw_data, aes(x = percent_asphalt, y = !!sym(response_var)), color = col, fill = col,
               shape = 21, size = 2, alpha = 0.5) +
    geom_ribbon(aes(ymax = conf.high, ymin =conf.low), fill = col, alpha = 0.35) +
    geom_line(size = 1.5, aes(linetype = sig), alpha = 0.75, color = col) +
    ylab(y_axis_title) +
    xlab("Percent impervious surface") +
    ggtitle(park) +
    coord_cartesian(xlim = c(0, 100.5), ylim = c(0, 1)) +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.2)) +
    # scale_color_manual("Site", values = cols) +
    scale_linetype_manual("Significance", values = c('twodash', 'solid'),
                          limits = c('NS', 'P < 0.05')) +
    ng1 + guides(linetype = guide_legend(override.aes = list(color = 'black') ) )
  
  return(plot)
}

plot_map_inset <- function(park_name, plant_df, ibutton_df, color_df){
  
  # Download map tiles
  coords <- plant_df %>% filter(Park == park_name)
  bbox <- make_bbox(Longitude_plant, Latitude_plant, data = coords, f = 0.1)
  map <- get_map(bbox, zoom = 15, source = 'stamen')
  
  # Set colors and axis labels
  col <- color_df %>% filter(Park == park_name) %>% pull(col)

  min_lat <- min(coords$Latitude_plant)
  max_lat <- max(coords$Latitude_plant)
  min_long <- min(coords$Longitude_plant)
  max_long <- max(coords$Longitude_plant)
  if(park_name == 'Erindale'){
    yran <- seq(from = min_lat, to = max_lat, length.out=8)
  }else{
    yran <- seq(from = min_lat, to = max_lat, length.out=4)
  }
  if(park_name == 'Erindale'){
    xran <- seq(from = min_long, to = max_long, length.out=4)
  }else{
    xran <- seq(from = min_long, to = max_long, length.out=8)
  }
  
  # Plot
  plot <- ggmap(map) +
    geom_point(data = plant_df %>% filter(Park == park_name),
               aes(x = Longitude_plant, y = Latitude_plant),
               size = 2, alpha = 0.6, shape = 21, fill = col) +
    geom_point(data = ibutton_df %>% filter(Park == park_name), 
               aes(x = Long, y = Lat), 
               size = 4.5, alpha = 0.75, shape = 22, fill = "#20235b") +
    scale_x_continuous(breaks = xran, expand = c(0, 0), 
                       labels = scales::number_format(accuracy = 0.001, decimal.mark = '.')) +
    scale_y_continuous(breaks = yran, expand = c(0, 0),
                       labels = scales::number_format(accuracy = 0.001, decimal.mark = '.')) +
    xlab("Longitude") + ylab('Latitude') + ggtitle(park_name) +
    coord_equal() + # For ggsn scalebar and north arrow later. Looks better.
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border=element_rect(colour = col, size = 5, fill = NA),
          axis.text = element_text(size = 22),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_blank(),
          title = element_text(size = 25, face = 'bold'))
  return(plot)
}
