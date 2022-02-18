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

#' Function to run and summarize model testing for changes in 
#'     gene or HCN frequencies for specific park
#' 
#' @param df Dataframe containing model terms
#' @param predictor_var Predictor variable for model. 
#'     One of either `distance`, `distance_set0`, or `percent_asphalt`
#' @param response_var Response variable for model
#'     One of either `HCN`, `Ac`, or `Li`
#' 
#' @return Beta coefficient and P-value of predictor as dataframe
model_by_park <- function(df, predictor_var, response_var){
  
  model_output <- df %>% 
    group_by(Park) %>% 
    do(mod = glm(as.formula(paste(response_var, predictor_var, sep = "~")), family = "binomial", data = .))  %>%
    tidy(., mod) %>% 
    filter(term == predictor_var) %>%
    mutate(response = response_var) %>%
    rename("predictor" = "term") %>%
    select(Park, response, predictor, everything()) %>%
    mutate_if(is.numeric, round, 3)
  
  
  return(model_output)
  
}

#' Plot presence/absence of HCN/gene against `percent_asphalt`
#' 
#' @param df Dataframe containing model terms
#' @param respnse_var Response variable for model
#'     One of either `HCN`, `Ac`, or `Li`
#' 
#' @return None. Writes plots to disk
plot_cline <- function(df, response_var){
  
  response <- df %>% pull(response_var)
  park <- df$Park[1]
  
  plot <- ggplot(df, aes(x=percent_asphalt, y=response)) + 
    geom_point(alpha=.5) +
    stat_smooth(method="glm", se=TRUE, fullrange=TRUE, 
                method.args = list(family=binomial),
                color = "black") + 
    ylab(sprintf("Presence/absence of %s", response_var)) +
    xlab("Percent asphalt") + 
    theme_bw()
  
  outpath <- sprintf("analysis/cline_plots/%s/%s_%s_by_asphalt.pdf", response_var, park, response_var)
  ggsave(filename = outpath, plot = plot, device = "pdf", 
         width = 6, height = 6, units = "in", dpi = 300)
}

#' Summarises model examining changes in herbivory by Park
#' 
#' @param df Dataframe containing model terms
#' @param predictor_var Predictor variable for model
#'     One of either `distance`, `distance_set0`, or `percent_asphalt`
#' @param response_var Response variable for model
#'     One of either `Herbivory`
#' 
#' @return Beta coefficient, P-value of predictor and model R-squared as dataframe
herb_by_park <- function(df, predictor_var, response_var){
  
  model_output <- df %>% 
    group_by(Park) %>% 
    do(mod = lm(as.formula(paste(response_var, predictor_var, sep = "~")), data = .)) 
  
  
  tidy_output <- model_output %>% 
    tidy(., mod) %>% 
    filter(term == predictor_var) %>% 
    mutate(response = response_var) %>% 
    rename("predictor" = "term") %>% 
    select(Park, response, predictor, everything()) %>% 
    mutate_if(is.numeric, round, 3)
  
  glance_output <- model_output %>% 
    glance(., mod) %>% 
    select(Park, r.squared) %>% 
    mutate(r.squared = round(r.squared, 3)) %>% 
    left_join(., tidy_output, by = "Park")
  
  return(glance_output)
}

#' Plot change in herbivory by Park. Treats herbivory as continuous
#' 
#' @param df Dataframe containing model terms
#' 
#' @return None. Writes plot to disk
herb_plot <- function(df){
  
  park <- df$Park[1]
  
  plot <- ggplot(df, aes(x=percent_asphalt, y=Herbivory)) + 
    geom_point(alpha=.5) +
    stat_smooth(method="lm", se=TRUE,
                color = "black") + 
    ylab(sprintf("Percent herbivory")) +
    xlab("Percent asphalt") + 
    theme_bw()
  
  outpath <- sprintf("analysis/herbivory_plots/%s_herbivory_by_asphalt.pdf", park)
  ggsave(filename = outpath, plot = plot, device = "pdf", 
         width = 6, height = 6, units = "in", dpi = 300)
}

#' Summarize temperature model for a given Park
#' 
#' @param df Dataframe containing model terms
#' @param predictor_var Predictor variable for model
#'     One of either  `Habitat`, or `percent_asphalt`
#' @param response_var Response variable for model
#'     One of either `maxTemp`, `meanTemp`, or `minTemp`
#' 
#' @return Beta coefficient, P-value of predictor and model R-squared as dataframe
tempMod_by_park <- function(df, response_var, predictor_var){
  
  model_output <- df %>% 
    group_by(Park, Round) %>% 
    do(mod = lm(as.formula(paste(response_var, predictor_var, sep = "~")), data = .)) 
  
  tidy_output <- model_output %>% 
    tidy(., mod) %>% 
    filter(term != "(Intercept)") %>% 
    mutate(response = response_var) %>% 
    rename("predictor" = "term") %>% 
    select(Park, response, predictor, everything()) %>% 
    mutate_if(is.numeric, round, 3)
  
  glance_output <- model_output %>% 
    glance(., mod) %>% 
    select(Park, r.squared) %>% 
    mutate(r.squared = round(r.squared, 3)) %>% 
    left_join(., tidy_output)
  
  
  return(glance_output)
  
}

#' Plots temperature variables against `percent_asphalt` for a given Park
#' 
#' @param respnse_var Response variable for model
#'     One of either `maxTemp`, `meanTemp`, or `minTemp`
#'     
#' @return None. Writes plot to disc
temp_plots <- function(df, response_var){
  
  park <- df$Park[1]
  round <- df$Round[1]
  
  plot <- df %>% 
    group_by(Button, Percent_asphalt) %>% 
    summarise(mean = mean(eval(parse(text = response_var))),
              sd = sd(eval(parse(text = response_var))),
              se = sd / sqrt(n()))  %>%
    ggplot(., aes(x = Percent_asphalt, y = mean)) +
    geom_errorbar(aes(min = mean - se, max = mean + se), width = 0.15, color = "black") +
    geom_point(size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    ylab(sprintf("Mean %s across weeks", response_var)) + xlab("Percent asphalt") +
    theme_bw()
  
  dir.create(sprintf("analysis/temp_plots/%s/%s_round", response_var, round), showWarnings = FALSE)
  outpath <- sprintf("analysis/temp_plots/%s/%s_round/%s_%s_%sRound_by_asphalt.pdf", 
                     response_var, round, park, response_var, round)
  ggsave(filename = outpath, plot = plot, device = "pdf",
         width = 6, height = 6, units = "in", dpi = 300)
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
  cols <- met.brewer('Lakota', type = 'discrete', n = 5)

  # Plot
  plot <-  predicted_vals %>% 
    ggplot(., aes(x=x, y=predicted)) +
    geom_line(size = 1.5, aes(color = group, linetype = sig)) +
    geom_line(data = predicted_vals_main, size = 1, aes(linetype = sig), 
              color = 'black', show.legend = FALSE, alpha = 1) +
    ylab(y_axis_title) +
    xlab("Percent impervious surface") +
    coord_cartesian(xlim = c(0, 100.5), ylim = c(0.1, 0.85)) +
    scale_y_continuous(breaks = seq(from = 0.1, to = 0.8, by = 0.1)) +
    scale_color_manual(values = cols) +
    scale_linetype_manual(values = c('twodash', 'solid'),
                          limits = c('NS', 'P < 0.05')) +
    ng1
  
  return(plot)
}

#' Plots all HCN or gene clines on single plot against `percent_asphalt`
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
  cols <- met.brewer('Lakota', type = 'discrete', n = 5)

  # Plot
  plot <-  predicted_vals %>%
    ggplot(., aes(x=x, y=predicted)) +
    geom_line(size = 1.5, aes(color = group, linetype = sig, group = group), show.legend = FALSE) +
    geom_line(data = predicted_vals_main, size = 1, aes(linetype = sig, group = group),
              color = 'black', show.legend = FALSE, alpha = 1) +
    geom_point(size = 5, shape = 21, aes(group = group, fill = group), show.legend = FALSE) +
    geom_point(data = predicted_vals_main, size = 3, shape = 23, fill = 'black', alpha = 1,
               show.legend = FALSE) +
    ylab(y_axis_title) +
    # xlab("") +
    # coord_cartesian(ylim = c(0.1, 0.85)) +
    # scale_y_continuous(breaks = seq(from = 0.1, to = 0.8, by = 0.1)) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_linetype_manual(values = c('twodash', 'solid'),
                          limits = c('NS', 'P < 0.05')) +
    ng1 + theme(axis.title.x = element_blank())
  
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
               size = 3, alpha = 1, shape = 22, fill = "#20235b") +
    scale_x_continuous(breaks = xran, expand = c(0, 0), 
                       labels = scales::number_format(accuracy = 0.001, decimal.mark = '.')) +
    scale_y_continuous(breaks = yran, expand = c(0, 0),
                       labels = scales::number_format(accuracy = 0.001, decimal.mark = '.')) +
    xlab("Longitude") + ylab('Latitude') + ggtitle(park_name) +
    coord_equal() + # For ggsn scalebar and north arrow later. Looks better.
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border=element_rect(colour = col, size = 2, fill = NA),
          axis.text = element_text(size = 13),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_blank(),
          title = element_text(size = 15, face = 'bold'))
  return(plot)
}
