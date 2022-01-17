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
#' @param df Dataframe containing model terms
#' @param response_var Response variable to be plotted. 
#'     One of either `HCN`, `Ac`, or `Li`
#'     
#'  @return ggplot object
plot_all_clines <- function(df, response_var){
  
  plot <-  df %>% 
    mutate(sig = ifelse(response == 'Ac' & Park %in% c('Riverdale', 'High Park'), 'Yes', 'No')) %>% 
    ggplot(., aes(x=percent_asphalt, y=!!sym(response_var))) + 
    geom_line(stat = "smooth", 
              method="glm", 
              size = 1.5,
              fullrange=TRUE,
              method.args = list(family = "binomial"),
              aes(color = Park, linetype = sig)) +
    ylab(sprintf("Presence of %s", response)) +
    xlab("Percent impervious surface") + 
    coord_cartesian(xlim = c(0, 100.5), ylim = c(0.1, 0.85)) +
    scale_y_continuous(breaks = seq(from = 0.1, to = 0.8, by = 0.1)) +
    scale_color_manual(values = cols) +
    scale_linetype_manual(values = c('dashed', 'solid'), 
                          limits = c('No', 'Yes')) +
    ng1  
  
  return(plot)
}