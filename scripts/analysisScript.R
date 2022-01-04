library(tidyverse)
library(broom)
library(car)
source("scripts/haversine.R")

# Load datasets with plant-level data for all parks
allPlants_allParks <- read_csv("data-clean/allPlants_allParks.csv")

# Does distance to park predict HCN or gene presence?
Anova(glm(HCN ~ Park * distance, data = allPlants_allParks, family = 'binomial'), type = 3)
Anova(glm(Ac ~ Park * distance, data = allPlants_allParks, family = 'binomial'), type = 3)
Anova(glm(Li ~ Park * distance, data = allPlants_allParks, family = 'binomial'), type = 3)

# Does herbivory predict HCN or gene presence?
Anova(glm(HCN ~ Park * Herbivory, data = allPlants_allParks, family = 'binomial'), type = 3)
Anova(glm(Ac ~ Park * Herbivory, data = allPlants_allParks, family = 'binomial'), type = 3)
Anova(glm(Li ~ Park * Herbivory, data = allPlants_allParks, family = 'binomial'), type = 3)

# How does herbivory vary with distance to park or percent asphalt?
Anova(lm(Herbivory ~ Park * percent_asphalt, data = allPlants_allParks), type = 3)
Anova(lm(Herbivory ~ Park * distance, data = allPlants_allParks), type = 3)

# Number of plants by Park
t <- allPlants_allParks %>% 
  group_by(Park) %>% 
  tally()

#### MODELS OF GENE FREQUENCY CHANGES BY PARK ####

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

# Run models with distance as the predictor
hcn_distance_models <- model_by_park(allPlants_allParks, "distance", "HCN")
Ac_distance_models <- model_by_park(allPlants_allParks, "distance", "Ac")
Li_distance_models <- model_by_park(allPlants_allParks, "distance", "Li")

# Run models with distance_set0 as the predictor
hcn_distanceSet0_models <- model_by_park(allPlants_allParks, "distance_set0", "HCN")
Ac_distanceSet0_models <- model_by_park(allPlants_allParks, "distance_set0", "Ac")
Li_distanceSet0_models <- model_by_park(allPlants_allParks, "distance_set0", "Li")

# Run models with percent_asphalt as the predictor
hcn_percentAsphalt_models <- model_by_park(allPlants_allParks, "percent_asphalt", "HCN")
Ac_percentAsphalt_models <- model_by_park(allPlants_allParks, "percent_asphalt", "Ac")
Li_percentAsphalt_models <- model_by_park(allPlants_allParks, "percent_asphalt", "Li")

# Merge all dataframes ending with "_models"
all_models <- mget(ls(pattern="*_models$")) %>% 
  bind_rows() %>% 
  arrange(Park)

# Write model outputs dataframe to disk
write_csv(all_models, "analysis/logistigRegs_byPark_output.csv")

#### PLOTS OF CHANGES IN GENE FREQUENCY WITH PERCENT ASPHALT ####

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

# Split dataframe by Park
parks_df_list <- allPlants_allParks %>% 
  group_split(Park)

# Plot change in presence/absence of genes for each gene and park
purrr::walk(parks_df_list, plot_cline, response_var = "HCN")
purrr::walk(parks_df_list, plot_cline, response_var = "Ac")
purrr::walk(parks_df_list, plot_cline, response_var = "Li")


#### ANALYSIS OF HERBIVORY DATA ####

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

# Run models with distance as the predictor
herbivory_distance_models <- herb_by_park(allPlants_allParks, "distance", "Herbivory")

# Run models with distance_set0 as the predictor
herbivory_distanceSet0_models <- herb_by_park(allPlants_allParks, "distance_set0", "Herbivory")

# Run models with percent_asphalt as the predictor
herbivory_percentAsphalt_models <- herb_by_park(allPlants_allParks, "percent_asphalt", "Herbivory")

# Merge all dataframes ending with "_models"
all_models_herbivory <- mget(ls(pattern="^herbivory_*")) %>% 
  bind_rows() %>% 
  arrange(Park)

# Write model outputs dataframe to disk
write_csv(all_models_herbivory, "analysis/herbivoryRegs_byPark_output.csv")

# Herbivory by gene presence
summary(lm(Herbivory ~ HCN, data = allPlants_allParks))
summary(lm(Herbivory ~ Ac, data = allPlants_allParks))
summary(lm(Herbivory ~ Li, data = allPlants_allParks))

#### HERBIVORY PLOTS ####

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

purrr::walk(parks_df_list, herb_plot)

#### ANALYSIS OF IBUTTON TEMPERATURE DATA ####

iButton_summaries <- read_csv("data-clean/iButton_summaries.csv") %>% 
  mutate(Habitat = ifelse(Location == "Park", "Park", "Transect"))

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

minTemp_habitat_models <- tempMod_by_park(iButton_summaries, "minTemp", "Habitat")
minTemp_asphalt_models <- tempMod_by_park(iButton_summaries, "minTemp", "Percent_asphalt")
maxTemp_habitat_models <- tempMod_by_park(iButton_summaries, "maxTemp", "Habitat")
maxTemp_asphalt_models <- tempMod_by_park(iButton_summaries, "maxTemp", "Percent_asphalt")
meanTemp_habitat_models <- tempMod_by_park(iButton_summaries, "meanTemp", "Habitat")
meanTemp_asphalt_models <- tempMod_by_park(iButton_summaries, "meanTemp", "Percent_asphalt")

# Merge all dataframes ending with "*Temp_*"
temp_models <- mget(ls(pattern="*Temp_.*_models")) %>% 
  bind_rows() %>% 
  arrange(Park)

# Write model outputs dataframe to disk
write_csv(temp_models, "analysis/tempRegs_byPark_output.csv")

#### PLOTS OF IBUTTON TEMPERATURE DATA ####

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

iButton_split <- iButton_summaries %>% 
  group_split(Park, Round)

purrr::walk(iButton_split, temp_plots, "minTemp")
purrr::walk(iButton_split, temp_plots, "maxTemp")
purrr::walk(iButton_split, temp_plots, "meanTemp")
