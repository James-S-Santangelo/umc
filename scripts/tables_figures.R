# Create figures & tables


#### TABLES ####

#### LOGISTIC REGRESSIONS BY PARK ####

## Run models separately for each Park

# For each of the above response variable (i.e., HCN, Ac, Li), run a logistic regression
#  using either distance, destance_set0, or percent asphalt as a predictor. Combine all these
#  models into a single dataframe and write to disc

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
all_models <- mget(ls(pattern="[hcn|Ac|Li]_.*_models$")) %>% 
  bind_rows() %>% 
  arrange(Park)

# Write model outputs dataframe to disk
write_csv(all_models, "analysis/logistigRegs_byPark_output.csv")

#### HERBIVORY MODELS BY PARK ####

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

#### TEMPERATURE MODELS BY PARK ####

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

#### FIGURES ####

#### PLOTS OF HCN/GENE CLINES BY PARK ####

# Split dataframe by Park
parks_df_list <- allPlants_allParks %>% 
  group_split(Park)

# Plot change in presence/absence of genes for each gene and park
purrr::walk(parks_df_list, plot_cline, response_var = "HCN")
purrr::walk(parks_df_list, plot_cline, response_var = "Ac")
purrr::walk(parks_df_list, plot_cline, response_var = "Li")

### PLOTS OF CHANGES IN HERBIVORY BY PARK ####

purrr::walk(parks_df_list, herb_plot)

#### PLOTS OF TEMPERATURE BY PARK ####

iButton_split <- iButton_summaries %>% 
  group_split(Park, Round)

purrr::walk(iButton_split, temp_plots, "minTemp")
purrr::walk(iButton_split, temp_plots, "maxTemp")
purrr::walk(iButton_split, temp_plots, "meanTemp")

#### FIGURE 2 ####

## Plot of HCN, Ac, and Li against percent asphalt, linetype by significance

cols <- met.brewer('Lakota', type = 'discrete', n = 5)

HCN_clinePlot <- plot_all_clines(allPlants_allParks, 'HCN')
Ac_clinePlot <- plot_all_clines(allPlants_allParks, 'Ac')
Li_clinePlot <- plot_all_clines(allPlants_allParks, 'Li')

figure2 <-( HCN_clinePlot | Ac_clinePlot | Li_clinePlot) +
  plot_layout(guides = 'collect') &
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'bottom', 
        legend.direction="horizontal",
        legend.text = element_text(size=15), 
        legend.key = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.key.size = unit(1, "cm"),
        legend.spacing.x = unit(0.5, "cm"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        plot.tag.position = c(0.1, 1.1),
        plot.tag = element_text(size = 20)) 
figure2

ggsave(filename = 'analysis/figure2_allClines_byImperv.png', plot = figure2, device = "png",
       width = 12, height = 5, units = "in", dpi = 600)
