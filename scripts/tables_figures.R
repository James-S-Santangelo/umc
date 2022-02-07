# Create figures & tables

#### SETUP ####

# Theme used for plotting
ng1 <- theme(aspect.ratio=0.7,panel.background = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border=element_blank(),
             axis.line.x = element_line(color="black",size=1),
             axis.line.y = element_line(color="black",size=1),
             axis.ticks=element_line(size = 1, color="black"),
             axis.ticks.length=unit(0.25, 'cm'),
             axis.text=element_text(color="black",size=15),
             axis.title=element_text(color="black",size=1),
             axis.title.y=element_text(vjust=2,size=17),
             axis.title.x=element_text(vjust=0.1,size=17),
             axis.text.x=element_text(size=15),
             axis.text.y=element_text(size=15),
             strip.text.x = element_text(size = 10, colour = "black",face = "bold"),
             strip.background = element_rect(colour="black"),
             legend.position = "top", legend.direction="vertical",
             legend.text=element_text(size=17), legend.key = element_rect(fill = "white"),
             legend.title = element_text(size=17),legend.key.size = unit(1.0, "cm"))


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

### Panels A, B, & C
## Plot of HCN, Ac, and Li against habitat, linetype by significance, colored by park

# Get predicted values from models
HCN_reactNorm <- plotReactNorm_Hab_allParks(modHCN_hab_T3, 'HCN')
Ac_reactNorm <- plotReactNorm_Hab_allParks(modAc_hab_T3, 'Ac')
Li_reactNorm <- plotReactNorm_Hab_allParks(modLi_hab_T2, 'Li')

### Panels D, E, &
## Plot of HCN, Ac, and Li against percent asphalt, linetype by significance, colored by park

HCN_clinePlot <- plotClines_Imperv_allParks(modHCN_imperv_herb_T2, 'HCN')
Ac_clinePlot <- plotClines_Imperv_allParks(modAc_imperv_herb_T3, 'Ac')
Li_clinePlot <- plotClines_Imperv_allParks(modLi_imperv_herb_T3, 'Li')

figure2 <-( HCN_reactNorm | Ac_reactNorm | Li_reactNorm) / ( HCN_clinePlot | Ac_clinePlot | Li_clinePlot )+
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

ggsave(filename = 'analysis/figure2_allClines_byImperv2.png', plot = figure2, device = "png",
       width = 14, height = 9, units = "in", dpi = 600)

#### MAIN EFFECT OF % IMPERV ON HCN ####

modHCN_imperv_herb_T2_predict <- ggeffect(modHCN_imperv_herb_T2, terms = "percent_asphalt [all]")
ggplot(modHCN_imperv_herb_T2_predict, aes(x, predicted)) + geom_line()

#### HERBIVORY BY PARK INTERACTION FOR LI ####

modLi_imperv_herb_T3_predict <- ggeffect(modLi_imperv_herb_T3, 
                                         terms = c("Herbivory [all]", "Park"))
ggplot(modLi_imperv_herb_T3_predict, aes(x, predicted, color = group)) + geom_line()
