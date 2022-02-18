# Create figures & tables

#### SETUP ####

# Theme used for plotting
ng1 <- theme(aspect.ratio=0.7,
             panel.background = element_blank(),
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

#### FIGURE 1 ####

### Map of Toronto with parks as points
### One inset for each park showing locations of plants and iButtons

## Major Toronto map

# Get coordinates of parks
cols <- met.brewer('Lakota', type = 'discrete', n = 5)
park_coords <- allPlants_allParks %>% 
  dplyr::select(Park, Latitude_park, Longitude_park) %>% 
  distinct() %>% 
  mutate(col = cols)

# Collect tiles used to build map
allParks_bbox <- make_bbox(park_coords$Longitude_park, park_coords$Latitude_park, f = 0.1)
allParks_map <- get_map(location = allParks_bbox, zoom = 11, source = 'stamen')

yran <- seq(from = min(park_coords$Latitude_park), to = max(park_coords$Latitude_park), length.out=6)
xran <- seq(from = min(park_coords$Longitude_park), to = max(park_coords$Longitude_park), length.out=6)
toronto_map <- ggmap(allParks_map) +
  geom_point(data = park_coords, aes(x = Longitude_park, y = Latitude_park, fill = Park), 
             size = 5, alpha = 0.7, shape = 21, show.legend = FALSE) +
  xlab("Longitude") + ylab('Latitude') +
  scale_x_continuous(breaks = xran, expand = c(0, 0), 
                     labels = scales::number_format(accuracy = 0.001, decimal.mark = '.')) +
  scale_y_continuous(breaks = yran, expand = c(0, 0),
                     labels = scales::number_format(accuracy = 0.001, decimal.mark = '.')) +
  scale_fill_manual(values = cols) +
  coord_equal() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border=element_rect(colour = 'black', size = 2, fill = NA),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17)) +
  scalebar(y.min=43.5375, y.max=43.5385, x.min=-79.2590, x.max=-79.1300, 
           dist = 5, dist_unit = 'km', transform= TRUE, model='WGS84',
           height = 3, st.dist = 5)

ggsave(filename = 'analysis/figures/main-text/figure1/figure1_toronto_map.pdf', plot = toronto_map, 
       device = 'pdf', dpi = 600, height = 8, width = 14, units = 'in')

## Erindale inset

# Get coordinates of iButtons
ibutton_df <- iButton_summaries %>% 
  mutate(Long = Long * -1) %>% 
  dplyr::select(Park, Lat, Long) %>%
  mutate(Park = ifelse(Park == 'HighPark', 'High Park', Park)) %>% 
  distinct()

erin_map_base <- plot_map_inset('Erindale', plant_df = allPlants_allParks, ibutton_df = ibutton_df, color_df = park_coords) +
  scalebar(y.min=43.5265, y.max=43.530, x.min=-79.6490, x.max=-79.6385, 
           dist = 500, dist_unit = 'm', transform= TRUE, model='WGS84',
           height = 0.13, st.dist = 0.2)
ggsave(filename = 'analysis/figures/main-text/figure1/figure1_erindale_inset.pdf', plot = erin_map_base, 
       device = 'pdf', dpi = 600, height = 10, width = 8, units = 'in')

## Humber inset

humber_map_base <- plot_map_inset('Humber', plant_df = allPlants_allParks, ibutton_df = ibutton_df, color_df = park_coords) +
  scalebar(y.min=43.6405, y.max=43.6440, x.min=-79.476, x.max=-79.4635, 
           dist = 500, dist_unit = 'm', transform= TRUE, model='WGS84',
           height = 0.15, st.dist = 0.2)
ggsave(filename = 'analysis/figures/main-text/figure1/figure1_humber_inset.pdf', plot = humber_map_base, 
       device = 'pdf', dpi = 600, height = 5, width = 13, units = 'in')

## High Park inset

highPark_map_base <- plot_map_inset('High Park', plant_df = allPlants_allParks, ibutton_df = ibutton_df, color_df = park_coords) +
  scalebar(y.min=43.6400, y.max=43.6435, x.min=-79.4370, x.max=-79.4300, 
           dist = 500, dist_unit = 'm', transform= TRUE, model='WGS84',
           height = 0.11, st.dist = 0.18)
ggsave(filename = 'analysis/figures/main-text/figure1/figure1_highPark_inset.pdf', plot = highPark_map_base, 
       device = 'pdf', dpi = 600, height = 5, width = 11, units = 'in')

# Riverdal inset

river_map_base <- plot_map_inset('Riverdale', plant_df = allPlants_allParks, ibutton_df = ibutton_df, color_df = park_coords) +
  scalebar(y.min=43.6655, y.max=43.6690, x.min=-79.3370, x.max=-79.3255, 
           dist = 500, dist_unit = 'm', transform= TRUE, model='WGS84',
           height = 0.13, st.dist = 0.18)
ggsave(filename = 'analysis/figures/main-text/figure1/figure1_riverdale_inset.pdf', plot = river_map_base, 
       device = 'pdf', dpi = 600, height = 3, width = 14, units = 'in')

## Rouge inset

rouge_map_base <- plot_map_inset('Rouge', plant_df = allPlants_allParks, ibutton_df = ibutton_df, color_df = park_coords) +
  scalebar(y.min=43.8045, y.max=43.8080, x.min=-79.1350, x.max=-79.1220, 
           dist = 500, dist_unit = 'm', transform= TRUE, model='WGS84',
           height = 0.15, st.dist = 0.2)
ggsave(filename = 'analysis/figures/main-text/figure1/figure1_rouge_inset.pdf', plot = rouge_map_base, 
       device = 'pdf', dpi = 600, height = 5, width = 13, units = 'in')


# + 
#   ggsn::scalebar(location = 'topleft', dist = 50, dist_unit = 'm', model = "WGS84", 
#                  height = 0.5, st.dist = 0.5)



ggsave(filename = 'analysis/humber_map.png', plot = humber_map, device = 'png', dpi = 600,
       height = 4, width = 10, units = 'in')

#### FIGURE 2 ####

### Panels A, B, & C
## Plot of HCN, Ac, and Li against habitat, linetype by significance, colored by park

# Get predicted values from models
HCN_reactNorm <- plotReactNorm_Hab_allParks(modHCN_hab_T3, 'HCN')
Ac_reactNorm <- plotReactNorm_Hab_allParks(modAc_hab_T3, 'Ac')
Li_reactNorm <- plotReactNorm_Hab_allParks(modLi_hab_T2, 'Li')

### Panels D, E, &
## Plot of HCN, Ac, and Li against percent asphalt, linetype by significance, colored by park
vals <- seq(from = 0, to = 100, length.out = 100)
HCN_clinePlot <- plotClines_Imperv_allParks(modHCN_imperv_herb_T2, 'HCN', vals = vals)
Ac_clinePlot <- plotClines_Imperv_allParks(modAc_imperv_herb_T3, 'Ac', vals = vals)
Li_clinePlot <- plotClines_Imperv_allParks(modLi_imperv_herb_T3, 'Li', vals = vals)

figure2 <-( HCN_reactNorm | Ac_reactNorm | Li_reactNorm) / ( HCN_clinePlot | Ac_clinePlot | Li_clinePlot ) +
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

ggsave(filename = 'analysis/figures/main-text/figure2_allClines_byImperv.png', plot = figure2, device = "png",
       width = 14, height = 9, units = "in", dpi = 600)
ggsave(filename = 'analysis/figures/main-text/figure2_allClines_byImperv.pdf', plot = figure2, device = "pdf",
       width = 14, height = 9, units = "in", dpi = 600)

#### FIGURE 3 ####

### Panels A & B

## TODO: Clean up code for figure 3. Lot of repeated snippets.

# Predicted change in herbivory across % imperv gradient
Herb_reactNorm <- plotReactNorm_Hab_allParks(modHerb_hab_T2, 'Herb')
maxTemp_reactNorm <- plotReactNorm_Hab_allParks(mod_summerTemps_hab_T2, 'maxTemp')

# Calculate Transect - Park difference in Herbivory, Temperature, Ac, and HCN
predHCN_byPark <- ggeffect(modHCN_hab_T3, terms = c("Habitat", 'Park')) %>% 
  dplyr::select(x, group, predicted) %>% 
  pivot_wider(names_from = x, values_from = predicted) %>% 
  group_by(group) %>% 
  summarise(diff = Transect - Park) %>% 
  mutate(var = 'HCN')
predAc_byPark <- ggeffect(modAc_hab_T3, terms = c("Habitat", 'Park'))  %>% 
  dplyr::select(x, group, predicted) %>% 
  pivot_wider(names_from = x, values_from = predicted) %>% 
  group_by(group) %>% 
  summarise(diff = Transect - Park) %>% 
  mutate(var = 'Ac')
predHerb_byPark <- ggeffect(modHerb_hab_T2, terms = c("Habitat", 'Park'))  %>% 
  dplyr::select(x, group, predicted) %>% 
  pivot_wider(names_from = x, values_from = predicted) %>% 
  group_by(group) %>% 
  summarise(Herb_diff = Transect - Park)
predMaxTemp_byPark <- ggeffect(mod_summerTemps_hab_T2, terms = c("Habitat", 'Park'))  %>% 
  mutate(group = fct_recode(group, 'High Park' = 'HighPark')) %>% 
  dplyr::select(x, group, predicted) %>% 
  pivot_wider(names_from = x, values_from = predicted) %>% 
  group_by(group) %>% 
  summarise(maxTemp_diff = Transect - Park)

allDiffs <- bind_rows(predHCN_byPark, predAc_byPark) %>% 
  left_join(., predHerb_byPark, by = 'group') %>% 
  left_join(., predMaxTemp_byPark, by = 'group')

HCN_Ac_byMaxTemp_diff_plot <- ggplot(allDiffs, aes(x = maxTemp_diff, y = diff)) +
  geom_point(size = 5, aes(fill = group, shape = var)) +
  geom_smooth(method = 'lm', se = FALSE, size = 2, color = ' black', aes(linetype = var)) +
  ylab("Suburban - Green space difference \n in HCN/Ac frequency") +
  xlab("Suburban - Green space difference \n in maximum temperature") +
  scale_fill_manual(values = cols, guide = 'none') +
  scale_shape_manual(values = c(21, 22)) +
  # scale_color_manual(values = cols) +
  scale_linetype_manual(values = c('dotted', 'dashed')) +
  ng1 +
  theme(legend.position = c(0.13, 0.86), 
        legend.direction="vertical",
        legend.text = element_text(size=15), 
        legend.key = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.key.height= unit(1.5, 'cm'),
        legend.key.width= unit(2, 'cm'),
        legend.spacing.x = unit(0.1, "cm"),
        legend.spacing.y = unit(-0.3, "cm"),
        legend.background = element_blank()) +
  guides(linetype = guide_legend(byrow = TRUE),
         shape = guide_legend(byrow = TRUE))

HCN_Ac_byHerb_diff_plot <- ggplot(allDiffs, aes(x = Herb_diff, y = diff)) +
  geom_point(size = 5, aes(fill = group, shape = var)) +
  geom_smooth(method = 'lm', se = FALSE, size = 2, color = ' black', aes(linetype = var)) +
  ylab("Suburban - Green space difference \n in HCN/Ac frequency") +
  xlab("Suburban - Green space difference \n in maximum temperature") +
  scale_fill_manual(values = cols, guide = 'none') +
  scale_shape_manual(values = c(21, 22)) +
  # scale_color_manual(values = cols) +
  scale_linetype_manual(values = c('dotted', 'dashed')) +
  ng1 +
  theme(legend.position = c(0.13, 0.86), 
        legend.direction="vertical",
        legend.text = element_text(size=15), 
        legend.key = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.key.height= unit(1.5, 'cm'),
        legend.key.width= unit(2, 'cm'),
        legend.spacing.x = unit(0.1, "cm"),
        legend.spacing.y = unit(-0.3, "cm"),
        legend.background = element_blank()) +
  guides(linetype = guide_legend(byrow = TRUE),
         shape = guide_legend(byrow = TRUE))
  

figure3_base <-( Herb_reactNorm | maxTemp_reactNorm ) / plot_spacer() / ( HCN_Ac_byHerb_diff_plot | HCN_Ac_byMaxTemp_diff_plot ) +
  plot_layout(heights = c(4, 0.1, 4), widths = c(4, 0.1, 4)) &
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag.position = c(0.1, 1.1),
        plot.tag = element_text(size = 20))
blank_p <- plot_spacer() + theme_void()
leg_fig2 <- get_legend(figure2)

leg <- plot_grid(leg_fig2,
                 blank_p,
                 nrow = 1,
                 ncol = 1,
                 align = "hv",
                 axis = "t") +
  theme(panel.background = element_rect(fill = 'white', color = 'white'))

figure3 <- plot_grid(leg,
                     figure3_base,
                     nrow = 2,
                     ncol = 1,
                     align = "hv",
                     axis = "t",
                     rel_heights = c(0.15, 1))

ggsave(filename = 'analysis/figures/main-text/figure3_herb_maxTemp_allParks.png', plot = figure3, device = "png",
       width = 15, height = 13, units = "in", dpi = 600)
ggsave(filename = 'analysis/figures/main-text/figure3_herb_maxTemp_allParks.pdf', plot = figure3, device = "pdf",
       width = 15, height = 13, units = "in", dpi = 600)
  