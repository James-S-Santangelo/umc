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
             axis.text=element_text(color="black",size=17),
             axis.title=element_text(color="black",size=19),
             axis.title.y=element_text(vjust=2,size=19),
             axis.title.x=element_text(vjust=0.1,size=19),
             # axis.text.x=element_text(size=15),
             # axis.text.y=element_text(size=15),
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
  arrange(Park) %>% 
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
             size = 8, alpha = 0.7, shape = 21, show.legend = FALSE) +
  xlab("Longitude") + ylab('Latitude') +
  scale_x_continuous(breaks = xran, expand = c(0, 0), 
                     labels = scales::number_format(accuracy = 0.001, decimal.mark = '.')) +
  scale_y_continuous(breaks = yran, expand = c(0, 0),
                     labels = scales::number_format(accuracy = 0.001, decimal.mark = '.')) +
  scale_fill_manual(values = c('Erindale' = park_coords %>% filter(Park == 'Erindale') %>% pull(col),
                               'Humber' = park_coords %>% filter(Park == 'Humber') %>% pull(col),
                               'High Park' = park_coords %>% filter(Park == 'High Park') %>% pull(col),
                               'Riverdale' = park_coords %>% filter(Park == 'Riverdale') %>% pull(col),
                               'Rouge' = park_coords %>% filter(Park == 'Rouge') %>% pull(col))) +
  coord_equal() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border=element_rect(colour = 'black', size = 3, fill = NA),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 17)) +
  scalebar(y.min=43.5420, y.max=43.5430, x.min=-79.2590, x.max=-79.1300, 
           dist = 5, dist_unit = 'km', transform= TRUE, model='WGS84',
           height = 8, st.dist = 5)

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
  coord_fixed(ratio = 1/1.6) +
  scalebar(y.min=43.5285, y.max=43.5335, x.min=-79.6490, x.max=-79.6385, 
           dist = 500, dist_unit = 'm', transform= TRUE, model='WGS84',
           height = 0.17, st.dist = 0.2)

ggsave(filename = 'analysis/figures/main-text/figure1/figure1_erindale_inset.pdf', plot = erin_map_base, 
       device = 'pdf', dpi = 600, height = 10, width = 10, units = 'in')

## Humber inset

humber_map_base <- plot_map_inset('Humber', plant_df = allPlants_allParks, ibutton_df = ibutton_df, color_df = park_coords) +
  coord_fixed(ratio = 2.75/1) +
  scalebar(y.min=43.640, y.max=43.6440, x.min=-79.475, x.max=-79.4640, 
           dist = 500, dist_unit = 'm', transform= TRUE, model='WGS84',
           height = 0.08, st.dist = 0.1)

ggsave(filename = 'analysis/figures/main-text/figure1/figure1_humber_inset.pdf', plot = humber_map_base, 
       device = 'pdf', dpi = 600, height = 10, width = 10, units = 'in')

## High Park inset

highPark_map_base <- plot_map_inset('High Park', plant_df = allPlants_allParks, ibutton_df = ibutton_df, color_df = park_coords) +
  coord_fixed(ratio = 2/1) +
  scalebar(y.min=43.6400, y.max=43.6435, x.min=-79.4370, x.max=-79.4300, 
           dist = 500, dist_unit = 'm', transform= TRUE, model='WGS84',
           height = 0.07, st.dist = 0.13)

ggsave(filename = 'analysis/figures/main-text/figure1/figure1_highPark_inset.pdf', plot = highPark_map_base, 
       device = 'pdf', dpi = 600, height = 10, width = 10, units = 'in')

# Riverdal inset

river_map_base <- plot_map_inset('Riverdale', plant_df = allPlants_allParks, ibutton_df = ibutton_df, color_df = park_coords) +
  coord_fixed(ratio = 2.5/1) +
  scalebar(y.min=43.6655, y.max=43.6690, x.min=-79.3370, x.max=-79.3255, 
           dist = 500, dist_unit = 'm', transform= TRUE, model='WGS84',
           height = 0.08, st.dist = 0.13)

ggsave(filename = 'analysis/figures/main-text/figure1/figure1_riverdale_inset.pdf', plot = river_map_base, 
       device = 'pdf', dpi = 600, height = 5, width = 12, units = 'in')

## Rouge inset

rouge_map_base <- plot_map_inset('Rouge', plant_df = allPlants_allParks, ibutton_df = ibutton_df, color_df = park_coords) +
  coord_fixed(ratio = 2.25/1) +
  scalebar(y.min=43.8050, y.max=43.8085, x.min=-79.1360, x.max=-79.1230, 
           dist = 500, dist_unit = 'm', transform= TRUE, model='WGS84',
           height = 0.11, st.dist = 0.15)

ggsave(filename = 'analysis/figures/main-text/figure1/figure1_rouge_inset.pdf', plot = rouge_map_base, 
       device = 'pdf', dpi = 600, height = 8, width = 10, units = 'in')

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
  guides(colour = guide_legend(title.position = "top"),
         linetype = guide_legend(title.position = "top")) &
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'bottom', 
        legend.direction="horizontal",
        legend.text = element_text(size=15, margin = margin(r = 0.35, unit = "cm")), 
        legend.key = element_rect(fill = "white"),
        legend.title = element_text(size=17, face = 'bold'),
        legend.title.align=0.5,
        legend.key.size = unit(1, "cm"),
        legend.spacing.x = unit(0.1, "cm"),
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

figure3 <-( Herb_reactNorm | maxTemp_reactNorm ) +
  plot_layout(guides = 'collect') &
  guides(colour = guide_legend(title.position = "top"),
         linetype = guide_legend(title.position = "top")) &
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'bottom', 
        legend.direction="horizontal",
        legend.text = element_text(size=15, margin = margin(r = 0.35, unit = "cm")), 
        legend.key = element_rect(fill = "white"),
        legend.title = element_text(size=17, face = 'bold'),
        legend.title.align=0.5,
        legend.key.size = unit(1, "cm"),
        legend.spacing.x = unit(0.1, "cm"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        plot.tag.position = c(0.1, 1.1),
        plot.tag = element_text(size = 20))
figure3

ggsave(filename = 'analysis/figures/main-text/figure3_herb_maxTemp_allParks.png', plot = figure3, device = "png",
       width = 15, height = 7, units = "in", dpi = 600)
ggsave(filename = 'analysis/figures/main-text/figure3_herb_maxTemp_allParks.pdf', plot = figure3, device = "pdf",
       width = 15, height = 7, units = "in", dpi = 600)

 
#### FIGURE S1 ####

S1a <- plotCline_withData(modHCN_imperv_herb_T2, "HCN", vals = vals, park = "Erindale", raw_data_df = allPlants_allParks, cols_df = park_coords)
S1b <- plotCline_withData(modHCN_imperv_herb_T2, "HCN", vals = vals, park = "High Park", raw_data_df = allPlants_allParks, cols_df = park_coords)
S1c <- plotCline_withData(modHCN_imperv_herb_T2, "HCN", vals = vals, park = "Humber", raw_data_df = allPlants_allParks, cols_df = park_coords)
S1d <- plotCline_withData(modHCN_imperv_herb_T2, "HCN", vals = vals, park = "Riverdale", raw_data_df = allPlants_allParks, cols_df = park_coords)
S1e <- plotCline_withData(modHCN_imperv_herb_T2, "HCN", vals = vals, park = "Rouge", raw_data_df = allPlants_allParks, cols_df = park_coords)
S1f <- plotCline_withData(modHCN_imperv_herb_T2, "HCN", vals = vals, park = "All Parks", raw_data_df = allPlants_allParks, cols_df = park_coords)

figureS1 <-( S1a | S1b | S1c) / ( S1d | S1e | S1f ) +
  plot_layout(guides = 'collect') &
  # guides(colour = guide_legend(title.position = "top"),
  #        linetype = guide_legend(title.position = "top")) &
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'bottom', 
        legend.direction="horizontal",
        legend.text = element_text(size=15, margin = margin(r = 0.35, unit = "cm")), 
        legend.key = element_rect(fill = "white"),
        legend.title = element_text(size=17, face = 'bold'),
        legend.title.align=0.5,
        legend.key.size = unit(1, "cm"),
        legend.spacing.x = unit(0.1, "cm"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        plot.tag.position = c(0.1, 1),
        plot.tag = element_text(size = 20),
        plot.title = element_text(size = 19, face = "bold"))
figureS1

ggsave(filename = 'analysis/figures/supplemental/figureS1_HCN_by_imperv_splitPark.png', plot = figureS1, device = "png",
       width = 14, height = 9, units = "in", dpi = 600)
ggsave(filename = 'analysis/figures/supplemental/figureS1_HCN_by_imperv_splitPark.pdf', plot = figureS1, device = "pdf",
       width = 14, height = 9, units = "in", dpi = 600)

#### FIGURE S1 ####

S2a <- plotCline_withData(modAc_imperv_herb_T3, "Ac", vals = vals, park = "Erindale", raw_data_df = allPlants_allParks, cols_df = park_coords)
S2b <- plotCline_withData(modAc_imperv_herb_T3, "Ac", vals = vals, park = "High Park", raw_data_df = allPlants_allParks, cols_df = park_coords)
S2c <- plotCline_withData(modAc_imperv_herb_T3, "Ac", vals = vals, park = "Humber", raw_data_df = allPlants_allParks, cols_df = park_coords)
S2d <- plotCline_withData(modAc_imperv_herb_T3, "Ac", vals = vals, park = "Riverdale", raw_data_df = allPlants_allParks, cols_df = park_coords)
S2e <- plotCline_withData(modAc_imperv_herb_T3, "Ac", vals = vals, park = "Rouge", raw_data_df = allPlants_allParks, cols_df = park_coords)
S2f <- plotCline_withData(modAc_imperv_herb_T3, "Ac", vals = vals, park = "All Parks", raw_data_df = allPlants_allParks, cols_df = park_coords)

figureS2 <-( S2a | S2b | S2c) / ( S2d | S2e | S2f ) +
  plot_layout(guides = 'collect') &
  # guides(colour = guide_legend(title.position = "top"),
  #        linetype = guide_legend(title.position = "top")) &
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'bottom', 
        legend.direction="horizontal",
        legend.text = element_text(size=15, margin = margin(r = 0.35, unit = "cm")), 
        legend.key = element_rect(fill = "white"),
        legend.title = element_text(size=17, face = 'bold'),
        legend.title.align=0.5,
        legend.key.size = unit(1, "cm"),
        legend.spacing.x = unit(0.1, "cm"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        plot.tag.position = c(0.1, 1),
        plot.tag = element_text(size = 20),
        plot.title = element_text(size = 19, face = "bold"))
figureS2

ggsave(filename = 'analysis/figures/supplemental/figureS2_Ac_by_imperv_splitPark.png', plot = figureS2, device = "png",
       width = 14, height = 9, units = "in", dpi = 600)
ggsave(filename = 'analysis/figures/supplemental/figureS2_Ac_by_imperv_splitPark.pdf', plot = figureS2, device = "pdf",
       width = 14, height = 9, units = "in", dpi = 600)

#### FIGURE S1 ####

S3a <- plotCline_withData(modLi_imperv_herb_T3, "Li", vals = vals, park = "Erindale", raw_data_df = allPlants_allParks, cols_df = park_coords)
S3b <- plotCline_withData(modLi_imperv_herb_T3, "Li", vals = vals, park = "High Park", raw_data_df = allPlants_allParks, cols_df = park_coords)
S3c <- plotCline_withData(modLi_imperv_herb_T3, "Li", vals = vals, park = "Humber", raw_data_df = allPlants_allParks, cols_df = park_coords)
S3d <- plotCline_withData(modLi_imperv_herb_T3, "Li", vals = vals, park = "Riverdale", raw_data_df = allPlants_allParks, cols_df = park_coords)
S3e <- plotCline_withData(modLi_imperv_herb_T3, "Li", vals = vals, park = "Rouge", raw_data_df = allPlants_allParks, cols_df = park_coords)
S3f <- plotCline_withData(modLi_imperv_herb_T3, "Li", vals = vals, park = "All Parks", raw_data_df = allPlants_allParks, cols_df = park_coords)

figureS3 <-( S3a | S3b | S3c) / ( S3d | S3e | S3f ) +
  plot_layout(guides = 'collect') &
  # guides(colour = guide_legend(title.position = "top"),
  #        linetype = guide_legend(title.position = "top")) &
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'bottom', 
        legend.direction="horizontal",
        legend.text = element_text(size=15, margin = margin(r = 0.35, unit = "cm")), 
        legend.key = element_rect(fill = "white"),
        legend.title = element_text(size=17, face = 'bold'),
        legend.title.align=0.5,
        legend.key.size = unit(1, "cm"),
        legend.spacing.x = unit(0.1, "cm"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", size = 1),
        plot.tag.position = c(0.1, 1),
        plot.tag = element_text(size = 20),
        plot.title = element_text(size = 19, face = "bold"))
figureS3

ggsave(filename = 'analysis/figures/supplemental/figureS3_HCN_by_imperv_splitPark.png', plot = figureS3, device = "png",
       width = 14, height = 9, units = "in", dpi = 600)
ggsave(filename = 'analysis/figures/supplemental/figureS3_HCN_by_imperv_splitPark.pdf', plot = figureS3, device = "pdf",
       width = 14, height = 9, units = "in", dpi = 600)
