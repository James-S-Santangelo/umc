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

#### FIGURES ####

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
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 25)) +
  scalebar(y.min=43.5420, y.max=43.5430, x.min=-79.2660, x.max=-79.1370, 
           dist = 5, dist_unit = 'km', transform= TRUE, model='WGS84',
           height = 8, st.dist = 7.5, st.size = 7.5)

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
  coord_fixed(ratio = 1/1.25) +
  # coord_fixed(ratio = 0.625/1) +
  scalebar(y.min=43.5285, y.max=43.5335, x.min=-79.6505, x.max=-79.6400, 
           dist = 500, dist_unit = 'm', transform= TRUE, model='WGS84',
           height = 0.19, st.dist = 0.2, st.size = 7.5)

ggsave(filename = 'analysis/figures/main-text/figure1/figure1_erindale_inset.pdf', plot = erin_map_base, 
       device = 'pdf', dpi = 600, height = 10, width = 10, units = 'in')

## Humber inset

humber_map_base <- plot_map_inset('Humber', plant_df = allPlants_allParks, ibutton_df = ibutton_df, color_df = park_coords) +
  coord_fixed(ratio = 1.5/1) +
  # coord_fixed(ratio = 1/1) +
  scalebar(y.min=43.6405, y.max=43.6445, x.min=-79.4770, x.max=-79.4660, 
           dist = 500, dist_unit = 'm', transform= TRUE, model='WGS84',
           height = 0.12, st.dist = 0.15, st.size = 7.5)

ggsave(filename = 'analysis/figures/main-text/figure1/figure1_humber_inset.pdf', plot = humber_map_base, 
       device = 'pdf', dpi = 600, height = 8, width = 13, units = 'in')

## High Park inset

highPark_map_base <- plot_map_inset('High Park', plant_df = allPlants_allParks, ibutton_df = ibutton_df, color_df = park_coords) +
  coord_fixed(ratio = 2/1) +
  # coord_fixed(ratio = 1/1) +
  scalebar(y.min=43.6400, y.max=43.6435, x.min=-79.4390, x.max=-79.4320, 
           dist = 500, dist_unit = 'm', transform= TRUE, model='WGS84',
           height = 0.11, st.dist = 0.13, st.size = 7.5)

ggsave(filename = 'analysis/figures/main-text/figure1/figure1_highPark_inset.pdf', plot = highPark_map_base, 
       device = 'pdf', dpi = 600, height = 10, width = 10, units = 'in')

# Riverdal inset

river_map_base <- plot_map_inset('Riverdale', plant_df = allPlants_allParks, ibutton_df = ibutton_df, color_df = park_coords) +
  coord_fixed(ratio = 1.75/1) +
  # coord_fixed(ratio = 1/1) +
  scalebar(y.min=43.6655, y.max=43.6690, x.min=-79.3387, x.max=-79.3272, 
           dist = 500, dist_unit = 'm', transform= TRUE, model='WGS84',
           height = 0.12, st.dist = 0.15, st.size = 7.5)

ggsave(filename = 'analysis/figures/main-text/figure1/figure1_riverdale_inset.pdf', plot = river_map_base, 
       device = 'pdf', dpi = 600, height = 5, width = 12, units = 'in')

## Rouge inset

rouge_map_base <- plot_map_inset('Rouge', plant_df = allPlants_allParks, ibutton_df = ibutton_df, color_df = park_coords) +
  coord_fixed(ratio = 1.5/1) +
  # coord_fixed(ratio = 1/1) +
  scalebar(y.min=43.8045, y.max=43.8080, x.min=-79.1380, x.max=-79.1250, 
           dist = 500, dist_unit = 'm', transform= TRUE, model='WGS84',
           height = 0.14, st.dist = 0.19, st.size = 7.5)

ggsave(filename = 'analysis/figures/main-text/figure1/figure1_rouge_inset.pdf', plot = rouge_map_base, 
       device = 'pdf', dpi = 600, height = 8, width = 13, units = 'in')

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
        plot.tag.position = c(0.1, 1.05),
        plot.tag = element_text(size = 20))
figure3

ggsave(filename = 'analysis/figures/main-text/figure3_herb_maxTemp_allParks.png', plot = figure3, device = "png",
       width = 15, height = 7.5, units = "in", dpi = 600)
ggsave(filename = 'analysis/figures/main-text/figure3_herb_maxTemp_allParks.pdf', plot = figure3, device = "pdf",
       width = 15, height = 7.5, units = "in", dpi = 600)

 
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
