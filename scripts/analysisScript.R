# Script with all main analyses
#
# Author: James S. Santangelo

#### SETUP ####

# Load datasets with plant-level data for all parks
# Convert Herbivory to proportion. Add 'Habitat'
allPlants_allParks <- read_csv("data-clean/allPlants_allParks.csv", show_col_types = FALSE) %>% 
  mutate(Herbivory = Herbivory / 100) %>% 
  mutate(Habitat = ifelse(distance_set0 == 0, 'Park', 'Transect'))


#### CHANGES IN HCN/GENE PRESENCE/ABSENCE ####

# Run global logistic regressions testing whether HCN or gene (i.e., Ac/Li) presence/absence
#   varies across parks, Habitat (i.e. park green space), or their interaction.
#   Also run follow-up models where "Habitat" is replaced with impervious surface and Herbivory

### HCN ###

## Habitat predictor

# Model with contrasts for type 3
modHCN_hab_T3 <- glm(HCN ~ Park * Habitat, 
                     family = 'binomial', data = allPlants_allParks,
                     contrasts=list(Park=contr.sum, Habitat=contr.sum))

# Model diagnostics. Looks good
diagn_HCN_hab <- simulateResiduals(fittedModel = modHCN_hab_T3, plot = T)

# F-tests with type 3 SS. Significant interaction.
modHCN_hab_AnT3 <- Anova(modHCN_hab_T3, type = 3)

## Percent imperv & Herbivory predictors

# Model with contrasts for type 3
modHCN_imperv_herb_T3 <- glm(HCN ~ Park * percent_asphalt * Herbivory, 
                          family = 'binomial', data = allPlants_allParks,
                          contrasts=list(Park=contr.sum))

# Model diagnostics. Looks good
diagn_HCN_imperv_herb <- simulateResiduals(fittedModel = modHCN_imperv_herb_T3, 
                                           plot = T)

# F-tests with type 3 SS. No interactions
modHCN_imperv_herb_AnT3 <- Anova(modHCN_imperv_herb_T3, type = 3)

# Refit with type 2 to test main effects
modHCN_imperv_herb_T2 <- glm(HCN ~ Park * percent_asphalt * Herbivory, 
                                   family = 'binomial', data = allPlants_allParks)
modHCN_imperv_herb_AnT2 <- Anova(modHCN_imperv_herb_T2, type = 2)

### Ac ###

## Habitat predictor

# Model with contrasts for type 3
modAc_hab_T3 <- glm(Ac ~ Park * Habitat, 
                     family = 'binomial', data = allPlants_allParks,
                     contrasts=list(Park=contr.sum, Habitat=contr.sum))

# Model diagnostics. Looks good
diagn_Ac_hab <- simulateResiduals(fittedModel = modAc_hab_T3, plot = T)
# More robust test of outliers. None detected. 
diagn_Ac_hab_outTest <- testOutliers(diagn_Ac_hab, type = 'bootstrap')

# F-tests with type 3 SS. Significant interaction.
modAc_hab_AnT3 <- Anova(modAc_hab_T3, type = 3)

## Percent imperv & Herbivory predictors

# Model with contrasts for type 3
modAc_imperv_herb_T3 <- glm(Ac ~ Park * percent_asphalt * Herbivory, 
                             family = 'binomial', data = allPlants_allParks,
                             contrasts=list(Park=contr.sum))

# Model diagnostics. Looks good
diagn_Ac_imperv_herb <- simulateResiduals(fittedModel = modAc_imperv_herb_T3, 
                                           plot = T)
# More robust test of outliers. None detected. 
diagn_Ac_imperv_herb_outTest <- testOutliers(diagn_Ac_imperv_herb, type = 'bootstrap')

# F-tests with type 3 SS. Interaction present
modAc_imperv_herb_AnT3 <- Anova(modAc_imperv_herb_T3, type = 3)

### Li ###

## Habitat predictor

# Model with contrasts for type 3
modLi_hab_T3 <- glm(Li ~ Park * Habitat, 
                    family = 'binomial', data = allPlants_allParks,
                    contrasts=list(Park=contr.sum, Habitat=contr.sum))

# Model diagnostics. Looks good
diagn_Li_hab <- simulateResiduals(fittedModel = modLi_hab_T3, plot = T)

# F-tests with type 3 SS. No significant interaction.
modLi_hab_AnT3 <- Anova(modLi_hab_T3, type = 3)

# Refit with type 2 to test main effects
modLi_imperv_herb_T2 <- glm(Li ~ Park * Habitat, 
                            family = 'binomial', data = allPlants_allParks)
modLi_imperv_herb_AnT2 <- Anova(modLi_imperv_herb_T2, type = 2)

## Percent imperv & Herbivory predictors

# Model with contrasts for type 3
modLi_imperv_herb_T3 <- glm(Li ~ Park * percent_asphalt * Herbivory, 
                            family = 'binomial', data = allPlants_allParks,
                            contrasts=list(Park=contr.sum))

# Model diagnostics. Looks good
diagn_Li_imperv_herb <- simulateResiduals(fittedModel = modLi_imperv_herb_T3, 
                                          plot = T)

# F-tests with type 3 SS. Interaction present
modLi_imperv_herb_AnT3 <- Anova(modLi_imperv_herb_T3, type = 3)
modLi_imperv_herb_AnT3

#### CHANGES IN HERBIVORY ####

## Habitat predictor

# Model using type 3 SS
modHerb_hab_T3 <- glm(Herbivory ~ Park * Habitat, 
                     data = allPlants_allParks, family = 'binomial',
                     contrasts=list(Park=contr.sum, Habitat=contr.sum))
modHerb_hab_AnT3<- Anova(modHerb_hab_T3, type = 3)

# Model diagnostics. Doesn't look great. 
diagn_modHerb_hab_T3 <- simulateResiduals(fittedModel = modHerb_hab_T3, plot = T)
# More robust test of outliers. Significant 
diagn_modHerb_hab_T3_outTest <- testOutliers(diagn_modHerb_hab_T3, type = 'bootstrap')

# Refit with type 2 since no interactions
modHerb_hab_T2 <- glm(Herbivory ~ Park * Habitat, 
                        data = allPlants_allParks, family = 'binomial')
modHerb_hab_AnT2 <- Anova(modHerb_hab_T2, type = 2)

## Percent Imperv as predictor

# Model using type 3 SS
modHerb_imperv_T3 <- glm(Herbivory ~ Park * percent_asphalt, 
                      data = allPlants_allParks, family = 'binomial',
                      contrasts=list(Park=contr.sum))
modHerb_imperv_AnT3<- Anova(modHerb_imperv_T3, type = 3)

# Model diagnostics. Doesn't look great. 
diagn_modHerb_imperv_T3 <- simulateResiduals(fittedModel = modHerb_imperv_T3, plot = T)
# More robust test of outliers. Significant 
diagn_modHerb_imperv_T3_outTest <- testOutliers(diagn_modHerb_imperv_T3, type = 'bootstrap')

# Refit with type 2 since no interactions
modHerb_imperv_T2 <- glm(Herbivory ~ Park * percent_asphalt, 
                      data = allPlants_allParks, family = 'binomial')
modHerb_imperv_AnT2 <- Anova(modHerb_imperv_T2, type = 2)



iButton_summaries <- read_csv("data-clean/iButton_summaries.csv") %>% 
  mutate(Habitat = ifelse(Location == "Park", "Park", "Transect"))


