# Script with all main analyses
#
# Author: James S. Santangelo

#### SETUP ####

# Load datasets with plant-level data for all parks
# Convert Herbivory to proportion. Add 'Habitat'
allPlants_allParks <- read_csv("data-clean/allPlants_allParks.csv", show_col_types = FALSE) %>% 
  mutate(Herbivory = Herbivory / 100) %>% 
  mutate(Habitat = ifelse(distance_set0 == 0, 'Park', 'Transect'))

# Load data with summaries of daily temperature values for each Ibutton
iButton_summaries <- read_csv("data-clean/iButton_summaries.csv", show_col_types = FALSE) %>% 
  mutate(Habitat = ifelse(Location == "Park", "Park", "Transect"),
         Park = ifelse(Park == 'HighPark', "High Park", Park))

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

modHCN_hab_T3_phoc <- emmeans(modHCN_hab_T3, specs = pairwise ~ Habitat|Park)

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

# Predicted change in probability of being HCN+ across % imperv gradient
probHCN_predicted <- ggeffects::ggeffect(modHCN_imperv_herb_T2, 
                                         terms = c('percent_asphalt[all]'))

imperv0_predHCN <- probHCN_predicted %>% filter(x == min(x)) %>% pull(predicted)
imperv1_predHCN <- probHCN_predicted %>% filter(x == max(x)) %>% pull(predicted)
pred_percent_changeHCN <- round((imperv1_predHCN - imperv0_predHCN) / imperv0_predHCN, 2) * 100

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

modAc_hab_T3_phoc <- emmeans(modAc_hab_T3, specs = pairwise ~ Habitat|Park)

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

# Predicted change in probability of being Ac+ across % imperv gradient
probAc_predicted <- ggeffects::ggeffect(modAc_imperv_herb_T3, 
                                         terms = c('percent_asphalt[all]'))

imperv0_predAc <- probAc_predicted %>% filter(x == min(x)) %>% pull(predicted)
imperv1_predAc <- probAc_predicted %>% filter(x == max(x)) %>% pull(predicted)
pred_percent_changeAc <- round((imperv1_predAc - imperv0_predAc) / imperv0_predAc, 2) * 100

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
modLi_hab_T2 <- glm(Li ~ Park * Habitat, 
                    family = 'binomial', data = allPlants_allParks)
modLi_hab_AnT2 <- Anova(modLi_hab_T2, type = 2)

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

# Predicted change in herbivory across % imperv gradient
propHerb_predicted <- ggeffects::ggeffect(modHerb_imperv_T2, 
                                          terms = c('percent_asphalt[all]'))

imperv0_predHerb <- propHerb_predicted %>% filter(x == min(x)) %>% pull(predicted)
imperv1_predHerb <- propHerb_predicted %>% filter(x == max(x)) %>% pull(predicted)
pred_percent_changeHerb <- round((imperv1_predHerb - imperv0_predHerb) / imperv0_predHerb, 2) * 100

#### TEMPERATURE ANALYSES ####

# Identify warmest and coldest months
warm_cold_months <- iButton_summaries %>% 
  group_by(Month) %>% 
  summarise(mean_minTemp = mean(minTemp),
            mean_maxTemp = mean(maxTemp)) %>% 
  filter(mean_minTemp == min(mean_minTemp) | mean_maxTemp == max(mean_maxTemp)) %>% 
  pull(Month)

# Create dataframes with summer and winter temperature
summer_temps <- iButton_summaries %>% filter(Month == 'July')
winter_temps <- iButton_summaries %>% filter(Month == 'February')

### Models

## Summer temps

# Habitat as predictor

# Model with type 3 SS. No interaction
mod_summerTemps_hab_T3 <- lmer(maxTemp ~ Park * Habitat + (1|Button), data = summer_temps,
                           contrasts=list(Park=contr.sum, Habitat=contr.sum))
mod_summerTemps__hab_AnT3 <- Anova(mod_summerTemps_hab_T3, type = 3)

# Model diagnostics. Looks like possible heteroscedasticity 
diagn_mod_summerTemps_hab_T3 <- simulateResiduals(fittedModel = mod_summerTemps_hab_T3, plot = T)
# More robust test of outliers. No outliers detected 
diagn_mod_summerTemps_hab_T3_outTest <- testOutliers(diagn_mod_summerTemps_hab_T3, type = 'bootstrap')

# Refit with type 2 SS since no interaction
mod_summerTemps_hab_T2 <- lmer(maxTemp ~ Park * Habitat + (1|Button), data = summer_temps)
mod_summerTemps_hab_AnT2 <- Anova(mod_summerTemps_hab_T2, type = 2)

# Percent Imperv as predictor

# Model with type 3 SS. Interaction present
mod_summerTemps_imperv_T3 <- lmer(maxTemp ~ Park * Percent_asphalt + (1|Button), data = summer_temps,
                               contrasts=list(Park=contr.sum))
mod_summerTemps_imperv_AnT3 <- Anova(mod_summerTemps_imperv_T3, type = 3)

# Model diagnostics. Looks like possible heteroscedasticity 
diagn_mod_summerTemps_imperv_T3 <- simulateResiduals(fittedModel = mod_summerTemps_imperv_T3, plot = T)
# More robust test of outliers. No outliers detected 
diagn_mod_summerTemps_imperv_T3_outTest <- testOutliers(diagn_mod_summerTemps_imperv_T3, type = 'bootstrap')

# Predicted change in maximum temperature across % imperv gradient for Erindale and Rouge
propMaxTemp_predicted <- ggeffects::ggeffect(mod_summerTemps_imperv_T3, 
                                          terms = c('Percent_asphalt[all]', 'Park'))

imperv0_predMaxTemp_Erin <- propMaxTemp_predicted %>% 
  filter(x == min(x) & group == 'Erindale') %>% 
  pull(predicted)
imperv1_predMaxTemp_Erin <- propMaxTemp_predicted %>% 
  filter(x == max(x) & group == 'Erindale') %>% 
  pull(predicted)
imperv0_predMaxTemp_Rouge <- propMaxTemp_predicted %>% 
  filter(x == min(x) & group == 'Rouge') %>% 
  pull(predicted)
imperv1_predMaxTemp_Rouge <- propMaxTemp_predicted %>% 
  filter(x == max(x) & group == 'Rouge') %>% 
  pull(predicted)

pred_percent_changeMaxTemp_Erin <- round((imperv1_predMaxTemp_Erin - imperv0_predMaxTemp_Erin) / imperv0_predMaxTemp_Erin, 2) * 100
pred_percent_changeMaxTemp_Rouge <- round((imperv1_predMaxTemp_Rouge - imperv0_predMaxTemp_Rouge) / imperv0_predMaxTemp_Rouge, 2) * 100

## Winter temps

# Habitat as predictor

# Model with type 3 SS. No interaction
mod_winterTemps_hab_T3 <- lmer(minTemp ~ Park * Habitat + (1|Button), data = winter_temps,
                               contrasts=list(Park=contr.sum, Habitat=contr.sum))
mod_winterTemps__hab_AnT3 <- Anova(mod_winterTemps_hab_T3, type = 3)

# Model diagnostics. Looks like possible heteroscedasticity 
diagn_mod_winterTemps_hab_T3 <- simulateResiduals(fittedModel = mod_winterTemps_hab_T3, plot = T)
# More robust test of outliers. No outliers detected 
diagn_mod_winterTemps_hab_T3_outTest <- testOutliers(diagn_mod_winterTemps_hab_T3, type = 'bootstrap')

# Refit with type 2 SS since no interaction
mod_winterTemps_hab_T2 <- lmer(minTemp ~ Park * Habitat + (1|Button), data = winter_temps)
mod_winterTemps_hab_AnT2 <- Anova(mod_winterTemps_hab_T2, type = 2)

# Percent Imperv as predictor

# Model with type 3 SS. No interactions
mod_winterTemps_imperv_T3 <- lmer(minTemp ~ Park * Percent_asphalt + (1|Button), data = winter_temps,
                                  contrasts=list(Park=contr.sum))
mod_winterTemps_imperv_AnT3 <- Anova(mod_winterTemps_imperv_T3, type = 3)

# Model diagnostics. Looks like possible heteroscedasticity 
diagn_mod_winterTemps_imperv_T3 <- simulateResiduals(fittedModel = mod_winterTemps_imperv_T3, plot = T)
# More robust test of outliers. No outliers detected 
diagn_mod_winterTemps_imperv_T3_outTest <- testOutliers(diagn_mod_winterTemps_imperv_T3, type = 'bootstrap')
