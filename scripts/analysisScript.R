# Script with all main analyses
#
# Author: James S. Santangelo

#### SETUP ####

# Load datasets with plant-level data for all parks
# Create binary category for presence/absence of herbivory
allPlants_allParks <- read_csv("data-clean/allPlants_allParks.csv", show_col_types = FALSE) %>% 
  mutate(Herbivory_bin = ifelse(Herbivory == 0, 0, 1))

#### CHANGES IN HCN/GENE PRESENCE/ABSENCE ####

# Run global logistic regressions testing whether HCN or gene (i.e., Ac/Li) presence/absence
#   varies across parks, with percent impervious surface, or the amount of herbivore damage. 
#   Herbivory treated as binary (yes/no) since many zeros made model fits look terrible if
#   treated as continuous. 

## HCN ##

# Model
modHCN_imperv_herb <- glm(HCN ~ Park * percent_asphalt * Herbivory_bin, 
                          family = 'binomial', data = allPlants_allParks)
par(mfrow = c(2, 2))
plot(modHCN_imperv_herb)
Anova(modHCN_imperv_herb, type = 3)

## Ac ##

# Model
modAc_imperv_herb <- glm(Ac ~ Park * percent_asphalt * Herbivory_bin, 
                          family = 'binomial', data = allPlants_allParks)
plot(modAc_imperv_herb)
Anova(modAc_imperv_herb, type = 3)

## Li ##

# Model
modLi_imperv_herb <- glm(Li ~ Park * percent_asphalt * Herbivory_bin, 
                          family = 'binomial', data = allPlants_allParks)
plot(modLi_imperv_herb)
Anova(modLi_imperv_herb, type = 3)

#### CHANGES IN HERBIVORY ####

# Binary herbivory
herb_mod_bin <- glm(Herbivory_bin ~ Park * percent_asphalt, data = allPlants_allParks, 
                    family = 'binomial')
plot(herb_mod_bin)
Anova(herb_mod_bin, type = 3)

# Continuous herbivory
# Note: This model looks terrible
herb_mod_cont <- lm(sqrt(Herbivory) ~ Park * percent_asphalt, data = allPlants_allParks)
plot(herb_mod_cont)
Anova(herb_mod_cont, type = 3)

iButton_summaries <- read_csv("data-clean/iButton_summaries.csv") %>% 
  mutate(Habitat = ifelse(Location == "Park", "Park", "Transect"))


