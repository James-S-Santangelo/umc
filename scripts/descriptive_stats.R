# Descriptive statistics

# Number of plants by Park
numPlants_byPark <- allPlants_allParks %>% 
  group_by(Park) %>% 
  tally()
