# Descriptive statistics

# Number of plants by Park
t <- allPlants_allParks %>% 
  group_by(Park) %>% 
  tally()