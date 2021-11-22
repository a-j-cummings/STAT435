library(tidyverse)

housing_raw <- read_csv('wilner_housing.csv')


# Proximity: 0 = close, 1 = distant
# Contact: 0 = frequent, 1 = infrequent
# Norms: 0 = favorable, 1 = unfavorable
housing <- housing_raw %>% 
  janitor::clean_names() %>% 
  mutate(proximity = proximity - 1,
         contact = contact - 1,
         norms = norms - 1) %>% 
  rename(favorable = fav, unfavorable = unfav) %>% 
  pivot_longer(-c(proximity, contact, norms), names_to = 'attitude') %>% 
  mutate(obs = map(value, ~rep_len(1, .x))) %>% 
  unnest(obs) %>% 
  select(-value, -obs)

# should you wish to use the raw form with some cleaning:
housing_raw <- housing %>% 
  group_by(proximity, contact, norms, attitude) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(proximity, contact, norms), names_from = attitude, values_from = n)
