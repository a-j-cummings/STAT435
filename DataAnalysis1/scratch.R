library(tidyverse)

farms <- read_csv('farms.csv')

fresno_area <- read_csv('fresno_area.csv')

temp <- fresno_area %>% 
  filter(acres < 150) %>% 
  group_by(city) %>% 
  summarise(n = n())

temp2 <- fresno_area %>% 
  filter(city %in% c('FRESNO', 'MERCED'),
         acres < 120) %>% 
  mutate(city = str_to_title(city),
         sale_price = sale_price,
         ppa = acres/sale_price) %>% 
  select(city, acres, sale_price, ppa, sale_date)
write_csv(temp2, 'farms.csv')
