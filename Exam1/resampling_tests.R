# Resampling tests (MC permutation test and bootstrap) for farms analysis
set.seed(811)

library(tidyverse)
library(foreach)
library(doParallel)
registerDoParallel(parallel::detectCores())

farms <- read_csv('farms.csv')
all_farms <- farms %>% 
  select(ppa) %>% 
  pull()
fresno <- farms %>% 
  filter(city == 'Fresno') %>% 
  select(ppa) %>% 
  pull()
merced <- farms %>% 
  filter(city == 'Merced') %>% 
  select(ppa) %>% 
  pull()

# Perform an MC permutation test on difference in medians (Merced - Fresno)
# assess MC error on the p-value
test_stat_true <- median(merced) - median(fresno)
n_merced <- length(merced)
nsamps <- 1e4
test_stats_all <- foreach (i=1:nsamps, .combine=c) %dopar% {
  all_farms_i <- sample(all_farms)
  merced_i <- all_farms_i[1:n_merced]
  fresno_i <- all_farms_i[-(1:n_merced)]
  median(merced_i) - median(fresno_i)
}

p <- mean(abs(test_stats_all) >= abs(test_stat_true))
mc_ci <- round(p + c(-1,1)*qnorm(0.975)*sqrt(p*(1-p)/nsamps), 3)

perm_fig <- ggplot() + 
  geom_histogram(aes(test_stats_all), bins = 30) + 
  geom_vline(aes(xintercept = test_stat_true), color = 'red', size = 2) + 
  geom_rect(aes(xmin = -Inf, xmax = -abs(test_stat_true), 
                ymin = -Inf, ymax = Inf), fill = 'red', alpha = 0.2)  +
  geom_rect(aes(xmin = abs(test_stat_true), xmax = Inf, 
                ymin = -Inf, ymax = Inf), fill = 'red', alpha = 0.2)  +
  labs(x = 'difference in medians', 
       #title = 'MC permutation test on difference in medians',
       subtitle = paste0('observed statistic: ', round(test_stat_true, 3),
                        '\np-value = ', round(p, 3), 
                        ' (95% MC CI: ', mc_ci[1], ', ', mc_ci[2], ')'))

# Perform a bootstrap test on difference in medians (Merced - Fresno)
# assess MC error on the p-value
test_stats_all_2 <- foreach (i=1:nsamps, .combine=c) %dopar% {
  all_farms_i <- sample(all_farms, replace = TRUE)
  merced_i <- all_farms_i[1:n_merced]
  fresno_i <- all_farms_i[-(1:n_merced)]
  median(merced_i) - median(fresno_i)
}

p2 <- mean(abs(test_stats_all_2) >= abs(test_stat_true))
mc_ci2 <- round(p2 + c(-1,1)*qnorm(0.975)*sqrt(p2*(1-p2)/nsamps), 3)

boot_fig <- ggplot() + 
  geom_histogram(aes(test_stats_all_2), bins = 30) + 
  geom_vline(aes(xintercept = test_stat_true), color = 'red', size = 2) + 
  geom_rect(aes(xmin = -Inf, xmax = -abs(test_stat_true), 
                ymin = -Inf, ymax = Inf), fill = 'red', alpha = 0.2)  +
  geom_rect(aes(xmin = abs(test_stat_true), xmax = Inf, 
                ymin = -Inf, ymax = Inf), fill = 'red', alpha = 0.2)  +
  labs(x = 'difference in medians', 
       #title = 'Bootstrap test on difference in medians',
       subtitle = paste0('observed statistic: ', round(test_stat_true, 3),
                        '\np-value = ', round(p2, 3), 
                        ' (95% MC CI: ', mc_ci2[1], ', ', mc_ci2[2], ')'))
