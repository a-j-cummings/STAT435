## ---- setup, include=FALSE--------------------------------
library(tidyverse)
farms <- read_csv('farms.csv') %>% 
  mutate(ppa = sale_price/acres/10000)


## ---- summary_stats, echo = FALSE-------------------------
farms %>% 
  group_by(city) %>% 
  summarise(n = n(),
            Mean = mean(ppa),
            StDev = sd(ppa),
            Median = median(ppa),
            IQR = diff(quantile(ppa, c(0.25, 0.75)))) %>% 
  rename(City = city) %>% knitr::kable(digits = 2, caption = 'Summary statistics of small farm sale price-per-acre in the Fresno and Merced in 2000-2020.')


## ---- boxplot, echo = FALSE, fig.cap = 'Boxplot comparison of price-per-acre of Fresno and Merced farm sales.', out.width='50%', fig.align='center'----
ggplot(farms) + 
  geom_boxplot(aes(ppa, fill = city)) +
  labs(x = 'Price-per-acre ($10k)') + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


## ---- results, echo = FALSE-------------------------------
fresno <- farms %>% 
  filter(city == 'Fresno') %>% 
  select(ppa) %>% 
  pull()
merced <- farms %>% 
  filter(city == 'Merced') %>% 
  select(ppa) %>% 
  pull()
tibble(test = c('2-sample t-test', 'Mann-Whitney U-test'),
       pvalue = c(t.test(merced, fresno, var.equal = TRUE)$p.value,
                  wilcox.test(merced, fresno, paired = FALSE, exact = FALSE)$p.value)) %>% knitr::kable(digits = 3, caption = 'P-values for tests of equal mean/median')


## ---- parametric_power_prep, echo = FALSE-----------------
d <- 0.5 # corresponds to overpaying 20-25% (on average)
m <- 31 # n farms in Fresno
n <- 38 # n farms in Merced
sigma <- sd(farms$ppa)
alpha <- seq(0.0, 0.5, length.out = 100)
power <- 1 - pt(qt(alpha, m+n-2), m+n-2, d*sqrt(m*n/(m+n))/sigma)


## ---- parametric_power, echo=FALSE, out.width='50%', fig.align='center', fig.cap='Type II error probability of two-sample t-test as a function of significance level if true difference between Fresno and Merced price-per-acre is 20 to 25 percent'----
ggplot() + 
  geom_line(aes(alpha, 1-power)) +
  labs(x = latex2exp::TeX('$\\alpha$'), y = 'Pr(Type II error)')

