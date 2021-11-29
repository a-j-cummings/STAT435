# Performs analysis on Fiji contraceptive use data

# load packages
library(tidyverse)
library(cowplot)
library(latex2exp)

# data manipulation
fiji_raw <- read_table('fiji_contraceptives.dat')

fiji_short <- fiji_raw %>% 
  janitor::clean_names() %>% 
  select(-x6)


# EDA
# marginal of education | age
education_plot <- fiji_short %>% 
  group_by(education, age) %>% 
  summarise(using = sum(using), 
         not_using = sum(not_using),
         n_responses = using + not_using,
         prop_using = using/n_responses, .groups = 'drop') %>% 
  ggplot(aes(age, prop_using, color = education)) +
    geom_point(aes(size = n_responses)) + 
    geom_line(aes(as.numeric(factor(age)))) + 
    labs(x = 'Age', y = 'Proportion using contraceptives',
         size = '# of respondents', color = 'Education')
# marginal of wants_more | age
wants_more_plot <- fiji_short %>% 
  group_by(wants_more, age) %>% 
  summarise(using = sum(using), 
            not_using = sum(not_using),
            n_responses = using + not_using,
            prop_using = using/n_responses, .groups = 'drop') %>% 
  ggplot(aes(age, prop_using, color = wants_more)) +
  geom_point(aes(size = n_responses)) + 
  geom_line(aes(as.numeric(factor(age)))) +
  labs(x = 'Age', y = 'Proportion using contraceptives',
       size = '# of respondents', color = 'Desires more children')
# both
both <- fiji_short %>% 
  mutate(n_responses = not_using + using,
         prop_using = using/n_responses) %>% 
  ggplot() + 
    geom_point(aes(age, prop_using,
                   color = interaction(education, wants_more),
                   size = n_responses))  +
    geom_line(aes(as.numeric(factor(age)), prop_using,
                  color = interaction(education, wants_more))) + 
    labs(x = 'Age', y = 'Proportion using contraceptives',
         size = '# of respondents', color = 'Interaction(Education, Desire for children)')
plot_grid(plot_grid(education_plot, wants_more_plot), both, nrow = 2)


# fit the model
mod_2wayc2 <- glm(cbind(using, not_using) ~ 
                  -1 + (age + wants_more)^2 + education,
                  family = binomial(link = "logit"), data = fiji_short)

# report coefficients
summary(mod_2wayc2)$coefficients %>% 
  as_tibble() %>% 
  add_column(` ` = c('beta 1,1',
                'beta 1,2',
                'beta 1,3',
                'beta 1,4',
                'beta 2',
                'beta 3',
                'alpha 1',
                'alpha 2',
                'alpha 3'),
            Coeficient = c('Age < 25',
                           'Age 25-29',
                           'Age 30-39',
                           'Age 40-49',
                           'Wants more kids',
                           'Low education',
                           'Age 25-29 : Wants more kids',
                           'Age 30-39 : Wants more kids',
                           'Age 40-49 : Wants more kids'),
            .before = 'Estimate') %>% 
  add_column(Sig. = c('Y', 'Y', 'Y', 'Y', 'N', 'Y', 'N', 'Y', 'Y'),
             .after = 'Pr(>|z|)') %>% 
  rename('z score' = 'z value') %>% 
  knitr::kable(digits = 3, align = 'lcllll', caption = "Parameter estimates, standard errors, z scores and p-values from the model defined in (2). Significance ('Sig.') is determined at a 0.05 cutoff. The increasing contraceptive usage with increasing age noted in Figure 1 is seen in the estimates of the beta 1,* coefficients. A desire for more children and lower education are both associated with lower contraceptive usage, though the effect of desire for more children is most strongly felt in conjuction with the higher two age buckets.")


# Assessing model fit
# summarizing the model
fiji_short2 <- fiji_short %>% 
  mutate(pred = predict(mod_2wayc2, type = 'response')*(using+not_using),
         obs = using)
actual <- xtabs(obs ~ age + wants_more + education, data = fiji_short2)
expected <- xtabs(pred ~ age + wants_more + education, data = fiji_short2)

df <- 7
# Pearson ChiSq
pearson <- sum((actual - expected)**2/expected)
p_pearson <- 1 - pchisq(pearson, df)
# Neyman ChiSq
neyman <- sum((actual - expected)**2/actual)
p_neyman <- 1 - pchisq(neyman, df)

