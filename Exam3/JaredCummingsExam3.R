# Code for repeating analysis from final report
knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)
library(MASS)

select <- dplyr::select

fiji_raw <- read_csv('fiji_contraceptives.csv')

fiji_short <- fiji_raw %>%
  janitor::clean_names()

fiji_long <- fiji_short %>%
  pivot_longer(-c(age, education, wants_more), names_to = 'usage', 
               values_to = 'count')

# Table 1
fiji_short %>%
  rename(Age = age, Education = education, 
         'Desires more children' = wants_more, 
         'Never used contraceptives' = not_using,
         'Used contraceptives' = using) %>%
  knitr::kable(caption = 'Data as reported by Little and Pullum. Age is a factor with four levels, education, desire for children (wants_more) and contraceptive usage (reported here in the not_using and using columns) all have two levels. The contingency table that the loglinear model will fit on will then be a 4x2x2x2 table.')


# Figure 1
fiji_long %>%
  mutate(usage = case_when(usage == 'not_using' ~ 'no',
                           TRUE ~ 'yes')) %>%
ggplot(aes(as.numeric(factor(age)), count, color = education,
                      shape = usage, linetype = wants_more)) + 
  geom_point() +
  geom_line() +
  labs(x = 'Age', y = 'Count', color = 'Education',
       linetype = 'Wants more children', shape = 'Contraceptive usage') + 
  scale_x_discrete(limits=unique(fiji_long$age))


# Fitting model
model <- glm(count ~ age + usage + education + wants_more +
                     age:usage + age:education + age:wants_more + 
                     usage:education + usage:wants_more,
             data = fiji_long, family = poisson(link = 'log'))


# Table 2
summary(model)$coefficients %>% 
  as_tibble() %>% 
  add_column(` ` = c('beta 0',
                'beta 1,1',
                'beta 1,2',
                'beta 1,3',
                'beta 2',
                'beta 3',
                'beta 4',
                paste('alpha', 1:11)),
            Coeficient = c('Intercept',
                           'Age 25-29',
                           'Age 30-39',
                           'Age 40-49',
                           'Used contraceptives',
                           'Low education',
                           'Wants more kids',
                           'Age 25-29 : Used contraceptives',
                           'Age 30-39 : Used contraceptives',
                           'Age 40-49 : Used contraceptives',
                           'Age 25-29 : Low education',
                           'Age 30-39 : Low education',
                           'Age 40-49 : Low education',
                           'Age 25-29 : Wants more kids',
                           'Age 30-39 : Wants more kids',
                           'Age 40-49 : Wants more kids',
                           'Used contraceptives : Low education',
                           'Used contraceptives : Wants more kids'),
            .before = 'Estimate') %>% 
  add_column(Sig. = c('Y', 'N', rep('Y', 16)),
             .after = 'Pr(>|z|)') %>% 
  rename('z score' = 'z value') %>% 
  knitr::kable(digits = 2, align = 'lcrrrrc', caption = "Parameter estimates, standard errors, z scores and p-values from the model defined in (2). Significance ('Sig.') is determined at a 0.05 cutoff. Of possible two-way interactions, all but an interaction between education and desire for more children were included in the model. The significance of interactions with age is not surprising after the discussion given in the Data section.")


# summarizing the model
fiji_long2 <- fiji_long %>% 
  mutate(pred = predict(model, type = 'response'),
         obs = count)
actual <- xtabs(obs ~ age + wants_more + education, data = fiji_long2)
expected <- xtabs(pred ~ age + wants_more + education, data = fiji_long2)

df <- 14
# Pearson ChiSq
pearson <- sum((actual - expected)**2/expected)
p_pearson <- 1 - pchisq(pearson, df)
# Neyman ChiSq
neyman <- sum((actual - expected)**2/actual)
p_neyman <- 1 - pchisq(neyman, df)


# Figure 2
fiji_long2 %>%
  mutate(lb = pred - qnorm(0.975)*1/sqrt(pred),
         ub = pred + qnorm(0.975)*1/sqrt(pred)) %>%
  mutate(lb = round(lb/sum(pred), 4)*100,
         ub = round(ub/sum(pred), 4)*100,
         ci = paste0('(', lb, '%, ', ub, '%)'),
         usage = case_when(usage == 'not_using' ~ 'no',
                           TRUE ~ 'yes'),
         pred2 = paste0(round(pred/sum(pred), 4)*100, '%'),
         other_vars = paste(education, wants_more, usage, sep = ' | ')) %>%
  select(other_vars, age, pred, pred2, ci) %>%
  ggplot(aes(other_vars, age, label=pred2)) + 
  geom_point(aes(size=pred), alpha = 0.5) + 
  geom_text(vjust = 2) +
  geom_text(aes(label = ci), vjust = 4) + 
  theme_minimal() + 
  labs(x = 'Education | Desire for more children | Contraceptive usage',
       y = 'Age') + 
  theme(legend.position = 'none')

