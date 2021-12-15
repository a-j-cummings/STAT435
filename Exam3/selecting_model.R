# Fitting a log-linear model to the Fiji contraceptives data

library(tidyverse)
library(MASS)

fiji_raw <- read_csv('fiji_contraceptives.csv')

fiji_short <- fiji_raw %>%
  janitor::clean_names()

fiji_long <- fiji_short %>%
  pivot_longer(-c(age, education, wants_more), names_to = 'usage', 
               values_to = 'count')

# saturated model
model_sat_pois <- glm(count ~ (age + education + wants_more + usage)^4,
                      data = fiji_long, family = poisson(link = 'log'))
anova(model_sat_pois)
# usage should be moved to be after age (ordering largest effects first)
# age:wants_more:usage should be dropped (not significant)
# education:wants_more:usage should be dropped (not significant)
# 4 way interaction should be dropped (not significant)

model_2 <- glm(count ~ (age + usage + education + wants_more)^2 + 
                 age:education:wants_more + age:education:usage,
               data = fiji_long, family = poisson(link = 'log'))
anova(model_2)
# this model actually seems like a good one;
# everything is significant at 95% level

# Checking for overdispersion
model_2b <- glm(count ~ (age + usage + education + wants_more)^2 + 
                  age:education:wants_more + age:education:usage,
                data = fiji_long, family = quasipoisson(link = 'log'))
disp_2b <- summary(model_2b)$dispersion # dispersion parameter > 1, so overdispersion
df <- model_2b$df.residual
pchisq(disp_2b*df, df, lower.tail = FALSE) # significant overdispersion exists

# Assessing model fit
# summarizing the model
fiji_long2 <- fiji_long %>% 
  mutate(pred = predict(model_2, type = 'response'),
         obs = count)
actual <- xtabs(obs ~ age + wants_more + education + usage, data = fiji_long2)
expected <- xtabs(pred ~ age + wants_more + education + usage, 
                  data = fiji_long2)

# Pearson ChiSq
pearson <- sum((actual - expected)**2/expected)
p_pearson <- 1 - pchisq(pearson, df)
# Neyman ChiSq
neyman <- sum((actual - expected)**2/actual)
p_neyman <- 1 - pchisq(neyman, df)
# Likelihood ratio
lratio <- sum(2*actual*(log(actual) - log(expected)))
p_lratio <- 1 - pchisq(lratio, df)


# Figures
ggplot(fiji_long, aes(as.numeric(factor(age)), count, color = education,
                      shape = usage, linetype = wants_more)) + 
  geom_point() +
  geom_line() +
  labs(x = 'Age', y = 'Count', color = 'Education',
       linetype = 'Wants more children', shape = 'Contraceptive usage') + 
  scale_x_discrete(limits=unique(fiji_long$age))
