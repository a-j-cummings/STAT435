library(tidyverse)
library(cowplot)

fiji_raw <- read_table('fiji_contraceptives.dat')

fiji_short <- fiji_raw %>% 
  janitor::clean_names() %>% 
  select(-x6)

fiji_long <- fiji_short %>%
  pivot_longer(c(using, not_using), names_to = 'use')

fiji_binary <- fiji_long %>% 
  mutate(obs = map(value, ~rep_len(1, .x))) %>% 
  unnest(obs) %>% 
  select(-value, -obs) %>% 
  mutate(use = case_when(use == 'using' ~ 1,
                         TRUE ~ 0))

# marginal of education | age
education_plot <- fiji_short %>% 
  group_by(education, age) %>% 
  summarise(using = sum(using), 
         not_using = sum(not_using),
         n_responses = using + not_using,
         prop_using = using/n_responses) %>% 
  ggplot(aes(age, prop_using, color = education)) +
    geom_point(aes(size = n_responses)) + 
    geom_line(aes(as.numeric(factor(age)))) + 
    labs(x = 'Age', y = 'Proportion using contraceptives',
         size = '# of respondents')
# marginal of wants_more | age
wants_more_plot <- fiji_short %>% 
  group_by(wants_more, age) %>% 
  summarise(using = sum(using), 
            not_using = sum(not_using),
            n_responses = using + not_using,
            prop_using = using/n_responses) %>% 
  ggplot(aes(age, prop_using, color = wants_more)) +
  geom_point(aes(size = n_responses)) + 
  geom_line(aes(as.numeric(factor(age)))) +
  labs(x = 'Age', y = 'Proportion using contraceptives',
       size = '# of respondents')
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
         size = '# of respondents')
plot_grid(plot_grid(education_plot, wants_more_plot), both, nrow = 2)


#####################
# Model exploration #
#####################
" Notes

- when you fit your full model, the most important variable in the model is the 
  one with the greatest residual deviance; the least important has the smallest
  residual deviance
- sequential sum of squares is the default in R's ANOVA function; b/c of this
  model construction has the 'best' interpretation when the model is fit 
  sequentially with the *most important* variable(s) first
- residual devaince contains the variability contained by *all* variables not 
  included in the model
- Psuedo R squared: 1 - (residual deviance)/(null residual deviance)
- in the ANOVA output, you can check statistical significance by comparing the 
  residual degrees of freedom to the residual deviance -- residual deviance is
  distributed on a X^2 distibution with residual degrees of freedom
  - the ANOVA output on an lm object has p-values, this trick just applies to
    GLMs
  - comparing the Deviance column to the Df column in the same way (X^2 
    distribution) gets a p-value on the effect of the fitted variable 
    (accounting for other variables per your type of sum of squares)
- For count data, big data always leads to significance eventually
  @ Why? Where can I learn about this? It is practically important
  @ Related to power, but how? How can I think about this and explain it simply?
"
################
# Binomial GLM #
################
fiji_short2 <- fiji_short %>% 
  mutate(age2 = as.numeric(factor(age))) # Age 2 is a numeric

# A model with all three way interactions
mod_all <- glm(cbind(using, not_using) ~ (age2 + education + wants_more)^3,
               family = binomial(link = "logit"), data = fiji_short2)
anova(mod_all)
"
- age2 is the most important variable, so keep it at the top
- of the single variables, education should be moved below wants_more
"
mod_all2 <- glm(cbind(using, not_using) ~ (age2 + wants_more + education)^3,
               family = binomial(link = "logit"), data = fiji_short2)
anova(mod_all2)
"
- The residual deviance is less than the residual degrees of freedom, we will 
  have to drop terms
- the 3 way interaction is the first to go
"
mod_2way <- glm(cbind(using, not_using) ~ (age2 + wants_more + education)^2,
                family = binomial(link = "logit"), data = fiji_short2)
anova(mod_2way)
"
- The age2:education interaction is not significant, it can be removed
"
mod_2way2 <- glm(cbind(using, not_using) ~ (age2 + wants_more)^2 + 
                                            education*wants_more,
                 family = binomial(link = "logit"), data = fiji_short2)
anova(mod_2way2)
"
- Now the wants_more:education interaction is not helping the model fit; drop it
"
mod_2way3 <- glm(cbind(using, not_using) ~ (age2 + wants_more)^2 + education,
                 family = binomial(link = "logit"), data = fiji_short2)
anova(mod_2way3)
"
This model explians approx. 88.9% of variability in the response. 
There does not seem to be significant outside effects beyond what I have 
  included (at the alpha = 0.05 level).
"

################
# Bernouli GLM #
################
# A model with all three way interactions -- cell means
mod_allb <- glm(use ~ -1 + (age + education + wants_more)^3,
               family = binomial(link = "logit"), data = fiji_binary)
anova(mod_allb)
"
- age2 is the most important variable, so keep it at the top
- of the single variables, education should be moved below wants_more
"
mod_all2b <- glm(use ~ -1 + (age + wants_more + education)^3,
               family = binomial(link = "logit"), data = fiji_binary)
anova(mod_all2b)
"
- the last three interactions are not significant, drop the last, the other two 
  are fairly borderline, so I'm going to keep them
- only dropping the 3-way interaction
"
mod_2wayb <- glm(use ~ -1 + (age + wants_more + education)^2,
                 family = binomial(link = 'logit'), data = fiji_binary)
anova(mod_2wayb)

############
# Binomial #
############

# A model with all three way interactions -- cell means
mod_allc <- glm(cbind(using, not_using) ~ -1 + (age + education + wants_more)^3,
                family = binomial(link = "logit"), data = fiji_short)
anova(mod_allc)
"
- age is the most important variable, so keep it at the top
- of the single variables, education should be moved below wants_more
- 3 way interaction can go
"
mod_2wayc <- glm(cbind(using, not_using) ~ -1 + (age + wants_more + education)^2,
                family = binomial(link = "logit"), data = fiji_short)
anova(mod_2wayc)
"
- Interactions with education are not improving the model, drop them
"
mod_2wayc2 <- glm(cbind(using, not_using) ~ -1 + (age + wants_more)^2 + education,
                 family = binomial(link = "logit"), data = fiji_short)
anova(mod_2wayc2)
"
Looks good. Use this model.
"


# summarizing the model
fiji_short2 <- fiji_short %>% 
  mutate(pred = predict(mod_2wayc2, type = 'response')*(using+not_using),
         obs = using)
fiji_short2
actual <- xtabs(obs ~ age + wants_more + education, data = fiji_short2)
expected <- xtabs(pred ~ age + wants_more + education, data = fiji_binary2)

# Pearson ChiSq
pearson <- sum((actual - expected)**2/expected)
pearson
# Neyman ChiSq
neyman <- sum((actual - expected)**2/actual)
neyman
# LR ChiSq
lr <- sum(2*actual*(log(actual) - log(expected)))
lr
# Del Scott's ChiSq
ds <- sum(2*expected*(log(expected) - log(actual)))
ds
"
How similar should these values be if I am not going to be concerned about 
convergence?

Also, what are the degrees of freedom on this 3-way table?

Third question -- I want this to be small, right? If this is small, then the
model is fitting well, correct?
"




# Conditional probabilites
count <- nrow(fiji_binary2)
xtabs(pred ~ age + wants_more + education, data = fiji_binary2)#/count
"
These proportions are the conditional probabilities. They all matter because
there is an interaction between all factors.

When making tables like this, the goal is to encourage comparisons that you
found meaningful.
"