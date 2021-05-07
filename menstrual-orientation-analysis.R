

# Reporting of menstrual orientation
# Author: DNB
# Date: April 2021

# Libraries
library(tidyverse)
library(dplyr)
library(naniar)
library(janitor)
library(brms)

# Load data
d = read.csv("reporting-of-menstrual-orientation.csv") %>%
  clean_names() %>%
  select(year, g1) %>%
  mutate(
    year_s = year-2010,
    reported = recode_factor({as.factor(g1)},
                             'Not_Reported' = '0',
                             'Amenorrheic/Oligomenorrheic' = '1',
                             'EUM' = '1',
                             'IUD' = '1',
                             'Not_Controlled' = '1',
                             'OCP' = '1',
                             'Pregnant' = '1'))

# Summary
d %>%
  filter(reported %in% c(0,1)) %>%
  group_by(reported) %>%
  summarise(
    count = n()
  ) %>%
  mutate(
    freq = count/sum(count)
  )

# Bayesian model
fit_bayes = brm(reported ~ year_s,
                family = bernoulli(link = "logit"),
                chains = 4, 
                cores = 8, 
                iter = 250000, 
                thin = 10,
                warmup = 25000,
                prior = set_prior("normal(0,1)", class = "b"),
                seed = 123,
                data = {d %>% filter(reported %in% c("0","1"))}
                )

# Posterior predictive check
pp_check(fit_bayes, re_formula = NULL, nsamples = 100)

# Summary
summary(fit_bayes)

# Plot parameters
plot(fit_bayes)

# Conditional effect of year
conditional_effects(fit_bayes)




