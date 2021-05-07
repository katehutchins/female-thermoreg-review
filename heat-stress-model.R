

# Female participants in thermoregulation research: A systematic review
# Author: Kate P Kutchins
# Data collection: Kate P Kutchins, David N Borg
# Analysis: Joshua J Bon, David N Borg

library(zoib)
library(tidybayes)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(pwr)

source("helper-zoib.R")

# Load data
d = read.csv("review_data_2.0.csv")

# Set NA to zero to ensure count
d[is.na(d)] = 0 

d = d %>%
  mutate(
    Total = Female + Male,
    Proportion = Female/Total,
    Year = Year-2010
  )

# Find non-missing data
ind = !is.na(d$Proportion)
dsub = d[ind,]
hist(dsub$Proportion, breaks = 100)

dsub %>% group_by(Year) %>% 
  summarise(mP = mean(Proportion, na.rm = T),
            mZ = mean(Female == 0),
            m1 = mean(Female == Total)
  ) %>%
  ggplot() + 
  geom_line(aes(x=Year, y = mP), colour = "black") + 
  geom_line(aes(x=Year, y = mZ), colour = "red") + 
  geom_line(aes(x=Year, y = m1), colour = "blue") +
  theme_bw()

#### ZOIB model
# fit model with just Year as a covariate, first year is continuous 0-1 denisty, second year is 0 componenet, third year is 1 component
fit <- zoib(Proportion ~ Year|1|Year|Year|1, data=dsub, EUID = 1:nrow(dsub), zero.inflation = T, one.inflation = T, n.iter = 50000, n.thin=10, n.burn=25000, n.chain = 4)
save(fit,file="prop-fitted.RData")
summary(fit$coeff)



#### Plot proportion of females over time
# Make "new" data to predict from
year_vals <- sort(unique(dsub$Year))
year_vals_df <- data.frame(Year = year_vals)

# Use wrapper function to get predictions
post_fitted_by_year <- get_fitted_values_by_var_zoib(fit, new_data = year_vals_df)

summary(post_fitted_by_year)

# Add row variable to "new" data
year_vals_df <- year_vals_df %>% 
  mutate(.row = paste("row", 1:nrow(.), sep = "_"))

# Convert posterior samples to tidy format and
# Join to "new" data frame
tidy_fitted_posterior <- tidy_draws(post_fitted_by_year) %>% 
  pivot_longer(cols = contains("row_"), 
               names_to = ".row", 
               values_to = "prop") %>%
  left_join(year_vals_df) %>%
  select(-.row)

# Plot using "tidybayes" plot styles
tidy_fitted_posterior %>% 
  group_by(Year) %>%
  mean_qi(.width = c(.95)) %>% # 95% CI
  ggplot(aes(x = Year, y = prop)) + geom_lineribbon(aes(ymin = .lower, max = .upper)) +
  scale_x_continuous("Year", breaks = 0:9, labels = 2010 + 0:9) +
  scale_y_continuous("Proportion of women", labels = percent_format(accuracy = 1)) +
  scale_fill_grey(start = 0.6) +
  theme_classic() +
  guides(fill = F)
ggsave("Figure2.tiff", units="in", width=5, height=3, dpi=600, compression = 'lzw')





#### Proportion female versus male when both included (as at 2019)
logit <- function(q) plogis(q = q)

zfit_coefs <- get_all_coef_samples_zoib(fit)

tidy_zfit_coefs <- tidy_draws(zfit_coefs) %>%
  mutate(y_01ex_2019_fit = logit(`b_(Intercept)` + b_Year * (2019 - 2010))) 

tidy_zfit_coefs %>% mean_qi(y_01ex_2019_fit)
# Could also do other components (0 or 1 component) or different years here





#### 2010 to 2019 posterior comparison

# Indirect comparison
tidy_fitted_posterior %>% group_by(Year) %>%
  mean_qi()

# Direct comparison
tidy_fitted_posterior %>% filter(Year %in% c(0,9)) %>% 
  pivot_wider(names_from = Year, values_from = prop, names_prefix = "year_") %>%
  mutate(dif_2019_2010 = year_9 - year_0) %>%
  mean_qi(dif_2019_2010)


#### Summarise slope of combined components of beta regression
summarise_slope <- function(prop, year){
  lm(prop ~ year) %>% coef() %>%
    getElement(name = "year")
}

avg_posterior_slope <- tidy_fitted_posterior %>% group_by(.chain, .iteration, .draw) %>%
  summarise(avg_slope = summarise_slope(prop, Year)) %>% ungroup()

# Summary in percentage:
avg_posterior_slope %>% tidybayes::mean_qi()







#### Plot total number of participants
d = read.csv("review_data_2.0.csv") %>%
  select(-Author, -DOI)
df = d %>% group_by(Year) %>% summarise_all(funs(sum), na.rm = T)

# Male subset
d1 = select(df, Year, Male) %>%
  mutate(
    Gender = 1,
    Year_total = Male
  )

# Female subset
d2 = select(df, Year, Female) %>%
  mutate(
    Gender = 2,
    Year_total = Female
  )

# Merge
data = d1 %>% bind_rows(d2) %>%
  select(-Male, -Female) %>%
  mutate(
    Year = as.factor(Year),
    Sex = as.factor(Gender)
  )

# Plot
Figure1 <- ggplot(data, aes(fill = Sex, y = Year_total, x = Year)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  theme_classic() +
  ylab("Number of participants") +
  scale_fill_grey(labels = c("Men", "Women"))
Figure1
ggsave("Figure1.tiff", figure1, units = "in", width = 5.5, height = 3, dpi = 600, compression = 'lzw')





#### Sample sizes of men and women
d = read.csv("review_data_2.0.csv")

# Set NA to zero to ensure count
d[is.na(d)] = 0

d = d %>%
  mutate(
    Total = Female + Male,
    Proportion = Female/Total,
    Year = Year - 2010
  )

# find non-missing data
ind = !is.na(d$Proportion)
dsub = d[ind,]
hist(dsub$Proportion, breaks = 100)

# Median sample size of studies with only females
df = dsub %>% filter(Proportion == "1")
ggplot(df, aes(Total)) + geom_histogram(bins = 200) + theme_bw()
median(df$Total); summary(df$Total)

# Median sample size of studies with both females and males
dsome = dsub %>% filter(Proportion >0)
dsome = dsome %>% filter(Proportion <1)
ggplot(dsome, aes(Total)) + geom_histogram(bins = 300) + theme_bw()
median(dsome$Total); summary(dsome$Total) # Total study

ggplot(dsome, aes(Female)) + geom_histogram(bins = 200) + theme_bw()

  # Female subgroup
  median(dsome$Female); summary(dsome$Female)
  
  # Male subgroup
  median(dsome$Male); summary(dsome$Male)

# Median sample size of studies with only males
dnone = dsub %>% filter(Proportion == "0")
ggplot(dnone, aes(Total)) + geom_histogram(bins = 500) + theme_bw()
median(dnone$Total); summary(dnone$Total)






#### Power calculation for discussion
# n = 6
pwr.t.test(n = 6, d = 0.8, sig.level = 0.05, type = "two.sample")
# n = 10
pwr.t.test(n = 10, d = 0.8, sig.level = 0.05, type = "two.sample")



















