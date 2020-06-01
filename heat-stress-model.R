#### Female participants in thermoregulation research: A systematic review
# author: Kate P Kutchins
# data collection: Kate P Kutchins, David N Borg
# analysis: Joshua J Bon, David N Borg

library(zoib)
library(tidybayes)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(pwr)

# helper
source("helper-zoib.R")

d <- read.csv("kh-data.csv") # select the data file

d[is.na(d)] <- 0 # set NA's to zero to ensure count
d$Total <- d$Female+d$Male
d$Proportion <- d$Female/d$Total

d$Year <- d$Year - 2010

# find non-missing data
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
  geom_line(aes(x=Year, y = m1), colour = "blue")

#### zoib model
# fit model with just Year as a covariate, first year is continuous 0-1 denisty, second year is 0 componenet, third year is 1 component
fit <- zoib(Proportion ~ Year|1|Year|Year|1, data=dsub, EUID = 1:nrow(dsub), zero.inflation = T, one.inflation = T, n.iter = 50000, n.thin=10, n.burn=25000, n.chain = 4)
summary(fit$coeff)

#### plot proportion of females over time
# make "new" data to predict from
year_vals <- sort(unique(dsub$Year))
year_vals_df <- data.frame(Year = year_vals)

# use wrapper function to get predictions
post_fitted_by_year <- get_fitted_values_by_var_zoib(fit, new_data = year_vals_df)
summary(post_fitted_by_year)

# add row variable to "new" data
year_vals_df <- year_vals_df %>% 
  mutate(.row = paste("row", 1:nrow(.), sep = "_"))

# convert posterior samples to tidy format and
# join to "new" data frame
tidy_fitted_posterior <- tidy_draws(post_fitted_by_year) %>% 
  pivot_longer(cols = contains("row_"), 
               names_to = ".row", 
               values_to = "prop") %>%
  left_join(year_vals_df) %>%
  select(-.row)

# plot using "tidybayes" plot styles
fig_2_plot <- tidy_fitted_posterior %>% 
  group_by(Year) %>%
  mean_qi(.width = c(.95)) %>% # 95% CI
  ggplot(aes(x = Year, y = prop)) + geom_lineribbon() +
  #geom_jitter(aes(x=Year, y = Proportion), data = dsub, alpha = 0.1) +
  scale_x_continuous("Year", breaks = 0:9, labels = 2010 + 0:9) +
  scale_y_continuous("Proportion of Females", labels = percent_format(accuracy = 1)) +
  scale_fill_grey(start = 0.6) +
  theme_classic() +
  guides(fill = F) # hides CI key
fig_2_plot 

#ggsave("Figure2.tiff", fig_2_plot, units="in", width=5, height=3, dpi=600, compression = 'lzw')


#### posterior comparison 2010 versus 2019 
# indirect comparison
tidy_fitted_posterior %>% group_by(Year) %>%
  mean_qi()

# direct comparison
tidy_fitted_posterior %>% filter(Year %in% c(0,9)) %>% 
  pivot_wider(names_from = Year, values_from = prop, names_prefix = "year_") %>%
  mutate(dif_2019_2010 = year_9 - year_0) %>%
  mean_qi(dif_2019_2010)


#### summarise slope of combined components of beta regression
summarise_slope <- function(prop, year){
  lm(prop ~ year) %>% coef() %>%
    getElement(name = "year")
}

avg_posterior_slope <- tidy_fitted_posterior %>% group_by(.chain, .iteration, .draw) %>%
  summarise(avg_slope = summarise_slope(prop, Year)) %>% ungroup()

# summary in percentage:
avg_posterior_slope %>% tidybayes::mean_qi()




#### plot total number of participants
d <- read.csv("kh-data.csv") # select the data file
d$Author = NULL
d$DOI = NULL
df <- d %>% group_by(Year) %>% summarise_all(funs(sum), na.rm = T)

d1 <- dplyr::select(df, Year, Male)
d1$Gender <- 1
d1$Year_total <- d1$Male

d2 <- dplyr::select(df, Year, Female)
d2$Gender <- 2
d2$Year_total <- d2$Female

# merge
data <- d1 %>% bind_rows(d2)
data$Male = NULL
data$Female = NULL
data$Year = as.factor(data$Year)
data$Gender = as.factor(data$Gender)
data$Sex = data$Gender

fig_1_plot <- ggplot(data, aes(x = Year, y = Year_total, fill = Sex)) +
  geom_bar(position = "dodge", stat = "identity", color = "black") +
  theme_classic()+
  ylab(Number~of~Participants) +
  scale_fill_grey(labels = c("Male", "Female"))
fig_1_plot
#ggsave("Figure1.tiff", fig_1_plot, units="in", width=6, height=3, dpi=600, compression = 'lzw')




#### median sample sizes
d <- read.csv("kh-data.csv") # select the data file

d[is.na(d)] <- 0 #set NA to zero to ensure count
d$Total <- d$Female + d$Male
d$Proportion <- d$Female/d$Total

d$Year <- d$Year - 2010

# find non-missing data
ind = !is.na(d$Proportion)
dsub = d[ind,]
hist(dsub$Proportion, breaks = 100)

# median sample size of studies with only females
df <- dsub %>% filter(Proportion == "1")
ggplot(df, aes(Total)) + geom_histogram(bins = 200) + theme_bw()
summary(df$Total)

# median sample size of studies with both sexes
dsome <- dsub %>% filter(Proportion >0)
dsome <- dsome %>% filter(Proportion <1)
ggplot(dsome, aes(Total)) + geom_histogram(bins = 300) + theme_bw()
summary(dsome$Total) # total study sample

  # female subgroup
  ggplot(dsome, aes(Female)) + geom_histogram(bins = 200) + theme_bw()
  summary(dsome$Female)
  
  # male subgroup
  ggplot(dsome, aes(Male)) + geom_histogram(bins = 200) + theme_bw()
  summary(dsome$Male)

# median sample size of studies with only males
dnone <- dsub %>% filter(Proportion == "0")
ggplot(dnone, aes(Total)) + geom_histogram(bins=500) + theme_bw()
summary(dnone$Total)




#### power calculation for discussion
pwr.t.test(n = 6,d = 0.8,sig.level = .05,type = "two.sample")
pwr.t.test(n = 10, d = 0.8, sig.level = .05, type = "two.sample")




#### end















