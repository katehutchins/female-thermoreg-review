source("helper-zoib.R")

# Getting the proportions by year (or any other variable(s)) should look something like this:

# make "new" data to predict from
year_vals <- sort(unique(data$year))
year_vals_df <- data.frame(year = year_vals)

# Pass to prediction function...
# Note: This function is actually already in the package.
#       Just needs the above steps to make it useful.
#       Below I made a wrapper function for it which give nicer outputs.
#       Called: get_fitted_values_by_var_zoib()
post_fitted_by_year_raw <- pred.zoib(object = fit, 
                                 xnew = year_vals_df
                                 )

# Use wrapper function instead:
post_fitted_by_year <- get_fitted_values_by_var_zoib(fit = fit, new_data = year_vals_df)

summary(post_fitted_by_year)


# plot for year:
library(ggplot2)
library(tidybayes)
library(tidyr)
library(dplyr)

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
tidy_fitted_posterior %>% 
  group_by(year) %>%
  median_qi(.width = c(.5, .8, .95)) %>%
  ggplot(aes(x = year, y = prop)) + geom_lineribbon() +
  scale_fill_brewer() +
  theme_bw() 

# There is geom_lineribbon() and a lot more geom_'s to choose from in tidybayes.
