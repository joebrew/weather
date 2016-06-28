library(ggplot2)
library(tidyr)
library(dplyr)
library(ggthemes)

# Read in the function
source('get_weather.R')

# Get weather
x = get_weather(station = "MPM", 
                start_year = 2014,
                end_year = 2016)

# Make data long instead of wide
long <- gather(x, key, value, temp_max:cloud_cover)

# Plot
ggplot(data = long %>%
         filter(key %in% c('temp_max', 'temp_min')) %>%
         mutate(key = gsub('temp', '', key)),
       aes(x = date,
           y = value,
           group = key, 
           color = key)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  theme_fivethirtyeight()