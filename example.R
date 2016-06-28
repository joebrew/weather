library(ggplot2)
library(tidyr)
library(dplyr)
library(ggthemes)

# Read in the function
source('get_weather.R')

# Get weather
x = get_weather(station = "MPM", 
                start_year = 2010,
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

# put all lines on one year
x$day <-as.numeric(format(x$date, "%j"))
x$mon <-as.numeric(format(x$date, "%m"))
x$year <-as.numeric(format(x$date, "%Y"))

# Get average precipitation by year month
y <- x %>%
  group_by(year, mon) %>%
  summarize(mprec=mean(precipitation))