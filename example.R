library(ggplot2)
library(tidyr)
library(dplyr)
library(ggthemes)

# Read in the function
source('get_weather.R')

# Get weather
x = get_weather(station = "FQMA", 
                start_year = 2000,
                end_year = 2016) 

# Get weather for barcelona
b <- get_weather(station = "BCN", 
                 start_year = 2000,
                 end_year = 2016)

# Combine
x <- bind_rows(x, b)
# Make data long instead of wide
long <- gather(x, key, value, temp_max:cloud_cover)
long$value <- as.numeric(as.character(long$value))

# Group by day number
long$week_number <- as.numeric(format(long$date, '%U'))
long$day_number <- as.numeric(format(long$date, '%j'))
long <- long %>% 
  group_by(week_number, location, key) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  ungroup
long <- long %>%
  mutate(key = gsub('_', ' ', key),
         location = ifelse(location == 'FQMA', 'MAP', location))
# Plot
ggplot(data = long %>%
         mutate(date = as.Date('2016-01-01') + (week_number * 7)),
          #%>% filter(key %in% c('temp_max', 'temp_min')) %>%
       aes(x = date,
           y = value,
           group = location, 
           color = location)) +
  # geom_point(alpha = 0.2) +
  geom_line(alpha = 0.6) +
  # geom_smooth() +
  theme_fivethirtyeight() +
  facet_wrap(~key, nrow = 4,
             scales = 'free_y') +
  labs(x = 'Day of year',
       title = 'Barcelona vs. Maputo',
       subtitle = 'Weather: 2000-2016 aggregated across years, averaged at weekly level') +
  scale_color_manual(name = '',
                     values = c('darkgreen', 'darkorange')) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_date(labels = scales::date_format("%B"))

# put all lines on one year
x$day <-as.numeric(format(x$date, "%j"))
x$mon <-as.numeric(format(x$date, "%m"))
x$year <-as.numeric(format(x$date, "%Y"))

# Get average precipitation by year month
y <- x %>%
  group_by(year, mon) %>%
  summarize(mprec=mean(precipitation)) %>%
  ungroup %>%
  mutate(year = factor(year))

ggplot(data = y,
       aes(x = mon, y = mprec, group = year, color = year)) +
  geom_line()
