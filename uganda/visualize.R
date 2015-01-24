setwd('weather/uganda')

# Read in scraped data
dat <- read.csv("uganda_weather.csv", stringsAsFactors = FALSE)

# Clean up
dat$date <- as.Date(dat$date, format = "%Y-%m-%d")

# PLOT
source("visualization_functions.R")

# Plot for each location
for (i in unique(sort(dat$loc))){
  
  # Set up multi-pane graphics
  par(mfrow = c(2,2))
  
  # Plot max temperature
  plot_temp(loc = i)
  # Add minimum temperature
  plot_temp(loc = i,
            var = "temp_min",
            data = dat,
            color = "darkgreen",
            add_lines = TRUE,
            add_rolling_lines = TRUE,
            rolling_days = 14,
            add = TRUE)
  # Add legend
  legend(x = "bottomleft",
         col = adjustcolor(c("darkred", "darkgreen"), alpha.f = 0.4),
         pch = 16,
         cex = 0.7, 
         lty = 1,
         legend = c("Max", "Min"),
         bty = "n")
  title(main = "Temperature")
  
  # Plot precipitation
  plot_rain(loc = i)
  title(main = "Precipitation")
  
  # Plot cloud cover
  plot_clouds(loc = i)
  title(main = "Cloud cover")
  
  # Plot humidity
  # maximum
  plot_temp(loc = i,
            var = "humidity_max",
            ylim = c(0,100))
  # minimum
  plot_temp(loc = i,
            var = "humidity_min",
            data = dat,
            color = "darkgreen",
            add_lines = TRUE,
            add_rolling_lines = TRUE,
            rolling_days = 14,
            add = TRUE)
  # Add title
  title(main = "Humidity")
  # Add legend
  legend(x = "bottomleft",
         col = adjustcolor(c("darkred", "darkgreen"), alpha.f = 0.4),
         pch = 16,
         cex = 0.7, 
         lty = 1,
         legend = c("Max", "Min"),
         bty = "n")
  
  # Add title
  title(main = i,
        outer = TRUE,
        line = -1)
}
par(mfrow = c(1,1))

# Visualize locations of weather stations
source("gadm.R")
gadm("UGANDA", level = 3)

# Make lat lon
library(rgdal)
uganda3 <- spTransform(uganda3,  CRS("+init=epsg:4326"))

# Remove the lake by getting rid of the largest area polygon
library(rgeos)
uganda3$area <- gArea(uganda3, byid = TRUE)
uganda3 <- uganda3[which(uganda3$area != max(uganda3$area)),]

# Plot map of Uganda
uganda_colors <- adjustcolor(colorRampPalette(c("black", "darkgrey"))(nrow(uganda3)), 
                             alpha.f = 0.7)
plot(uganda3,
     col = uganda_colors,
     border = "black",
     lwd = 0.2)

# Get the lat lon for our stations
library(ggmap)
x <- geocode(paste0(c("Kajjansi",
               "Entebbe",
               "Kampala",
               "Soroti"), 
               ", Uganda"))
# Draw these onto the map
points(x = x$lon,
       y = x$lat,
       col = adjustcolor("darkred", alpha.f = 0.8),
       pch = 16,
       cex = 2)

# Label the capital
text(x[1,c("lon", "lat")], 
     col = "red",
     labels = "Kampala")

# Label the non-capital station
text(x[4,c("lon", "lat")], 
     col = "red",
     labels = "Soroti")
