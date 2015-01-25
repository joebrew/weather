library(dplyr)

# Define function for plotting max/min temperature
plot_temp <- function(loc = "Entebbe",
                      var = "temp_max",
                      data = dat,
                      color = "darkred",
                      add_lines = TRUE,
                      add_rolling_lines = TRUE,
                      rolling_days = 14,
                      ylim = c(60, 95), 
                      add = FALSE,
                      ylab = "Degrees (F)"){
  
  # Standardize location names
  loc <- toupper(loc)
  
  #subset data
  data <- data[which(data$loc == loc),]
  
  #sort by date
  data <- data[order(data$date),]
  
  # Fill in the uncaptured dates
  dates <- data.frame(date = seq(min(data$date),
                                 max(data$date),
                                 by = 1))
  data <- left_join(x = dates,
                    y = data)
  
  #plot new plot if needed
  #if(is.null(dev.list())){
  if(!add){
    plot(x = data$date,
         y = data[,var],
         col = adjustcolor(color, alpha.f = 0.2),
         pch = 16,
         xlab = "Date",
         ylab = ylab,
         ylim = ylim,
         cex = 0.5,
         cex.axis = 0.6,
         cex.lab = 0.6)
  } else {
    points(x = data$date,
           y = data[,var],
           col = adjustcolor(color, alpha.f = 0.2),
           pch = 16,
           cex = 0.5)
  }
  if(add_lines){
    lines(x = data$date,
          y = data[,var],
          col = adjustcolor(color, alpha.f = 0.1))
  }
  if(add_rolling_lines){
    rolled_y <- vector(mode = "numeric", length = length(data[,var]))
    for (i in 1:length(data[,var])){
      rolled <- data[,var][ifelse(i-rolling_days < 1, 1, i-rolling_days):i]
      rolled_y[i] <- mean(rolled, na.rm = TRUE)
    }
    lines(x = data$date,
          y = rolled_y,
          col = adjustcolor(color, alpha.f = 0.4),
          lwd = 2)
  }
}

# Define function for plotting precipitation
plot_rain <- function(loc = "Entebbe",
                      var = "precipitation",
                      data = dat,
                      color = "darkblue",
                      add_rolling_lines = TRUE,
                      rolling_days = 14,
                      #ylim = c(55, 90), 
                      ylab = "Precipitation (in)",
                      rolled_color = "darkred"){
  
  # Standardize location names
  loc <- toupper(loc)
  
  #subset data
  data <- data[which(data$loc == loc),]
  
  #sort by date
  data <- data[order(data$date),]
  
  # Fill in the uncaptured dates
  dates <- data.frame(date = seq(min(data$date),
                                 max(data$date),
                                 by = 1))
  data <- left_join(x = dates,
                    y = data)
  
  # Plot
  bp <- barplot(data[,var],
                col = adjustcolor(color, alpha.f = 0.3),
                pch = 16,
                xlab = "Date",
                ylab = ylab,
                border = adjustcolor(color, alpha.f = 0.3),
                space = 0,
                las = 3,
                cex.axis = 0.6,
                cex.lab = 0.6)
  
  # Axis
  axis(side = 1,
       at = bp[,1][which(format(data$date, "%m%d") == "0101")],
       labels = format(data$date[which(format(data$date, "%m%d") == "0101")], format = "%Y"),
       cex.axis = 0.6,
       tick = FALSE)

  if(add_rolling_lines){
    rolled_y <- vector(mode = "numeric", length = length(data[,var]))
    for (i in 1:length(data[,var])){
      rolled <- data[,var][ifelse(i-rolling_days < 1, 1, i-rolling_days):i]
      rolled_y[i] <- mean(rolled)
    }
    lines(x =  bp[,1],
          y = rolled_y,
          col = adjustcolor(rolled_color, alpha.f = 0.4),
          lwd = 2)
  }
  legend(x = "topleft",
         col = adjustcolor(c(color, rolled_color), alpha.f = 0.4),
         lwd = 1,
         bty = "n",
         cex = 0.4,
         legend = c("Daily observation", 
                    paste(rolling_days, "rolling average")))
  box("plot")
}


# Define function for plotting cloud cover
plot_clouds <- function(loc = "Entebbe",
                      var = "cloud_cover",
                      data = dat,
                      color = "darkorange",
                      add_lines = TRUE,
                      add_rolling_lines = TRUE,
                      rolling_days = 14,
                      ylab = "cloud cover (units?)",
                      rolled_color = "darkred"){
  
  # Standardize location names
  loc <- toupper(loc)
  
  #subset data
  data <- data[which(data$loc == loc),]
  
  # Plot nothing if not available
  if(sum(data[,var], na.rm = TRUE) == 0){
    plot(1:10, 1:10, xlab = NA, ylab = NA,
         type = "n",
         xaxt = "n",
         yaxt = "n")
    text(x = 5.5,
         y = 5,
         pos = 3,
         labels = "Precipitation\ndata\nnot\navailable")
  } else{
    
    #sort by date
    data <- data[order(data$date),]
    
    # Fill in the uncaptured dates
    dates <- data.frame(date = seq(min(data$date),
                                   max(data$date),
                                   by = 1))
    data <- left_join(x = dates,
                      y = data)
    
    # Plot
    
      plot(x = data$date,
           y = data[,var],
           col = adjustcolor(color, alpha.f = 0.2),
           pch = 16,
           xlab = "Date",
           ylab = ylab,
           cex = 0.5,
           cex.axis = 0.6,
           cex.lab = 0.6)
   
    if(add_lines){
      lines(x = data$date,
            y = data[,var],
            col = adjustcolor(color, alpha.f = 0.1))
    }
    if(add_rolling_lines){
      rolled_y <- vector(mode = "numeric", length = length(data[,var]))
      for (i in 1:length(data[,var])){
        rolled <- data[,var][ifelse(i-rolling_days < 1, 1, i-rolling_days):i]
        rolled_y[i] <- mean(rolled, na.rm = TRUE)
      }
      lines(x = data$date,
            y = rolled_y,
            col = adjustcolor(rolled_color, alpha.f = 0.4),
            lwd = 2)
    }
    legend(x = "topleft",
           col = adjustcolor(c(color, rolled_color), alpha.f = 0.4),
           lwd = 1,
           cex = 0.4,
           bty = "n",
           legend = c("Daily observation", 
                      paste(rolling_days, "rolling average")))
    
  }  
}

