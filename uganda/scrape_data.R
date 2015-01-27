setwd('weather/uganda')
get_weather <- function(station = "KAMPALA", # IWESTERN307
                        start_date = Sys.Date() - 365,
                        end_date = Sys.Date(),
                        airport = TRUE){
  

  # Format station name
  station <- toupper(gsub(" ", "%20", station))
  
  # Parse date components
  start_date <- as.Date(start_date, format = "%Y-%m-%d")
  start_day <- as.numeric(format(start_date, "%d"))
  start_month <- as.numeric(format(start_date, "%m"))
  start_year <- as.numeric(format(start_date, "%Y"))
  
  end_date <- as.Date(end_date, format = "%Y-%m-%d")
  end_day <- as.numeric(format(end_date, "%d"))
  end_month <- as.numeric(format(end_date, "%m"))
  end_year <- as.numeric(format(end_date, "%Y"))
  
  # Define link format for weather stations
  if(!airport){
    link <- paste0( "http://www.wunderground.com/weatherstation/WXDailyHistory.asp?ID=", 
                    station,
                    "&day=", start_day,
                    "&month=", start_month, 
                    "&year=", start_year, 
                    "&dayend=", end_day, 
                    "&monthend=", end_month, 
                    "&yearend=", end_year, 
                    "&graphspan=custom&format=1")  
    
    # Read in data from link
    df <- read.csv(link)
    
    #Clean up some html formatting issues
    df <- df[which(df$Date != "<br>"),]
    
    # Clean up names (to be compatible with airports)
    df$PrecipitationIn <- df$PrecipitationSumIn.br.
    df$CloudCover <- NA
    df$PrecipitationSumIn.br. <- NULL
    df <- df[,which(!grepl("WindSpeed|Gust", names(df)))]
    
  } else {
    # Define link format for airports
    link <- paste0("http://www.wunderground.com/history/airport/",
                   station,
                   "/", start_year, 
                   "/", start_month, 
                   "/", start_day, 
                   "/CustomHistory.html?dayend=", end_day, 
                   "&monthend=", end_month, 
                   "&yearend=", end_year, 
                   "&req_city=NA&req_state=NA&req_statename=NA&format=1")
    
    # Read in data from link
    df <- read.csv(link)
    
    # Clean up some column names (to be compatible with weather stations)
    Date <- df$EAT ; df$EAT <- NULL
    df$MeanSeaLevelPressureIn <- NULL
    df <- cbind(Date, df)
    names(df) <- gsub("[.]", "", names(df))
    df <- df[,which(!grepl("Visibility|Wind|Gust|MeanSeaLevelPressureIn|Events", names(df)))]
  }
   
  # Format date to date object
  #df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
  
  # Standardize names
  names(df) <- c("date",
                 "temp_max",
                 "temp_mean",
                 "temp_min",
                 "dewpoint_max",
                 "dewpoint_mean",
                 "dewpoint_min",
                 "humidity_max",
                 "humidity_mean",
                 "humidity_min",
                 "pressure_max",
                 "pressure_min",
                 "precipitation",
                 "cloud_cover")
  
  # Add a location column
  df$loc <- toupper(as.character(station))
  
  # print url source
  print(link)
  
  return(df)
}

# # Airport example:
# airport <- get_weather("HUEN", airport = TRUE)
# # Weather station example: 
# station <- get_weather("ICENTRAL28", airport = FALSE)

# Define vector of Ugandan cities 
# (These are the cities available on wundeground,
#  I went through and commented out those that don't work)
ugandan_cities <- data.frame(loc = c(#"Arua", 
                    #"Entebbe", # same as Huen
                    #"Fort Portal", 
                    #"Gulu",
                    "Huen",
                    #"Jinja", 
                    #"Kabale", 
                    "Kampala",
                    #"Kasese",
                    #"Masindi",
                    #"Mbarara",
                    #"Moroto",
                    #"Paraa", 
                    #"Tororo",
                    "Soroti"),
                    airport = TRUE)
ugandan_cities <- rbind(ugandan_cities, data.frame(loc = "ICENTRAL28",
                        airport = FALSE))


# Define some start and end dates (query fails on big date ranges)
dates_df <- data.frame(end_date = seq(as.Date("2011-12-31",
                                                format = "%Y-%m-%d"),
                                        as.Date("2014-12-31",
                                                format = "%Y-%m-%d"),
                                        by = 365))
dates_df$start_date <- NA
for (i in 1:nrow(dates_df)){
  if(i != 1){
    previous_date <- dates_df$end_date[i-1]
    dates_df$start_date[i] <- 
      previous_date + 1
  }
}
dates_df$start_date <- as.Date(dates_df$start_date, origin = "1970-01-01")
dates_df$start_date[1] <- dates_df$start_date[2] - 365

updated_df <- data.frame()
# Get weather for all airport locations
for (i in 1:nrow(ugandan_cities)){
  for (j in 1:nrow(dates_df)){
    
    df <- data.frame(get_weather(station = ugandan_cities$loc[i],
                                 start_date = dates_df$start_date[j],
                                 end_date = dates_df$end_date[j],
                                 airport = ugandan_cities$airport[i]))
    
    assign(paste0(ugandan_cities$loc[i], "_df_", dates_df$end_date[j]), 
           df)
    
    if(i == 1 & j == 1){
      updated_df <- df
    } else{
      updated_df <- rbind(df, updated_df) 
    }
    rm(list = (paste0(ugandan_cities$loc[i], "_df_", dates_df$end_date[j])))
  }
}

# Rename updated_df to dat
dat <- updated_df ; updated_df <- NULL

# Clean up dates
dat$date <- as.Date(dat$date, format = "%Y-%m-%d")

# Remove extreme precipitation outliers
dat$precipitation[which(dat$precipitation >
                          mean(dat$precipitation, na.rm = TRUE) * 
                          3 * sd(dat$precipitation))] <- NA

# Remove extreme temperatures
dat$temp_max[which(dat$temp_max > 150)] <- NA
dat$temp_min[which(dat$temp_min < 20)] <- NA

# Write csv
write.csv(dat, "uganda_weather.csv")

