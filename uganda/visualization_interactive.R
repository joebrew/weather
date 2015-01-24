library(dplyr)
suppressPackageStartupMessages(library(googleVis))

# GOOGLE VIS 
# Flash TS
dat <- dat %>%
  mutate(week = format(date, "%U"),
         year = format(date, "%Y"))

dat_week <- dat %>%
  group_by(loc, week, year) %>%
  summarise(precipitation = sum(precipitation, na.rm = TRUE),
            temp_max = max(temp_max, na.rm = TRUE),
            temp_min = min(temp_min, na.rm = TRUE),
            cloud_cover = mean(cloud_cover, na.rm = TRUE))
dat_week$year_week <- paste0(dat_week$year, "W", dat_week$week)

dat_week <- data.frame(dat_week)

Motion=gvisMotionChart(dat_week, 
                       idvar="loc", 
                       timevar="year_week")
#xvar = "temp_max",
#yvar = "precipitation",
#colorvar = "loc",
#sizevar = "cloud_cover")
plot(Motion)

dat$precipitation <- as.numeric(dat$precipitation)
plot_calendar <- function(loc = "ENTEBBE"){
  data <- dat[which(dat$loc == loc),]
  Cal <- gvisCalendar(data, 
                      datevar="date", 
                      numvar="precipitation",
                      options=list(
                        title=paste0("Daily precipitation in ", as.character(loc)),
                        height=500,
                        calendar="{yearLabel: { fontName: 'Times-Roman',
                               fontSize: 32, color: '#1A8763', bold: true},
                               cellSize: 10,
                               cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                               focusedCellColor: {stroke:'red'}}")
  )
  return(Cal)
}
plot(plot_calendar())