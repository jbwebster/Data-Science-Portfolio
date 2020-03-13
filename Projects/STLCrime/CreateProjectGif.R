library(ggplot2)
library(tidyr)
library(dplyr)
library(gganimate)
library(transformr)
library(ggmap)
library(rgdal) 


months <- c("January", "February", "March",
            "April", "May", "June",
            "July", "August", "September",
            "October", "November", "December")
# Years available
years <- c("2016", "2017", "2018", "2019")
# Columns of interest that appear in each dataset
cols <- c("Complaint", "CodedMonth", "DateOccur", "FlagCrime",
          "Count", "Crime", "District", "ILEADSAddress",
          "ILEADSStreet", "LocationName", "LocationComment",
          "Neighborhood", "XCoord", "YCoord")
dat <- NULL
for (year in years) {
  for (month in months) {
    # Path to data file
    filename <- paste0("Datasets/", month, year, ".csv")
    file_data <- read.csv(filename)
    # Some datasets include periods in their column names. Remove them
    names(file_data) <- gsub("\\.", "", names(file_data))
    # Add a leading 0 to crime codes that got truncated
    file_data$Crime <- formatC(file_data$Crime, width=6, format="d", flag="0")
    # Subset to the columns of potential interest that are common
    # to all datasets
    file_data <- subset(file_data, select = cols)
    # Remove reports that are not coded as actual crimes
    file_data <- subset(file_data, file_data$Count == 1)
    # Set a flag for crimes that are considered "Class I" crimes and "Class II" crimes
    file_data$ClassI <- ifelse(as.numeric(substr(file_data$Crime, start = 1, stop = 2)) < 9, 1, 0)
    file_data$ClassII <- ifelse(file_data$ClassI == 1, 0, 1)
    # Split up year and month info
    file_data$Month <- as.numeric(substr(file_data$CodedMonth, start = 6, stop = 7))
    file_data$Year <- as.numeric(substr(file_data$CodedMonth, start = 1, stop = 4))
    # Combine datasets into a single data.frame
    if (is.null(dat)) dat <- file_data
    else dat <- rbind(dat, file_data)
  }
}
# Add a leading 0 to crime codes that got truncated
dat$Crime <- formatC(dat$Crime, width=6, format="d", flag="0")

# Remove reports that are not coded as actual crimes
dat <- subset(dat, dat$Count == 1)

# Set a flag for crimes that are considered "Class I" crimes and "Class II" crimes
dat$ClassI <- ifelse(as.numeric(substr(dat$Crime, start = 1, stop = 2)) < 9, 1, 0)
dat$ClassII <- ifelse(dat$ClassI == 1, 0, 1)

# Split up info from the date/time of the event
dat$Date <- substr(dat$DateOccur, start = 1, stop = 10)
dat$Date <- gsub("/", "-", dat$Date)
dat$Time <- substr(dat$DateOccur, start = 12, stop = 16)
dat$DateObj <- as.Date(dat$Date, format = "%m-%d-%Y") 

sub <- subset(dat, year(dat$DateObj) > "2018")
nad83_coords <- data.frame(x=sub$XCoord, y=sub$YCoord)
nad83_coords$x <- nad83_coords$x * .3048
nad83_coords$y <- nad83_coords$y * 0.3048
coordinates(nad83_coords) <- c("x","y")
proj4string(nad83_coords)=CRS("+init=epsg:6512")#epsg:6512, 3602
coords <- spTransform(nad83_coords,CRS("+init=epsg:4326"))
coords <- data.frame(coords@coords)
names(coords) <- c("lon", "lat")
sub$Long <- coords$lon
sub$Lat <- coords$lat
sub$Month <- month(sub$DateObj)
sub$Month <- month.abb[sub$Month]
sub$Month <- factor(sub$Month, levels = month.abb)
sub <- subset(sub, sub$ClassI == 1)
sub$Category <- substring(sub$Crime, 0, 2)
categories <- c("Homicide", "Rape", "Robbery", "Aggravated Assault",
                "Burglary", "Larceny", "Vehicle Theft", "Arson")
sub$Category <- categories[as.numeric(sub$Category)]
sub$Category <- factor(sub$Category, levels = categories)

# Alternate function for calculating inter-state densities
StatDensityContour <- ggproto('StatDensityContour', StatDensity2d,
                              compute_group = function (data, scales, na.rm = FALSE, h = NULL, contour = TRUE, 
                                                        n = 100, bins = NULL, binwidth = NULL) {
                                StatDensity2d$compute_group(data, scales, na.rm = na.rm, h = h, contour = FALSE, 
                                                            n = n, bins = bins, binwidth = binwidth)
                              },
                              finish_layer = function(self, data, params) {
                                names(data)[names(data) == 'density'] <- 'z'
                                do.call(rbind, lapply(split(data, data$PANEL), function(d) {
                                  StatContour$compute_panel(d, scales = NULL, bins = params$bins, 
                                                            binwidth = params$binwidth)
                                }))
                              }
)

myMap <- readRDS("StLouisMap.rds")
projectgif <- ggmap(myMap) +
  geom_contour(data = sub,
               aes(x=Long,y=Lat), stat = "density_contour") +
  labs(x="Longitude",y="Latitude",
       title="Crime Location - {closest_state}") +
  scale_x_continuous(limits = c(-90.3, -90.182)) +
  scale_y_continuous(limits = c(38.56, 38.67)) +
  theme(legend.position = "none") +
  transition_states(
    Month,
    transition_length = 12,
    state_length = 1,
    wrap = F
  )
anim_save("project_gif.gif", animation = animate(projectgif))


