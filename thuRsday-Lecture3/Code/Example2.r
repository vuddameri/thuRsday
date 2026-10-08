
# Add time-series data to a map

# Step 1: Load libraries
library(sf)        # Spatial/GIS vector data
library(zoo)       # Time-series data
library(plotly)    # Interactive plots
library(leaflet)   # Interactive web maps
library(leafpop)   # Graphs/tables in Leaflet popups
library(base64enc) # Embed images in HTML/popups

# Step2: Set Path, Working Directory and File Names
path <- '/home/vuddameri/MathWorkshop/thuRsday-Lecture3/Data/'
setwd(path)

fname1 <- 'PRISM-SATX.csv'
fname2 <- 'SanAntonioBND.gpkg'

# Step 3: Read the Climate Data 
a <- read.csv(fname1,skip =10)  # skip first 10 lines
head(a)

#Step 3a: Change column names
colnames(a) <- c('Date','PPTIN','TMINF','TMEANF','TMAXF','TDMEAN','VPDMINHPA',
                 'VPDMAXHPA','SOLRAD')  # change column names

# Step 4: Create a time-series object to plot
Date <- as.Date(a$Date)  # create a 
Tppt <- zoo(a$PPTIN,order.by=Date)

#  Step 5: Store the plot as a png
png("TpptPlot.png", width = 6, height = 4,units='in',res=300)
par(mgp=c(2,1,0)) #spacing of axis lables
plot(Tppt,xlab='Year',ylab="Preciptation (Inches)")
title('Daily Total Precipitation',cex.main=0.9)
grid()
dev.off()  # close the connection to png

# Step 6: Read the GeoPackage of San Antonio Boundary
sa <- st_read(fname2)
# Get the CRS
st_crs(sa)
sa <- st_transform(sa, 4326) # change to WGS84


# Step 7: Make an Airport SF object
airport <- st_as_sf(
  data.frame(
    Name = "San Antonio Airport",
    Lon = -98.47,  # longitude
    Lat = 29.53 # Latitude
  ),
  coords = c("Lon", "Lat"),
  crs = 4326 #WGS Datum
)

# Step 8: Read the SVG Image file back
img <- dataURI(file = "TpptPlot.png",
               mime = "image/png")

# Step 9: Create a popup box 
popup <- paste0(
  "<b>San Antonio Airport</b><br>",
  "<img src='", img, "' width='500'>"
)

# Step 10: Make the plot
m <- leaflet()  # initiate the leaflet plot
m <- addTiles(m)  # add San Antonio tile
m <- addPolygons(  # Add san Antonio boundary
  m,
  data = sa,
  fill = FALSE
)

m <- addCircleMarkers(  # add marker with popup box
  m,
  data = airport,
  radius = 6,
  popup = popup
)

m # display

