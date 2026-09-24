
# Lecture 1 - Script
# Analysis of various data types in R
#set working directory
path <- '/home/vuddameri/MathWorkshop/Data-Lecture1/Lecture1-Data/'
setwd(path)

# Read a CSV file containing ACS data
# Read the Data
fname <- 'SanAntonioACS.csv'
a <- read.csv(fname)

# show some sample
head(a)

# Get column names
colnames(a)

# Make a Boxplot of Median Income
boxplot(a$MedIncome,ylab='Median Salary $',xlab="")
title('Median Income across San Antonio Census Tracts',cex.head=0.8)
grid()

# Example 2: Read Tiff Images
library(tiff) # to read tiff files
library(fields) # plot with scalebar
fname1 <- 'NAIP102016TAMUSA.tif'
naip2016 <- readTIFF(fname1)
#Look at its characteristics
class(naip2016) # what data structure
typeof(naip2016) #what data type
dim(naip2016)  # how many bands
plot(as.raster(naip2016[,,1:3])) # make a plot using RGB

# Example 3: Perform NDVI calculation
# Plot NIR
nir2016 <- naip2016[,,4]  # NIR Band
red2016 <- naip2016[,,1]  # Red Band
ndvi2016 <- (nir2016-red2016)/(nir2016+red2016)
image.plot(ndvi2016, main = "NDVI 2016", 
           xlab = "Easting", ylab = "Northing")

# Example 4: Read a MP3 Sound File
library(tuneR)
fname2 <- 'alamostrong.mp3'
audio <- readMP3(fname2) # read the file
x <- audio@left  # get amplitudes
fs <- audio@samp.rate # get sampling rate
x10 <- x[1:(10*fs)] #keep first 10 sec
time <- seq(0,10*fs-1,1)/fs #get time
plot(time,x10,xlab='Time(s)',ylab='Amplitude',
     type='l',lwd=2,col='blue') # make plot
grid() # add grid

#Example 5: Time Series Data
library(zoo)
library(forecast)
fname3 <- 'J17Well.csv'
wl <- read.csv(fname3)
head(wl)
WL <- wl$DHE # extract Daily high WL
date <- seq(as.Date('1993-01-01'),as.Date('2025-12-31')
            ,freq=1) #create date sequence
WLTS <- zoo(WL,order.by=date) # create ts object
# Create a 10 day moving average
wlma10 <- rollmean(WLTS, k = 10, fill = NA)
# plot 10-D Moving average time-series
par(mgp=c(2,1,0))
plot(wlma10,xlab='Time',
     ylab='10 Day Average Daily High WL (ft AMSL)',
     col='blue',lty=2,lwd=2,
     main = 'J-17 10 Day Average Daily High Water Elevation',cex.title=0.8)
grid()
abline(h=660,lty=2,lwd=1,col='red')
# Compute ACF
wlma10acf <- Acf(wlma10,lag.max=365)
plot(wlma10acf,type='l',xlab='Lag (days)',ylab='ACF',lty=3,lwd=2,main="")
grid()
title('ACF of J17 10 Day Moving Average',cex.title=0.8)

# Example 6: GIS in R Vector
library(sf)
fname4 <- 'SanAntonioACS.gpkg'
sa <- st_read(fname4)
head(sa)
plot(sa["PovertyRate"],main = "Poverty Rate %")


# Example 7: Vegetation changes around TAMUSA
library(terra) # Load library terra for raster
# Read NAIP images
naip16 <- rast("NAIP102016TAMUSA.tif")
naip24 <- rast("NAIP102024TAMUSA.tif")
compareGeom(naip16, naip24)
# resample to make them both extents
naip24a <- resample(naip24, naip16, method = "bilinear")
compareGeom(naip16, naip24a) # check crs and extent

# NAIP: Band 1 = Red, Band 4 = NIR
ndvi16 <- (naip16[[4]] - naip16[[1]]) /
  (naip16[[4]] + naip16[[1]])
ndvi24 <- (naip24a[[4]] - naip24a[[1]]) /
  (naip24a[[4]] + naip24a[[1]])

# Change: 2024 minus 2016
ndviChange <- ndvi24 - ndvi16

# Make plots
ndviCols <- rev(hcl.colors(100, "YlGn"))
layout(matrix(c(1, 1, 2, 2, 0, 3, 3, 0), nrow = 2,
              byrow = TRUE))
plot(ndvi16, col = ndviCols, range = c(-1, 1), main = "NDVI 2016")

plot(ndvi24, col = ndviCols, range = c(-1, 1), main = "NDVI 2024")

plot(ndviChange, col = hcl.colors(100, "Blue-Red 3", rev = TRUE),
     main = "Change in NDVI (2024 - 2016)")

# Example 8: Sentiment analysis
library(syuzhet)
fname5 <- "SAwinterfloods.txt"
txt <- readLines(fname5, warn = FALSE, encoding = "UTF-8")
head(txt)
corpus <- paste(txt, collapse = " ") # collapse into a single corpus
sentences <- get_sentences(corpus) # break into sentences
nrc <- get_nrc_sentiment(sentences) # perform sentiment analysis
totals <- colSums(nrc) # overall emotions totals
totals.sort <- sort(totals) # sort emotions
barplot(totals.sort, horiz = TRUE, las = 1,
        col = viridis(length(totals.sort)),
        xlab = "NRC Lexicon Matches",
        main = "Emotional Profile of San Antonio Flood Story")
box()




