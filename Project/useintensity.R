# # # # # # # #  # # # # # # # # # # # # # # # # 
# INTENSITY OF ELK USE AND AVAILABLE NUTRITION #
#            WILD562 CLASS PROJECT             #
#               KRISTIN BARKER                 #
#                 APRIL 2017                   #
# # # # # # # #  # # # # # # # # # # # # # # # # 



#### SETUP ####

## working directory ##
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\HabitatModelling\\Project"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\HabitatModelling\\Project"
ifelse(file.exists(wd_workcomp), setwd(wd_workcomp), setwd(wd_laptop))
rm(wd_workcomp, wd_laptop)


## packages ##
library(raster)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)

## data ##
rawlocs <- read.csv("../../ElkDatabase/collardata-locsonly-equalsampling.csv")
de14 <- raster("../../Vegetation/DE2014.tif")
de15 <- raster("../../Vegetation/DE2015.tif")
hbm14 <- raster("../../Vegetation/gherb2014.tif")
hbm15 <- raster("../../Vegetation/gherb2015.tif")

## projection definition ##
latlong <- CRS("+init=epsg:4326") # WGS84 projection

#### DATA PREP ####

# format dates and times; add IndivYr
locs <- rawlocs %>%
  within(Date <- as.Date(Date)) %>%
  within(Time <- as.numeric(gsub("[[:punct:]]", 
                                 "", locs$Time))) %>%
  mutate(IndivYr = ifelse(Date < "2015-01-01", 
                       paste(AnimalID, "-14", sep=""),
                       paste(AnimalID, "-15", sep="")))  

# rm non-foraging times; subset 1 rndm loc/day; spatialize
l14 <- locs %>%
  filter(Sex == "Female") %>%
  subset(between(Date, as.Date("2014-07-15"), as.Date("2014-08-31"))) %>%
  subset(Time < 1400 | Time > 1800) %>%
  group_by(IndivYr, Date) %>%
  sample_n(1) %>%
  ungroup()
  xy14 <- data.frame("x" = l14$Long, "y" = l14$Lat)
  ll14 <- SpatialPointsDataFrame(xy14, l14,proj4string = latlong)
  sp14 <- spTransform(ll14, de14@crs)
l15 <- locs %>%
  filter(Sex == "Female") %>%
  subset(between(Date, as.Date("2015-07-15"), as.Date("2015-08-31"))) %>%
  subset(Time < 1400 | Time > 1800) %>%
  group_by(IndivYr, Date) %>%
  sample_n(1) %>%
  ungroup()
  xy15 <- data.frame("x" = l15$Long, "y" = l15$Lat)
  ll15 <- SpatialPointsDataFrame(xy15, l15,proj4string = latlong)
  sp15 <- spTransform(ll15, de15@crs)



#### KRISTIN YOU LEFT OFF IN THIS SECTION ####
  
  # working on counting number of points per cell
  # rasterize returns NAs
  # count.points gives error w[[1]] : no [[ method for object without attributes
  # could possibly do this more manually,see stackoverflow stuff
  # but i think rasterize should be your best bet
  
# for each PIXEL,
  # calculate #locs/#total
test14 <- rasterize(xy14, de14, fun='count')
plot(test14)
unique(values(test14))

library(adehabitatMA)
pixels <- as(de14, 'SpatialPixels')

test14 <- count.points(sp14, pixels)
plot(pixels)
plot(sp14, color = "blue")

a <- table(cellFromXY(de14, sp14))






#### CUTS AND MISC ####

# verify subsetting worked as expected
length(unique(l14$IndivYr))*length(unique(l14$Date))
length(unique(l15$IndivYr))*length(unique(l15$Date))
 #14 is right but 15 is off by 59, huh
 #im ignoring it bc this is just a class project

