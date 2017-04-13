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

## raw data ##
rawlocs <- read.csv("../../ElkDatabase/collardata-locsonly-equalsampling.csv")
rawde14 <- raster("../../Vegetation/DE2014.tif")
rawde15 <- raster("../../Vegetation/DE2015.tif")
rawhbm14 <- raster("../../Vegetation/gherb2014.tif")
rawhbm15 <- raster("../../Vegetation/gherb2015.tif")
migstatus <- read.csv("../../Nutrition/migstatus.csv")

## projection definition ##
latlong <- CRS("+init=epsg:4326") # WGS84 projection



#### DATA PREP ####

## GDM prelim estimation ##
gdm14 <- rawde14*rawhbm14
gdm15 <- rawde15*rawhbm15

## format dates and times; add IndivYr ##
locs <- rawlocs
locs$Date <- as.Date(locs$Date)
locs$Time <- as.numeric(gsub("[[:punct:]]", 
                             "", locs$Time))
locs$IndivYr <- ifelse(locs$Date < "2015-01-01", 
                       paste(locs$AnimalID, 
                             "-14", sep=""),
                       paste(locs$AnimalID, 
                             "-15", sep=""))  

## rm non-foraging times; subset 1 rndm loc/day; spatialize ##
l14 <- locs %>%
  filter(Sex == "Female") %>%
  subset(between(Date, as.Date("2014-07-15"), as.Date("2014-08-31"))) %>%
  subset(Time < 0300 | Time > 2300) %>%
  group_by(IndivYr, Date) %>%
  sample_n(1) %>%
  ungroup()
    xy14 <- data.frame("x" = l14$Long, "y" = l14$Lat)
    ll14 <- SpatialPointsDataFrame(xy14, l14,proj4string = latlong)
    sp14 <- spTransform(ll14, rawde14@crs)
l15 <- locs %>%
  filter(Sex == "Female") %>%
  subset(between(Date, as.Date("2015-07-15"), as.Date("2015-08-31"))) %>%
  subset(Time < 0300 | Time > 2300) %>%
  group_by(IndivYr, Date) %>%
  sample_n(1) %>%
  ungroup()
    xy15 <- data.frame("x" = l15$Long, "y" = l15$Lat)
    ll15 <- SpatialPointsDataFrame(xy15, l15,proj4string = latlong)
    sp15 <- spTransform(ll15, rawde15@crs)


    
#### COUNT NUMBER OF POINTS PER PIXEL ####


## XY coordinates from 2014 and 2015 data ##   
coords14 <- data.frame(sp14@coords)
coords15 <- data.frame(sp15@coords)


## reference raster - 250m2 resolution ##
refraster <- raster(extent(rawde14), crs = rawde14@crs,
                    res = c(250, 250))

## nlocs/250m2 pixel ##
n14.250 <- rasterize(coords14, refraster, fun='count')
n15.250 <- rasterize(coords15, refraster, fun='count')

## resampled nute/250m2 pixel ##
de14.250 <- resample(rawde14, refraster, method='bilinear')
de15.250 <- resample(rawde15, refraster, method='bilinear')
hbm14.250 <- resample(rawhbm14, refraster, method='bilinear')
hbm15.250 <- resample(rawhbm15, refraster, method='bilinear')
gdm14.250 <- resample(gdm14, refraster, method='bilinear')
gdm15.250 <- resample(gdm15, refraster, method='bilinear')

## combine nlocs with underlying veg data ##
brick14 <- brick(n14.250, de14.250, hbm14.250, gdm14.250)
locs14 <- data.frame(getValues(brick14))
locs14 <- locs14 %>%
  rename(nLocs = layer.1,
         DE = DE2014,
         Biomass = gherb2014,
         GDM = layer.2) %>%
  mutate(Year = "2014") %>%
  filter(!is.na(nLocs))
locs14$UseIntensity <- locs14$nLocs/sum(locs14$nLocs)
brick15 <- brick(n15.250, de15.250, hbm15.250, gdm15.250)
locs15 <- data.frame(getValues(brick15))
locs15 <- locs15 %>%
  rename(nLocs = layer.1,
         DE = DE2015,
         Biomass = gherb2015,
         GDM = layer.2) %>%
  mutate(Year = "2015") %>%
  filter(!is.na(nLocs)) 
locs15$UseIntensity <- locs15$nLocs/sum(locs15$nLocs)
locsnute.250 <- bind_rows(locs14, locs15)


#### USE/NUTE MODEL ####



#### VISUALS ####

## DE raster with elk locs in it, 2014 ##
plot(de14)
plot(sp14, add=T)


## relationship bt nlocs and DE ##
plot(UseIntensity ~ DE, data=locsnute.250)
ggplot(locsnute.250, aes(x=DE, y=UseIntensity)) +
  stat_smooth(method = glm) +
  geom_point()
# gross
ggplot(locsnute.250, aes(x=DE, y=nLocs)) +
  stat_smooth(method = "auto") +
  geom_point()


## freq distn of per cap nute ##
test <- locsnute.250 %>%
  mutate(pcDE = DE/UseIntensity,
         pcBM = Biomass/UseIntensity,
         pcGDM = GDM/UseIntensity)
par(mfrow=c(3,1))
hist(test$pcDE)
hist(test$pcBM)
hist(test$pcGDM)

## poisson requires counts; can't do intensity
testmod <- glm(nLocs ~ pcGDM,
  family = poisson, 
  data = test)
summary(testmod)







#### CUTS AND MISC ####



## VERIFY SUBSETTING WORKED AS EXPECTED ##

length(unique(l14$IndivYr))*length(unique(l14$Date))
length(unique(l15$IndivYr))*length(unique(l15$Date))
 #14 is right but 15 is off by 59, huh
 #im ignoring it bc this is just a class project



## COUNT NUMBER OF POINTS PER PIXEL ##

library(adehabitatMA)
pixels <- as(de14, 'SpatialPixels')
test14 <- count.points(sp14, pixels)
plot(pixels)
plot(sp14, color = "blue")
# error w[[1]] : no [[ method for object without attributes

a <- table(cellFromXY(de14, sp14))

test <- rasterize(sp14, de14, fun='count')
plot(test)
# returns brick with NA values
# it's trying to count occurrence of each column i think


# below (stolen) code works
r <- raster(ncols=36, nrows=18)
n <- 1000
x <- runif(n) * 360 - 180
y <- runif(n) * 180 - 90
xy <- cbind(x, y)
# prensence/absensce (NA) (is there a point or not?)
r1 <- rasterize(xy, r, field=1)
# how many points?
r2 <- rasterize(xy, r, fun=function(x,...)length(x))
r3 <- rasterize(xy, r, fun='count')
plot(r2)
plot(r3)
# those 2 functions do the same thing, good


testlocs <- data.frame(sp14@data$Long, sp14@data$Lat)
test <- rasterize(testlocs, de14, fun='count') 
# still NAs but closer... it's trying to plot now

testlocs <- data.frame(sp14@data$Long, sp14@data$Lat)
x2 <- sp14@data$Long
test <- rasterize(testlocs, de14, 
                  fun=function(x2,...)length(x2)) 
# stilllllll NAs...
# maybe i need a blank raster for this

testrast <- raster(extent(de14), crs = de14@crs)
test <- rasterize(sp14, testrast, fun='count')  
# nope still brick

testrast <- raster(extent(de14), crs = de14@crs)
test <- rasterize(testlocs, testrast, fun='count')
plot(test)
unique(test@data@values)
# fuckkkkkkk
plot(test); plot(testlocs, add=T)
# ohhhh duh, using latlong from @data slot
# rather than stateplace from @coords
# smart, kristin

testlocs <- data.frame(sp14@coords)
test <- rasterize(testlocs, de14, fun='count')
plot(test)
unique(test@data@values)
length(which(!is.na(test@data@values)))
# seems right, but plot doesn't show any data
writeRaster(test, "nlocs-test", format="GTiff")
# ok looks correct in arcmap, just hard to see data
# here because pixels are so small

testlocs <- data.frame(sp14@coords)
testrast <- raster(extent(de14), crs = de14@crs)
test2 <- rasterize(testlocs, testrast, fun='count')
plot(test2) 
# eeeeeee
unique(test2@data@values)
length(unique(test2@data@values))
summary(test2@data@values)
# now the plot does show data
# but pixels are way too big
# next step: choose a resolution between this and above
  # which will require resampling DE raster

coords14 <- data.frame(sp14@coords) # make df of just XYs
resraster <- raster(extent(de14), crs = de14@crs)
res(resraster) <- c(100, 100) # reference raster
de14 <- resample(rawde14, resraster, method='bilinear')
plot(de14)
test <- rasterize(coords14, de14, fun='count')
plot(test)
unique(test@data@values)
summary(test@data@values)
writeRaster(test, "nlocs-test2", format="GTiff")
# seems better but may still need a little coarser

resraster2 <- raster(extent(de14), crs = de14@crs)
res(resraster2) <- c(500, 500)
de14 <- resample(rawde14, resraster2, method='bilinear')
plot(de14)
test <- rasterize(coords14, de14, fun='count')
plot(test)
unique(test@data@values)
summary(test@data@values)
writeRaster(test, "nlocs-test3", format="GTiff")
# wayyyyyyy too coarse; covers shitloads of DE pixels

resraster3 <- raster(extent(de14), crs = de14@crs)
res(resraster3) <- c(250, 250) #justify w ndvi, haha
de14 <- resample(rawde14, resraster3, method='bilinear')
plot(de14)
test <- rasterize(coords14, de14, fun='count')
plot(test)
unique(test@data@values)
summary(test@data@values)
writeRaster(test, "nlocs-test4", format="GTiff")

## biomass ##
bm14 <- resample(rawhbm14, refraster, method='bilinear')
nbm14 <- rasterize(coords14, bm14, fun='count')
bm15 <- resample(rawhbm15, refraster, method='bilinear')
nbm15 <- rasterize(coords15, bm15, fun='count')
# didn't need to do this; it gives exact same info
# in fact could've just used your refraster to count nlocs
# now need to combine nlocs with assoc'd nute vals

## digestible energy ##
de14 <- resample(rawde14, refraster, method='bilinear')
nde14 <- rasterize(coords14, de14, fun='count')
de15 <- resample(rawde15, refraster, method='bilinear')
nde15 <- rasterize(coords15, de15, fun='count')


## creating df of nlocs and nute ##
test <- data.frame(getValues(locsnute))
test <- test %>%
  rename(nLocs14 = layer.1,
         nLocs15 = layer.2) %>%
 filter(!is.na(nLocs14))
# ah right, nas in 2014 not same as in 2015
# make separate dfs to retain as much info as possible
