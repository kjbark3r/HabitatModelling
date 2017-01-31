#### Script for WILD 562 -  Lab 1: Introduction to R

R.Version()
citation()

?read.csv

banffEast <-read.csv("/Users/mark.hebblewhite/Dropbox/WILD 562/Spring2017/Lab1_rintro/Banffeast.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
banffWest <-read.csv("/Users/mark.hebblewhite/Dropbox/WILD 562/Spring2017//Lab1_rintro/Banffwest.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
## or 
setwd("/Users/mark.hebblewhite/Dropbox/WILD 562/Spring2017/Lab1_rintro/new/Lab1_data")
banffEast <-read.csv("Banffeast.csv")


str(banffEast)
str(banffWest)

head(banffEast)
head(banffWest)
tail(banffWest)

# Calculate some basic summary statistics on the data entered so far 
summary(banffEast)
summary(banffWest)

## simple graphical exploration
hist(banffEast$EastElk)
hist(banffWest$Elkwestraw)

# Make a time-series scatterplot
plot(banffEast$Year, banffEast$EastElk)
?plot
# change the labels

plot(EastElk~Year, reg.line=FALSE, smooth=FALSE, spread=FALSE, 
     boxplots=FALSE, span=0.5, xlab="Years", ylab="East Zone Population", 
     data=banffEast)

plot(Elkwestraw~Year, reg.line=FALSE, smooth=FALSE, spread=FALSE, 
     boxplots=FALSE, span=0.5, xlab="Years", ylab="East Zone Population", 
     data=banffWest)

lines(lowess(banffEast$Year,banffEast$EastElk), col="blue")

plot(Elkwestraw~Year, type = "l", data=banffWest)
plot(Elkwestraw~Year, type = "p", data=banffWest)

par(mfrow = c(1,2))
plot(EastElk~Year, reg.line=FALSE, smooth=FALSE, spread=FALSE, 
     boxplots=FALSE, span=0.5, xlab="Years", ylab="East Zone Population", 
     data=banffEast)

plot(Elkwestraw~Year, reg.line=FALSE, smooth=FALSE, spread=FALSE, 
     boxplots=FALSE, span=0.5, xlab="Years", ylab="East Zone Population", 
     data=banffWest)

## explore different plotting options
## http://www.statmethods.net/graphs/scatterplot.html
library(car)
scatterplot(Elkwestraw~Year, data=banffWest)
library(ggplot2)
citation("ggplot2")

ggplot(banffWest, aes(x=Year, y=Elkwestraw)) +geom_point(shape=2, size = 8)
ggplot(banffWest, aes(x=Year, y=Elkwestraw)) +geom_point(shape=2, size = 8) + stat_smooth(method=lm)
ggplot(banffWest, aes(x=Year, y=Elkwestraw)) +geom_point(shape=2, size = 8) + stat_smooth(method=loess)


# You may have noticed that the Year column was not an option in the
# compute new variable window. 
# To fix this, Data, Active data set, Refresh active data set.

# Make a time-series scatterplot
plot(EastElk~Year, reg.line=FALSE, smooth=FALSE, spread=FALSE, 
  boxplots=FALSE, span=0.5, xlab="Years", ylab="East Zone Population", 
  data=banffEast)


######### Working with the combined data set
# Merge the data sets and save the result in MergedDataset
banffelk <- merge(banffEast, banffWest, all.x= TRUE, all.y = TRUE)
str(banffelk)
head(banffelk)

## rename row columing heading to be the same/simpler
names(banffelk)[c(4)] <- c("WestElk")
names(banffelk)[c(5)] <- c("Wolf")
str(banffelk)
# Manipulating an existing data frame
#Calculate density for the east zone elk and store it in a variable named Density
## The eastern zone is 67 km^2 in Banff
banffelk$Density <- with(banffelk, EastElk/67)

## while the western zone is 186 km^2
banffelk$Density <- with(banffelk, WestElk/186)

## regraph
par(mfrow = c(1,2))
plot(EastElk~Year, reg.line=FALSE, smooth=FALSE, spread=FALSE, 
     boxplots=FALSE, span=0.5, xlab="Years", ylab="East Zone Population", 
     data=banffelk)

plot(WestElk~Year, reg.line=FALSE, smooth=FALSE, spread=FALSE, 
     boxplots=FALSE, span=0.5, xlab="Years", ylab="West Zone Population", 
     data=banffelk)

## reshaping data from WIDE into LONG format
banffelk_long <- reshape(banffelk, varying=c("EastElk", "WestElk"), v.names="Elk", timevar = "N", direction = "long")
banffelk_long
str(banffelk_long)
banffelk_long$zone <- ifelse(banffelk_long$N == 1, "EastElk", "WestElk")

## other useful packages and commands for converting from wide to long data format
? reshape
? melt


# Create a scatterplot of both elk populations on the same axes,
# using the zones columns as the grouping variable.
scatterplot(Elk~Year | zone, reg.line=lm, smooth=FALSE, spread=FALSE, 
  boxplots=FALSE, span=0.5, xlab="Year", ylab="Elk Population", 
  by.groups=TRUE, data=banffelk_long)

# Fit linear model to east zone
?glm
eastlm <- glm(Elk ~ Year, family=gaussian(identity), data=banffelk_long, subset=zone == "EastElk")
summary(eastlm)

## why is the intercept so weird?
banffelk_long$t <- (banffelk_long$Year - 1984)
head(banffelk_long)
par(mfrow = c(1,1))
plot(banffelk_long$t, banffelk_long$Year)
plot(banffelk_long$t, banffelk_long$Elk)
eastlm <- glm(Elk ~ t, family=gaussian(identity), data=banffelk_long, subset=zone == "EastElk")
summary(eastlm)

# Fit linear model to west zone
westlm <- glm(Elk ~ t, family=gaussian(identity), data=banffelk_long, subset=zone == "WestElk")
summary(westlm)

# Fit a model with an interaction between the zone and Year
bothZones <- glm(Elk ~ Year*zone, family=gaussian(identity), data=banffelk_long)
summary(bothZones)

# Start considering density as compared to counts with side-by-side boxplots
par(mfrow = c(1,2))
boxplot(Density~zone, ylab="Density", xlab="zone", data=banffelk_long)
boxplot(Elk~zone, ylab="Population Count", xlab="zone", data=banffelk_long)
# what do we concldue about differences in counts versus density between zones?

# Conduct a 2-sample t-test of densities between the two zones
t.test(Elk~zone, alternative='greater', conf.level=.95, 
       var.equal=FALSE, data=banffelk_long)

t.test(Density~zone, alternative='greater', conf.level=.95, 
  var.equal=FALSE, data=banffelk_long)

# Fit a linear model with an interaction term to density
densityModel <- glm(Density ~ Year * zone, family=gaussian(identity), 
  data=banffelk_long)
summary(densityModel)

###############################################################################

#### Cougar Track Count Analysis #####


# Import the data csv 
cougar <- read.table("/Users/mark.hebblewhite/Dropbox/WILD 562/Spring2017/Lab1_rintro/golfcourse_cougar.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
head(cougar)
str(cougar)


# Convert the USENONUSE column to a factor for future analysis. 
# Stored as factorUSE
cougar$factorUSE <- as.factor(cougar$UseNonUse)

# Calculate some basic summary statistics for the data set...
summary(cougar)
# ... by used and unused locations
library(plyr)
?ddply
ddply(cougar, "UseNonUse", summarise, mean = mean(Slope))
ddply(cougar, "UseNonUse", summarise, mean = mean(AllTrails))
ddply(cougar, "UseNonUse", summarise, mean = mean(CoverDist))
ddply(cougar, "UseNonUse", summarise, mean = mean(Roads))


# Visualize this differences with overlaid histograms for each variable
# The multhist function from the plotrix package does a nice job of doing this.
install.packages("plotrix")
library(plotrix)
# Use a 2x2 plotting matrix to see all of the histograms at once
par(mfrow = c(2,2))
multhist(list(cougar$AllTrails[cougar$factorUSE==1],cougar$AllTrails[cougar$factorUSE==0]), freq = TRUE, main = "Trails")
# I chose to put a legend in the upper right hand graph. 
# That's what the additional arguments in the line below specify.
multhist(list(cougar$CoverDist[cougar$factorUSE==1],cougar$CoverDist[cougar$factorUSE==0]), freq = TRUE, main = "Cover Distance", legend.text = c("Used", "Unused"), args.legend = list(bty = "n"))
multhist(list(cougar$Roads[cougar$factorUSE==1],cougar$Roads[cougar$factorUSE==0]), freq = TRUE, main = "Roads")
multhist(list(cougar$Slope[cougar$factorUSE==1],cougar$Slope[cougar$factorUSE==0]), freq = TRUE, main = "Slope")

# Box plots also do a good job of visualizing these differences.
# These are plotted in a similar fashion as above.
par(mfrow= c(2,2))
boxplot(AllTrails~factorUSE, ylab="Distance (m)", xlab="Used",main = "Trails", data=cougar)
boxplot(CoverDist~factorUSE, ylab="Distance (m)", xlab="Used", main = "Cover", data=cougar)
boxplot(Roads~factorUSE, ylab="Distance (m)", xlab="Used",main = "Roads", data=cougar)
boxplot(Slope~factorUSE, ylab="Slope", xlab="Used", main = "Slope", data=cougar)


# Test for significant differences in each of the covariates
t.test(AllTrails~factorUSE, alternative='two.sided', conf.level=.95, 
  var.equal=FALSE, data=cougar)
t.test(CoverDist~factorUSE, alternative='two.sided', conf.level=.95, 
  var.equal=FALSE, data=cougar)
t.test(Roads~factorUSE, alternative='two.sided', conf.level=.95, 
  var.equal=FALSE, data=cougar)
t.test(Slope~factorUSE, alternative='two.sided', conf.level=.95, 
  var.equal=FALSE, data=cougar)


# Create a data set of only the used locations
USEonly <- subset(cougar, subset=UseNonUse == 1)

# Construct linear models of track counts as a function of 4 covariates
# Distance to trails model
trails <- glm(Use ~ AllTrails, family=gaussian(identity), data=USEonly)
summary(trails)
# Slope model
slope <- glm(Use ~ Slope, family=gaussian(identity), data=USEonly)
summary(slope)
# Distance to cover model
cover <- glm(Use ~ CoverDist, family=gaussian(identity), data=USEonly)
summary(cover)
# Distance to roads model
roads <- glm(Use ~ Roads, family=gaussian(identity), data=USEonly)
summary(roads)

# Test track counts for normality
shapiro.test(USEonly$Use)
# Visualize with a histogram
par(mfrow= c(1,1))
hist(USEonly$Use, scale="frequency", breaks="Sturges", col="darkgray")
## not really that normal

# Re-fit linear models with a ln transform
# First make the ln transform
USEonly$lnUSE <- with(USEonly, log(Use))
hist(USEonly$lnUSE) ## a bit more normal

### Now re-fit the models
# Distance to trails model
ln.trails <- glm(lnUSE ~ AllTrails, family=gaussian(identity), data=USEonly)
summary(ln.trails)
# Slope model
ln.slope <- glm(lnUSE ~ Slope, family=gaussian(identity), data=USEonly)
summary(ln.slope)
# Distance to cover model
ln.cover <- glm(lnUSE ~ CoverDist, family=gaussian(identity), data=USEonly)
summary(ln.cover)
# Distance to roads model
ln.roads <- glm(lnUSE ~ Roads, family=gaussian(identity), data=USEonly)
summary(ln.roads)

### Switch back to the complete data set. 
# Re-do the Shapiro-Wilk Test and make another histogram for all of the data
shapiro.test(cougar$Use)
hist(cougar$Use, scale="frequency", breaks="Sturges", col="darkgray")

# Use logistic regression to model the 
# log-odds of USE for each of the 4 covariates
# Trails model
logitTrails <- glm(UseNonUse ~ AllTrails, family=binomial(logit), data=cougar)
summary(logitTrails)
# Slope model
logitSlope <- glm(UseNonUse ~ Slope, family=binomial(logit),   data=cougar)
summary(logitSlope)
# Cover model
logitCover <- glm(UseNonUse~ CoverDist, family=binomial(logit),   data=cougar)
summary(logitCover)
# Roads model
logitRoads <- glm(UseNonUse ~ Roads, family=binomial(logit),  data=cougar)
summary(logitRoads)


################# Objective 3 - exploring SPATIAL data in R. 

## install packages we will need today
install.packages(c("ks", "lattice","adehabitatHR","maptools","foreign","rgdal","sp","raster","plot3D","rasterVis","colorRamps","rgeos"), dependencies = TRUE)
library("ks")
library("lattice")
library("adehabitatHR")
library("maptools")
library("foreign")
library("rgdal")
library("sp")
library("raster")
library("plot3D")
library("rasterVis")
library("colorRamps")
library("rgeos")
## note how tedious this was loading (library) each package at a time. 
## note this nifty little wrapper package Dan Eacker adapted from here: https://gist.github.com/stevenworthington/3178163
#function to install and load required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#load or install these packages:
packages <- c("ks", "lattice", "adehabitatHR", "maptools", "foreign", "rgdal", "sp", "raster","plot3D","rasterVis",
              "colorRamps","rgeos")

#run function to install packages - e.g., library command
ipak(packages)

################################################################################################
# WORKING WITH SHAPEFILES IN R

#first set working directory
setwd("/Users/mark.hebblewhite/Dropbox/WILD 562/Spring2017/Lab1_rintro/new/Lab1_data")

# reading in shapefiles (raster package)
elc_habitat<-shapefile("elc_habitat.shp")
humanaccess<-shapefile("humanacess.shp")
mcp2<-shapefile("mcp2.shp")
wolfyht<-shapefile("wolfyht.shp")

# make a very basic plot of shapefile after resetting graphical parameters
par(mfrow= c(1,1))
plot(elc_habitat)
plot(wolfyht)

# look at the class of the shapefile
class(elc_habitat)

# look at structure of shapefile
str(elc_habitat)

# look at first 20 rows of data for shapefile
head(elc_habitat@data, n=20)

# look at the projection of the shapefile (note the use of "@" instead of "$")
elc_habitat@proj4string@projargs

# look at the spatial extent of the shapefile
extent(elc_habitat)

# change the projection of the shapefile to another projection (package rgdal)
# using an epsg (Geodetic Parameter Dataset) code
# Find a list of these spatial reference codes here: http://spatialreference.org/ref/epsg/

elc_habitat <- spTransform(elc_habitat, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

# check new projection in geographic coordinate system WGS84
elc_habitat@proj4string@projargs

# reset projection back to what is was previously
elc_habitat <-  spTransform(elc_habitat, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

# look at the projection of the shapefile 
elc_habitat@proj4string@projargs

# another way to change it back to previous 
elc_habitat <-  spTransform(elc_habitat, CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

#write shapefile to files
writeOGR(elc_habitat,".","elc_habitat_NEW",driver="ESRI Shapefile")

#####################################################################################################
# WORKING WITH RASTERS IN R
par(mfrow= c(1,1)) ## reset graphical parameters

# reading in raster files (raster package)
deer_w<-raster("deer_w2.tif")
moose_w<-raster("moose_w2.tif") ## missing moose
elk_w<-raster("elk_w2.tif")
sheep_w<-raster("sheep_w2.tif") ## missing sheep
goat_w<-raster("goat_w2.tif")
wolf_w<-raster("wolf_w2.tif")#
elevation2<-raster("Elevation2.tif") #resampled
disthumanaccess2<-raster("DistFromHumanAccess2.tif") #resampled

# make a very basic plot of raster
plot(deer_w)

# look at the class of the raster
class(deer_w)

# look at basic raster summary
deer_w

# look at raster data
deer_w@data

# look at structure of raster
str(deer_w)

# look at the projection of the raster (note the use of "@" instead of "$")
deer_w@crs@projargs

# look at the spatial extent of the raster
extent(deer_w)

# look at the resolution of the raster
res(deer_w)

# change the projection of the raster to another projection (package rgdal)
deer_w2 <- projectRaster(deer_w, crs="+init=epsg:4326")
## or try this the same way we did above
deer_w2 <- projectRaster(deer_w, crs="+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

#check projection of the raster
deer_w2@crs@projargs

#change it back to what it was using another raster layers projection
deer_w <- projectRaster(deer_w2, wolf_w)

#check projection of the raster
deer_w@crs@projargs

# create a raster stack
all.rasters<-stack(deer_w, moose_w, elk_w, sheep_w, goat_w, elevation2, disthumanaccess2)

#check class
class(all.rasters)

# write raster to files in GeoTiff format
writeRaster(deer_w, "new_deer.tiff", "GTiff")


#####################################################################################################
# Pretty spatial plots in R

# Lets plot the wolf GPS points for the two packs using
# the "ggmap" package to connect to google maps

install.packages("ggmap", dependencies=T,repos="http://cran.rstudio.com/")
require(ggmap)

# ggmap only works in lat/longs (one of its downsides)
# and it would take a bit of code hacking to 
# get it to UTMs
# - for now, lets change the projection to a geographic projection
# so we can use ggmaps

# check projection of wolfyht
wolfyht@proj4string@projargs

wolfyht <- spTransform(wolfyht, CRS("+init=epsg:4269"))
wolfyht <- spTransform(wolfyht, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

deer_w <- projectRaster(deer_w, crs = wolfyht@proj4string@projargs)

#check projection of the raster
deer_w@crs@projargs

#calculate centroid of the wolf VHF data extent
lon<-(extent(wolfyht)[2] + extent(wolfyht)[1])/2
lat<-(extent(wolfyht)[4] + extent(wolfyht)[3])/2

# store extents for ggplot
minx <- extent(deer_w)[1]
maxx <- extent(deer_w)[2]
miny <- extent(deer_w)[3]
maxy <- extent(deer_w)[4]

#get satelite google map
sat.map<-get_map(location = c(lon = lon,
                              lat = lat),
                 color = "color", 
                 source = "google",
                 maptype = "satellite",
                 zoom = 9)

#get terrain map  
terr.map<-get_map(location = c(lon = lon,
                               lat = lat),
                  color = "color", 
                  source = "google",
                  maptype = "terrain",
                  zoom = 9)

#load Windows font "Times New Roman"
windowsFonts(Times = windowsFont("Times New Roman"))

# create data.frame for wolfyht
wolfyht.data <- data.frame(coordinates(wolfyht)[,1],coordinates(wolfyht)[,2],
                           wolfyht@data$Pack) 

# reset names for wolfyht.data
names(wolfyht.data) <- c("Longitude", "Latitude", "Pack")

# look at first 6 rows of data
head(wolfyht.data)

#start ggplot
x1<-ggmap(sat.map) +ylab("Latitude")+xlab("Longitude")+
  geom_point(data=wolfyht.data, aes(x=Longitude, y=Latitude,color=Pack))+ 
  theme(legend.position=c(0.875,0.89))+
  scale_colour_manual(values = c("red", "green"))
x2 <- x1+theme(legend.text=element_text(family="Times"),
               legend.title=element_text(face="bold", family="Times"))+
  theme(axis.text.y = element_text(family="Times"),axis.text.x = element_text(family="Times"),text = element_text( family="Times"),axis.title.x=element_text( family="Times"),axis.title.y=element_text(family="Times",vjust=1))


x2

#start ggplot
x1a<-ggmap(terr.map) +ylab("Latitude")+xlab("Longitude")+
  geom_point(data=wolfyht.data, aes(x=Longitude, y=Latitude,color=Pack))+ 
  theme(legend.position=c(0.875,0.89))+
  scale_colour_manual(values = c("red", "green"))
x3 <- x1a+theme(legend.text=element_text(family="Times"),
                legend.title=element_text(face="bold", family="Times"))+
  theme(axis.text.y = element_text(family="Times"),axis.text.x = element_text(family="Times"),text = element_text( family="Times"),axis.title.x=element_text( family="Times"),axis.title.y=element_text(family="Times",vjust=1))

x3

#write as .tiff file to working directory	 
tiff("wolfyht.tiff", res=600, compression = "lzw", height=5, width=5, units="in")
x2
dev.off()

#write as .tiff file to working directory	 
tiff("wolfyht2.tiff", res=600, compression = "lzw", height=5, width=5, units="in")
x3 
dev.off()

####################################################
# 3D plot for DEM (note that you can rotate it manuallly)
library(rasterVis)
plot3D(elevation2)

##################################################################################
# use open street maps so you can easily change projection of map

#install and load package OpenStreetMap 
install.packages("OpenSteetMap",dependencies=T)
library(OpenStreetMap)

#install and load package ggsn for add scale bar and north arrow 
# to ggplot2 maps
install.packages("ggsn",dependencies=T)
library(ggsn)



#change projection of deer_w to native mercator coords to use extent for plot
deer_w <- projectRaster(deer_w, crs="+init=epsg:4326")

# store extents for ggplot
minx <- extent(deer_w)[1]
maxx <- extent(deer_w)[2]
miny <- extent(deer_w)[3]
maxy <- extent(deer_w)[4]

#plot bing map in native mercator coords (note upper left, and lower right
# coordinates must be given in latlong)
map_bing <- openmap(c(maxy,minx),
                    c(miny,maxx),zoom=9,type="bing")

#change open map projection to utm datum WGS84
map_utm <- openproj(map_bing, projection="+proj=utm +zone=11 +units=km +datum=WGS84")

#change wolf data projection to utm datum WGS84
wolfyht <- spTransform(wolfyht, CRS("+proj=utm +zone=11 +units=km +datum=WGS84"))

# create data.frame for wolfyht
wolfyht.data <- data.frame(coordinates(wolfyht)[,1],coordinates(wolfyht)[,2],
                           wolfyht@data$Pack) 

# reset names for wolfyht.data
names(wolfyht.data) <- c("Easting", "Northing", "Pack")



# use autoplot to plot map in ggplot2
x1<- autoplot(map_utm) + theme_classic() +ylab("Northing (km)") +xlab("Easting (km)") +scale_y_continuous(limits = c(5635, 5760), expand = c(0, 0))+
  
  scale_x_continuous(limits = c(525, 650), expand = c(0, 0))+
  geom_point(data=wolfyht.data, aes(x=Easting, y=Northing,color=Pack))+ 
  theme(legend.position=c(0.8725,0.8925))+
  scale_colour_manual(values = c("red", "green"))
x4 <- x1+theme(legend.text=element_text(family="Times"),
               legend.title=element_text(face="bold", family="Times"))+
  theme(axis.text.y = element_text(family="Times"),axis.text.x = element_text(family="Times"),text = element_text( family="Times"),axis.title.x=element_text( family="Times"),axis.title.y=element_text(family="Times",vjust=1),
        plot.margin = unit(c(0,0.5,0.5,0.5), "cm"))

#write as .tiff file to working directory	 
tiff("wolfyhtOSM1.tiff", res=600, compression = "lzw", height=5, width=5, units="in")
x4
dev.off()



