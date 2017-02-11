######################################################################################################
# WILD 562 - R code for LAB 2
######################################################################################################
#R.Version()
#function to install and load required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#load or install these packages:
packages <- c("ks", "lattice", "plotrix", "adehabitatHR", "maptools", "foreign", "rgdal", "sp", "raster",
              "ggplot2","colorRamps","rgeos")

#run function to install packages
ipak(packages)

##################################################################################################################
#GETTING STARTED
############################################################################################################
#load shapefiles 

#set working directory for SPATIAL data
setwd("C:\\Users\\kjbark3r\\Documents\\GitHub\\HabitatModelling\\Lab2_HomeRange\\")

# read in shapefiles - note that I have already turned these into rasters and saved them to file below
elc_habitat<-shapefile("../SpatialData/elc_habitat.shp")
humanaccess<-shapefile("../SpatialData/humanacess.shp")
#mcp2<-shapefile("C:/Users/danea/OneDrive/Documents/Archive (1)/lab2/data/mcp2.shp")
#elevation<-raster("elevation_CopyRaster.tif")
#disthumaccess<-raster("DistFromHumanAccess.tif")
## but we will bring in the wolf shapefile here
wolfyht<-shapefile("../SpatialData/wolfyht.shp")


#############################################################################################################################
# base plot of wolf packs by color with legend

plot(wolfyht@data$EASTING,wolfyht@data$NORTHING, col=c("red","blue")[wolfyht@data$PackID],ylab="Northing",xlab="Easting")
legend(555000,5742500,unique(wolfyht@data$Pack),col=c("blue","red"),pch=1)

##############################################################################################################################
# ggplot of elk habitat based on DEER_W or MOOSE_W

# convert spatialpolygonsdataframe to regular dataframe for ggplot2
elc_habitat@data$id <- rownames(elc_habitat@data)
elc_habitatPoly <- fortify(elc_habitat, region = "id")
elc_habitatDF <- merge(elc_habitatPoly, elc_habitat@data, by = "id")

#construct ggplot2 plot
elk_plot<-ggplot(elc_habitatDF, aes(x = long, y = lat,group=group, fill = as.factor(MOOSE_W))) + 
  geom_polygon() + labs(x="Easting",y="Northing") + theme(axis.text.y = element_text(angle = 90, hjust=0.5))

#adjust fill colors of MOOSE_W  (note that I just selected some random colors, but made "7" as blue)
elk_plot2 <- elk_plot + scale_fill_manual(name="MOOSE_W",values=c("brown","green","yellow","red","aquamarine","sienna1",
                                                                  "darkblue"))
elk_plot2

#####################################################################################################################################
##################################################################################################################################
#
# OBJECTIVE 1
# Part 1a

# Create a mask raster to use as a template for converting shapefile data to rasters

#create an empty raster
mask.raster <- raster()

#set extent (note that I customized this extent so it covered both elc_habitat and humanacess)
extent(mask.raster) <- c(xmin=443680.6, xmax=650430.4, ymin=5618405, ymax=5789236) 	

#set the resolution to 30 m 
res(mask.raster)<-30

#match projection to elc_habitat shapefile
projection(mask.raster)<- "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# or
#projection(mask.raster) <- "+init=epsg:4269" #or whatever

#set all values of mask.raster to zero
mask.raster[]<-0

# create rasters for DEER_W, MOOSE_W, ELK_W, SHEEP_W, GOAT_W, WOLF_W
## note for today's Lab 2 we have masked out these steps as they take a LONG time. 
#deer_w<-rasterize(elc_habitat, mask.raster, field="DEER_W")
#moose_w<-rasterize(elc_habitat, mask.raster, field="MOOSE_W")
#elk_w<-rasterize(elc_habitat, mask.raster, field="ELK_W")
#sheep_w<-rasterize(elc_habitat, mask.raster, field="SHEEP_W")
#goat_w<-rasterize(elc_habitat, mask.raster, field="GOAT_W")
#wolf_w<-rasterize(elc_habitat, mask.raster, field="WOLF_W")

#plot result
#plot(wolf_w)

#resample elevation and humanaccess to match mask.raster
#elevation2<-resample(elevation, mask.raster, method="bilinear")
#disthumaccess2<-resample(disthumaccess, mask.raster, method="bilinear")

#write raster layers to file
#writeRaster(deer_w, "deer_w2.tiff", "GTiff")
#writeRaster(moose_w, "moose_w2.tiff", "GTiff")
#writeRaster(elk_w, "elk_w2.tiff", "GTiff")
#writeRaster(sheep_w, "sheep_w2.tiff", "GTiff")
#writeRaster(goat_w, "goat_w2.tiff", "GTiff")
#writeRaster(wolf_w, "wolf_w2.tiff", "GTiff")
#writeRaster(elevation2, "Elevation2.tiff", "GTiff")
#writeRaster(disthumaccess2, "DistFromHumanAccess2.tiff", "GTiff")

#re-read in new rasters
deer_w<-raster("../SpatialData/deer_w2.tif")
moose_w<-raster("../SpatialData/moose_w2.tif")
elk_w<-raster("../SpatialData/elk_w2.tif")
sheep_w<-raster("../SpatialData/sheep_w2.tif")
goat_w<-raster("../SpatialData/goat_w2.tif")
wolf_w<-raster("../SpatialData/wolf_w2.tif")#
elevation2<-raster("../SpatialData/Elevation2.tif") #resampled
disthumanaccess2<-raster("../SpatialData/DistFromHumanAccess2.tif") #resampled

plot(deer_w)
plot(elevation2)

########################################################################################################################
# Part 1b - creation of Distance to Human Access Layer _ I already created this raster layer
## Again, this took days in R on a desktop. So this might be an example where using ArcGIS might save time. 
#first create an empty raster
#dist.raster <- raster()

#set extent 
#extent(dist.raster) <- extent(humanaccess)

#set the resolution to 30 m (Note that this takes a very long time with a 30 m resolution-even on my machine)
#res(dist.raster)<-30

#match projection to humanaccess shapefile
#projection(dist.raster)<- "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

#set all values of dist.raster to zero
#dist.raster[]<-0

#now rasterize the humanaccess layer and set human features (e.g., roads, trails) to 1
#human.raster<-rasterize(humanaccess, dist.raster, 1)

#calculate distance to human access- NOTE : DO NOT RUN THIS IT TAKES FOREVER; I ENDED UP
#DOING THIS CALCULATION JUST FOR THE POINTS
#accessdist <-system.time(distance(human.raster))

#write raster to file
#writeRaster(accessdist, "DistFromHumanAccess.tiff", "GTiff")


# ADVANCED QUESTION##########################################################################################

#first reclassify labels on humanaccess.shp file so they are correct (note: need to bring in humanaccess.shp above)
levels(as.factor(humanaccess$SUM_CLASS))
#[1] "0"         "High"      "HIGH"      "Low"       "LOW"      
#[6] "MEDIUM"    "Moderate"  "Nil"       "NIL"       "VERY HIGH"
#convert humanaccess$SUM_CLASS to a factor
humanaccess$SUM_CLASS<-as.factor(humanaccess$SUM_CLASS)

levels(humanaccess$SUM_CLASS)[1]<-"NIL"
levels(humanaccess$SUM_CLASS)[2]<-"HIGH"
levels(humanaccess$SUM_CLASS)[3]<-"LOW"
levels(humanaccess$SUM_CLASS)[5]<-"MODERATE"
levels(humanaccess$SUM_CLASS)[6]<-"NIL"

#create indicator variable for high or not high human access
highaccess<-humanaccess[humanaccess@data$SUM_CLASS=="HIGH" | humanaccess@data$SUM_CLASS=="VERY HIGH", ]

plot(humanaccess)
plot(highaccess, col="red", add=T)
## now you would go through and rasterize this following the steps above to make this a raster file. 

#FOLLOW STEPS AT BOTTOM TO EXTRACT DISTANCES FOR POINTS (GPS LOCATIONS AND RANDOMLY SAMPLED)

########################################################################################################################################
########################################################################################################################################

# OBJECTIVE 2 - Home Range Analysis - note I've done these here for the individual level I added the pack-level
# analysis below

########################################################
# calculate 99% minimum convex polygon for individual wolves in Red Deer wolf pack

#first convert the spatialpointsdataframe to spatial points object
rd.data<-wolfyht[wolfyht@data$Pack=="Red Deer",]
x<-rd.data@data$EASTING
y<-rd.data@data$NORTHING
xy<-cbind(x,y)

rd <- data.frame(as.character(rd.data@data$NAME))
coordinates(rd) <- xy
proj4string(rd) <-  CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# Fit 99% mpc
cp.rd <- mcp(rd, percent=99)
#note error that one animal does not have at least 5 locations
table(rd@data$NAME)
#42 60 69 70 81 82 84 
#43 25  4 15  3  2  1  #looks like 4 of the wolves do not have enough locations

#remove these individuals with too few of locations
names(rd)<-"NAME"
rd<-rd[rd@data$NAME!="69" & rd@data$NAME!="81" & rd@data$NAME!="82" & rd@data$NAME!="84",]
#remove unused NAME levels
rd@data$NAME<-factor(rd@data$NAME)
table(rd@data$NAME)

# Fit 99% mpc
cp.rd <- mcp(rd, percent=99)

plot(rd, col="black")
plot(cp.rd[cp.rd@data$id=="42",], col="blue", add=TRUE)
plot(cp.rd[cp.rd@data$id=="70",], col="green", add=TRUE)
plot(cp.rd[cp.rd@data$id=="60",], col="red", add=TRUE)
plot(rd, col="black", add=TRUE)

#check area for each Red Deer wolf pack
as.data.frame(cp.rd)

#calculate area for different percents of MCP
hrs <- mcp.area(rd, percent=seq(50, 100, by=5))
hrs
# rarefaction curves - 
# want hr estimate to be unaffected by sample size to reduce bias
# but this is a small sample size, wah wah

#calculate 99% KDE for Red Deer wolf pack
red.deerUD <- kernelUD(rd, grid=30, extent=0.5, same4all=TRUE) # reference grid
image(red.deerUD)

#get polygons for home ranges
homerangeRD <- getverticeshr(red.deerUD)
as.data.frame(homerangeRD)
class(homerangeRD)
plot(homerangeRD, col=2:4)


#Estimate UD in raster mode
red.deerud <- getvolumeUD(red.deerUD) 
red.deerud

## Set up graphical parameters for the output of getvolumeUD 
par(mar=c(0,0,2,0)) #set margin
image(red.deerud[[1]]) #for first wolf only
title("Output of getvolumeUD") 
xyzv <- as.image.SpatialGridDataFrame(red.deerud[[1]]) 
contour(xyzv, add=TRUE)

## store the volume under the UD (as computed by getvolumeUD) 
## of the first animal in fud 
fud <- red.deerud[[1]] #for first wolf only
## store the value of the volume under UD in a vector hr95 
hr95 <- as.data.frame(fud)[,1] 
## if hr95 is <= 95 then the pixel belongs to the home range
## (takes the value 1, 0 otherwise)
hr95 <- as.numeric(hr95 <= 95) 
## Converts into a data frame 
hr95 <- data.frame(hr95) 
## Converts to a SpatialPixelsDataFrame 
coordinates(hr95) <- coordinates(red.deerud[[1]])
gridded(hr95) <- TRUE 
## display the results 
image(hr95)

#############################################################
# calculate 99% minimum convex polygon for individual wolves in Bow valley wolf pack

#first convert the spatialpointsdataframe to spatial points object
bv.data<-wolfyht[wolfyht@data$Pack=="Bow valley",]
x<-bv.data@data$EASTING
y<-bv.data@data$NORTHING
xy<-cbind(x,y)

bv <- data.frame(as.character(bv.data@data$NAME))
coordinates(bv) <- xy
proj4string(bv) <-  CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# Fit 99% mpc
cp.bow <- mcp(bv, percent=99)
plot(bv, col="black")
plot(cp.bow[cp.bow@data$id=="63",], col="blue",add=TRUE)
plot(cp.bow[cp.bow@data$id=="87",], col="red",add=TRUE,)
plot(cp.bow[cp.bow@data$id=="44",], col="green",add=TRUE)
plot(bv, col="black", add=TRUE)

#check area for each Red Deer wolf pack
as.data.frame(cp.bow)

#calculate area for different percents of MPC
hrs <- mcp.area(bv, percent=seq(50, 100, by=5))
hrs

#calculate 99% KDE for Red Deer wolf pack
bow.valleyUD <- kernelUD(bv, grid=30, extent=0.1, same4all=TRUE) # reference grid
image(bow.valleyUD)

#get polygons for home ranges
homerangeBV <- getverticeshr(bow.valleyUD)
as.data.frame(homerangeBV)
class(homerangeBV)
plot(homerangeBV, col=2:4)


#Estimate UD in raster mode
bow.valleyud <- getvolumeUD(bow.valleyUD) 
bow.valleyud

## Set up graphical parameters for the output of getvolumeUD 
par(mar=c(0,0,2,0)) #set margin
image(bow.valleyud[[1]])
title("Bow Valley Pack UD") 
xyzv <- as.image.SpatialGridDataFrame(bow.valleyud[[1]]) 
contour(xyzv, add=TRUE)

## store the volume under the UD (as computed by getvolumeUD) 
## of the first animal in fud 
fud <- bow.valleyud[[1]]
## store the value of the volume under 95% UD in a vector hr95 
hr95 <- as.data.frame(fud)[,1] 
## if hr95 is <= 95 then the pixel belongs to the home range
## (takes the value 1, 0 otherwise)
hr95 <- as.numeric(hr95 <= 95) 
## Converts into a data frame 
hr95 <- data.frame(hr95) 
## Converts to a SpatialPixelsDataFrame 
coordinates(hr95) <- coordinates(bow.valleyud[[1]])
gridded(hr95) <- TRUE 
## display the results 
image(hr95)

########################################################################################################################################
########################################################################################################################################
# calculate 99% minimum convex polygon for both wolf packs


#first convert the spatialpointsdataframe to spatial points object
x<-wolfyht@data$EASTING
y<-wolfyht@data$NORTHING
xy<-cbind(x,y)

all <- data.frame(as.character(wolfyht@data$Pack))
coordinates(all) <- xy
proj4string(all) <-  CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# Fit 99% mpc
cp.all <- mcp(all, percent=99)

plot(wolfyht, col="black")
plot(cp.all[cp.all@data$id=="Bow valley",], col="blue",add=TRUE)
plot(cp.all[cp.all@data$id=="Red Deer",], col="green",add=TRUE)
plot(wolfyht, col="black", add=TRUE)

#check area for each Red Deer wolf pack
as.data.frame(cp.all)

#calculate area for different percents of MPC
hrs <- mcp.area(all, percent=seq(50, 100, by=5))
hrs

#calculate 99% KDE for both wolf packs
allUD <- kernelUD(all, grid=30, extent=0.5, same4all=TRUE) # reference grid
image(allUD)

#get polygons for home ranges
homerangeALL <- getverticeshr(allUD)
as.data.frame(homerangeALL)
class(homerangeALL)
plot(homerangeALL, col=2:3)


#Estimate UD in raster mode
allud <- getvolumeUD(allUD) 
allud

## Set up graphical parameters for the output of getvolumeUD 
par(mar=c(0,0,2,0)) #set margin
image(allud[[1]]) #for first wolf only
title("Output of getvolumeUD") 
xyzv <- as.image.SpatialGridDataFrame(allud[[1]]) 
contour(xyzv, add=TRUE)

## store the volume under the UD (as computed by getvolumeUD) 
## of the first animal in fud 
fud <- allud[[1]] #for first wolf pack only
## store the value of the volume under UD in a vector hr95 
hr95 <- as.data.frame(fud)[,1] 
## if hr95 is <= 95 then the pixel belongs to the home range
## (takes the value 1, 0 otherwise)
hr95 <- as.numeric(hr95 <= 95) 
## Converts into a data frame 
hr95 <- data.frame(hr95) 
## Converts to a SpatialPixelsDataFrame 
coordinates(hr95) <- coordinates(allud[[1]])
gridded(hr95) <- TRUE 
## display the results 
image(hr95)

########################################################################################################################################
########################################################################################################################################

# OBJECTIVE 3 - Learn how to sample availabiliity within home ranges - also see GeospatialModelingEnvironment

#subset polygons by wolf pack
red.deerPOLY<-homerangeALL[homerangeALL@data$id=="Red Deer",]
bow.valleyPOLY<-homerangeALL[homerangeALL@data$id=="Bow valley",]

#generate 1000 points from Red Deer wolf pack KDE polygon
rd.avail<-spsample(red.deerPOLY, 1000, "random")
plot(rd.avail); plot(red.deerPOLY, add = TRUE)

#check distance between randomly sampled points is less than 30 m - this may become important later to avoid sampling the same pixels. 
# bc sometimes want to ensure points are wi certain dist to avoid contamination
out.rd<- spDists(rd.avail)
out2.rd<-ifelse(out.rd==0, NA, out.rd)
which(out2.rd<=30)

#generate 1000 points from Bow valley wolf pack KDE polygon
bv.avail<-spsample(bow.valleyPOLY, 1000, "random")
plot(bv.avail)
plot(bow.valleyPOLY, add = TRUE)

#check distance between randomly sampled points is less than 30 m
out.bv<- spDists(bv.avail) #calculate distance matrix between all points
out2.bv<-ifelse(out.bv==0, NA, out.bv) # set 0's to NA (0 is the distance between each point and itself)
which(out2.bv<=30) #get the indexing positions for points with distance <=30

# lets plot them all together, used and home-range level availability
plot(wolfyht@data$EASTING,wolfyht@data$NORTHING, col=c("red","blue")[wolfyht@data$PackID],ylab="Northing",xlab="Easting")
legend(555000,5742500,unique(wolfyht@data$Pack),col=c("blue","red"),pch=1)
plot(bv.avail, col = "green", add=TRUE)
plot(rd.avail, add=TRUE)
# shows what we know they used vs what was actually "available" to them
  # we do not know they didn't use locations where we didn't observe them


######################################################################################################################################
######################################################################################################################################

# OBJECTIVE 4 - Extracting GIS covariates for point location data

# note the raster layers we created earlier
# deer_w, moose_w, elk_w, sheep_w, goat_w, wolf_w, elevation2, disthumanaccess2

#########################################################################################################
#calculate distance between human access and points for Bow Valley
#dist.outBV<-gDistance(bv.data, humanaccess, byid=TRUE)

#note that the above function calculates the distance between each point and all road segments,
#but we want the minimum value

#dist.outBV2<-data.frame(apply(dist.outBV,2,min))
#names(dist.outBV2)<-"Dist.to.access"

#######################################################################################################

#calculate distance between human access and points for Red Deer
#dist.outRD<-gDistance(rd.data, humanaccess, byid=TRUE)

#note that the above function calculates the distance between each point and all road segments,
#but we want the minimum value

#dist.outRD2<-data.frame(apply(dist.outRD,2,min))
#names(dist.outRD2)<-"Dist.to.access"

############################################################################################################
#calculate distances to human access for randomly sampled points for Bow valley

#calculate distance between human access and points for Red Deer
#dist.availBV<-gDistance(bv.avail, humanaccess, byid=TRUE)

#dist.availBV2<-data.frame(apply(dist.availBV,2,min))
#names(dist.availBV2)<-"Dist.to.access"

################################################################################################################

#calculate distances to human access for randomly sampled points for Bow valley

#calculate distance between human access and points for Red Deer
#dist.availRD<-gDistance(rd.avail, humanaccess, byid=TRUE)

#dist.availRD2<-data.frame(apply(dist.availRD,2,min))
#names(dist.availRD2)<-"Dist.to.access"

#############################################################################################################
#stack raster layers (i.e., create raster stack for sampling; must have same extent and resolution)
all_rasters<-stack(deer_w, moose_w, elk_w, sheep_w, goat_w, wolf_w,elevation2, disthumanaccess2)

#Extract covariate values for Red Deer wolf data  
cov.outRD<-extract(all_rasters, rd.data)
head(cov.outRD)

#Extract covariate values for available points
cov.availRD<-extract(all_rasters, rd.avail)

#Extract covariate values for Bow valley wolf data  
cov.outBV<-extract(all_rasters, bv.data)

#Extract covariate values for available points
cov.availBV<-extract(all_rasters, bv.avail)

#NOW column bind (i.e., cbind) distance values to other extracted covariates
#cov.outRD.2<-cbind(cov.outRD, dist.outRD2)
#cov.outBV.2<-cbind(cov.outBV, dist.outBV2)
#cov.availRD.2<-cbind(cov.availRD, dist.availRD2)
#cov.availBV.2<-cbind(cov.availBV, dist.availBV2)

#FINALLY SAVE R WORKSPACE! (note add date to file name)
save.image("wolf_rsf01_10_17.Rdata")

#######################################################################################################################################

##5) Objective Five – Exploratory analyses of wolf habitat use with R

## Today we will focus on some exploratory analyses of JUST use of habitat covariates by the 2 wolf packs. 
## the first step is to merge the cov.outRD and cov.outBV dataframes with a new field for pack. 
## But before that we need to convert the matrix (a list) to a data frame, and add a new column for pack name

rdused <- as.data.frame(cov.outRD)
str(rdused)
rdused$pack <- c("Red Deer") #create identifier
str(rdused)

## repeat for Bow Valley pack
bvused <- as.data.frame(cov.outBV)
str(bvused)
bvused$pack <- c("Bow Valley")
str(bvused)

wolfused <- merge(rdused, bvused, all.x= TRUE, all.y = TRUE)
str(wolfused)
head(wolfused)

## and for next week, lets add a new column for a 1=used 0 = avail
wolfused$used <- 1

## calculate some quick summaries
summary(wolfused)

# Clean up missing data in the distance to access variable. 
# missing data are a problem, we will replace NA's with 0's for now. Discuss in class. 
wolfyht$distacc[wolfyht$distacc<0]=0 

# calculate aummary stats for indiv cov relationships
aggregate(Elevation2 ~ pack, data=wolfused, FUN=mean)
aggregate(DistFromHumanAccess2 ~ pack, data=wolfused, FUN=mean)
aggregate(.~pack, data=wolfused[c("pack", "deer_w2", "elk_w2", "moose_w2", "sheep_w2", "goat_w2")], mean)
# those animal data are from prey spp HSI models
# bow valley uses habitat that's less suitable for sheep and goats

sapply(wolfused, mean, na.rm=TRUE)
## or using the psych package
install.packages("psych")
library("psych")
describe.by(wolfused, wolfused$pack)


## Graphical data visualization
par(mfrow = c(2,3))
hist(wolfused$deer_w2)
hist(wolfused$elk_w2)
hist(wolfused$moose_w2)
hist(wolfused$sheep_w2)
hist(wolfused$goat_w2)
par(mfrow = c(2,1))
hist(wolfused$Elevation2)
hist(wolfused$DistFromHumanAccess2)
# most wolf telem pts are at lower elevs and within 1m of human activity

### Data exploration by wolf pack
par(mfrow = c(1,1))
# Plot Bow Valley
hist(wolfused$Elevation2[wolfused$pack=="Bow Valley"],breaks=50, xlim = c(1400,2250), probability = TRUE, main="Wolf Habitat Selection", xlab="Elevation") 

#Plot Red Deer
hist(wolfused$Elevation2[wolfused$pack=="Red Deer"],breaks=50, col="darkgray",probability =TRUE, add=TRUE)
# Add legend
legend("topright", c("Bow Valley", "Red Deer"), fill = c("white","darkgray"),border = "black")
# frequency by elevation (red deer uses higher elevs)

## repeat on your own for all other covariates
## note that multihist's are from the plotrix package
par(mfrow = c(2,1))
multhist(list(wolfused$Elevation2[wolfused$pack=="Bow Valley"],wolfused$Elevation2[wolfused$pack=="Red Deer"]), freq = TRUE, main = "Elevation")
# I chose to put a legend in the upper right hand graph. 
# That's what the additional arguments in the line below specify.
multhist(list(wolfused$DistFromHumanAccess2[wolfused$pack=="Bow Valley"],wolfused$DistFromHumanAccess2[wolfused$pack=="Red Deer"]), freq = TRUE, main = "Cover Distance", legend.text = c("Bow Valley", "Red Deer"), args.legend = list(bty = "n"))

## now the prey figures

par(mfrow = c(2,3))
multhist(list(wolfused$elk_w2[wolfused$pack=="Bow Valley"],wolfused$elk_w2[wolfused$pack=="Red Deer"]), freq = TRUE, main = "Elk", legend.text = c("Bow Valley", "Red Deer"), args.legend = list(bty = "n"))
multhist(list(wolfused$deer_w[wolfused$pack=="Bow Valley"],wolfused$deer_w[wolfused$pack=="Red Deer"]), freq = TRUE, main = "Deer", legend.text = c("Bow Valley", "Red Deer"), args.legend = list(bty = "n"))
multhist(list(wolfused$moose_w2[wolfused$pack=="Bow Valley"],wolfused$moose_w2[wolfused$pack=="Red Deer"]), freq = TRUE, main = "Moose", legend.text = c("Bow Valley", "Red Deer"), args.legend = list(bty = "n"))
multhist(list(wolfused$sheep_w2[wolfused$pack=="Bow Valley"],wolfused$sheep_w2[wolfused$pack=="Red Deer"]), freq = TRUE, main = "Sheep", legend.text = c("Bow Valley", "Red Deer"), args.legend = list(bty = "n"))
multhist(list(wolfused$goat_w2[wolfused$pack=="Bow Valley"],wolfused$goat_w2[wolfused$pack=="Red Deer"]), freq = TRUE, main = "Goat", legend.text = c("Bow Valley", "Red Deer"), args.legend = list(bty = "n"))

## Boxplots (from the lattice package)
bwplot(elk_w2 + deer_w2+moose_w2+ sheep_w2+goat_w2~pack, auto.key=TRUE,allow.multiple = TRUE,data=wolfused, outer=TRUE)
bwplot(DistFromHumanAccess2 + Elevation2~pack, auto.key=TRUE,allow.multiple = TRUE,data=wolfused, outer=TRUE)

