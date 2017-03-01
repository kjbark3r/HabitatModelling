## WILDLIFE HABITAT MODELING LAB feb2017 ##
##  LAB 4 - Feb.2017 - categorical rsc selxn  ##

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

#clan workspace
rm(packages, ipak)

#~# rspatial.org - sweet online resource


##### 1.0 Preliminaries: Data Management ####
#~# SKIP THIS 

## learning about spatial data analysis in R
## http://rspatial.org/spatial/index.html

####### 1.1 Importing Landcover map from an ArcGIS GRID folder with structure Landcover/
setwd("C:\\Users\\kjbark3r\\Documents\\GitHub\\HabitatModelling\\Lab4_CategoricalRscSlxn\\Landcover\\")
landcover<-raster("landcover")
image(landcover, col=rainbow(16))
#~# IMPORTANT: these are not vegetation types
str(landcover)
landcover@data@attributes
#~# rmt sensing based measures don't see veg cmtys; they see image (spectrums of light)
  ## categories we classify in diff ways (supervised and unsupervised) to ID diff types
  ## of reflectant light
  ## law of classification: 1category => perfect landcover classn success
    ## more categories => decreased classification success (we have 16)
  ## need to go through variable redxn in landcover classn for animals
    ## eg collapse the 3 conifers
  ## count gives # 30m pixels per hab type

landcover@crs@projargs
extent(landcover)
landcover
setwd("C:\\Users\\kjbark3r\\Documents\\GitHub\\HabitatModelling\\Lab4_CategoricalRscSlxn\\New\\") 
# now lets write this as a TIFF format (easier to use)
#~# now it's a geotiff
writeRaster(landcover, "landcover16.tif", "GTiff", overwrite = TRUE)
landcover16 <- raster("landcover16.tif") # bringing it back in
str(landcover16)
head(landcover16@data@attributes)
landcover16@data@values <- getValues(landcover16)
landcover16@data@attributes
#~# oh no, we've lost the legend! (no names for landcover types)

## lets make a plot with the wolfyht points 
wolfyht<-shapefile("wolfyht.shp")
plot(landcover16, col=rainbow(16))
plot(wolfyht, add=TRUE, type="p", color = "gray25", pch=19, cex = 0.75)

extent(landcover16)  
extent(wolfyht) #~#, shit, no match

#~# set new raster with new extent that we want them all to be
## recall that we then have to reset the extent to be the same before we can create a Raster Stack for extracting this landcover covariate again
mask.raster <- raster()
extent(mask.raster) <- c(xmin=443680.6, xmax=650430.4, ymin=5618405, ymax=5789236) 	
res(mask.raster) = 30
#match projection to elc_habitat shapefile
projection(mask.raster)<- "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#set all values of mask.raster to zero
mask.raster[]<-0


## lets make a second plot zooming into a specific area of the Red Deer pack
yht.raster <- raster()
extent(yht.raster) <- c(xmin=570000, xmax=600000, ymin=5720000, ymax=5740000) #~# defined visually/manually
plot(landcover16, col=rainbow(16), ext=yht.raster)
legend("topleft", legend = c("Open Conifer", "Mod. Conifer", "Closed Conifer", "Deciduous", "Mixed", "Regen", "Herb", "Shrub", "Water", "Rock-Ice", "Cloud", "Burn-Forest", "Burn-Grassland", "Burn-Shrub", "Alpine Herb", "Alpine Shrub"), fill = rainbow(16), cex=0.75)
plot(wolfyht, add=TRUE, type="p", color = "gray25", pch=19)

## lets make a zoomed in model of the elk prey layer
bv.raster <- raster()
extent(bv.raster) <- c(xmin=560000, xmax=600000, ymin=5660000, ymax=5690000) 
elk_w<-raster("../../SpatialData/elk_w2.tif")
plot(elk_w, col=heat.colors(7), ext=bv.raster)
legend("topleft", legend = c("1", "2", "3", "4", "5", "6", "7"), fill = rainbow(16), cex=0.75)
plot(wolfyht, add=TRUE, type="p", color = "gray25", pch=19)

#~# see bivand book - manually set pixel size to speed up plotting
  ## and keep underlying data behind it

## now review the different extents between the elk_w and landcover16 models
extent(elk_w)
#res(elk_w)
extent(landcover16)
#res(landcover16)

## recall that we then have to reset the extent to be the same before we can create a Raster Stack for extracting this landcover covariate again
## we have done this above with the mask.raster file
#landcover2<-extend(landcover16, mask.raster)  Note that extending did not perfectly match the extent so we resampled using nearest neighbor
landcover2<-resample(landcover16, mask.raster, method="ngb") 
## note here we are resampling to nearest neighbor to preseve the integer value - if you do this using the bilinear resampling you get double format numbers (i.e., not integer values)
extent(landcover2)
#~# bilinear method for real #s like landcovs or slope
  ## numbers here are categories, not real #s
  ## nearest neighbor method is for categories and integers
writeRaster(landcover2, file = "landcover2.tif", format = "GTiff", overwrite = TRUE)
###

######### 1.2 Bringing in the Distance to High Human Access layer ####
##- see Lab 2 section 1b. 
disthhu<-raster("DistFromHighHumanAccess2.tif")
plot(disthhu)
extent(disthhu)
disthhu2<-resample(disthhu, mask.raster, method="bilinear")
extent(disthhu2)
writeRaster(disthhu2, file = "disthhu2.tif", format = "GTiff", overwrite = TRUE)
###

######### 1.3. Re-extracting both spatial data frame ####
## lets bring in all the data from our original lab 1 data - where ever you still have these data

# setwd
#laptop#setwd("C:\\Users\\kjbark3r\\Documents\\GitHub\\HabitatModelling\\Lab4_CategoricalRscSlxn\\New\\")
setwd("C:\\Users\\kristin.barker\\Documents\\GitHub\\HabitatModelling\\Lab4_CategoricalRscSlxn\\New\\") 

# read in wolf data
wolfyht<-shapefile("wolfyht.shp")
# read in and stack rasters
deer_w<-raster("../../SpatialData/deer_w2.tif")
moose_w<-raster("../../SpatialData/moose_w2.tif")
elk_w<-raster("../../SpatialData/elk_w2.tif") # already brought in above
sheep_w<-raster("../../SpatialData/sheep_w2.tif")
goat_w<-raster("../../SpatialData/goat_w2.tif")
wolf_w<-raster("../../SpatialData/wolf_w2.tif")
elevation2<-raster("../../SpatialData/Elevation2.tif") #resampled
disthumanaccess2<-raster("../../SpatialData/DistFromHumanAccess2.tif") #resampled
disthhu2 <-raster("../New/disthhu2.tif") ## note created in 0.2 above and extent reset. 
landcover2 <- raster("../New/landcover2.tif")
all_rasters<-stack(deer_w, moose_w, elk_w, sheep_w, goat_w, wolf_w,elevation2, disthumanaccess2, disthhu2, landcover2)
# read in extracted values and skip remainder of this section
wolfkde <- read.csv("wolfkde.csv", sep=",")

#Extract covariate values for Red Deer wolf data  
## note rd.data is a spatialDataPointsFrame needed from Lab 2
## We are going to use SOURCE as a command to re-run 
## a trimmed down set of commands from Lab 2 
##to re-extract the rd.data, rd.avail, bv.data, and bv.avail data
#?source
## note we are going to have to make sure our working directory 
## in the Lab2NeededforLab5.R file
## is correctly pointed to where we have the wolfyht.shp file (in Lab 4/new)

source("Lab2NeededforLab5.R", verbose = FALSE)
wolfyht<-shapefile("wolfyht.shp")
rd.data<-wolfyht[wolfyht@data$Pack=="Red Deer",]
cov.outRD<-extract(all_rasters, rd.data) ## note rd.data is a spatialDataPointsFrame needed from Lab 2

#Extract covariate values for available points
cov.availRD<-extract(all_rasters, rd.avail)

#Extract covariate values for Bow valley wolf data
bv.data<-wolfyht[wolfyht@data$Pack=="Bow valley",]
cov.outBV<-extract(all_rasters, bv.data)

#Extract covariate values for available points
cov.availBV<-extract(all_rasters, bv.avail)

###

######## 1.3  Attaching spatial X, Y data to Used and Available data (visualization, etc) ####
##it is sometimes useful to have X, Y locations associated with the different spatial datasets
#attach coordinates for red deer pack used points
## here we go back to Lab 2 Objective 4 and after we have creted the cov.outRD spatialPointsDataFrame object, we create a new object with X, Y
head(rd.data@coords)

## Red Deer Pack
cov.outRD2<-cbind(cov.outRD, coordinates(rd.data)[,1],coordinates(rd.data)[,2])
#set column names for coordinates
colnames(cov.outRD2)[11:12] <- c("EASTING","NORTHING")
#attach coordinates for red deer pack available points
cov.availRD2<-cbind(cov.availRD, coordinates(rd.avail)[,1],coordinates(rd.avail)[,2])
#set column names for coordinates
colnames(cov.availRD2)[11:12] <- c("EASTING","NORTHING")
head(cov.outRD2)

#attach coorvinates for bow valley pack used points
cov.outBV2<-cbind(cov.outBV, coordinates(bv.data)[,1],coordinates(bv.data)[,2])
#set column names for coordinates
colnames(cov.outBV2)[11:12] <- c("EASTING","NORTHING")
#attach coobvinates for bow valley pack available points
cov.availBV2<-cbind(cov.availBV, coordinates(bv.avail)[,1],coordinates(bv.avail)[,2])
#set column names for coordinates
colnames(cov.availBV2)[11:12] <- c("EASTING","NORTHING")
head(cov.outBV2)

## now lets combine the USED datasets again
## now lets combine them again
rdused <- as.data.frame(cov.outRD2)
rdused$pack <- c("Red Deer")
str(rdused)

## repeat for Bow Valley pack
bvused <- as.data.frame(cov.outBV2)
bvused$pack <- c("Bow Valley")
str(bvused)

## merge the two USED samples together
wolfused <- rbind(rdused, bvused)
## and for next week, lets add a new column for a 1=used 0 = avail
wolfused$used <- 1
head(wolfused)

## now lets combine the AVAIL datasets again 
rdavail <- as.data.frame(cov.availRD2)
rdavail$pack <- c("Red Deer")
str(rdavail)

## repeat for Bow Valley pack
bvavail <- as.data.frame(cov.availBV2)
bvavail$pack <- c("Bow Valley")
str(bvavail)

## merge the two availability samples together
wolfavail <- rbind(rdavail, bvavail)
## and for next week, lets add a new column for a 1=used 0 = avail
wolfavail$used <- 0
head(wolfavail)

## and Merge the wolf used and avail samples together for the KDE availability estimator
wolfkde <- rbind(wolfused, wolfavail)
str(wolfkde)
table(wolfkde$used, wolfkde$pack)
wolfkde$usedFactor <- factor(wolfkde$used, labels=c('0','1'))
write.table(wolfkde, file = "wolfkde.csv", row.names=FALSE, na="", col.names=TRUE, sep=",")
wolfkde <- read.csv("wolfkde.csv", sep=",")

## Now we can make a single nice plot of the X and Y locations by Used by Pack
#ggplot(wolfkde, aes(x=EASTING, y = NORTHING, colour=used)) + geom_point() + facet_grid(pack ~ ., scales="free")
#~#~# NO DON'T DO IT!

###
###
### Objective 2.0  Univariate Model-fitting commands from lab 3 - extracting covariate tables from multiple models ####


library(dplyr)

#setwd("C:\\Users\\kjbark3r\\Documents\\GitHub\\HabitatModelling\\Lab4_CategoricalRscSlxn\\Landcover\\")
setwd("C:\\Users\\kristin.barker\\Documents\\GitHub\\HabitatModelling\\Lab4_CategoricalRscSlxn\\New\\")
wolfkde <- read.csv("wolfkde.csv", sep=",") %>%
  rename(DistFromHighHumanAccess2 = disthhu2)

### First for all packs
elev <- glm(used ~ Elevation2, family=binomial(logit), data=wolfkde)
distacc <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolfkde)
disthha <- glm(used ~ DistFromHighHumanAccess2, family=binomial(logit), data=wolfkde)
sheep <- glm(used ~ sheep_w2, family=binomial(logit), data=wolfkde)
goat <- glm(used ~ goat_w2, family=binomial(logit), data=wolfkde)
elk <- glm(used ~ elk_w2, family=binomial(logit), data=wolfkde)
moose <- glm(used ~ moose_w2, family=binomial(logit), data=wolfkde)
deer <- glm(used ~ deer_w2, family=binomial(logit), data=wolfkde)

# Now for Bow Valley pack
bvelev <- glm(used ~ Elevation2, family=binomial(logit), data=wolfkde,  subset=pack=="Bow Valley")
bvdisthha <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolfkde, subset=pack=="Bow Valley")
bvdistacc <- glm(used ~ DistFromHighHumanAccess2, family=binomial(logit), data=wolfkde,subset=pack=="Bow Valley")
bvsheep <- glm(used ~ sheep_w2, family=binomial(logit), data=wolfkde, subset=pack=="Bow Valley")
bvgoat <- glm(used ~ goat_w2, family=binomial(logit), data=wolfkde, subset=pack=="Bow Valley")
bvelk <- glm(used ~ elk_w2, family=binomial(logit), data=wolfkde, subset=pack=="Bow Valley")
bvmoose <- glm(used ~ moose_w2, family=binomial(logit), data=wolfkde,subset=pack=="Bow Valley")
bvdeer <- glm(used ~ deer_w2, family=binomial(logit), data=wolfkde, subset=pack=="Bow Valley")

# Now for Red Deer pack
rdelev <- glm(used ~ Elevation2, family=binomial(logit), data=wolfkde, subset=pack=="Red Deer")
rddistacc <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolfkde,subset=pack=="Red Deer")
rddisthha <- glm(used ~ DistFromHighHumanAccess2, family=binomial(logit), data=wolfkde, subset=pack=="Red Deer")
rdsheep <- glm(used ~ sheep_w2, family=binomial(logit), data=wolfkde, subset=pack=="Red Deer")
rdgoat <- glm(used ~ goat_w2, family=binomial(logit), data=wolfkde, subset=pack=="Red Deer")
rdelk <- glm(used ~ elk_w2, family=binomial(logit), data=wolfkde, subset=pack=="Red Deer")
rdmoose <- glm(used ~ moose_w2, family=binomial(logit), data=wolfkde, subset=pack=="Red Deer")
rddeer <- glm(used ~ deer_w2, family=binomial(logit), data=wolfkde,  subset=pack=="Red Deer")

# creating tables of B, SE
# First grab all of the estimates and standard errors
models = rbind(summary(elev)$coefficients[,1:2], summary(disthha)$coefficients[,1:2], summary(distacc)$coefficients[,1:2], summary(sheep)$coefficients[,1:2], summary(goat)$coefficients[,1:2], summary(elk)$coefficients[,1:2], summary(moose)$coefficients[,1:2], summary(deer)$coefficients[,1:2])
# Name your models
modelnames = c("elev","disthha", "distacc", "sheep", "goat", "elk", "moose", "deer")
# Now put all of your estimates in a pretty table with names that you'll remember!
estimates.all = matrix(models, nrow=2*length(modelnames), ncol=2, dimnames = list(paste(rep(modelnames, each=2),c("intercept", "coefficient")), c("B", "SE")))
estimates.all

### Repeat for Bow Valley
# First grab all of the estimates and standard errors
modelsBV = rbind(summary(bvelev)$coefficients[,1:2], summary(bvdisthha)$coefficients[,1:2], summary(bvdistacc)$coefficients[,1:2], summary(bvsheep)$coefficients[,1:2], summary(bvgoat)$coefficients[,1:2], summary(bvelk)$coefficients[,1:2], summary(bvmoose)$coefficients[,1:2], summary(bvdeer)$coefficients[,1:2])
# Name your models
modelnamesBV = c("bvelev", "bvdisthha", "bvdistacc", "bvsheep", "bvgoat", "bvelk", "bvmoose", "bvdeer")
# Now put all of your estimates in a pretty table with names that you'll remember!
estimates.BV = matrix(modelsBV, nrow=2*length(modelnamesBV), ncol=2, dimnames = list(paste(rep(modelnamesBV, each=2),c("intercept", "coefficient")), c("B", "SE")))

### Repeat for Red Deer
# Grab all of the estimates and standard errors
modelsRD = rbind(summary(rdelev)$coefficients[,1:2], summary(rddisthha)$coefficients[,1:2], summary(rddistacc)$coefficients[,1:2], summary(rdsheep)$coefficients[,1:2], summary(rdgoat)$coefficients[,1:2], summary(rdelk)$coefficients[,1:2], summary(rdmoose)$coefficients[,1:2], summary(rddeer)$coefficients[,1:2])
# Name your models
modelnamesRD = c("rdelev", "rddisthha", "rddistacc", "rdsheep", "rdgoat", "rdelk", "rdmoose", "rddeer")
# Now put all of your estimates in a pretty table with names that you'll remember!
estimates.RD = matrix(modelsRD, nrow=2*length(modelnamesRD), ncol=2, dimnames = list(paste(rep(modelnamesRD, each=2),c("intercept", "coefficient")), c("B", "SE")))

### combine and export model outputs
estimates <- data.frame(rbind(estimates.all, estimates.BV, estimates.RD)) %>%
  mutate(Value = rownames(estimates))
write.csv(estimates, "estimates.csv", row.names=F)


###
###### KRISTIN START HERE, RUN EVERYTHING Objective 3.0  Categorical Resource Selection Functions #########

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\HabitatModelling\\Lab4_CategoricalRscSlxn\\New\\"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\HabitatModelling\\Lab4_CategoricalRscSlxn\\New\\"
if (file.exists(wd_workcomp)) {setwd(wd_workcomp)
} else {setwd(wd_laptop)}
rm(wd_workcomp, wd_laptop)

library(dplyr)

wolfkde <- read.csv("wolfkde.csv", sep=",") %>%
  rename(DistFromHighHumanAccess2 = disthhu2)

names(wolfkde)[10]<-"landcover16"
# lets make a new column for landcover 'name'
wolfkde$habitatType = ifelse(wolfkde$landcover16 == 0, "NA", 
                            ifelse(wolfkde$landcover16 == 1, "Open Conifer", 
                            ifelse(wolfkde$landcover16 == 2, "Moderate Conifer", 
                            ifelse(wolfkde$landcover16 == 3, "Closed Conifer", 
                            ifelse(wolfkde$landcover16 == 4, "Deciduous", 
                            ifelse(wolfkde$landcover16 == 5, "Mixed", 
                            ifelse(wolfkde$landcover16 == 6, "Regen", 
                            ifelse(wolfkde$landcover16 == 7, "Herbaceous",                                   
                            ifelse(wolfkde$landcover16 == 8, "Shrub",                               
                            ifelse(wolfkde$landcover16 == 9, "Water", 
                            ifelse(wolfkde$landcover16 == 10, "Rock-Ice", 
                            ifelse(wolfkde$landcover16 == 11, "Cloud", 
                            ifelse(wolfkde$landcover16 == 12, "Burn-Forest",                               
                            ifelse(wolfkde$landcover16 == 13, "Burn-Grassland", 
                            ifelse(wolfkde$landcover16 == 14, "Burn-Shrub", 
                            ifelse(wolfkde$landcover16 == 15, "Alpine Herb", 
                                   "Alpine Shrub"))))))))))))))))
table(wolfkde$landcover16, wolfkde$used)
hist(wolfkde$landcover16)

## note we will have to discuss what to do with NA's and Cloud. 
## remove clouds as missing data
wolfkde2 <- wolfkde[wolfkde$landcover16 != 11, ] #rm clouds
table(wolfkde2$landcover16, wolfkde2$usedFactor)
wolfkde3 <-wolfkde2[wolfkde2$landcover16 != 0, ]
table(wolfkde3$landcover16, wolfkde3$usedFactor)


names.m = data.frame(unique(wolfkde3$landcover16),unique(wolfkde3$habitatType))
# Now I put it order
names.m = names.m[order(names.m)[1:14],]
names.m

names.m.export <- names.m
colnames(names.m.export) <- c("Number", "Landcover")
row.names(names.m.export) <- NULL
write.csv(names.m.export, "landcover-legend.csv", row.names=F)


# Define a factor variable, landcov.f, 
# the sorted table makes defining the names of your factor level easy!

#~# need categories for a categorical analysis

wolfkde3$landcov.f = factor(wolfkde3$landcover16,
                            labels = names.m$unique.wolfkde3.habitatType)
#Note that there are many alternative ways of defining your landcover/habitattype 
# as a factor. This method seemed most explicit in terms of defining the design matrix
table(wolfkde3$landcov.f, wolfkde3$usedFactor)

 
# Univariate Selection Ratio's

landcovSelection <- table(wolfkde3$landcov.f, wolfkde3$usedFactor)
landcovSelection2 <- as.data.frame.matrix(landcovSelection)
colnames(landcovSelection2)[1:2] <- c("avail","used")
landcovSelection2$selection <- landcovSelection2$used / landcovSelection2$avail
landcovSelection2$lnSelection <- log(landcovSelection2$selection)
landcovSelection2
hist(landcovSelection2$lnSelection)

write.table(landcovSelection2, "wolfselection.csv", sep=",", row.names = TRUE, col.names=TRUE)

## SELECTION RATIOS WITH LUMPED BURN ##

landcovSelection3 <- as.data.frame.matrix(landcovSelection)
View(landcovSelection3)
landcovSelection3$landcov <- rownames(landcovSelection3)
landcovSelection3
colnames(landcovSelection3)[1:2] <- c("avail","used")
landcovSelection3$selection <- landcovSelection3$used / landcovSelection3$avail
landcovSelection3$lnSelection <- log(landcovSelection3$selection)
landcovSelection3
hist(landcovSelection3$lnSelection)

write.table(landcovSelection2, "wolfselection.csv", sep=",", row.names = TRUE, col.names=TRUE)


#~# kristin this rowname label is fucked up

#~# can't estimate selection of 0s, duh
## reln bt ln selxn ration from use-avail vs 
  ## selectivity coeff from logistic regression

#### EXCEL Spreadsheet exercise
## open wolfselection.csv in Excel and repeat selection ratio analysis.


###
### Objective 4.  Logistic Regression ####

## first we have to think about analyzing categorical variables using a new approach compared to categories.

contrasts(wolfkde3$landcov.f) = contr.treatment(14) 
## note here also that in my case I had 15 landcover types
#~# kristin you did not rerun your code using set.seed, 

# To see the design matrix assigned
attributes(wolfkde3$landcov.f)


### note that while we have cleaned up the clouds and NA's, what shoudl we do about Burned-Grassland
### Herbaceous and Burn-Forests? Reclassify as Burned? We will return to this in a minute. 
## checking above, we see that 11, 12, and 13 are all burns. 
levels(wolfkde3$landcov.f)[10:12] = "Burn"
## note this then reduces us from 15 to 13 categories
contrasts(wolfkde3$landcov.f) = contr.treatment(12)
attributes(wolfkde3$landcov.f)


################################################################################
# skip from here to your models ##

# note how the design matrix has collapsed burn into one category? 
## What other categories should we consider?

## Logistic regression  
# incorectly analyzed treating landcover16 as a continuous covariate

naive.nf = glm(used~landcover16,data=wolfkde2, family=binomial(logit))
summary(naive.nf)
#~# ISSUE: treating categories like continuous covariate

# Univariate regression example
oc = glm(used~I(landcov.f=="Open Conifer"),data=wolfkde3, family = binomial(logit))
summary(oc)
## now lets manually evaluate the predicted probability of 
##a wolf used location occuring in Open Conifer
exp(-1.61844+0.644*1)/(1+exp(-1.6184+0.644*1))
## now compare to the probability of wolf use in non-conifer landcovers ?
exp(-1.61844+0.644*0)/(1+exp(-1.6184+0.644*0))


# Multivariate regression example
ocb = glm(used~I(landcov.f=="Open Conifer")+I(landcov.f=="Burn"), 
          data = wolfkde3, family = binomial(logit))
summary(ocb)

#~# do wolves select for burns more than open conifer?'
  ## if youput two in the model, selxn coeff of each is in relation to the other
  ## but also in reln to all other lc types
  ## stronger selection for burn than for open conifer
    ## with all other lc types being considered

### and with a few more variables
conif = glm(used~I(landcov.f=="Open Conifer")+I(landcov.f=="Moderate Conifer")
                  +I(landcov.f=="Closed Conifer"), data = wolfkde3, 
            family = binomial(logit))
summary(conif)
## how do we interpret the intercept in each model? 
##In model ocb the intercept is everything EXCEPT burns and open conifer.  
##Whereas  in the second, its everything except conifers.


# Full model
full = glm(used~I(landcov.f), data=wolfkde3, family = binomial(logit))
summary(full)
## What is the intercept?
## Where did alpine (landcover 15) go?
## Why did landcover types 4 (decid), 6 (regen) and alpine- herb (12) 'blow' up? 
##Go back and look at this table to undestand
table(wolfkde3$landcov.f, wolfkde3$usedFactor)
## They blew up because there was 0 used observed.  See what its trying to estimate?
exp(-0.974 - 15.592*1)/(1+exp(-0.974 - 15.592*1)) ## these are the intercept and coefficient for deciduous
## which is telling us that the probability of wolves using decid is essentially 0, but with no precision 
##(look at the SE) because its unestimable. 
## in this case, all landcover types without observations should technically be dropped and or 
##reclasses into the intercept category.
## so our options are to delete these rows of data like NA's above or Cloud
##  or reclass as equivalent to the intercept. 
##The latter is my recommendation, but lets wait to do that 'manually' below. 


#~# landcov11 NOW15 = alpine herb
  ## intercept is fucked because no observations in that lc type
  ## samesies stderr
    ## so you always have to keep track of used and available
    ## bc it's not valid to fit a model for a lc with no observations
#~# probabilities tell likelihood that selxn ratio = 1

# all independently interpreted in selxn ration relative to 1
# in ln relatvie to 0
# in selectivity coeff relative to reference category (open conifer)

## lets use the R notation to manually force no intercept in the model
full.NoInt = glm(used~I(landcov.f) -1, data=wolfkde3, family = binomial(logit))
summary(full.NoInt)

## note that the model with no intercept in it keeps Open Conifer.  
## Compare these coefficients to the coefficients with the same model but with an intercept.
## how do they differ??

#~# -1 removes intercept
# now have selection coefficient for every category

# now lets fit the model manually with each factor with open conifer as the intercept
full.model = glm(used~I(landcov.f=="Moderate Conifer")+I(landcov.f=="Closed Conifer")
                 +I(landcov.f=="Deciduous")+I(landcov.f=="Mixed")+I(landcov.f=="Herbaceous")
                 +I(landcov.f=="Regen")+I(landcov.f=="Shrub")+I(landcov.f=="Water")+I(landcov.f=="Rock-Ice")
                 +I(landcov.f=="Burn")+I(landcov.f=="Alpine Herb")+I(landcov.f=="Alpine Shrub"), data = wolfkde3, family = binomial(logit))
summary(full.model)
## note here that the Intercept is now manually defined as Open Conifer
#~# equivalent to full

## note that it is the same as the model full above. 
## Also, note here that the Intercept is now manually defined as Open Conifer


###
#### Objective 5.	Gain an appreciation for the influence of changing the reference category on the model. ####

## To change the reference level to say Rock and ICe (9), 
##you simply reset the contrast/design matrix, for example,
## first recheck which # Rock-Ice is
levels(wolfkde3$landcov.f) ## Ok it is # 9

contrasts(wolfkde3$landcov.f) = contr.treatment(12, base = 9)
attributes(wolfkde3$landcov.f)
# and note that rock-ice now is 0. 

rockintercept.model = glm(used~I(landcov.f=="Moderate Conifer")+I(landcov.f=="Closed Conifer")
                 +I(landcov.f=="Deciduous")+I(landcov.f=="Mixed")+I(landcov.f=="Herbaceous")
                 +I(landcov.f=="Regen")+I(landcov.f=="Shrub")+I(landcov.f=="Water")+I(landcov.f=="Open Conifer")
                 +I(landcov.f=="Burn")+I(landcov.f=="Alpine Herb")+I(landcov.f=="Alpine Shrub"), data = wolfkde3, family = binomial(logit))
summary(rockintercept.model)

## now compare coefficients from each model with open conifer vs. Rock and Ice as the intercept models?
## what has changed?
## NEWNOTE: Make a table comparing coefficients from different models 
## with different intercepts?

## now chose other reference categories. 

#~# mark finds that the r-based contrast coding is complicated
## he likes creating his own set of dummy vrbls and add them to df

## In practice I find working through the Design matrix coding of R confusing. 
## Instead, I often just create my own 'manual' dummy variables in my data frame, 
## sometimes even beforehand in excel

### Manually creating 'dummy' variables that replace using the interaction expansion used ~ I.
wolfkde3$closedConif = ifelse(wolfkde3$habitatType == "Closed Conifer", 1, 0)
wolfkde3$modConif = ifelse(wolfkde3$habitatType == "Moderate Conifer", 1, 0)
wolfkde3$openConif = ifelse(wolfkde3$habitatType == "Open Conifer", 1, 0)
wolfkde3$decid = ifelse(wolfkde3$habitatType == "Deciduous", 1, 0)
wolfkde3$regen = ifelse(wolfkde3$habitatType == "Regen", 1, 0)
wolfkde3$mixed = ifelse(wolfkde3$habitatType == "Mixed", 1, 0)
wolfkde3$herb = ifelse(wolfkde3$habitatType == "Herbaceous", 1, 0)
wolfkde3$shrub = ifelse(wolfkde3$habitatType == "Shrub", 1, 0)
wolfkde3$water = ifelse(wolfkde3$habitatType == "Water", 1, 0)
wolfkde3$rockIce = ifelse(wolfkde3$habitatType == "Rock-Ice", 1, 0)
## note here I reclassified all burn = 1 
wolfkde3$burn = ifelse(wolfkde3$habitatType == "Burn-Grassland", 1, 
                       ifelse(wolfkde3$habitatType == "Burn-Shrub", 1, 
                              ifelse(wolfkde3$habitatType == "Burn-Forest", 1,0 )))
#~# reclassified alineherb and alpineshrub as alpine
wolfkde3$alpine = ifelse(wolfkde3$habitatType == "Alpine Herb", 1, 
                       ifelse(wolfkde3$habitatType == "Alpine Shrub", 1,0 ))
#wolfkde3$alpineHerb = ifelse(wolfkde3$habitatType == "Alpine Herb", 1, 0)
#wolfkde3$alpineShrub = ifelse(wolfkde3$habitatType == "Alpine Shrub", 1, 0)

head(wolfkde3)
### note now that the design matrix is manually set in the data.frame.  

#This is inefficient, but might be easier to keep track of.
## note you can also easily reclassify categories now, 
##but you have to mentally keep track of the unit-sum constraint

#wolfkde3$alpine = wolfkde3$alpineHerb + wolfkde3$alpineShrub
##kristin you can't do this here because then it still keeps alpineherb
##and alpineshrub in the model as well

### refitting model with just Alpine and Rock and Ice as the intercept

rockintercept.alpine.model = glm(used~closedConif + openConif + modConif + 
                                   decid+ mixed+herb+water+burn+alpine, 
                                 data = wolfkde3, family = binomial(logit))
summary(rockintercept.alpine.model)


## say we think alpine and rock-ice are the same biologically
## can add them together in the same intercept
## by manually leaving them out of the analysis after you
## manually coded the reference levels
## so, same as above but just delete alpine too


### refitting model with Open Conifer as the intercept and alpine/burn pooled

oc.intercept.model = glm(used~closedConif + modConif + decid+ 
                           regen+mixed+herb+water+rockIce+burn+alpine, 
                         data = wolfkde3, family = binomial(logit))
summary(oc.intercept.model)

### refitting model with just Alpine and Rock and Ice as the intercept
rockintercept.alpine.model = glm(used~closedConif + openConif + modConif + 
                                   decid+ regen+mixed+herb+water+burn+alpine, 
                                 data = wolfkde3, family = binomial(logit))
summary(rockintercept.alpine.model)

### refitting model manually dropping Decid and Regen - where do they no go?
rock.alpine.regen.decid.intercept.model = glm(used~closedConif + openConif + 
                                                modConif + mixed+herb+water+
                                                burn+alpine, data = wolfkde3, 
                                              family = binomial(logit))
summary(rock.alpine.regen.decid.intercept.model)
#~# this subsumes decid and regen in the intercept

## comparing coefficients from two different models with different intercepts

#### I adopt the code from section 2.0 above to pull out all the coefficients and SE's and put them in one long table

### CREATE TABLE OF COEFFS YOU WANT TO LOOK AT ###
## using the code below for the models you're interested in ##
oc.ri.coefs.long = data.frame(rbind(
  summary(oc.intercept.model)$coefficients[,1:2], 
  summary(rockintercept.alpine.model)$coefficients[,1:2]))
# Name your model
coef.names = c("openConif", "closedConif", "modConif", "decid", #"regen", 
               "mixed", "herb", "water", "rockIce", "burn", "alpine")
model.names = c("Open Conif Intercept", "RockIce Intercept")
oc.ri.coefs.long$habitatType = paste(rep(coef.names, each=1))
oc.ri.coefs.long$model = paste(rep(model.names, each=1))
oc.ri.coefs.long 

## now use this table to compare the ABSOLUTE differences say between burn and alpine in both models
## in the oc.model the B coefficient for Burn = 0.29 and Alpine = -3.002 an absolute differences of 3.29
## in the rock.model the B coefficient for Burn = 2.2 and Alpine is -1.086, an absolute difference of 3.29
## the same!

rockintercept.alpine.model.df <- data.frame(summary(rockintercept.alpine.model)$coefficients[,1:2])
oc.intercept.model.df <- data.frame(summary(oc.intercept.model)$coefficients[,1:2])
coef.table <- rbind(rockintercept.alpine.model.df,oc.intercept.model.df)
coef.table$habitatType <- c(row.names((summary(rockintercept.alpine.model)$coefficients[,1:2])
                                      ),row.names(summary(oc.intercept.model)$coefficients[,1:2]))
coef.table$habitatType[1] <- "rockIce"
coef.table$habitatType[12] <- "openConif"
coef.table$model <-c(rep("Open Conif Intercept",10),rep( "RockIce Intercept",10))
coef.table
ggplot(coef.table, aes(x=habitatType, y=Estimate, colour=model)) + geom_point(size = 5) +
  theme(axis.text.x = element_text(angle = 90))
## this figure tells us that RELATIVELY nothing has changed, only where the coefficients are relative to the yAxis



###### MY MODELS #####

# global
m.glob <- glm(used~I(landcov.f), data=wolfkde3, family = binomial(logit))
summary(m.glob)

# global no intercept
m.glob.noi <- glm(used~I(landcov.f) -1, data=wolfkde3, family = binomial(logit))
summary(m.glob.noi)

## global model without decid, regen, alpine herb
m.glob3 <- glm(used~
                  I(landcov.f=="Open Conifer")+
                  I(landcov.f=="Moderate Conifer")+
                  I(landcov.f=="Closed Conifer")+
                  I(landcov.f=="Mixed")+
                  I(landcov.f=="Herbaceous")+
                  I(landcov.f=="Shrub")+
                  I(landcov.f=="Water")+
                  I(landcov.f=="Rock-Ice")+
                  I(landcov.f=="Burn")+
                  I(landcov.f=="Alpine Shrub"), 
                data = wolfkde3, family = binomial(logit))
summary(m.glob3)
 
## same as above but mixed as reference cat 
m.mixed <- glm(used~
                  I(landcov.f=="Open Conifer")+
                  I(landcov.f=="Moderate Conifer")+
                  I(landcov.f=="Closed Conifer")+
                  I(landcov.f=="Herbaceous")+
                  I(landcov.f=="Shrub")+
                  I(landcov.f=="Water")+
                  I(landcov.f=="Rock-Ice")+
                  I(landcov.f=="Burn")+
                  I(landcov.f=="Alpine Shrub"), 
                data = wolfkde3, family = binomial(logit))
summary(m.mixed)

## ref cat = modcon
m.modcon <- glm(used~
                  I(landcov.f=="Open Conifer")+
                  I(landcov.f=="Closed Conifer")+
                  I(landcov.f=="Mixed")+
                  I(landcov.f=="Herbaceous")+
                  I(landcov.f=="Shrub")+
                  I(landcov.f=="Water")+
                  I(landcov.f=="Rock-Ice")+
                  I(landcov.f=="Burn")+
                  I(landcov.f=="Alpine Shrub"), 
                data = wolfkde3, family = binomial(logit))
summary(m.mixed)

## refcat = rockice
m.rockice <- glm(used~
                  I(landcov.f=="Open Conifer")+
                  I(landcov.f=="Moderate Conifer")+
                  I(landcov.f=="Closed Conifer")+
                  I(landcov.f=="Mixed")+
                  I(landcov.f=="Herbaceous")+
                  I(landcov.f=="Shrub")+
                  I(landcov.f=="Water")+
                  I(landcov.f=="Burn")+
                  I(landcov.f=="Alpine Shrub"), 
                data = wolfkde3, family = binomial(logit))
summary(m.rockice)



### model selection ###

library(AICcmodavg)

## m.glob, m.glob.noi, m.glob3, m.mixed, m.modcon, m.rockice ##

Cand.set <- list( )
Cand.set[[1]] <- m.glob3
Cand.set[[2]] <- m.mixed
Cand.set[[3]] <- m.modcon
Cand.set[[4]] <- m.rockice
names(Cand.set) <- c("global-interceptwdecidregenalpherb",
                     "mixed(decidregenalpherb)",
                     "modcon(decidregenalpherb)",
                     "rockice(decidregenalpherb)")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)


### extract coefficients from models ###

## i have a list of models
## for each model,

## X names, jenky
coeff.m.glob.noi <- data.frame(summary(m.glob.noi)$coefficients[,1:2])
xnames <- data.frame(rownames(coeff.m.glob.noi))
colnames(xnames) <- "temp"
unique(xnames$temp)
xnames$X <- c("Open Conifer", 
              "Moderate Conifer",
              "Closed Conifer",
              "Deciduous",
              "Mixed",
              "Herbaceous",
              "Shrub",
              "Water",
              "Rock-Ice",
              "Burn",
              "Alpine Herb",
              "Alpine Shrub")
fml <- data.frame(temp="(Intercept)", X="Intercept")
cnames <- rbind(fml, xnames)

coeff.m.glob3 <- data.frame(summary(m.glob3)$coefficients[,1:2])
xnames <- data.frame(rownames(coeff.m.glob3))
colnames(xnames) <- "temp"
unique(xnames$temp)
xnames$X <- c("Intercept",
              "Open Conifer", 
              "Moderate Conifer",
              "Closed Conifer",
              "Mixed",
              "Herbaceous",
              "Shrub",
              "Water",
              "Rock-Ice",
              "Burn",
              "Alpine Shrub")



## extract coefficients ####

# coeff.m.glob <- data.frame(summary(m.glob)$coefficients[,1:2])
# coeff.m.glob$temp <- rownames(coeff.m.glob)
# coeff.m.glob$X <- c("Open Conifer", "Moderate Conifer", "Closed Conifer",
#                         "Deciduous", "Mixed", "Herbaceous", "Shrub",
#                         "Water", "Rock-Ice", "Burn", "Alpine Herb", 
#                         "Alpine Shrub")
# coeff.m.glob <- coeff.m.glob %>%
#   select(-temp) %>%
#   mutate(Model = "m.glob")

coeff.m.glob.noi <- data.frame(summary(m.glob.noi)$coefficients[,1:2])
coeff.m.glob.noi$temp <- rownames(coeff.m.glob.noi)
coeff.m.glob.noi$X <- c("Open Conifer", "Moderate Conifer", "Closed Conifer",
                        "Deciduous", "Mixed", "Herbaceous", "Shrub",
                        "Water", "Rock-Ice", "Burn", "Alpine Herb", 
                        "Alpine Shrub")
coeff.m.glob.noi <- coeff.m.glob.noi %>%
  select(-temp) %>%
  mutate(Model = "m.glob.noi")

coeff.m.glob3 <- data.frame(summary(m.glob3)$coefficients[,1:2])
coeff.m.glob3$temp <- rownames(coeff.m.glob3)
coeff.m.glob3 <- left_join(coeff.m.glob3, cnames, by = "temp") %>%
  select(-temp) %>%
  mutate(Model = "m.glob3")

coeff.m.mixed <- data.frame(summary(m.mixed)$coefficients[,1:2])
coeff.m.mixed$temp <- rownames(coeff.m.mixed)
coeff.m.mixed <- left_join(coeff.m.mixed, cnames, by = "temp") %>%
  select(-temp) %>%
  mutate(Model = "m.mixed")

coeff.m.modcon <- data.frame(summary(m.modcon)$coefficients[,1:2])
coeff.m.modcon$temp <- rownames(coeff.m.modcon)
coeff.m.modcon <- left_join(coeff.m.modcon, cnames, by = "temp") %>%
  select(-temp) %>%
  mutate(Model = "m.modcon")

coeff.m.rockice <- data.frame(summary(m.rockice)$coefficients[,1:2])
coeff.m.rockice$temp <- rownames(coeff.m.rockice)
coeff.m.rockice <- left_join(coeff.m.rockice, cnames, by = "temp") %>%
  select(-temp) %>%
  mutate(Model = "m.rockice")

mostcoeffs <- rbind(coeff.m.glob3, coeff.m.mixed,
                    coeff.m.modcon, coeff.m.rockice)
write.csv(mostcoeffs, file = "mostcoeffs.csv", row.names=F)

mostcoeffs2 <- filter(mostcoeffs, Model != "m.glob3")
## plotting ####

betas <- mostcoeffs2 %>%
  dplyr::filter(X != "Intercept" &
                  X != "Deciduous" &
                  X != "Alpine Herb") %>%
  mutate(Intercept = ifelse(#Model == "m.glob3",
                       #"No observed use",
                  ifelse(Model == "m.mixed",
                         "Mixed",
                  ifelse(Model == "m.modcon",
                         "Moderate Conifer",
                  "Rock-Ice"))))
# 
# betas$Estimate <- factor(betas$Estimate, 
#                          levels = c("Mixed, Water",
#                                     "Burn", "Herbaceous",
#                                     "Shrub", "Open Conifer",
#                                     "Moderate Conifer", 
#                                     "Closed Conifer",
#                                     "Alpine Shrub",
#                                     "Rock-Ice"))

ggplot(betas, aes(x=X, 
                  y=Estimate, 
                  shape=Intercept)) + 
  geom_point(position = position_dodge(width = 0.2)) +
  scale_shape_manual(values=c(0, 1, 2, 3, 4)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "")


#### shit i meant to