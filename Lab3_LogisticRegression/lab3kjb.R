## WILDLIFE HABITAT MODELING LAB feb2017 ##
##  LAB 3 - Feb.2017 - Use/Availability  ##


#### SETUP ####

# function to install and load required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#load or install these packages:
packages <- c("ggplot2","foreign", "lattice", "psych", "effects", "plyr")

# run function to install packages
ipak(packages)

# set working directory
setwd("C:\\Users\\kjbark3r\\Documents\\GitHub\\HabitatModelling\\Lab3_LogisticRegression\\")

# read in data
wolfused <-read.csv("wolfused.csv", header = TRUE)
wolfavail <-read.csv("wolfavail.csv", header = TRUE)

# combine data into used/available dataframe
wolfkde <- rbind(wolfused, wolfavail)
table(wolfkde$used, wolfkde$pack)
wolfkde$usedFactor <- factor(wolfkde$used, labels=c('0','1'))


#### KDE SELECTION ####


#### All wolves ####

outputs <- data.frame(
	Model = character(),
	OddsRatio = numeric(),
	LowCI = numeric(),
	HighCI = numeric(),
	Intercept = integer(),
	stringsAsFactors = FALSE)

modnames <- c("elev100",
			  "distHuman",
			  "sheep",
			  "goat",
			  "moose",
			  "elk ",
			  "deer",
			  stringsAsFactors=FALSE)
			  
for(i in 1:7) {
	mod = modnames[i]
	outputs[i,1] <- paste(mod)
	}


## elevation (m) ##
elev <- glm(used ~ Elevation2, family=binomial(logit), data=wolfkde)
summary(elev)
str(elev)
## CI's using profile log-likelihood's
confint(elev)
## odds ratio's
exp(coefficients(elev))
## how to obtain 95% CI's on odds ratio's
exp(cbind(OR=coef(elev), confint(elev)))



## elevation (100m) ##
wolfkde$elev100 <- wolfkde$Elevation2 / 100
elev100 <- glm(used ~ elev100, family=binomial(logit), data=wolfkde)
summary(elev100)
exp(coef(elev100)) #~# gives odds ratio (rather than log-odds)
# for every 100m inc in elev, odds of wolf using that area is 43% less likely
#get confidence intervals to see that even tho it doesn't look that diff from 1 it is
exp(cbind(OR=coef(elev100), confint(elev100)))
elevBnp = 0:3000 ## creates a new vector elevBnp with ranges from 0 - 3000 in it. 
str(elevBnp)
## use predict function to predict Y values using the model object elev
 # predicts probability of use of elev values based on modeled relnship
elevPred = predict(elev, newdata=data.frame(Elevation2=elevBnp), type = "response")
hist(elevPred)
#~# elev discriminates between used/unused very well
plot(elevBnp, elevPred, type="l", ylim = c(0,1.0), ylab= "Pr(Used)")
# but were there elevations from 0 - 1300m in Banff?
plot(wolfkde$Elevation2, wolfkde$used)
lines(elevBnp, elevPred, type="l", ylab= "Pr(Used)", add = TRUE)
# extract fitted values from our model (Pr(use))
wolfkde$fitted.Elev <- fitted(elev)
# store model results
outputs[1,2] <- exp(coef(elev100)[2])
outputs[1,3] <- exp(confint(elev100)[2,1])
outputs[1,4] <- exp(confint(elev100)[2,2])
outputs[1,5] <- exp(coef(elev100)[1])


## human use ##
distHuman <- glm(used ~ I(DistFromHumanAccess2/100), family=binomial(logit), data=wolfkde)
summary(distHuman)
exp(coefficients(distHuman)) #~# GIVES ODDS RATIO
hist(wolfkde$DistFromHumanAccess2) #~# most predictions are of low use
disthumanBnp = 0:7000
disthumanPred = predict(distHuman, newdata=data.frame(DistFromHumanAccess2=disthumanBnp), type="response")
hist(disthumanPred)
plot(disthumanBnp, disthumanPred, type="l", ylab= "Pr(Used)")
plot(wolfkde$DistFromHumanAccess2, wolfkde$used)
lines(disthumanBnp, disthumanPred, type="l", ylab= "Pr(Used)", add = TRUE)
wolfkde$fitted.distHuman <- fitted(distHuman)
# store model results
outputs[2,2] <- exp(coef(distHuman)[2])
outputs[2,3] <- exp(confint(distHuman)[2,1])
outputs[2,4] <- exp(confint(distHuman)[2,2])
outputs[2,5] <- exp(coef(distHuman)[1])


## hsi vals for predictions ##
habvalues = 0:7

## sheep ##
sheep <- glm(used ~ sheep_w2, family=binomial(logit), data=wolfkde)
summary(sheep) 
exp(coefficients(sheep))
sheeppred = predict(sheep, newdata = data.frame(sheep_w2 = habvalues), type = "response")
wolfkde$fitted.Sheep <- fitted(sheep)
# store model results
outputs[3,2] <- exp(coef(sheep)[2])
outputs[3,3] <- exp(confint(sheep)[2,1])
outputs[3,4] <- exp(confint(sheep)[2,2])
outputs[3,5] <- exp(coef(sheep)[1])

## goat ##
goat <- glm(used ~ goat_w2, family=binomial(logit), data=wolfkde)
summary(goat) 
exp(coefficients(goat))
goatpred = predict(goat, newdata = data.frame(goat_w2 = habvalues), type = "response")
wolfkde$fitted.Goat <- fitted(goat)
# store model results
outputs[4,2] <- exp(coef(goat)[2])
outputs[4,3] <- exp(confint(goat)[2,1])
outputs[4,4] <- exp(confint(goat)[2,2])
outputs[4,5] <- exp(coef(goat)[1])

## moose ##
moose <- glm(used ~ moose_w2, family=binomial(logit), data=wolfkde)
summary(moose) 
exp(coefficients(moose))
moosepred = predict(moose, newdata = data.frame(moose_w2 = habvalues), type = "response")
wolfkde$fitted.Moose <- fitted(moose)
# store model results
outputs[5,2] <- exp(coef(moose)[2])
outputs[5,3] <- exp(confint(moose)[2,1])
outputs[5,4] <- exp(confint(moose)[2,2])
outputs[5,5] <- exp(coef(moose)[1])

## elk ##
elk <- glm(used ~ elk_w2, family=binomial(logit), data=wolfkde)
summary(elk) 
exp(coefficients(elk))
elkpred = predict(elk, newdata = data.frame(elk_w2 = habvalues), type = "response")
wolfkde$fitted.Elk <- fitted(elk)
# store model results
outputs[6,2] <- exp(coef(elk)[2])
outputs[6,3] <- exp(confint(elk)[2,1])
outputs[6,4] <- exp(confint(elk)[2,2])
outputs[6,5] <- exp(coef(elk)[1])

## deer ##
deer <- glm(used ~ deer_w2, family=binomial(logit), data=wolfkde)
summary(deer) 
exp(coefficients(deer))
deerpred = predict(deer, newdata = data.frame(deer_w2 = habvalues), type = "response")
wolfkde$fitted.Deer <- fitted(deer)
# store model results
outputs[7,2] <- exp(coef(deer)[2])
outputs[7,3] <- exp(confint(deer)[2,1])
outputs[7,4] <- exp(confint(deer)[2,2])
outputs[7,5] <- exp(coef(deer)[1])



#### Red Deer ####

rdkde <- wolfkde[wolfkde$pack=="Red Deer",]
rdkde <- na.omit(rdkde)

outputsrd <- data.frame(
	Model = character(),
	OddsRatio = numeric(),
	LowCI = numeric(),
	HighCI = numeric(),
	Intercept = integer(),
	stringsAsFactors = FALSE)

modnamesrd <- c("elevrd100",
			  "distHumanrd",
			  "sheeprd",
			  "goatrd",
			  "mooserd",
			  "elkrd",
			  "deerrd",
			  stringsAsFactors=FALSE)
			  
for(i in 1:7) {
	mod = modnamesrd[i]
	outputsrd[i,1] <- paste(mod)
	}

## elevation (m) ##

elevrd <- glm(used ~ Elevation2, family=binomial(logit), data=rdkde)
summary(elevrd)
str(elevrd)
## CI's using profile log-likelihood's
confint(elevrd)
## odds ratio's
exp(coefficients(elevrd))
## how to obtain 95% CI's on odds ratio's
exp(cbind(OR=coef(elevrd), confint(elevrd)))


## elevation (100m) ##
rdkde$elev100 <- rdkde$Elevation2 / 100
elevrd100 <- glm(used ~ elev100, family=binomial(logit), data=rdkde)
summary(elevrd100)
exp(coef(elevrd100)) #~# gives odds ratio
# for every 100m inc in elev, odds of wolf using that area is 43% less likely
#get confidence intervals to see that even tho it doesn't look that diff from 1 it is
exp(cbind(OR=coef(elevrd100), confint(elevrd100)))
elevBnp = 0:3000 ## creates a new vector elevBnp with ranges from 0 - 3000 in it. 
str(elevBnp)
## use predict function to predict Y values using the model object elev
 # predicts probability of use of elev values based on modeled relnship
elevrdPred = predict(elevrd, newdata=data.frame(Elevation2=elevBnp), type = "response")
hist(elevrdPred)
#~# elev discriminates between used/unused very well
plot(elevBnp, elevrdPred, type="l", ylim = c(0,1.0), ylab= "Pr(Used)")
# but were there elevations from 0 - 1300m in Banff?
plot(rdkde$Elevation2, rdkde$used)
lines(elevBnp, elevrdPred, type="l", ylab= "Pr(Used)", add = TRUE)
# extract fitted values from our model (Pr(use))
rdkde$fitted.elevrd <- fitted(elevrd)
# store model results
outputsrd[1,2] <- exp(coef(elevrd100)[2])
outputsrd[1,3] <- exp(confint(elevrd100)[2,1])
outputsrd[1,4] <- exp(confint(elevrd100)[2,2])
outputsrd[1,5] <- exp(coef(elevrd100)[1])


## human use ##
distHumanrd <- glm(used ~ I(DistFromHumanAccess2/100), family=binomial(logit), data=rdkde)
summary(distHumanrd)
exp(coefficients(distHumanrd)) #~# GIVES ODDS RATIO
hist(rdkde$DistFromHumanAccess2) #~# most predictions are of low use
distHumanrdBnp = 0:7000
distHumanrdPred = predict(distHumanrd, newdata=data.frame(DistFromHumanAccess2=distHumanrdBnp), type="response")
hist(distHumanrdPred)
plot(distHumanrdBnp, distHumanrdPred, type="l", ylab= "Pr(Used)")
plot(rdkde$DistFromHumanAccess2, rdkde$used)
lines(distHumanrdBnp, distHumanrdPred, type="l", ylab= "Pr(Used)", add = TRUE)
rdkde$fitted.distHumanrd <- fitted(distHumanrd)
# store model results
outputsrd[2,2] <- exp(coef(distHumanrd)[2])
outputsrd[2,3] <- exp(confint(distHumanrd)[2,1])
outputsrd[2,4] <- exp(confint(distHumanrd)[2,2])
outputsrd[2,5] <- exp(coef(distHumanrd)[1])

## hsi vals for predictions ##
habvalues = 0:7

## sheep ##
sheeprd <- glm(used ~ sheep_w2, family=binomial(logit), data=rdkde)
summary(sheeprd) 
exp(coefficients(sheeprd))
sheeprdpred = predict(sheeprd, newdata = data.frame(sheep_w2 = habvalues), type = "response")
rdkde$fitted.sheeprd <- fitted(sheeprd)
# store model results
outputsrd[3,2] <- exp(coef(sheeprd)[2])
outputsrd[3,3] <- exp(confint(sheeprd)[2,1])
outputsrd[3,4] <- exp(confint(sheeprd)[2,2])
outputsrd[3,5] <- exp(coef(sheeprd)[1])

## goat ##
goatrd <- glm(used ~ goat_w2, family=binomial(logit), data=rdkde)
summary(goatrd) 
exp(coefficients(goatrd))
goatrdpred = predict(goatrd, newdata = data.frame(goat_w2 = habvalues), type = "response")
rdkde$fitted.goatrd <- fitted(goatrd)
# store model results
outputsrd[4,2] <- exp(coef(goatrd)[2])
outputsrd[4,3] <- exp(confint(goatrd)[2,1])
outputsrd[4,4] <- exp(confint(goatrd)[2,2])
outputsrd[4,5] <- exp(coef(goatrd)[1])

## moose ##
mooserd <- glm(used ~ moose_w2, family=binomial(logit), data=rdkde)
summary(mooserd) 
exp(coefficients(mooserd))
mooserdpred = predict(mooserd, newdata = data.frame(moose_w2 = habvalues), type = "response")
rdkde$fitted.mooserd <- fitted(mooserd)
# store model results
outputsrd[5,2] <- exp(coef(mooserd)[2])
outputsrd[5,3] <- exp(confint(mooserd)[2,1])
outputsrd[5,4] <- exp(confint(mooserd)[2,2])
outputsrd[5,5] <- exp(coef(mooserd)[1])

## elk ##
elkrd <- glm(used ~ elk_w2, family=binomial(logit), data=rdkde)
summary(elkrd) 
exp(coefficients(elkrd))
elkrdpred = predict(elkrd, newdata = data.frame(elk_w2 = habvalues), type = "response")
rdkde$fitted.elkrd <- fitted(elkrd)
# store model results
outputsrd[6,2] <- exp(coef(elkrd)[2])
outputsrd[6,3] <- exp(confint(elkrd)[2,1])
outputsrd[6,4] <- exp(confint(elkrd)[2,2])
outputsrd[6,5] <- exp(coef(elkrd)[1])


## deer ##
deerrd <- glm(used ~ deer_w2, family=binomial(logit), data=rdkde)
summary(deerrd) 
exp(coefficients(deerrd))
deerrdpred = predict(deerrd, newdata = data.frame(deer_w2 = habvalues), type = "response")
rdkde$fitted.deerrd <- fitted(deerrd)
# store model results
outputsrd[7,2] <- exp(coef(deerrd)[2])
outputsrd[7,3] <- exp(confint(deerrd)[2,1])
outputsrd[7,4] <- exp(confint(deerrd)[2,2])
outputsrd[7,5] <- exp(coef(deerrd)[1])


#### Bow Valley ####

bvkde <- wolfkde[wolfkde$pack=="Bow Valley",]
bvkde <- na.omit(bvkde)

outputsbv <- data.frame(
	Model = character(),
	OddsRatio = numeric(),
	LowCI = numeric(),
	HighCI = numeric(),
	Intercept = integer(),
	stringsAsFactors = FALSE)

modnamesbv <- c("elevbv100",
			  "distHumanbv",
			  "sheepbv",
			  "goatbv",
			  "moosebv",
			  "elkbv",
			  "deerbv",
			  stringsAsFactors=FALSE)
			  
for(i in 1:7) {
	mod = modnamesbv[i]
	outputsbv[i,1] <- paste(mod)
	}


## elevation (m) ##
elevbv <- glm(used ~ Elevation2, family=binomial(logit), data=bvkde)
summary(elevbv)
str(elevbv)
## CI's using profile log-likelihood's
confint(elevbv)
## odds ratio's
exp(coefficients(elevbv))
## how to obtain 95% CI's on odds ratio's
exp(cbind(OR=coef(elevbv), confint(elevbv)))


## elevation (100m) ##
bvkde$elev100 <- bvkde$Elevation2 / 100
elevbv100 <- glm(used ~ elev100, family=binomial(logit), data=bvkde)
summary(elevbv100)
exp(coef(elevbv100)) #~# gives odds ratio
exp(cbind(OR=coef(elevbv100), confint(elevbv100)))
elevBnp = 0:3000 ## creates a new vector elevBnp with ranges from 0 - 3000 in it. 
str(elevBnp)
## use predict function to predict Y values using the model object elev
 # predicts probability of use of elev values based on modeled relnship
elevbvPred = predict(elevbv, newdata=data.frame(Elevation2=elevBnp), type = "response")
hist(elevbvPred)
#~# elev discriminates between used/unused very well
plot(elevBnp, elevbvPred, type="l", ylim = c(0,1.0), ylab= "Pr(Used)")
# but were there elevations from 0 - 1300m in Banff?
plot(bvkde$Elevation2, bvkde$used)
lines(elevBnp, elevbvPred, type="l", ylab= "Pr(Used)", add = TRUE)
# extract fitted values from our model (Pr(use))
bvkde$fitted.elevbv <- fitted(elevbv)
# store model results
outputsbv[1,2] <- exp(coef(elevbv100)[2])
outputsbv[1,3] <- exp(confint(elevbv100)[2,1])
outputsbv[1,4] <- exp(confint(elevbv100)[2,2])
outputsbv[1,5] <- exp(coef(elevbv100)[1])

## human use ##
distHumanbv <- glm(used ~ I(DistFromHumanAccess2/100), family=binomial(logit), data=bvkde)
summary(distHumanbv)
exp(coefficients(distHumanbv)) #~# GIVES ODDS RATIO
hist(bvkde$DistFromHumanAccess2) #~# most predictions are of low use
distHumanbvBnp = 0:7000
distHumanbvPred = predict(distHumanbvrd, newdata=data.frame(DistFromHumanAccess2=distHumanbvBnp), type="response")
hist(distHumanbvPred)
plot(distHumanbvBnp, distHumanbvPred, type="l", ylab= "Pr(Used)")
plot(bvkde$DistFromHumanAccess2, bvkde$used)
lines(distHumanbvBnp, distHumanbvPred, type="l", ylab= "Pr(Used)", add = TRUE)
bvkde$fitted.distHumanbv <- fitted(distHumanbv)
# store model results
outputsbv[2,2] <- exp(coef(distHumanbv)[2])
outputsbv[2,3] <- exp(confint(distHumanbv)[2,1])
outputsbv[2,4] <- exp(confint(distHumanbv)[2,2])
outputsbv[2,5] <- exp(coef(distHumanbv)[1])


## hsi vals for predictions ##
habvalues = 0:7

## sheep ##
sheepbv <- glm(used ~ sheep_w2, family=binomial(logit), data=bvkde)
summary(sheepbv) 
exp(coefficients(sheepbv))
sheepbvpred = predict(sheepbv, newdata = data.frame(sheep_w2 = habvalues), type = "response")
bvkde$fitted.sheepbv <- fitted(sheepbv)
# store model results
outputsbv[3,2] <- exp(coef(sheepbv)[2])
outputsbv[3,3] <- exp(confint(sheepbv)[2,1])
outputsbv[3,4] <- exp(confint(sheepbv)[2,2])
outputsbv[3,5] <- exp(coef(sheepbv)[1])

## goat ##
goatbv <- glm(used ~ goat_w2, family=binomial(logit), data=bvkde)
summary(goatbv) 
exp(coefficients(goatbv))
goatbvpred = predict(goatbv, newdata = data.frame(goat_w2 = habvalues), type = "response")
bvkde$fitted.goatbv <- fitted(goatbv)
# store model results
outputsbv[4,2] <- exp(coef(goatbv)[2])
outputsbv[4,3] <- exp(confint(goatbv)[2,1])
outputsbv[4,4] <- exp(confint(goatbv)[2,2])
outputsbv[4,5] <- exp(coef(goatbv)[1])

## moose ##
moosebv <- glm(used ~ moose_w2, family=binomial(logit), data=bvkde)
summary(moosebv) 
exp(coefficients(moosebv))
moosebvpred = predict(moosebv, newdata = data.frame(moose_w2 = habvalues), type = "response")
bvkde$fitted.moosebv <- fitted(moosebv)
# store model results
outputsbv[5,2] <- exp(coef(moosebv)[2])
outputsbv[5,3] <- exp(confint(moosebv)[2,1])
outputsbv[5,4] <- exp(confint(moosebv)[2,2])
outputsbv[5,5] <- exp(coef(moosebv)[1])

## elk ##
elkbv <- glm(used ~ elk_w2, family=binomial(logit), data=bvkde)
summary(elkbv) 
exp(coefficients(elkbv))
elkbvpred = predict(elkbv, newdata = data.frame(elk_w2 = habvalues), type = "response")
bvkde$fitted.elkbv <- fitted(elkbv)
# store model results
outputsbv[6,2] <- exp(coef(elkbv)[2])
outputsbv[6,3] <- exp(confint(elkbv)[2,1])
outputsbv[6,4] <- exp(confint(elkbv)[2,2])
outputsbv[6,5] <- exp(coef(elkbv)[1])


## deer ##
deerbv <- glm(used ~ deer_w2, family=binomial(logit), data=bvkde)
summary(deerbv) 
exp(coefficients(deerbv))
deerbvpred = predict(deerbv, newdata = data.frame(deer_w2 = habvalues), type = "response")
bvkde$fitted.deerbv <- fitted(deerbv)
# store model results
outputsbv[7,2] <- exp(coef(deerbv)[2])
outputsbv[7,3] <- exp(confint(deerbv)[2,1])
outputsbv[7,4] <- exp(confint(deerbv)[2,2])
outputsbv[7,5] <- exp(coef(deerbv)[1])

## combine data ##

results <- rbind(outputs, outputsrd, outputsbv)
write.csv(results, file = "kde-results.csv", row.names=F)


#### MCP SELECTION ####

## MCPs ##
library(raster)
wolfyht<-shapefile("../SpatialData/wolfyht.shp")

# all wolves - 95mcp w 1000 rndm avail locs #
x<-wolfyht@data$EASTING
y<-wolfyht@data$NORTHING
xy<-cbind(x,y)
all <- data.frame(as.character(wolfyht@data$Pack))
coordinates(all) <- xy
proj4string(all) <-  CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
mcp.all <- mcp(all, percent=95)
all.avail<-spsample(mcp.all, 1000, "random")
plot(all.avail); plot(mcp.all, add = TRUE)

# rasters for extract #
deer_w<-raster("../SpatialData/deer_w2.tif")
moose_w<-raster("../SpatialData/moose_w2.tif")
elk_w<-raster("../SpatialData/elk_w2.tif")
sheep_w<-raster("../SpatialData/sheep_w2.tif")
goat_w<-raster("../SpatialData/goat_w2.tif")
wolf_w<-raster("../SpatialData/wolf_w2.tif")#
elevation2<-raster("../SpatialData/Elevation2.tif") #resampled
disthumanaccess2<-raster("../SpatialData/DistFromHumanAccess2.tif") #resampled

# red deer #
rd.data<-wolfyht[wolfyht@data$Pack=="Red Deer",]
x<-rd.data@data$EASTING
y<-rd.data@data$NORTHING
xy<-cbind(x,y)
rd <- data.frame(as.character(rd.data@data$NAME))
coordinates(rd) <- xy
proj4string(rd) <-  CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
names(rd)<-"NAME"
rd<-rd[rd@data$NAME!="69" & rd@data$NAME!="81" & rd@data$NAME!="82" & rd@data$NAME!="84",]
rd@data$NAME<-"RedDeer"
mcp.rd <- mcp(rd, percent=95)
rd.avail<-spsample(mcp.rd, 1000, "random")
plot(rd.avail); plot(mcp.rd, add = TRUE)

# bow valley #
bv.data<-wolfyht[wolfyht@data$Pack=="Bow valley",]
x<-bv.data@data$EASTING
y<-bv.data@data$NORTHING
xy<-cbind(x,y)
bv <- data.frame(as.character(bv.data@data$NAME))
coordinates(bv) <- xy
proj4string(bv) <-  CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
bv@data$NAME <- "BowValley"
mcp.bv <- mcp(bv, percent=95)
bv.avail<-spsample(mcp.bv, 1000, "random", iter=20)
plot(bv.avail); plot(mcp.bv, add = TRUE)

# extract covariates and combine dfs
all_rasters<-stack(deer_w, moose_w, elk_w, sheep_w, goat_w, wolf_w,elevation2, disthumanaccess2)
  #mcp.availALL <- extract(all_rasters, all.avail)
  #allavail <- as.data.frame(mcp.availALL)
##rd
mcp.availRD<-extract(all_rasters, rd.avail)
availrd <- as.data.frame(mcp.availRD)
availrd$pack <- "Red Deer"
availrd$used <- 0
##bv
mcp.availBV<-extract(all_rasters, bv.avail)
availbv <- as.data.frame(mcp.availBV)
availbv$pack <- "Bow Valley"
availbv$used <- 0
##avail-rd and bv
availall <- rbind(availrd, availbv)
#used+avail
wolfmcp <- rbind(wolfused, availall)
table(wolfmcp$used, wolfmcp$pack)
wolfmcp$usedFactor <- factor(wolfmcp$used, labels=c('0','1'))
wolfmcp <- na.omit(wolfmcp)


#### All wolves ####

outputsmcp <- data.frame(
	Model = character(),
	OddsRatio = numeric(),
	LowCI = numeric(),
	HighCI = numeric(),
	Intercept = integer(),
	stringsAsFactors = FALSE)

modnames <- c("elev100",
			  "distHuman",
			  "sheep",
			  "goat",
			  "moose",
			  "elk ",
			  "deer",
			  stringsAsFactors=FALSE)
			  
for(i in 1:7) {
	mod = modnames[i]
	outputsmcp[i,1] <- paste(mod)
	}



## elevation (100m) ##
wolfmcp$elev100 <- wolfmcp$Elevation2 / 100
elev100mcp <- glm(used ~ elev100, family=binomial(logit), data=wolfmcp)
summary(elev100)
exp(coef(elev100)) #~# gives odds ratio (rather than log-odds)
# for every 100m inc in elev, odds of wolf using that area is 43% less likely
#get confidence intervals to see that even tho it doesn't look that diff from 1 it is
exp(cbind(OR=coef(elev100), confint(elev100)))
elevBnp = 0:3000 ## creates a new vector elevBnp with ranges from 0 - 3000 in it. 
str(elevBnp)
## use predict function to predict Y values using the model object elev
 # predicts probability of use of elev values based on modeled relnship
elevPred = predict(elev, newdata=data.frame(Elevation2=elevBnp), type = "response")
hist(elevPred)
#~# elev discriminates between used/unused very well
plot(elevBnp, elevPred, type="l", ylim = c(0,1.0), ylab= "Pr(Used)")
# but were there elevations from 0 - 1300m in Banff?
plot(wolfmcp$Elevation2, wolfmcp$used)
lines(elevBnp, elevPred, type="l", ylab= "Pr(Used)", add = TRUE)
# extract fitted values from our model (Pr(use))
wolfmcp$fitted.Elev <- fitted(elev)
# store model results
outputsmcp[1,2] <- exp(coef(elev100)[2])
outputsmcp[1,3] <- exp(confint(elev100)[2,1])
outputsmcp[1,4] <- exp(confint(elev100)[2,2])
outputsmcp[1,5] <- exp(coef(elev100)[1])


## human use ##
distHumanmcp <- glm(used ~ I(DistFromHumanAccess2/100), family=binomial(logit), data=wolfmcp)
summary(distHuman)
exp(coefficients(distHuman)) #~# GIVES ODDS RATIO
hist(wolfmcp$DistFromHumanAccess2) #~# most predictions are of low use
disthumanBnp = 0:7000
disthumanPred = predict(distHuman, newdata=data.frame(DistFromHumanAccess2=disthumanBnp), type="response")
hist(disthumanPred)
plot(disthumanBnp, disthumanPred, type="l", ylab= "Pr(Used)")
plot(wolfmcp$DistFromHumanAccess2, wolfmcp$used)
lines(disthumanBnp, disthumanPred, type="l", ylab= "Pr(Used)", add = TRUE)
wolfmcp$fitted.distHuman <- fitted(distHuman)
# store model results
outputsmcp[2,2] <- exp(coef(distHuman)[2])
outputsmcp[2,3] <- exp(confint(distHuman)[2,1])
outputsmcp[2,4] <- exp(confint(distHuman)[2,2])
outputsmcp[2,5] <- exp(coef(distHuman)[1])


## hsi vals for predictions ##
habvalues = 0:7

## sheep ##
sheepmcp <- glm(used ~ sheep_w2, family=binomial(logit), data=wolfmcp)
summary(sheep) 
exp(coefficients(sheep))
sheeppred = predict(sheep, newdata = data.frame(sheep_w2 = habvalues), type = "response")
wolfmcp$fitted.Sheep <- fitted(sheep)
# store model results
outputsmcp[3,2] <- exp(coef(sheep)[2])
outputsmcp[3,3] <- exp(confint(sheep)[2,1])
outputsmcp[3,4] <- exp(confint(sheep)[2,2])
outputsmcp[3,5] <- exp(coef(sheep)[1])

## goat ##
goatmcp <- glm(used ~ goat_w2, family=binomial(logit), data=wolfmcp)
summary(goat) 
exp(coefficients(goat))
goatpred = predict(goat, newdata = data.frame(goat_w2 = habvalues), type = "response")
wolfmcp$fitted.Goat <- fitted(goat)
# store model results
outputsmcp[4,2] <- exp(coef(goat)[2])
outputsmcp[4,3] <- exp(confint(goat)[2,1])
outputsmcp[4,4] <- exp(confint(goat)[2,2])
outputsmcp[4,5] <- exp(coef(goat)[1])

## moose ##
moosemcp <- glm(used ~ moose_w2, family=binomial(logit), data=wolfmcp)
summary(moose) 
exp(coefficients(moose))
moosepred = predict(moose, newdata = data.frame(moose_w2 = habvalues), type = "response")
wolfmcp$fitted.Moose <- fitted(moose)
# store model results
outputsmcp[5,2] <- exp(coef(moose)[2])
outputsmcp[5,3] <- exp(confint(moose)[2,1])
outputsmcp[5,4] <- exp(confint(moose)[2,2])
outputsmcp[5,5] <- exp(coef(moose)[1])

## elk ##
elkmcp <- glm(used ~ elk_w2, family=binomial(logit), data=wolfmcp)
summary(elk) 
exp(coefficients(elk))
elkpred = predict(elk, newdata = data.frame(elk_w2 = habvalues), type = "response")
wolfmcp$fitted.Elk <- fitted(elk)
# store model results
outputsmcp[6,2] <- exp(coef(elk)[2])
outputsmcp[6,3] <- exp(confint(elk)[2,1])
outputsmcp[6,4] <- exp(confint(elk)[2,2])
outputsmcp[6,5] <- exp(coef(elk)[1])

## deer ##
deermcp <- glm(used ~ deer_w2, family=binomial(logit), data=wolfmcp)
summary(deer) 
exp(coefficients(deer))
deerpred = predict(deer, newdata = data.frame(deer_w2 = habvalues), type = "response")
wolfmcp$fitted.Deer <- fitted(deer)
# store model results
outputsmcp[7,2] <- exp(coef(deer)[2])
outputsmcp[7,3] <- exp(confint(deer)[2,1])
outputsmcp[7,4] <- exp(confint(deer)[2,2])
outputsmcp[7,5] <- exp(coef(deer)[1])



#### Red Deer ####

rdmcp <- wolfmcp[wolfmcp$pack=="Red Deer",]
rdmcp <- na.omit(rdmcp)

outputsmcprd <- data.frame(
	Model = character(),
	OddsRatio = numeric(),
	LowCI = numeric(),
	HighCI = numeric(),
	Intercept = integer(),
	stringsAsFactors = FALSE)

modnamesrd <- c("elevrd100",
			  "distHumanrd",
			  "sheeprd",
			  "goatrd",
			  "mooserd",
			  "elkrd",
			  "deerrd",
			  stringsAsFactors=FALSE)
			  
for(i in 1:7) {
	mod = modnamesrd[i]
	outputsmcprd[i,1] <- paste(mod)
	}

## elevation (m) ##

elevrdmcp <- glm(used ~ Elevation2, family=binomial(logit), data=rdmcp)
summary(elevrd)
str(elevrd)
## CI's using profile log-likelihood's
confint(elevrd)
## odds ratio's
exp(coefficients(elevrd))
## how to obtain 95% CI's on odds ratio's
exp(cbind(OR=coef(elevrd), confint(elevrd)))


## elevation (100m) ##
rdmcp$elev100 <- rdmcp$Elevation2 / 100
elevrd100mcp <- glm(used ~ elev100, family=binomial(logit), data=rdmcp)
summary(elevrd100)
exp(coef(elevrd100)) #~# gives odds ratio
# for every 100m inc in elev, odds of wolf using that area is 43% less likely
#get confidence intervals to see that even tho it doesn't look that diff from 1 it is
exp(cbind(OR=coef(elevrd100), confint(elevrd100)))
elevBnp = 0:3000 ## creates a new vector elevBnp with ranges from 0 - 3000 in it. 
str(elevBnp)
## use predict function to predict Y values using the model object elev
 # predicts probability of use of elev values based on modeled relnship
elevrdPred = predict(elevrd, newdata=data.frame(Elevation2=elevBnp), type = "response")
hist(elevrdPred)
#~# elev discriminates between used/unused very well
plot(elevBnp, elevrdPred, type="l", ylim = c(0,1.0), ylab= "Pr(Used)")
# but were there elevations from 0 - 1300m in Banff?
plot(rdmcp$Elevation2, rdmcp$used)
lines(elevBnp, elevrdPred, type="l", ylab= "Pr(Used)", add = TRUE)
# extract fitted values from our model (Pr(use))
rdmcp$fitted.elevrd <- fitted(elevrd)
# store model results
outputsmcprd[1,2] <- exp(coef(elevrd100)[2])
outputsmcprd[1,3] <- exp(confint(elevrd100)[2,1])
outputsmcprd[1,4] <- exp(confint(elevrd100)[2,2])
outputsmcprd[1,5] <- exp(coef(elevrd100)[1])


## human use ##
distHumanrdmcp <- glm(used ~ I(DistFromHumanAccess2/100), family=binomial(logit), data=rdmcp)
summary(distHumanrd)
exp(coefficients(distHumanrd)) #~# GIVES ODDS RATIO
hist(rdmcp$DistFromHumanAccess2) #~# most predictions are of low use
distHumanrdBnp = 0:7000
distHumanrdPred = predict(distHumanrd, newdata=data.frame(DistFromHumanAccess2=distHumanrdBnp), type="response")
hist(distHumanrdPred)
plot(distHumanrdBnp, distHumanrdPred, type="l", ylab= "Pr(Used)")
plot(rdmcp$DistFromHumanAccess2, rdmcp$used)
lines(distHumanrdBnp, distHumanrdPred, type="l", ylab= "Pr(Used)", add = TRUE)
rdmcp$fitted.distHumanrd <- fitted(distHumanrd)
# store model results
outputsmcprd[2,2] <- exp(coef(distHumanrd)[2])
outputsmcprd[2,3] <- exp(confint(distHumanrd)[2,1])
outputsmcprd[2,4] <- exp(confint(distHumanrd)[2,2])
outputsmcprd[2,5] <- exp(coef(distHumanrd)[1])

## hsi vals for predictions ##
habvalues = 0:7

## sheep ##
sheeprdmcp <- glm(used ~ sheep_w2, family=binomial(logit), data=rdmcp)
summary(sheeprd) 
exp(coefficients(sheeprd))
sheeprdpred = predict(sheeprd, newdata = data.frame(sheep_w2 = habvalues), type = "response")
rdmcp$fitted.sheeprd <- fitted(sheeprd)
# store model results
outputsmcprd[3,2] <- exp(coef(sheeprd)[2])
outputsmcprd[3,3] <- exp(confint(sheeprd)[2,1])
outputsmcprd[3,4] <- exp(confint(sheeprd)[2,2])
outputsmcprd[3,5] <- exp(coef(sheeprd)[1])

## goat ##
goatrdmcp <- glm(used ~ goat_w2, family=binomial(logit), data=rdmcp)
summary(goatrd) 
exp(coefficients(goatrd))
goatrdpred = predict(goatrd, newdata = data.frame(goat_w2 = habvalues), type = "response")
rdmcp$fitted.goatrd <- fitted(goatrd)
# store model results
outputsmcprd[4,2] <- exp(coef(goatrd)[2])
outputsmcprd[4,3] <- exp(confint(goatrd)[2,1])
outputsmcprd[4,4] <- exp(confint(goatrd)[2,2])
outputsmcprd[4,5] <- exp(coef(goatrd)[1])

## moose ##
mooserdmcp <- glm(used ~ moose_w2, family=binomial(logit), data=rdmcp)
summary(mooserd) 
exp(coefficients(mooserd))
mooserdpred = predict(mooserd, newdata = data.frame(moose_w2 = habvalues), type = "response")
rdmcp$fitted.mooserd <- fitted(mooserd)
# store model results
outputsmcprd[5,2] <- exp(coef(mooserd)[2])
outputsmcprd[5,3] <- exp(confint(mooserd)[2,1])
outputsmcprd[5,4] <- exp(confint(mooserd)[2,2])
outputsmcprd[5,5] <- exp(coef(mooserd)[1])

## elk ##
elkrdmcp <- glm(used ~ elk_w2, family=binomial(logit), data=rdmcp)
summary(elkrd) 
exp(coefficients(elkrd))
elkrdpred = predict(elkrd, newdata = data.frame(elk_w2 = habvalues), type = "response")
rdmcp$fitted.elkrd <- fitted(elkrd)
# store model results
outputsmcprd[6,2] <- exp(coef(elkrd)[2])
outputsmcprd[6,3] <- exp(confint(elkrd)[2,1])
outputsmcprd[6,4] <- exp(confint(elkrd)[2,2])
outputsmcprd[6,5] <- exp(coef(elkrd)[1])


## deer ##
deerrdmcp <- glm(used ~ deer_w2, family=binomial(logit), data=rdmcp)
summary(deerrd) 
exp(coefficients(deerrd))
deerrdpred = predict(deerrd, newdata = data.frame(deer_w2 = habvalues), type = "response")
rdmcp$fitted.deerrd <- fitted(deerrd)
# store model results
outputsmcprd[7,2] <- exp(coef(deerrd)[2])
outputsmcprd[7,3] <- exp(confint(deerrd)[2,1])
outputsmcprd[7,4] <- exp(confint(deerrd)[2,2])
outputsmcprd[7,5] <- exp(coef(deerrd)[1])


#### Bow Valley ####

bvmcp <- wolfmcp[wolfmcp$pack=="Bow Valley",]
bvmcp <- na.omit(bvmcp)

outputsmcpbv <- data.frame(
	Model = character(),
	OddsRatio = numeric(),
	LowCI = numeric(),
	HighCI = numeric(),
	Intercept = integer(),
	stringsAsFactors = FALSE)

modnamesbv <- c("elevbv100",
			  "distHumanbv",
			  "sheepbv",
			  "goatbv",
			  "moosebv",
			  "elkbv",
			  "deerbv",
			  stringsAsFactors=FALSE)
			  
for(i in 1:7) {
	mod = modnamesbv[i]
	outputsmcpbv[i,1] <- paste(mod)
	}


## elevation (m) ##
elevbv <- glm(used ~ Elevation2, family=binomial(logit), data=bvmcp)
summary(elevbv)
str(elevbv)
## CI's using profile log-likelihood's
confint(elevbv)
## odds ratio's
exp(coefficients(elevbv))
## how to obtain 95% CI's on odds ratio's
exp(cbind(OR=coef(elevbv), confint(elevbv)))


## elevation (100m) ##
bvmcp$elev100 <- bvmcp$Elevation2 / 100
elevbv100 <- glm(used ~ elev100, family=binomial(logit), data=bvmcp)
summary(elevbv100)
exp(coef(elevbv100)) #~# gives odds ratio
exp(cbind(OR=coef(elevbv100), confint(elevbv100)))
elevBnp = 0:3000 ## creates a new vector elevBnp with ranges from 0 - 3000 in it. 
str(elevBnp)
## use predict function to predict Y values using the model object elev
 # predicts probability of use of elev values based on modeled relnship
elevbvPred = predict(elevbv, newdata=data.frame(Elevation2=elevBnp), type = "response")
hist(elevbvPred)
#~# elev discriminates between used/unused very well
plot(elevBnp, elevbvPred, type="l", ylim = c(0,1.0), ylab= "Pr(Used)")
# but were there elevations from 0 - 1300m in Banff?
plot(bvmcp$Elevation2, bvmcp$used)
lines(elevBnp, elevbvPred, type="l", ylab= "Pr(Used)", add = TRUE)
# extract fitted values from our model (Pr(use))
bvmcp$fitted.elevbv <- fitted(elevbv)
# store model results
outputsmcpbv[1,2] <- exp(coef(elevbv100)[2])
outputsmcpbv[1,3] <- exp(confint(elevbv100)[2,1])
outputsmcpbv[1,4] <- exp(confint(elevbv100)[2,2])
outputsmcpbv[1,5] <- exp(coef(elevbv100)[1])

## human use ##
distHumanbv <- glm(used ~ I(DistFromHumanAccess2/100), family=binomial(logit), data=bvmcp)
summary(distHumanbv)
exp(coefficients(distHumanbv)) #~# GIVES ODDS RATIO
hist(bvmcp$DistFromHumanAccess2) #~# most predictions are of low use
distHumanbvBnp = 0:7000
distHumanbvPred = predict(distHumanbvrd, newdata=data.frame(DistFromHumanAccess2=distHumanbvBnp), type="response")
hist(distHumanbvPred)
plot(distHumanbvBnp, distHumanbvPred, type="l", ylab= "Pr(Used)")
plot(bvmcp$DistFromHumanAccess2, bvmcp$used)
lines(distHumanbvBnp, distHumanbvPred, type="l", ylab= "Pr(Used)", add = TRUE)
bvmcp$fitted.distHumanbv <- fitted(distHumanbv)
# store model results
outputsmcpbv[2,2] <- exp(coef(distHumanbv)[2])
outputsmcpbv[2,3] <- exp(confint(distHumanbv)[2,1])
outputsmcpbv[2,4] <- exp(confint(distHumanbv)[2,2])
outputsmcpbv[2,5] <- exp(coef(distHumanbv)[1])


## hsi vals for predictions ##
habvalues = 0:7

## sheep ##
sheepbv <- glm(used ~ sheep_w2, family=binomial(logit), data=bvmcp)
summary(sheepbv) 
exp(coefficients(sheepbv))
sheepbvpred = predict(sheepbv, newdata = data.frame(sheep_w2 = habvalues), type = "response")
bvmcp$fitted.sheepbv <- fitted(sheepbv)
# store model results
outputsmcpbv[3,2] <- exp(coef(sheepbv)[2])
outputsmcpbv[3,3] <- exp(confint(sheepbv)[2,1])
outputsmcpbv[3,4] <- exp(confint(sheepbv)[2,2])
outputsmcpbv[3,5] <- exp(coef(sheepbv)[1])

## goat ##
goatbv <- glm(used ~ goat_w2, family=binomial(logit), data=bvmcp)
summary(goatbv) 
exp(coefficients(goatbv))
goatbvpred = predict(goatbv, newdata = data.frame(goat_w2 = habvalues), type = "response")
bvmcp$fitted.goatbv <- fitted(goatbv)
# store model results
outputsmcpbv[4,2] <- exp(coef(goatbv)[2])
outputsmcpbv[4,3] <- exp(confint(goatbv)[2,1])
outputsmcpbv[4,4] <- exp(confint(goatbv)[2,2])
outputsmcpbv[4,5] <- exp(coef(goatbv)[1])

## moose ##
moosebv <- glm(used ~ moose_w2, family=binomial(logit), data=bvmcp)
summary(moosebv) 
exp(coefficients(moosebv))
moosebvpred = predict(moosebv, newdata = data.frame(moose_w2 = habvalues), type = "response")
bvmcp$fitted.moosebv <- fitted(moosebv)
# store model results
outputsmcpbv[5,2] <- exp(coef(moosebv)[2])
outputsmcpbv[5,3] <- exp(confint(moosebv)[2,1])
outputsmcpbv[5,4] <- exp(confint(moosebv)[2,2])
outputsmcpbv[5,5] <- exp(coef(moosebv)[1])

## elk ##
elkbv <- glm(used ~ elk_w2, family=binomial(logit), data=bvmcp)
summary(elkbv) 
exp(coefficients(elkbv))
elkbvpred = predict(elkbv, newdata = data.frame(elk_w2 = habvalues), type = "response")
bvmcp$fitted.elkbv <- fitted(elkbv)
# store model results
outputsmcpbv[6,2] <- exp(coef(elkbv)[2])
outputsmcpbv[6,3] <- exp(confint(elkbv)[2,1])
outputsmcpbv[6,4] <- exp(confint(elkbv)[2,2])
outputsmcpbv[6,5] <- exp(coef(elkbv)[1])


## deer ##
deerbv <- glm(used ~ deer_w2, family=binomial(logit), data=bvmcp)
summary(deerbv) 
exp(coefficients(deerbv))
deerbvpred = predict(deerbv, newdata = data.frame(deer_w2 = habvalues), type = "response")
bvmcp$fitted.deerbv <- fitted(deerbv)
# store model results
outputsmcpbv[7,2] <- exp(coef(deerbv)[2])
outputsmcpbv[7,3] <- exp(confint(deerbv)[2,1])
outputsmcpbv[7,4] <- exp(confint(deerbv)[2,2])
outputsmcpbv[7,5] <- exp(coef(deerbv)[1])

## combine data ##

resultsmcp <- rbind(outputsmcp, outputsmcprd, outputsmcpbv)


write.csv(resultsmcp, file = "mcp-results.csv", row.names=F)



#### MCP/KDE COMPARISONS ####
results$Method <- "KDE"
resultsmcp$Method <- "MCP"
ors <- rbind(results, resultsmcp) %>%
  mutate(Pack = ifelse(grepl('rd', Model),
                       "Red Deer", 
                       ifelse(grepl('bv', Model),
                              "Bow Valley", "All")))
## 
eledat <- subset(ors, Model == "elev100" |
                   Model == "elevrd100" |
                   Model == "elevbv100")
eleplot <- ggplot(data = eledat, 
       aes(x = Pack,
           y = OddsRatio,
           shape = Method,
           ymin = LowCI, 
           ymax = HighCI)) +
        geom_point(position = position_dodge(width = 0.2)) +
        geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
        scale_shape_manual(values=c(1,4)) +
        labs(title = "Elevation", x = "") +
        theme(legend.position="none")

humdat <- subset(ors, Model == "distHuman" |
                   Model == "distHumanrd" |
                   Model == "distHumanbv")
humplot <- ggplot(data = humdat, 
       aes(x = Pack,
           y = OddsRatio,
           shape = Method,
           ymin = LowCI, 
           ymax = HighCI)) +
        geom_point(position = position_dodge(width = 0.2)) +
        geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
        scale_shape_manual(values=c(1,4)) +
        labs(title = "Dist from Human", x = "") +
        theme(legend.position="none")

deerdat <- subset(ors, Model == "deer" |
                   Model == "deerrd" |
                   Model == "deerbv")
deerplot <- ggplot(data = deerdat, 
       aes(x = Pack,
           y = OddsRatio,
           shape = Method,
           ymin = LowCI, 
           ymax = HighCI)) +
        geom_point(position = position_dodge(width = 0.2)) +
        geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
        scale_shape_manual(values=c(1,4)) +
        labs(title = "Deer", x = "") +
        theme(legend.position="none")

elkdat <- subset(ors, Model == "elk " |
                   Model == "elkrd" |
                   Model == "elkbv")
elkplot <- ggplot(data = elkdat, 
       aes(x = Pack,
           y = OddsRatio,
           shape = Method,
           ymin = LowCI, 
           ymax = HighCI)) +
        geom_point(position = position_dodge(width = 0.2)) +
        geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
        scale_shape_manual(values=c(1,4)) +
        labs(title = "Elk", x = "") +
        theme(legend.position="none")

moosedat <- subset(ors, Model == "moose" |
                   Model == "mooserd" |
                   Model == "moosebv")
mooseplot <- ggplot(data = moosedat, 
       aes(x = Pack,
           y = OddsRatio,
           shape = Method,
           ymin = LowCI, 
           ymax = HighCI)) +
        geom_point(position = position_dodge(width = 0.2)) +
        geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
        scale_shape_manual(values=c(1,4)) +
        labs(title = "Moose", x = "") +
        theme(legend.position="none")

sheepdat <- subset(ors, Model == "sheep" |
                   Model == "sheeprd" |
                   Model == "sheepbv")
sheepplot <- ggplot(data = sheepdat, 
       aes(x = Pack,
           y = OddsRatio,
           shape = Method,
           ymin = LowCI, 
           ymax = HighCI)) +
        geom_point(position = position_dodge(width = 0.2)) +
        geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
        scale_shape_manual(values=c(1,4)) +
        labs(title = "Sheep", x = "") +
        theme(legend.position="none")

goatdat <- subset(ors, Model == "goat" |
                   Model == "goatrd" |
                   Model == "goatbv")
goatplot <- ggplot(data = goatdat, 
       aes(x = Pack,
           y = OddsRatio,
           shape = Method,
           ymin = LowCI, 
           ymax = HighCI)) +
        geom_point(position = position_dodge(width = 0.2)) +
        geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
        scale_shape_manual(values=c(1,4)) +
        labs(title = "Goat", x = "") 

grid.arrange(eleplot, humplot,
             deerplot, elkplot, mooseplot,
             sheepplot, goatplot,
             ncol = 2)

#### STATS SHIT - for report ####



#### VISUALIZATIONS - for report ####

# pr(used) ~ prey HSI

par(mfrow=c(3,1))
# all wolves
plot(habvalues, elkpred, 
     type ="l", 
     ylim = c(0,1.0), 
     ylab = "Pr(Used)",
     xlab = "Habitat Suitability Index (HSI)",
     lty = 1,
     main = "All Wolves")
lines(habvalues, goatpred, lty = 2)
lines(habvalues, moosepred, lty = 3) 
lines(habvalues, sheeppred, lty = 5) 
lines(habvalues, deerpred, lty = 6) 
legend(x="topleft", 
       legend= c("Elk","Mountain Goat", "Moose", "Sheep", "Deer"), 
       lty=c(1,2,3,5,6), bty = "n")
# red deer
plot(habvalues, elkrdpred, 
     type ="l", 
     ylim = c(0,1.0), 
     ylab = "Pr(Used)",
     xlab = "Habitat Suitability Index (HSI)",
     lty = 1,
     main = "Red Deer pack")
lines(habvalues, goatrdpred, lty = 2)
lines(habvalues, mooserdpred, lty = 3) 
lines(habvalues, sheeprdpred, lty = 5) 
lines(habvalues, deerrdpred, lty = 6) 
legend(x="topleft", 
       legend= c("Elk","Mountain Goat", "Moose", "Sheep", "Deer"), 
       lty=c(1,2,3,5,6), bty = "n")
# bow valley
plot(habvalues, elkbvpred, 
     type ="l", 
     ylim = c(0,1.0), 
     ylab = "Pr(Used)",
     xlab = "Habitat Suitability Index (HSI)",
     lty = 1,
     main = "Bow Valley pack")
lines(habvalues, goatbvpred, lty = 2)
lines(habvalues, moosebvpred, lty = 3) 
lines(habvalues, sheepbvpred, lty = 5) 
lines(habvalues, deerbvpred, lty = 6) 
legend(x="topleft", 
       legend= c("Elk","Mountain Goat", "Moose", "Sheep", "Deer"), 
       lty=c(1,2,3,5,6), bty = "n")

## pretty prediction plots
aa <- ggplot(wolfkde, aes(x=Elevation2, y=used, linetype=pack))  + 
  stat_smooth(method="glm", method.args = list(family="binomial"), level=0.95)+
        labs(title = "Elevation", x = "m", y = "Pr(Use)") +
        theme(legend.position="none")
ab <- ggplot(wolfkde, aes(x=DistFromHumanAccess2, y=used, linetype=pack)) + 
  stat_smooth(method="glm", method.args = list(family="binomial"), level=0.95)+
        labs(title = "Distance Human", x = "m", y = "Pr(Use)") +
        theme(legend.position="none")
ac <- ggplot(wolfkde, aes(x=deer_w2, y=used, linetype=pack)) + 
  stat_smooth(method="glm", method.args = list(family="binomial"), level=0.95)+
        labs(title = "Deer", x = "HSI", y = "Pr(Use)") +
        theme(legend.position="none")
ad <- ggplot(wolfkde, aes(x=elk_w2, y=used, linetype=pack)) + 
  stat_smooth(method="glm", method.args = list(family="binomial"), level=0.95)+
        labs(title = "Elk", x = "HSI", y = "Pr(Use)") +
        theme(legend.position="none")
ae <- ggplot(wolfkde, aes(x=moose_w2, y=used, linetype=pack)) + 
  stat_smooth(method="glm", method.args = list(family="binomial"), level=0.95)+
        labs(title = "Moose", x = "HSI", y = "Pr(Use)") +
        theme(legend.position="none")
af <- ggplot(wolfkde, aes(x=sheep_w2, y=used, linetype=pack)) + 
  stat_smooth(method="glm", method.args = list(family="binomial"), level=0.95)+
        labs(title = "Sheep", x = "HSI", y = "Pr(Use)") +
        theme(legend.position="none")
ag <- ggplot(wolfkde, aes(x=goat_w2, y=used, linetype=pack)) + 
  stat_smooth(method="glm", method.args = list(family="binomial"), level=0.95)+
        labs(title = "Goat", x = "HSI", y = "Pr(Use)") 
grid.arrange(aa,ab,ac,ad,ae,af,ag,
             ncol = 2)


#### VISUALIZATIONS - prelim for me ####

# predicted values
##kde
sheeppred = predict(sheep, newdata = data.frame(sheep_w2 = habvalues), type = "response")
goatpred = predict(goat, newdata = data.frame(goat_w2 = habvalues), type = "response")
moosepred = predict(moose, newdata = data.frame(moose_w2 = habvalues), type = "response")
elkpred = predict(elk, newdata = data.frame(elk_w2 = habvalues), type = "response")
deerpred = predict(deer, newdata = data.frame(deer_w2 = habvalues), type = "response")
##mcp
sheeppredmcp = predict(sheepmcp, newdata = data.frame(sheep_w2 = habvalues), type = "response")
goatpredmcp = predict(goatmcp, newdata = data.frame(goat_w2 = habvalues), type = "response")
moosepredmcp = predict(moosemcp, newdata = data.frame(moose_w2 = habvalues), type = "response")
elkpredmcp = predict(elkmcp, newdata = data.frame(elk_w2 = habvalues), type = "response")
deerpredmcp = predict(deermcp, newdata = data.frame(deer_w2 = habvalues), type = "response")
# plots
par(mfrow=c(2,1))
plot(habvalues, elkpred, type ="l", ylim = c(0,1.0), ylab = "Pr(Used)", col = "green")
lines(habvalues, goatpred, col = "blue")
lines(habvalues, moosepred, col = "red") 
lines(habvalues, sheeppred, col = "black") 
lines(habvalues, deerpred, col = "gray") 
legend(x="topleft", legend= c("Elk","Mountain Goat", "Moose", "Sheep", "Deer"), lty=1, col = c("green", "blue", "red", "black", "gray"), bty = "n")
plot(habvalues, elkpredmcp, type ="l", ylim = c(0,1.0), ylab = "Pr(Used)", col = "green")
lines(habvalues, goatpredmcp, col = "blue")
lines(habvalues, moosepredmcp, col = "red") 
lines(habvalues, sheeppredmcp, col = "black") 
lines(habvalues, deerpredmcp, col = "gray") 
legend(x="topleft", legend= c("Elk","Mountain Goat", "Moose", "Sheep", "Deer"), lty=1, col = c("green", "blue", "red", "black", "gray"), bty = "n")



# boxplots - used vs avail for each pack on same plot
par(mfrow = c(1,1))
boxplot(Elevation2~used+pack, data = wolfkde, main = "Elevation")
boxplot(DistFromHumanAccess2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer Human Access Used-Avail")
boxplot(deer_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer DEER Used-Avail")
boxplot(moose_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer MOOSE Used-Avail")
boxplot(elk_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer ELK Used-Avail")
boxplot(goat_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer GOAT Used-Avail")
boxplot(sheep_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer SHEEP Used-Avail")


library(gridExtra)
eplot <- ggplot(data = wolfkde,
       aes(y = Elevation2,
           x = pack,
           fill = usedFactor)) +
  labs(title = "Elevation",
            x = "", y = "Meters") +
  theme(legend.position="none") +
  scale_fill_manual(labels = c("Available", "Used"),
                    values=c("grey40","white")) +
  geom_boxplot()
hplot <- ggplot(data = wolfkde,
       aes(y = DistFromHumanAccess2,
           x = pack,
           fill = usedFactor)) +
  labs(title = "Distance Human Access",
            x = "", y = "Meters") +
              theme(legend.position="none") +
  scale_fill_manual(labels = c("Available", "Used"),
                    values=c("grey40","white")) +
  geom_boxplot()
elkplot <- ggplot(data = wolfkde,
       aes(y = elk_w2,
           x = pack,
           fill = usedFactor)) +
    labs(title = "Elk",
            x = "", y = "HSI") +
              theme(legend.position="none") +
  scale_fill_manual(labels = c("Available", "Used"),
                    values=c("grey40","white")) +
  geom_boxplot()
dplot <- ggplot(data = wolfkde,
       aes(y = deer_w2,
           x = pack,
           fill = usedFactor)) +
      labs(title = "Deer",
            x = "", y = "HSI") +
              theme(legend.position="none") +
  scale_fill_manual(labels = c("Available", "Used"),
                    values=c("grey40","white")) +
  geom_boxplot()
mplot <- ggplot(data = wolfkde,
       aes(y = moose_w2,
           x = pack,
           fill = usedFactor)) +
      labs(title = "Moose",
            x = "", y = "HSI") +
              theme(legend.position="none") +
  scale_fill_manual(labels = c("Available", "Used"),
                    values=c("grey40","white")) +
  geom_boxplot()
gplot <- ggplot(data = wolfkde,
       aes(y = goat_w2,
           x = pack,
           fill = usedFactor)) +
      labs(title = "Goat",
            x = "", y = "HSI") +
              theme(legend.position="none") +
  scale_fill_manual(labels = c("Available", "Used"),
                    values=c("grey40","white")) +
  geom_boxplot()
splot <- ggplot(data = wolfkde,
       aes(y = sheep_w2,
           x = pack,
           fill = usedFactor)) +
      labs(title = "Sheep",
            x = "", y = "HSI") +
    scale_fill_manual(labels = c("Available", "Used"),
                    values=c("grey40","white")) +
  theme(legend.title = element_blank()) +
  geom_boxplot()
grid.arrange(eplot, hplot,
             elkplot, dplot, mplot,
             gplot, splot,
             ncol = 2)


## used:avail ##

# all wolves
par(mfrow=c(4,2))
#par(mfrow = c(2,3))
boxplot(deer_w2~usedFactor, data=wolfkde, main = "Deer Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(elk_w2~usedFactor, main = "Elk Used-Avail", ylab="elk_w2", xlab="usedFactor", data=wolfkde)
boxplot(moose_w2~usedFactor, main = "Moose Used-Avail", ylab="moose_w2", xlab="usedFactor", data=wolfkde)
boxplot(goat_w2~usedFactor, main = "Goat Used-Avail", ylab="goat_w2", xlab="usedFactor", data=wolfkde)
boxplot(sheep_w2~usedFactor, main = "Sheep Used-Avail", ylab="sheep_w2", xlab="usedFactor", data=wolfkde)
## Now lets do for Elevation and Distance from Human Access2
#par(mfrow = c(1,2))
boxplot(Elevation2~usedFactor, data=wolfkde, main = "Elevation Used-Avail", xlab="usedFactor", ylab="elev")
boxplot(DistFromHumanAccess2~usedFactor, data=wolfkde, main = "Human Access Used-Avail", ylab="elk_w2", xlab="usedFactor")

# red deer
par(mfrow=c(4,2))
#par(mfrow = c(2,3))
boxplot(deer_w2~usedFactor, data=rdkde, main = "Deer Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(elk_w2~usedFactor, main = "Elk Used-Avail", ylab="elk_w2", xlab="usedFactor", data=rdkde)
boxplot(moose_w2~usedFactor, main = "Moose Used-Avail", ylab="moose_w2", xlab="usedFactor", data=rdkde)
boxplot(goat_w2~usedFactor, main = "Goat Used-Avail", ylab="goat_w2", xlab="usedFactor", data=rdkde)
boxplot(sheep_w2~usedFactor, main = "Sheep Used-Avail", ylab="sheep_w2", xlab="usedFactor", data=rdkde)
## Now lets do for Elevation and Distance from Human Access2
#par(mfrow = c(1,2))
boxplot(Elevation2~usedFactor, data=rdkde, main = "Elevation Used-Avail", xlab="usedFactor", ylab="elev")
boxplot(DistFromHumanAccess2~usedFactor, data=rdkde, main = "Human Access Used-Avail", ylab="elk_w2", xlab="usedFactor")

# bow valley
par(mfrow=c(4,2))
#par(mfrow = c(2,3))
boxplot(deer_w2~usedFactor, data=bvkde, main = "Deer Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(elk_w2~usedFactor, main = "Elk Used-Avail", ylab="elk_w2", xlab="usedFactor", data=bvkde)
boxplot(moose_w2~usedFactor, main = "Moose Used-Avail", ylab="moose_w2", xlab="usedFactor", data=bvkde)
boxplot(goat_w2~usedFactor, main = "Goat Used-Avail", ylab="goat_w2", xlab="usedFactor", data=bvkde)
boxplot(sheep_w2~usedFactor, main = "Sheep Used-Avail", ylab="sheep_w2", xlab="usedFactor", data=bvkde)
## Now lets do for Elevation and Distance from Human Access2
#par(mfrow = c(1,2))
boxplot(Elevation2~usedFactor, data=bvkde, main = "Elevation Used-Avail", xlab="usedFactor", ylab="elev")
boxplot(DistFromHumanAccess2~usedFactor, data=bvkde, main = "Human Access Used-Avail", ylab="elk_w2", xlab="usedFactor")



#### VISUALIZATIONS - from lab ####



# quick plots of predicted use ~ each covariate (separate)
hist(wolfkde$fitted.Elev)
plot(wolfkde$Elevation2, wolfkde$fitted.Elev)

# better plots of predicted use - elev only
# ggplot 2 explore basic histogram function
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev)) + geom_histogram()
# lets explore faceting
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev)) + geom_histogram(binwidth=0.05, fill="gray70", colour="black") + facet_grid(used ~ .)
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev)) + geom_histogram(binwidth=0.05, fill="gray70", colour="black") + facet_grid(used ~ ., scales = "free")
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev, fill=usedFactor)) + geom_histogram(binwidth=0.05, position="identity", alpha=0.7) + xlab("Predicted Probability of Wolf Use") + theme(axis.title.x=element_text(size=16))
#~# he likes the above one
# orig data were 1s and 0s, but predictions are continuous
# this screws with your r2 val - how can you evaluate the model??
# so splitting out by 0 and 1 helpd visualize the relationship or something
# ponder this
# lets redo this graph using faceting by pack
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev, y=..density.., fill=usedFactor)) + geom_histogram(binwidth=0.05, position="identity", alpha=0.7) + xlab("Predicted Probability of Wolf Use") + theme(axis.title.x=element_text(size=16)) + facet_grid(pack ~ ., scales="free")
#looks at model that doesn't consider effect of pack
#and visualizes it by pack
#tells you the red deer pack is existing at higher elevations
# Now lets explore fitting functions to the distributions
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev)) + geom_density()
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev), fill=usedFactor) + geom_density(alpha=0.5) + xlim(0,1)+xlab("Predicted Probability of Wolf Use") + theme(axis.title.x=element_text(size=16)) 
# kernel lines
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev, y=..density.., fill=usedFactor)) + geom_histogram(binwidth=0.05) + geom_density(alpha = 0.5) + facet_grid(pack ~ .)



# boxplots - KDE - used/avail - all wolves
par(mfrow=c(4,2))

#par(mfrow = c(2,3))
boxplot(deer_w2~usedFactor, data=wolfkde, main = "Deer Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(elk_w2~usedFactor, main = "Elk Used-Avail", ylab="elk_w2", xlab="usedFactor", data=wolfkde)
boxplot(moose_w2~usedFactor, main = "Moose Used-Avail", ylab="moose_w2", xlab="usedFactor", data=wolfkde)
boxplot(goat_w2~usedFactor, main = "Goat Used-Avail", ylab="goat_w2", xlab="usedFactor", data=wolfkde)
boxplot(sheep_w2~usedFactor, main = "Sheep Used-Avail", ylab="sheep_w2", xlab="usedFactor", data=wolfkde)
## Now lets do for Elevation and Distance from Human Access2
#par(mfrow = c(1,2))
boxplot(Elevation2~usedFactor, data=wolfkde, main = "Elevation Used-Avail", xlab="usedFactor", ylab="elev")
boxplot(DistFromHumanAccess2~usedFactor, data=wolfkde, main = "Human Access Used-Avail", ylab="elk_w2", xlab="usedFactor")

## boxplots - kde- used vs avail - Bow Valley 
bvkde<- subset(wolfkde, subset=pack =="Bow Valley")
par(mfrow = c(2,3))
boxplot(deer_w2~usedFactor, data=bvkde, main = "Deer Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(elk_w2~usedFactor, data=bvkde, main = "Elk Used-Avail", ylab="elk_w2", xlab="usedFactor")
boxplot(moose_w2~usedFactor, data=bvkde, main = "Moose Used-Avail", ylab="moose_w2", xlab="usedFactor")
boxplot(goat_w2~usedFactor, data=bvkde, main = "Goat Used-Avail", ylab="goat_w2", xlab="usedFactor")
boxplot(sheep_w2~usedFactor, data=bvkde, main = "Sheep Used-Avail", ylab="sheep_w2", xlab="usedFactor")
## Now lets do for Elevation and Distance from Human Access2
par(mfrow = c(1,2))
boxplot(Elevation2~usedFactor, data=bvkde, main = "Elevation Used-Avail", xlab="usedFactor", ylab="elev")
boxplot(DistFromHumanAccess2~usedFactor, data=bvkde, main = "Human Access Used-Avail", ylab="elk_w2", xlab="usedFactor")

## boxplots - kde- used vs avail - Red Deer
rdkde <- subset(wolfkde, subset=pack=="Red Deer")
table(rdkde$used, rdkde$pack)
par(mfrow = c(2,3))
boxplot(deer_w2~usedFactor, data=rdkde, main = "Deer Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(elk_w2~usedFactor, data=rdkde, main = "Elk Used-Avail", ylab="elk_w2", xlab="usedFactor")
boxplot(moose_w2~usedFactor, data=rdkde, main = "Moose Used-Avail", ylab="moose_w2", xlab="usedFactor")
boxplot(goat_w2~usedFactor, data=rdkde, main = "Goat Used-Avail", ylab="goat_w2", xlab="usedFactor")
boxplot(sheep_w2~usedFactor, data=rdkde, main = "Sheep Used-Avail", ylab="sheep_w2", xlab="usedFactor")
## Now lets do for Elevation and Distance from Human Access2
par(mfrow = c(1,2))
boxplot(Elevation2~usedFactor, data=rdkde, main = "Elevation Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(DistFromHumanAccess2~usedFactor, data=rdkde, main = "Human Access Used-Avail", ylab="elk_w2", xlab="usedFactor")


# boxplots - used vs avail for each pack on same plot
par(mfrow = c(1,1))
boxplot(Elevation2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer Elevation Used-Avail")
boxplot(DistFromHumanAccess2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer Human Access Used-Avail")
boxplot(deer_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer DEER Used-Avail")
boxplot(moose_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer MOOSE Used-Avail")
boxplot(elk_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer ELK Used-Avail")
boxplot(goat_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer GOAT Used-Avail")
boxplot(sheep_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer SHEEP Used-Avail")


### SUMMARIES, STATS, ETC ####
library(dplyr)
library(tidyr)
esum <- data.frame(ddply(wolfkde, c("pack", "used"), summarize, 
                   mean=mean(Elevation2, na.rm=TRUE))) %>%
  mutate(Covariate = "Elevation")
hsum <- ddply(wolfkde, c("pack", "used"), summarize, 
              mean=mean(DistFromHumanAccess2, na.rm=TRUE)) %>%
  mutate(Covariate = "Dist Human")
elksum <- ddply(wolfkde, c("pack", "used"), summarize, 
                mean=mean(elk_w2, na.rm=TRUE)) %>%
  mutate(Covariate = "Elk")
dsum <- ddply(wolfkde, c("pack", "used"), summarize, 
              mean=mean(deer_w2, na.rm=TRUE)) %>%
  mutate(Covariate = "Deer")
gsum <- ddply(wolfkde, c("pack", "used"), summarize, 
              mean=mean(goat_w2, na.rm=TRUE)) %>%
  mutate(Covariate = "Goat")
msum <- ddply(wolfkde, c("pack", "used"), summarize, 
              mean=mean(moose_w2, na.rm=TRUE)) %>%
  mutate(Covariate = "Moose")
ssum <- ddply(wolfkde, c("pack", "used"), summarize, 
              mean=mean(sheep_w2, na.rm=TRUE)) %>%
  mutate(Covariate = "Sheep")

slxn <- rbind(esum,  hsum, elksum, dsum, gsum, msum, ssum) %>%
  group_by(Covariate, pack) %>%
  summarise(SI = log(mean[2]/mean[1])) %>%
  ungroup() %>%
  arrange(desc(abs(SI)))
slxnrd <- filter(slxn, pack == "Red Deer") %>%
  arrange(desc(SI))
slxnbv <- filter(slxn, pack == "Bow Valley") %>%
  arrange(desc(SI))

aesum <- data.frame(ddply(wolfkde, "used", summarize, 
                   mean=mean(Elevation2, na.rm=TRUE))) %>%
  mutate(Covariate = "Elevation")
ahsum <- ddply(wolfkde, "used", summarize, 
              mean=mean(DistFromHumanAccess2, na.rm=TRUE)) %>%
  mutate(Covariate = "Dist Human")
aelksum <- ddply(wolfkde, "used", summarize, 
                mean=mean(elk_w2, na.rm=TRUE)) %>%
  mutate(Covariate = "Elk")
adsum <- ddply(wolfkde, "used", summarize, 
              mean=mean(deer_w2, na.rm=TRUE)) %>%
  mutate(Covariate = "Deer")
agsum <- ddply(wolfkde, "used", summarize, 
              mean=mean(goat_w2, na.rm=TRUE)) %>%
  mutate(Covariate = "Goat")
amsum <- ddply(wolfkde, "used", summarize, 
              mean=mean(moose_w2, na.rm=TRUE)) %>%
  mutate(Covariate = "Moose")
assum <- ddply(wolfkde, "used", summarize, 
              mean=mean(sheep_w2, na.rm=TRUE)) %>%
  mutate(Covariate = "Sheep")
aslxn <- rbind(aesum, ahsum, aelksum, adsum, agsum, amsum, assum) %>%
  group_by(Covariate) %>%
  summarise(SI = log(mean[2]/mean[1])) %>%
  ungroup() %>%
  arrange(desc(abs(SI))) %>%
  mutate(pack = "All") %>%
  dplyr::select(Covariate, pack, SI) %>%
  bind_rows(slxn)
write.csv(aslxn, file = "selectivityindices.csv", row.names=F)

# plotting wolf locns and densities, faceted by pack
## First - revisiting plotting functions in ggplot2 - lets bring in old shapefile data with X and Y
wolfyht <-read.csv("wolfyht.csv", header = TRUE)
str(wolfyht)
# download the ggplot2 pdf manual and make sure you have the R Graphics Cookbook open too
# chapter 6 in R Graphics cookbook
# Data exploration
ggplot(wolfyht, aes(x=EASTING, y = NORTHING)) + 
  geom_point() + 
  stat_density2d() + 
  facet_grid(Pack ~ ., scales="free")
#~# facet is how you split separate packs in diff plots


## comparison to last week's results

# deciding whether to use SI or OR

  
