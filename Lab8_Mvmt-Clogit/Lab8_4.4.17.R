
# WILD 562 - R code for LAB 8 - Step Selection Functions


### Mark Hebblewhite
### Adapted from code written
#'author: Björn Reineking 
#'email: bjoern.reineking@irstea.fr

####' Step selection functions: Literature
#' ========================================================
#' - Forester et al. (2009) Ecology 90: 3554-3565
#' - Potts et al. (2014) J. R. Soc. Interface 11: 20140333
#' - Avgar et al. (2016) Methods in Ecology and Evolution 7: 619-630

#function to install and load required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#load or install these packages:
packages <- c("sp","raster","rgdal","ggmap","survival","TwoStepCLogit","pbs","dplyr","lubridate","move","MASS","fdrtool","circular","CircStats", "coxme", "mclogit")


#run function to install packages
ipak(packages)

##### 0.1 Preliminaries: setting working directory #######################################################################

## define working directory on each computer
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\HabitatModelling\\Lab8_Mvmt-Clogit"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\HabitatModelling\\Lab8_Mvmt-Clogit" 

## automatically set working directory depending which computer you're on
ifelse(file.exists(wd_workcomp), setwd(wd_workcomp), setwd(wd_laptop)) 
getwd()
list.files() ## handy command to see what is inside the working directory

#~# clean shit up
rm(packages, wd_desktop, wd_laptop, wd_workcomp)


# Let's just work with elk 29 to start out with, and then we will do 
#the entire dataset

# read in csv file
yl29 <- read.csv("yl29.csv", header=T)

# inspect the dataset
head(yl29)

## Plot the data
ggplot(yl29, aes(x=UTMX, y = UTMY)) + geom_point()
ggplot(yl29, aes(x=Date, y= UTMY)) + geom_point()
ggplot(yl29, aes(x=Date, y= UTMY)) + geom_point()
### What date did this elk migrate?

## ## ## ##
# load custom functions from BjÃ¶rn Reineking 
source("ssf_fun_2016_final.R", verbose = FALSE)

#~# see Day5_Bjorn_ssf_animove_2016_final.html 
# to allow selxn for ndvi to vary as fcn of hour of day
# using GAM smoother
# and other stuff
# it's rmarkdown from one day of the animove workshop

#~# now we have a bunch of sweet fcns he wrote

## ## ## ##
# create "move" object using move package

# first format time stamp to bring date and time together

# format Date as date class
yl29$Date <- as.Date(yl29$Date, format="%m/%d/%y")

# make Time variable to standardize number of digits
yl29$Time <- as.character(yl29$Time)

new.time <- numeric(length(yl29$Time))

for(i in 1:length(yl29$Time)){
  if(nchar(substr(yl29$Time[i], 1, 5))==4) {new.time[i] <- paste("0",yl29$Time[i],sep="")}else
    if(nchar(substr(yl29$Time[i], 1, 5))==5) {new.time[i] <- yl29$Time[i]}
}
#~# creates a list of times that were recorded by elk collar
# the ifthen craziness just adds a 0 in front of the ones
# that are only one-digit hours rather than 2
# seems like could've done this with a posix object...


# now paste together Date and Time to make timestamp and convert to POSIXct format
yl29$rtime <- as.POSIXct(paste(yl29$Date,new.time, sep=" "),  
                         format="%Y-%m-%d %H:%M", tz="UMT")
#~# tz specifies time zone, so make sure you have it right...


### Make the yl29 data a Move object or MoveStack (multiple Move objects, i.e., animals)
### ?move
yl29.mv <- move(x=yl29$UTMX, y=yl29$UTMY, 
                time=yl29$rtime, 
                data=yl29, proj=CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"), animal=yl29$ELKUID)
#~# move object incorporates x, y, and t
# time must be posixct; can convert within argument


# visualize the movement data
par(mfrow=c(1,1))
plot(yl29.mv) ## note you can do this with the Move Object
#~# this "plot" is from move package specifically


yl29.mv2 <- spTransform(yl29.mv, CRS("+init=epsg:4326"))
#~# still a move object
# rprojected bc move wants it to be latlong or something
# basically we jsut changed the extent
yl29.mv2 ## investigate what you just created by making this a sp object
str(yl29.mv2)
head(yl29.mv2)
#~# this is "3d move obj" now
# but head just shows you the data part

# look at time lag between successive locations
head(timeLag(yl29.mv, units="hours")) # just gives avgs
hist(timeLag(yl29.mv, units="hours")) # shows all the time lags
summary(timeLag(yl29.mv, units="hours")) # distn
## So most of the data are 2 hours, but there are some missed 
# locations (GPS error) that we need to think about.  

# set step duration to thin movement data into <3 hour intervals
step_duration <- 3

# extract distances and turn angles (in degrees)
dist <- distance(yl29.mv2)
hist(dist)
#~# note this is prob related to time lab
abs.angle <- angle(yl29.mv2)
#~# angle() is from move pkg - 
# extract absolute angle in degrees
# note x axis automatically goes to 200,
# would need to manually constrict it to 180

# determine index positions of data to keep that have less than a 3 hour time lag between locations
thinned_index <- which(timeLag(yl29.mv2, units="hours") < step_duration)

# thin observations where timeLag is < 3 hours
dist2 <-  dist[thinned_index]
abs.angle2 <-  abs.angle[thinned_index]

#~# below fcn is from bjorn
# note that 1 radian is just under 57.3 degrees
# note that this function does not seem to work right
#degrees2radians <-function(x) {
#  -2*pi*(x-90)/360
#}

#This function seems correct (a negative of an angle should be the same
#radians as the positive of the angle just with an opposite sign.)
## The function is currently defined as
degrees2radians <- function (x) 
{
  radians <- (x * (pi/180))
  return(radians)
}

abs.angle3 <- degrees2radians(abs.angle2)

# I also think this is unnecesary; the resulting degrees2 radians is within
#within pi and -pi
# wrap_rel_angle <- function(x) {
# wraps relative angles to the range from -pi to pi
#  ifelse(abs(x) > pi, sign(x) * (abs(x) - 2 * pi), x)
#}
rel.angle3 <- ifelse(abs(abs.angle3) > pi, 
                     sign(abs.angle3) * (abs(abs.angle3) - 2 * pi), 
                     abs.angle3)
hist(rel.angle3)
#~# converted from absolute radians 
# (which go from negative pi to pi)
# to relative radians
# but this is apparently the same as absolute
# also now we're all confused

# basic summary of angles (in degrees) and distances (in meters) 
# between successive locations
angle_dist_summary <- data.frame(cbind(summary(dist), summary(rel.angle3)))
names(angle_dist_summary) <- c("distance", "relative angle (radians)")
angle_dist_summary

# examine empirical distances and turning angles
par(mfrow = c(1, 1))
hist(dist2, breaks = 20, main = "", xlab = "Distance (m)")
hist(abs.angle3, breaks = 20, main="", xlab="Angle (radians)")
hist(rel.angle3, breaks = seq(-pi, pi,len=11), main="", xlab="Relative angle (radians)")

par(mfrow = c(1, 3))
hist(dist2, breaks = 20, main = "", xlab = "Distance (m)")
hist(abs.angle3, breaks = 20, main="", xlab="Angle (radians)")
hist(rel.angle3, breaks = seq(-pi, pi,len=11), main="", xlab="Relative angle (radians)")
#~# note these distns are pretty common for mvmt
# super r-skewed distance moved distn
# super noisy flat-looking turning angle distn

# Fit distributions to distances
fexp <- fitdistr(dist2, "exponential")
fgam <- fitdistr(dist2, "gamma")
flogn <- fitdistr(dist2, "lognormal")

par(mfrow = c(1,1))
#~# dist2 are our measured distances
#~# compare them to the distns defined above
hist(dist2, breaks = 50, prob = TRUE, xlim = c(0, 8000), 
     ylim = c(0, 2e-3),xlab = "Step length (m)", main = "")
plot(function(x) dexp(x, rate = fexp$estimate), add = TRUE, 
     from = 0, to = 5000)
plot(function(x) dgamma(x, shape = fgam$estimate["shape"], 
                        rate = fgam$estimate["rate"]), add = TRUE, 
     from = 0, to = 5000, col = "red")
plot(function(x) dlnorm(x, meanlog = flogn$estimate["meanlog"], 
                        sdlog = flogn$estimate["sdlog"]), add = TRUE, 
     from = 0, to = 5000, col = "blue")
plot(function(x) dhalfnorm(x, theta = 1/mean(dist2)), add = TRUE, 
     from = 0, to = 5000, col = "green")
legend("topright", lty = 1, col = c("black", "red", "blue", "green"),
       legend = c("exp", "gamma", "lnorm", "halfnorm"))
#~# "now it's just an eyeball fest"
# can use aic or other model fit test to pick best fit
# or just eyeball it apparently...


# Fit distributions to angles
fkappa <- est.kappa(abs.angle3)
fkappa

hist(abs.angle3, prob = TRUE, main = "", xlab = "Turning angle")
# use distribution von Mi
plot(function(x) dvonmises(x, circular(0), kappa=fkappa), add = TRUE, 
     from = -pi, to = pi, col = "red")


## ## ## ##
#### Objective 2.0 Create match case control data points ####

utm_crs <-CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

#Control steps: sample with one of the following distance distributions: exponential, 
#gamma, lognormal, halfnormal, potentially drawing turning angles from a von Mises 
#distribution. We will use an lognormal distribution here because it seems to fit the 
#data well (the gamma and exponential seem to fit pretty close as well). 
#We will use a radially symmetric proposal 
#distribution because it is easier to interpret parameter estimates for 
#cos(rel.angle), and because in an earlier analysis on these data it showed less 
#residual autocorrelation (note that this was not checked for the elk dataset).	 

# set the number of control locations
my_K <- 5

# set random number generating seed
#~# maybe change this later
# currently matches mark's to make sure i dont eff something up
set.seed(50)

# use prepare_ssf_steps to create the step selection function data
#~#below is from bjorn
## can chack out his code to see the fcn
ssf_data_lnorm <- prepare_ssf_steps(yl29.mv2, 
                                    method = "lognormal", K = my_K, 
                                    theta = flogn$estimate / 2, 
                                    crs = utm_crs)
#~# created spatial pts df
# this is dataset of random pts
# (n = n(data)*5 bc myK = 5)
# stratum = day and time
# 1 used : 5 unused locs per stratum


# Transform to UTMs									
yl29.mv3 <- spTransform(yl29.mv2, CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))							

# check control and case points graphically
#~# here we're plotting one day
# 7 1st point, 8 2nd point, 9 3rd point
# labels used pt as 1; others are 0 under used
ssf_data <- ssf_data_lnorm
dat <- subset(ssf_data, stratum == "X29_0_9")
dat0 <- subset(ssf_data, stratum == "X29_0_7" & used == 1)
plot(rbind(dat, dat0), axes = TRUE)

points(subset(dat, used == 0), col ="black")
points(subset(dat, used == 1), col ="red")
points(subset(ssf_data, stratum == "X29_0_8" &
                used == 1), col ="blue")
points(subset(ssf_data, stratum == "X29_0_7" &
                used == 1), col ="green")
dat3 <- as.data.frame(subset(ssf_data, 
                             stratum %in% c("X29_0_7", "X29_0_8") &
                               used == 1))
dat2 <- as.data.frame(subset(ssf_data, 
                             stratum %in% c("X29_0_8", "X29_0_9") &
                               used == 1))
lines(dat2$x, dat2$y, col ="red")
lines(dat3$x, dat3$y, col ="blue")
legend("topright", col = c("green", "blue", "red"), pch = 1, 
       legend = c("t-1", "t", "t+1"))
#~# points not connected by lines are the "available" ones


#### This just makes a plot of 1 example case

## ## ## ##
######## Objective 3.0 Build covariate dataset ####
#### match data for raster layer covariate

#### Note I have cleaned up all these GIS files and they are in the 
# same extent and projection using resample()
#### See lab 1 and 4 for details. 

elevation<-raster("Elevation2.tif") #resampled
disthumanaccess2<-raster("DistFromHumanAccess2.tif") #resampled
disthhu<-raster("DistFromHighHumanAccess2.tif")
landcover<-raster("landcover.tif") # DAN it would be great if you could help 
# me figure out an easier way to associate the 'legend' with the ID's in this file - 
# the file comes in with an empty list in landcover@data@attributes, and the code 
# from lab 4 didnt quite work tonight so I punted. 

all_rasters <- stack(elevation, disthumanaccess2, disthhu, landcover)

ssf_data2 <- spTransform(ssf_data, CRS("+proj=utm +zone=11 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

all_points<-SpatialPoints(coordinates(ssf_data2), 
                          proj4string=CRS("+proj=utm +zone=11 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

# extract covariates
cov.ext<-extract(all_rasters, all_points)
head(cov.ext)
str(cov.ext)

# add extracted covariates to ssf_data
ssf_data@data$elevation <- cov.ext[,1]
ssf_data@data$distha <- cov.ext[,2]
ssf_data@data$disthha <- cov.ext[,3]
ssf_data@data$landcover <- cov.ext[,4]
head(ssf_data)

### Add landcover categories to ssf_data
landcover.legend <- read.csv("landcover_legend.csv", header = TRUE, sep = ",")
landcover.legend 

ssf_data@data$landcover.cat = ifelse(ssf_data@data$landcover == 0, "NA", 
       ifelse(ssf_data@data$landcover == 1, "Open Conifer", 
          ifelse(ssf_data@data$landcover == 2, "Moderate Conifer", 
             ifelse(ssf_data@data$landcover == 3, "Closed Conifer", 
                ifelse(ssf_data@data$landcover == 4, "Deciduous", 
                   ifelse(ssf_data@data$landcover == 5, "Mixed", 
                      ifelse(ssf_data@data$landcover == 6, "Regen", 
                        ifelse(ssf_data@data$landcover == 7, "Herbaceous",                                   
                           ifelse(ssf_data@data$landcover == 8, "Shrub",                               
                              ifelse(ssf_data@data$landcover == 9, "Water", 
                                 ifelse(ssf_data@data$landcover == 10, "Rock-Ice", 
                                    ifelse(ssf_data@data$landcover == 11, "Cloud", 
                                         ifelse(ssf_data@data$landcover == 12, "Burn-Forest",                               
                                               ifelse(ssf_data@data$landcover == 13, "Burn-Grassland", 
                                                    ifelse(ssf_data@data$landcover == 14, "Burn-Shrub", 
                                                           ifelse(ssf_data@data$landcover == 15, "Alpine Herb", "Alpine Shrub"))))))))))))))))
table(ssf_data$landcover.cat, ssf_data$used)
head(ssf_data)


#### Objective 4.0 FIT step selection function Clogit model ####

## first make sure we understand the stratum field
str(ssf_data)
#~# not currently organized by stratum
head(ssf_data)
head(sort(ssf_data$stratum))
#~# this would put them in order
head(table(ssf_data$stratum, ssf_data$used))
#~# shows that for each stratum, there are 5 avail and 1 used

### So, there are 5 matched case availabile locations for each 1 used location. 
# It is this stratum that distinguishes clogit from logistic regression. 

## start with univariate models 
m.elev <- clogit(used ~ elevation + strata(stratum), data = ssf_data)
summary(m.elev)	
#~# reports exponentiated (OR) coeffs and regular betas
#~# elev not affecting this elk's mvmts (bc p=0.8)

#### Compare to naive logistic regression model ignoring structure
#~# re-evaluate matched case-control ignoring the conditioning
elev <- glm(used ~ elevation, data = ssf_data, family = binomial(logit))
summary(elev)	
#~# now response is " one zero weaker"
#~# diff coeff reflects diff in question we asked


### Compare clogit and naive logit for next variables. 
### Distance to human access
m.distha <- clogit(used ~ distha + strata(stratum), data = ssf_data)
summary(m.distha)
#~# doesn't give a shit abt human activity given availability

distha <- glm(used ~ distha, data = ssf_data, family = binomial(logit))
summary(distha)

#### Distance to high human access
m.disthha <- clogit(used ~ disthha + strata(stratum), data = ssf_data)
summary(m.disthha)

disthha <- glm(used ~ disthha, data = ssf_data, family = binomial(logit))
summary(disthha)
# taking into acct human activity elk's exposed to
# showing stronger response on conditional basis 
# than over entire summer


#### Landcover
m.landcov <- clogit(used ~ I(landcover.cat) + strata(stratum), data = ssf_data)
summary(m.landcov)

landcov  <- glm(used ~ I(landcover.cat), data = ssf_data, family = binomial(logit))
summary(landcov)

#### Discuss interpretation in class, but fair enough comparison is a bit lame 
# because nothing is turning out significant. 

#### Compare Predictions from full models

m.full  <- clogit(used ~ elevation + distha + disthha + I(landcover.cat) +strata(stratum), data = ssf_data)
summary(m.full)

full  <- glm(used ~ elevation + distha + disthha + I(landcover.cat), data = ssf_data, family = binomial(logit))
summary(full)

#### You can now get predictions from a Clogit model, but the predictions 
# mean something very different here
#### The Cox model is a relative risk model; predictions of type "linear predictor", 
# "risk", and "terms" are all relative to the sample from which they came. 
# By default, the reference value for each of these is the mean covariate within 
# strata. The primary underlying reason is statistical: a Cox model only predicts 
# relative risks between pairs of subjects within the same strata, and hence the 
# addition of a constant to any covariate, either overall or only within a 
# particular stratum, has no effect on the fitted results. Using the 
# reference="strata" option causes this to be true for predictions as well.
#~# predictions from glm uses mean across whole dataset
#~# cox predicts conditional on strata
ssf_data$m.full.pred <- predict(m.full, type="expected")
hist(ssf_data$m.full.pred)

ssf_data$full.pred <- predict(full, type="response")

plot(ssf_data$full.pred, ssf_data$m.full.pred)
abline(lm(ssf_data$full.pred~ ssf_data$m.full.pred))
cor(ssf_data$full.pred, ssf_data$m.full.pred)
#~# xaxis = naive predxns
#~#yaxis = conditional predictions, conditioned on ea strata
# correlation coeff only about 0.4, not so great


## Discussion: what to make of the different interpretations of Clogit 
# versus Logit for the same dataset?
#~# think of this in light of mark's love for french fries



#### MOdel Selection for cLogit Models

## fit null model
#~# a null model has no intercept
m.null  <- clogit(used ~ strata(stratum), data = ssf_data)
summary(m.null)
#~# no can do in clogit!

AIC(m.full, m.landcov, m.disthha, m.distha, m.elev, m.null)


stepAIC(m.full, direction = "backward")
#~# error bc it wants to kick out all the parameters
# but cox models can't handle that

## So - you cannot fit models with 0 parameters in Coxph model
AIC(m.full, m.landcov, m.disthha, m.distha, m.elev, full)
## note you get an error message, cannot compare to the Naive Logit model for 2 reasons
## 1) differnet # of data points (why?)
## 2) different calculation of the log-likelihood - confiditonal likelihood vs. maximum likelihood. apples and oranges. 
## Cannot solve whether or not to add conditioning based on AIC. 
#~# can't compare clogit (bc estimated by conditional partial ilkelihood)
# with glm bc fit using maximimum likelihood
# can never compare those

####### Objective 4.0 Matched-case control over multiple individual - Mixed-effects Clogit

##### 1) Here are the first 2 papers that figured out how to add a random intercept for each individual animal (e.g.), however, it did so in MATLAB. So, its mostly inaccessible to biologists. 
### Craiu, R. V., T. Duchesne, D. Fortin, and S. Baillargeon. 2011. Conditional Logistic Regression With Longitudinal Follow-up and Individual-Level Random Coefficients: A Stable and Efficient Two-Step Estimation Method. Journal of Computational and Graphical Statistics 20:767-784.
### Duchesne, T., D. Fortin, and N. Courbin. 2010. Mixed conditional logistic regression for habitat selection studies. Journal of Animal Ecology 79:548-555.

### 2) However, there have been a few big breakthrough’s lately with the 
# mclogit package http://cran.r-project.org/web/packages/mclogit/mclogit.pdf  
# or the coxme package here http://cran.r-project.org/web/packages/coxme/coxme.pdf 
# I just played around with both of these packages and they are actually. 
### I figured out the mclogit package just now, and, for example, provide example 
# code here. Imagine instead of using just elk29’s data in our analysis, we had 
# 2 elk, identified by an elkid field. I created 1 at random in the wolf id field 
# to keep track of the 2 wolf packs in the original mccwolf data. 

install.packages(c("coxme", "mclogit"))
library(coxme)
library(mclogit)

##### Bring in Bison dataset 

##### This data set was collected in order to study habitat selection by groups of 
# free-ranging bison. For each observed group, 
#### two individuals (dyad) equipped with GPS radio-collars were followed 
# simultaneously. A cluster is defined here as a pair of bison. 
#### This data set contains 20 clusters. The number of strata per cluster varies 
# between 13 and 345 for a total of 1410 strata. 
### A stratum is composed of two visited GPS locations (one for each individual) 
# gathered at the same time, together with 10 random locations 
### (five drawn within 700 m of each of the two focal bison). Therefore, there are 
# 12 observations per stratum, with 2 cases (Y=1) and 10 controls 
### (Y=0). However, due to problems in the data collection, 17 of the 1410 strata 
# have only 6 observations (1 case and 5 controls).

install.packages("TwoStepCLogit")
library(TwoStepCLogit)
head(bison)
str(bison)
head(table(bison$Strata,bison$Cluster))
hist(bison$biomass)
hist(bison$meadow)
boxplot(bison$biomass~ bison$meadow)

#~# Y is used variable within strata
#~# note comma instead of plus on the random effect
#~# cluster are animals
bison.mcclogit <- mclogit(cbind(Y, Strata) ~pmeadow + biomass, random ~1|Cluster, 
                          data=bison)
summary(bison.mcclogit)
#~# as ppn meadow incs, prob of animals selecting it decs
# variance in cluster "not much to sneeze about"
# i.., not significant within-cluster variation
# so do you need the random effect of bison?

bison.mcfixed <- mclogit(cbind(Y, Strata) ~pmeadow + biomass, data=bison)
summary(bison.mcfixed)
#~# not big diff in coeffs

bison.naive <- glm(Y ~ pmeadow + biomass, data = bison, family = binomial(logit))
summary(bison.naive)
#~# this ones the unconditional model
# much diff coeff of selection for meadow now
# on avg over summer, bison show stronger avoidance of meadow than they do
# on hourly timescale. show stronger selxn for biomass over whole summer
# that at any given timestep

AIC(bison.mcclogit, bison.mcfixed)

AIC(bison.naive)
#### Note that the AIC's are not comparable!!

bison$mcclogit <- predict(bison.mcclogit, response="expected")
bison$mcfixed <- predict(bison.mcfixed, response="expected")
bison$naive <- predict(bison.naive, type="response")
str(bison)

plot(bison$mcclogit, bison$naive)
plot(bison$mcclogit, bison$mcfixed)
#~# mcclogit on x acis not constrained bt 0 and 1
# so not true probability
# bc using that cox model parameter stuff
# which fits using cox 
# which basically uses the numerator of the rsf

