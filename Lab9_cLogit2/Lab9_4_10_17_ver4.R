
# WILD 562 - R code for LAB 9 - Step Selection Functions & Dynamic NDVI



### Mark Hebblewhite
### Daniel Eacker
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
packages <- c("sp","raster","rgdal","ggmap","survival","TwoStepCLogit","pbs","dplyr","lubridate","move","MASS","fdrtool","circular","CircStats", "coxme", "mclogit","data.table","scales")


#run function to install packages
ipak(packages)



##### 0.1 Preliminaries: setting working directory ####

## define working directory on each computer
wd_desktop <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\HabitatModelling\\Lab9_cLogit2"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\HabitatModelling\\Lab9_cLogit2"
## automatically set working directory depending which computer you're on
ifelse(file.exists(wd_laptop), setwd(wd_laptop), setwd(wd_desktop)) 
list.files()

rm(packages, wd_desktop, wd_laptop, ipak)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# load custom functions from BjÃ¶rn Reineking 
source("ssf_fun_2016_final.R", verbose = FALSE)



## Create ssf for MoveStack - multiple individuals 
# (note that this should follow the Björn Reineking code more 
# closely since that code was created for a MoveStack

# Read in data for all elk
yl.all <- read.csv("yl_all.csv",header=T)

# look at data
head(yl.all)

yl.all$elkuidF <- as.factor(yl.all$ELKUID)

#### Get to know our data graphically
ggplot(yl.all, aes(x=UTMX, y = UTMY, color = elkuidF)) + 
  geom_point()

#look at elk ids
levels(yl.all$elkuidF)
table(yl.all$elkuidF, yl.all$Year)
## all in 2003! 

#create time stamp
# first format time stamp to bring date and time together

# format Date as date class
yl.all$Date <- as.Date(yl.all$Date, format="%m/%d/%Y")

# make Time variable to standardize number of digits
new.hour <- numeric(length(yl.all$Hour))

#first fix hours
for(i in 1:length(yl.all$Hour)){
  if(nchar(substr(yl.all$Hour[i], 1, 2))==1) {new.hour[i] <- paste("0",yl.all$Hour[i],sep="")}else
    if(nchar(substr(yl.all$Hour[i], 1, 2))==2) {new.hour[i] <- yl.all$Hour[i]}
}

# make Time variable to standardize number of digits
new.minute <- numeric(length(yl.all$Minute))

#first fix hours
for(i in 1:length(yl.all$Minute)){
  if(nchar(substr(yl.all$Minute[i], 1, 2))==1) {new.minute[i] <- paste("0",yl.all$Minute[i],sep="")}else
    if(nchar(substr(yl.all$Minute[i], 1, 2))==2) {new.minute[i] <- yl.all$Minute[i]}
}

hr_min <- paste(new.hour,":",new.minute,sep="")

# now paste together Date and Time to make timestamp and convert to POSIXct format
yl.all$rtime <- as.POSIXct(paste(yl.all$Date,hr_min,sep=" "),  format="%Y-%m-%d %H:%M", tz="America/Denver")

# inspect first 6 elements of rtime
head(yl.all$rtime)

# order by timestamps
yl.all <- yl.all[order(yl.all$rtime),]

# order by individuals
yl.all <- yl.all[order(yl.all$ElkID),]

# create move stack 
all.mv <- move(x=yl.all$UTMX, y=yl.all$UTMY, 
                time=yl.all$rtime, 
                data=yl.all, proj=CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"), animal=yl.all$ElkID, removeDuplicatedTimestamps=F)


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# get distance step length and turn angle summaries
angle_dist <- prepare_angle_dist(all.mv)
# Inspect the data
summary(angle_dist)

dist <- angle_dist[,1]
rel.angle3 <- angle_dist[,2]

# examine empirical distances and turning angles
par(mfrow = c(1, 2))
hist(dist, breaks = 20, main = "", xlab = "Distance (m)")
hist(rel.angle3, breaks = seq(-pi, pi,len=11), main="", xlab="Relative angle (radians)")
#~# here we assume step length and turning angles
#~# are distributed the same for all elk


summary(dist)
## seems likely that the 35km movement in 2 hours is an 
# outlier, would need to screen out?


# Fit distributions to distances
fexp <- fitdistr(dist, "exponential")
fgam <- fitdistr(dist, "gamma")
flogn <- fitdistr(dist, "lognormal")

AICout <- AIC(flogn, fgam, fexp)
AICout <- cbind(AICout, c(0, diff(AICout$AIC)))
names(AICout) <- c("df", "AIC", "deltaAIC")
AICout

## So logn seems to be the best fitting function

par(mfrow = c(1,1))
hist(dist, breaks = 50, prob = TRUE, xlim = c(0, 8000), 
     ylim = c(0, 2e-3),
     xlab = "Step length (m)", main = "")
plot(function(x) dexp(x, rate = fexp$estimate), add = TRUE, 
     from = 0, to = 5000)
plot(function(x) dexp(x, rate = fexp$estimate/2), add = TRUE, 
     from = 0, to = 5000, col="purple")
plot(function(x) dgamma(x, shape = fgam$estimate["shape"], 
                        rate = fgam$estimate["rate"]), add = TRUE, 
     from = 0, to = 5000, col = "red")
plot(function(x) dlnorm(x, meanlog = flogn$estimate["meanlog"], 
                        sdlog = flogn$estimate["sdlog"]), add = TRUE, 
     from = 0, to = 5000, col = "blue")
plot(function(x) dhalfnorm(x, theta = 1/mean(dist)), add = TRUE, 
     from = 0, to = 5000, col = "green")
legend("topright", lty = 1, col = c("black", "purple", "red", "blue", "green"),
       legend = c("exp","exp/2" ,"gamma", "lnorm", "halfnorm"))

# Fit distributions to angles
fkappa <- est.kappa(rel.angle3)
fkappa

hist(rel.angle3, prob = TRUE, main = "", xlab = "Turning angle")
# use distribution von Mi
plot(function(x) dvonmises(x, circular(0), kappa=fkappa), 
     add = TRUE, from = -pi, to = pi, col = "red")
#~# shows quite a bit of directional presistence,
#~# not so many 180-degree turns

#~# PS - these data are already thinned for 2-hr step lengths

# set projection for estimation
utm_crs <- "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# run prepare_ssf_steps on all elk

# set the number of control locations
my_K <- 5

# set random number generating seed
set.seed(5)

# use prepare_ssf_steps to create the step selection function data
#source("prepare_ssf_steps_eacker.R")
ssf_data5 <- prepare_ssf_steps(all.mv, 
                                    method = "lognormal", K = my_K, 
                                    theta = flogn$estimate, kappa = fkappa,
                                    crs = utm_crs)
	

#set projection to UTMs
ssf_data6 <- spTransform(ssf_data5 , CRS("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))				

# create plot for all individuals

#tiff("ssf_all.tiff", res=600, compression = "lzw", height=5, width=5, units="in")
par(mfrow=c(1,1), mar = c(0.5, 0.5, 2, 0.5))

used1 <- ssf_data6[ssf_data6$used==1,]
used0 <- ssf_data6[ssf_data6$used==0,]
plot(used0, col="green",pch = 1, main="all individuals",axes=T)
plot(used1, col="red", add=T,pch = 1)
legend("topleft", legend=c("available", "used"), col = c("green","red"),pch = 1) 

# dev.off()


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
######## Objective 3.0 Build covariate dataset ####


#create an empty raster
mask.raster <- raster()

#set extent (note that I customized this extent so it 
# covered both elc_habitat and humanacess)
extent(mask.raster) <- c(xmin=443680.6, xmax=650430.4, 
                         ymin=5618405, ymax=5789236) 	

#set the resolution to 30 m 
res(mask.raster)<-30
			
#match projection to elc_habitat shapefile
projection(mask.raster)<- utm_crs
			
#set all values of mask.raster to zero
mask.raster[]<-0

# read in rasters
elevation<-raster("../Lab8_Mvmt-Clogit/Elevation2.tif") #resampled
disthumanaccess2<-raster("../Lab8_Mvmt-Clogit/DistFromHumanAccess2.tif") #resampled
disthhu<-raster("../Lab8_Mvmt-Clogit/DistFromHighHumanAccess2.tif")
landcover<-raster("../Lab8_Mvmt-Clogit/landcover16.tif") 

#  set the resolution to utm_crs above (same as ssf_data6) 
# and 're'project these covariate layers, note this will take 
# a while
elevation <- projectRaster(elevation, mask.raster)
disthumanaccess2 <- projectRaster(disthumanaccess2, mask.raster)
disthhu <- projectRaster(disthhu, mask.raster)
landcover <- projectRaster(landcover, mask.raster)

# check projection of elevation
elevation@crs@projargs

#  MAKE NDVI_ID variable

## Need to know the start and stop dates for each MODIS tile for 
# each MODIS product for the year in which you are extracting data

# check min and max dates of timestamp for ssf_data6 
# (a SpatialPointsDataFrame for all individuals ssf)
min(ssf_data6@data$date)
#"2003-04-15 02:00:00 MDT"

max(ssf_data6@data$date)
# "2003-10-14 20:00:00 MDT"

# extract Month from ssf_data6
ssf_data6@data$Month <- as.numeric(substr(ssf_data6@data$date, 6, 7))

# extract Day from ssf_data6
ssf_data6@data$Day <- as.numeric(substr(ssf_data6@data$date, 9, 10))

# create NDVI_ID 
ssf_data6@data$NDVI_ID<-with(ssf_data6@data, 
	ifelse(Month=="4" & Day<23, 7,
	ifelse(Month=="4" & Day>=23 | Month=="5" & Day<9, 8,
	ifelse(Month=="5" & Day>=9  & Day<25, 9,
	ifelse(Month=="5" & Day>=25 | Month=="6" & Day<10, 10,
	ifelse(Month=="6" & Day>=10  & Day<26, 11,
	ifelse(Month=="6" & Day>=26 | Month=="7" & Day<12, 12,
	ifelse(Month=="7" & Day>=12  & Day<28, 13,
	ifelse(Month=="7" & Day>=28 | Month=="8" & Day<13, 14,
	ifelse(Month=="8" & Day>=13  & Day<29, 15,
	ifelse(Month=="8" & Day>=29 | Month=="9" & Day<14, 16,
	ifelse(Month=="9" & Day>=14 & Day<30, 17,
	ifelse(Month=="9" & Day>=30 | Month=="10" & Day<16, 18,
	ifelse(Month=="10" & Day>=16 & Day<=31, 19,NA))))))))))))))

# examine NDVI_ID
table(ssf_data6@data$NDVI_ID)
table(ssf_data@data$Month, ssf_data@data$NDVI_ID)
#~# above verifies where data falls in NDVI id's
#~# makes diagonal, as expected

ssf_data6@data$NDVI_ID2 <- as.character(ssf_data6@data$NDVI_ID)

#first NDVI_ID to make 2 digit to match data
for(i in 1:length(ssf_data6@data$NDVI_ID)){
  if(nchar(substr(ssf_data6@data$NDVI_ID[i], 1, 2))==1) {ssf_data6@data$NDVI_ID2[i] <- paste("0",ssf_data6@data$NDVI_ID[i],sep="")}else
    if(nchar(substr(ssf_data6@data$NDVI_ID[i], 1, 2))==2) {ssf_data6@data$NDVI_ID2[i] <- ssf_data6@data$NDVI_ID[i]}
}


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
#read in 2003 NDVI Raster layers (note these have been resampled to 
# the resolution and extent and crs of the other rasters)


# read in NDVI rasters
NDVI_2003_07<-raster("2003_NDVI/NDVI_2003_07.tif") 
NDVI_2003_08<-raster("2003_NDVI/NDVI_2003_08.tif") 
NDVI_2003_09<-raster("2003_NDVI/NDVI_2003_09.tif") 
NDVI_2003_10<-raster("2003_NDVI/NDVI_2003_10.tif") 
NDVI_2003_11<-raster("2003_NDVI/NDVI_2003_11.tif") 
NDVI_2003_12<-raster("2003_NDVI/NDVI_2003_12.tif") 
NDVI_2003_13<-raster("2003_NDVI/NDVI_2003_13.tif") 
NDVI_2003_14<-raster("2003_NDVI/NDVI_2003_14.tif") 
NDVI_2003_15<-raster("2003_NDVI/NDVI_2003_15.tif") 
NDVI_2003_16<-raster("2003_NDVI/NDVI_2003_16.tif") 
NDVI_2003_17<-raster("2003_NDVI/NDVI_2003_17.tif") 
NDVI_2003_18<-raster("2003_NDVI/NDVI_2003_18.tif") 



# mask raster stack of NDVI tiles
NDVIstk <- stack(NDVI_2003_07, NDVI_2003_08, NDVI_2003_09, 
                 NDVI_2003_10, NDVI_2003_11, NDVI_2003_12, 
                 NDVI_2003_13, NDVI_2003_14, NDVI_2003_15, 
                 NDVI_2003_16, NDVI_2003_17, NDVI_2003_18)

plot(NDVIstk)

## Note that the scale for NDVI is the typical range from -0.2 to 1.0, following Pettorelli, which scales non-vegetated communities from ~ -0.2 to 0, and growing vegetation from 0 to 1. Not all quesitons should use rescaled NDVI. 

# examine CRS of raster files 
NDVIstk@crs@projargs

# average over stacked NDVI tiles (note this takes ~ 1 minutes)
meanNDVI <- mean(NDVIstk, na.rm=T)

# # resample mean NDVI to extent and resolution and projection of other layers
meanNDVI <- projectRaster(meanNDVI, mask.raster)
# ## note this takes a while


# get values for meanNDVI
meanNDVI@data@values <- getValues(meanNDVI)

# look at mean NDVI values plot
plot(meanNDVI)

#now try stacking them
all_base_rasters <- stack(elevation, disthumanaccess2, disthhu, landcover, meanNDVI)

# extract covariates
cov.ext<-extract(all_base_rasters, ssf_data6)
head(cov.ext)
str(cov.ext)

# add extracted covariates to ssf_data
ssf_data6@data$elevation <- cov.ext[,1]
ssf_data6@data$distha <- cov.ext[,2]
ssf_data6@data$disthha <- cov.ext[,3]
ssf_data6@data$landcover <- cov.ext[,4]
ssf_data6@data$meanNDVI <- cov.ext[,5]

# look at first 6 rows of data
head(ssf_data6@data)

# look at histogram of mean NDVI values
hist(ssf_data6@data$meanNDVI)
		  
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# Extract NDVI values for time-dependent	

# split data by NDVI_ID
temp_data <- split(ssf_data6, ssf_data6@data$NDVI_ID)

# create empty list to hold output
timeNDVI <- vector("list",length(temp_data))

# extract time dependent variables by looping over data listed by NDVI period
for(i in 1:length(temp_data)){
	
	timeNDVI[[i]] <- extract(NDVIstk[[i]], temp_data[[i]])
	timeNDVI[[i]] <- as.data.frame(timeNDVI[[i]])

}

# okay now row bind list and create variable
temp <- as.vector(as.data.frame(rbindlist(timeNDVI)))
ssf_data6@data <- cbind(ssf_data6@data,temp[,1])
names(ssf_data6@data)[17] <- "timeNDVI"

# change ssf_data6 to ssf_data
ssf_data <- ssf_data6

### Add landcover categories to ssf_data # note that you shouldn't need this landcover.legend file
#setwd("/Users/mark.hebblewhite/Dropbox/WILD 562/Spring2017/Lab8/")
#landcover.legend <- read.csv("landcover_legend.csv", header = TRUE, sep = ",")
#landcover.legend 

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


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# create regular data frame with data
ssf_data <- data.frame(ssf_data@data)

# subset NAs out of data for model fitting and selection

# check for NA

length(which(is.na(ssf_data$meanNDVI)==T))
length(which(is.na(ssf_data$elevation)==T))
length(which(is.na(ssf_data$distha)==T))
length(which(is.na(ssf_data$disthha)==T))
length(which(is.na(ssf_data$landcover.cat)==T))
length(which(is.na(ssf_data$timeNDVI)==T))

# looks like just landcover is an issue
# subset out NA's for landcover (i.e., remove rows so data frames are consistent across models)

indexes.to.keep <- which(is.na(ssf_data$landcover.cat)==F)
ssf_data <- ssf_data[indexes.to.keep, ]

#alternatively we could have done
# ssf_data <- ssf_data[is.na(ssf_data$landcover.cat)==F,]

# check that NA's are removed from landcover
length(which(is.na(ssf_data$landcover.cat)==T))


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
#### Objective 4.0 FIT step selection function Clogit model with Dynamic NDVI ####

## first make sure we understand the stratum field
head(table(ssf_data$stratum, ssf_data$used))

### So, there are 5 matched case availabile locations for 
# each 1 used location. It is this stratum that distinguishes 
# clogit from logistic regression. 

## start with univariate model for mean NDVI
m.elk.meanNDVI <- clogit(used ~ meanNDVI + strata(stratum), 
                         data = ssf_data)
summary(m.elk.meanNDVI)	   

#### Compare to naive logistic regression model ignoring structure
elk.meanNDVI <- glm(used ~ meanNDVI, data = ssf_data, 
                    family = binomial(logit))
summary(elk.meanNDVI)	

## so elk do not select for NDVI all summer long, but 
# show 'more' selection at step scale. Next compare to 
# time-varying NDVI

## start with univariate model for time-varying NDVI
m.elk.timeNDVI <- clogit(used ~ timeNDVI + strata(stratum), 
                         data = ssf_data)
summary(m.elk.timeNDVI)	   

#### Compare to naive logistic regression model ignoring structure
elk.timeNDVI <- glm(used ~ timeNDVI, data = ssf_data, 
                    family = binomial(logit))
summary(elk.timeNDVI)	

## Whereas, with time-varying NDVI, elk show the strongest 
# avoidance of high NDVI at the step scale, and about half 
# as strong avboidance of NDVI at the naive logit, unconditioned 
# scale. 

ssf_data$pred.m.elk.timeNDVI <- predict(m.elk.timeNDVI, 
                                        type="expected")
hist(ssf_data$pred.m.elk.timeNDVI)

ssf_data$pred.elk.timeNDVI <- predict(elk.timeNDVI, 
                                      type="response")
hist(ssf_data$pred.elk.timeNDVI)

plot(ssf_data$pred.m.elk.timeNDVI, ssf_data$pred.elk.timeNDVI)
abline(lm(ssf_data$pred.m.elk.timeNDVI~ ssf_data$pred.elk.timeNDVI), 
       col = "red")
cor(ssf_data$pred.m.elk.timeNDVI, ssf_data$pred.elk.timeNDVI)

### so once again there is a positive but not 1:1 relationship 
# between the conditional and naive predictions from even a 
# simple model.

### Now lets compare mean and time-varying from the clogit models. 
ssf_data$pred.m.elk.meanNDVI <- predict(m.elk.meanNDVI, 
                                        type="expected")
hist(ssf_data$pred.m.elk.meanNDVI)

plot(ssf_data$pred.m.elk.timeNDVI, ssf_data$pred.m.elk.meanNDVI)
abline(lm(ssf_data$pred.m.elk.timeNDVI~ ssf_data$pred.m.elk.meanNDVI), col = "red")
cor(ssf_data$pred.m.elk.timeNDVI, ssf_data$pred.m.elk.meanNDVI)

### Wow! no relationship whatsoever?!

### Now lets consider more meaningful models. 

### Compare clogit and naive logit for next variables. 
#### Compare Predictions from full models with and without 
# mean/time NDVI

mean.full  <- clogit(used ~ meanNDVI + elevation + distha + disthha + I(landcover.cat) +strata(stratum), data = ssf_data)
summary(mean.full)

time.full  <- clogit(used ~ timeNDVI + elevation + distha + disthha + I(landcover.cat) +strata(stratum), data = ssf_data)
summary(time.full)

#### You can now get predictions from a Clogit model, but the predictions 
# mean something very different here
#### The Cox model is a relative risk model; predictions of 
# type "linear predictor", "risk", and "terms" are all relative to the sample 
# from which they came. By default, the reference value for each of these is the
# mean covariate within strata. The primary underlying reason is statistical: 
# a Cox model only predicts relative risks between pairs of subjects within the 
# same strata, and hence the addition of a constant to any covariate, either overall 
# or only within a particular stratum, has no effect on the fitted results. Using 
# the reference="strata" option causes this to be true for predictions as well.
ssf_data$mean.full <- predict(mean.full, type="expected")
ssf_data$time.full <- predict(time.full, type="expected")

par(mar=c(2.5,2.5,2.5,2.5))
plot(ssf_data$mean.full ~ ssf_data$time.full)
abline(lm(ssf_data$time.full~ ssf_data$mean.full), col="red")
cor(ssf_data$mean.full, ssf_data$time.full)
#~# i'm not convinced the above keeps X and Y correct

#### plotting predictions

ggplot(ssf_data, aes(x=timeNDVI, y = time.full)) + 
  stat_smooth(method="glm", method.args = list(family="binomial"))
ggplot(ssf_data, aes(x=meanNDVI, y = mean.full)) + 
  stat_smooth(method="glm", method.args = list(family="binomial"))

#### MOdel Selection for cLogit Models with dynamic variables

AIC(mean.full, time.full, m.elk.timeNDVI, m.elk.meanNDVI)
## Time-varying NDVI always outperforms. 

####### Objective 5.0 Matched-case control over multiple individual - Mixed-effects Clogit ####

library(mclogit)
head(ssf_data)
str(ssf_data) ## need a numeric id field and stratum field
levels(ssf_data$id)

ssf_data$elkid = ifelse(ssf_data$id == "GP2", 1, 
                                     ifelse(ssf_data$id == "yl2", 2, 
                                            ifelse(ssf_data$id == "yl25", 25, 
                                                   ifelse(ssf_data$id == "yl29", 29,
                                                          ifelse(ssf_data$id == "yl42", 42, 5)))))
                                                                 
str(ssf_data)
table(ssf_data$elkidF)

ssf_data$stratum2 <- 0
ssf_data$stratum2 <- factor(ssf_data$stratum)
ssf_data$stratum2 <- as.numeric(ssf_data$stratum2)
table(ssf_data$stratum2)
table(ssf_data$stratum)


elk.timeNDVI.mclogit <- mclogit(cbind(used, stratum2) ~meanNDVI, 
                                random =~1|elkid, data=ssf_data)
#~# to be continued...
#~# error: insufficient residual variance
#~# works without the random effect
summary(elk.timeNDVI.mclogit)

# test <- ssf_data
# test$elkid <- as.factor(test$elkid)
# elk.timeNDVI.mclogit <- mclogit(cbind(used, stratum2) ~meanNDVI, 
#                                 random =~1|elkid, data=test)

test <- mclogit(cbind(used, stratum2) ~meanNDVI, data=ssf_data)
summary(test)

#~# time-varying covs help explain rsc selxn with step selxn especially




library(coxme)
#make faketime variable to trick cox proportional hazards model 
# that time is irrelevant in your conditional logistic model
ssf_data$faketime <- ifelse(ssf_data$used == 0, 2, 1) #2 for control, 1 for case
table(ssf_data$faketime, ssf_data$used)

test2 <- coxme(Surv(faketime,used)~ timeNDVI + (1|elkid) + 
                 strata(stratum2), ties = "efron",data=ssf_data)
