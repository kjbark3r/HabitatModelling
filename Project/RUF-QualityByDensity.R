# # # # # # # #  # # # # # # # # # # # # # # # # 
# INTENSITY OF ELK USE AND AVAILABLE NUTRITION #
#               INDIV-BASED RUF                #
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

library(VGAM) #vglm(), zero-truncated poisson/negbin
library(MASS) #glm.nb(), negbin
library(lme4)
library(adehabitatHR)
library(AICcmodavg) 
library(raster)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)


## projection definition ##

latlong <- CRS("+init=epsg:4326") # WGS84 projection


## "raw" data ##

rawlocs <- read.csv("../../ElkDatabase/collardata-locsonly-equalsampling.csv")
rawde14 <- raster("../../Vegetation/DE2014.tif")
rawde15 <- raster("../../Vegetation/DE2015.tif")
migstatus <- read.csv("../../Nutrition/migstatus.csv")
mig <- dplyr::select(migstatus, c(IndivYr, MigStatus))


## tweaked data ##

## locs - format dates and times; add IndivYr ##

locs <- rawlocs
locs$Date <- as.Date(locs$Date)
locs$Time <- as.numeric(gsub("[[:punct:]]", "", locs$Time))
locs$IndivYr <- ifelse(locs$Date < "2015-01-01", 
                       paste(locs$AnimalID, "-14", sep=""),
                       paste(locs$AnimalID, "-15", sep=""))  
locs <- left_join(locs, mig, by = "IndivYr")
locs <- locs[!is.na(locs["MigStatus"]),]


## rm non-foraging times; subset 1 rndm loc/day; spatialize ##

l14 <- locs %>%
  filter(Sex == "Female") %>%
  subset(between(Date, as.Date("2014-07-15"), as.Date("2014-08-31"))) %>%
  #subset(Time < 0300 | Time > 2300) %>%
  subset(Time < 1400 | Time > 1800) %>%
  group_by(IndivYr, Date) %>%
  sample_n(5) %>%
  #sample_n(1) %>%
  ungroup() %>%
  group_by(IndivYr) %>%
  mutate(tIndiv = n()) %>%
  ungroup()
    xy14 <- data.frame("x" = l14$Long, "y" = l14$Lat)
    ll14 <- SpatialPointsDataFrame(xy14, l14,proj4string = latlong)
    sp14 <- spTransform(ll14, rawde14@crs)
l15 <- locs %>%
  filter(Sex == "Female") %>%
  subset(between(Date, as.Date("2015-07-15"), as.Date("2015-08-31"))) %>%
  #subset(Time < 0300 | Time > 2300) %>%
  subset(Time < 1400 | Time > 1800) %>%
  group_by(IndivYr, Date) %>%
  filter(n() >= 5) %>%
  sample_n(5) %>%
  #sample_n(1) %>%
  ungroup() %>%
  group_by(IndivYr) %>%
  mutate(tIndiv = n()) %>%
  ungroup()
    xy15 <- data.frame("x" = l15$Long, "y" = l15$Lat)
    ll15 <- SpatialPointsDataFrame(xy15, l15,proj4string = latlong)
    sp15 <- spTransform(ll15, rawde15@crs)
    

#### COUNT NUMBER OF POINTS PER PIXEL PER INDIVIDUAL ####

## indiv lists ##
elk14 <- data.frame(unique(l14$IndivYr))
nelk14 <- nrow(elk14)
elk15 <- data.frame(unique(l15$IndivYr))
nelk15 <- nrow(elk15)
    
## reference raster - 250m2 resolution ##
refraster <- raster(extent(rawde14), crs = rawde14@crs,
                    res = c(250, 250))


## count nlocs per pixel per indiv ##
  # separately for 2014 and 2015, how embarrassing


locstack14 <- stack() # empty raster stack to store output
for(i in 1:nelk14) {
  elk <- elk14[i,] # identify individual
  subdat <- subset(sp14, IndivYr == elk) # subset indiv locs
  coords <- data.frame(subdat@coords) # create locs df
  n14 <- rasterize(coords, refraster, fun ='count') # count locs/pixel
  names(n14) <- paste("e", elk, sep="") # name layer per indiv
  locstack14 <- stack(locstack14, n14) # store layer in stack
}
locstack14[is.na(locstack14)] <- 0 # 0 if no locs (not NA)


locstack15 <- stack()
for(i in 1:nelk15) {
  elk <- elk15[i,] 
  subdat <- subset(sp15, IndivYr == elk) 
  coords <- data.frame(subdat@coords) 
  n15 <- rasterize(coords, refraster, fun ='count') 
  names(n15) <- paste("e", elk, sep="")
  locstack15 <- stack(locstack15, n15) 
}
locstack15[is.na(locstack15)] <- 0 



#### ADD SPATIAL DATA TO COUNT DATA ####

de14.250 <- resample(rawde14, refraster, method='bilinear')
stack14 <- stack(locstack14, de14.250)
dat14 <- data.frame(getValues(stack14)) %>%
  mutate(HabSuit= ifelse(DE2014 >= 2.90, "Excellent", 
                 ifelse(DE2014 >= 2.75 & DE2014 < 2.90, "Good",
                 ifelse(DE2014 < 2.75, "Marginal", "Poor"))),
         HabAd = ifelse(DE2014 >= 2.75, 1, 0),
         Year = 2014) %>%
  rename(DE = DE2014)
dat14$nElkPix <- rowSums(dat14[1:nelk14]) # total locs per pixel
long14 <- dat14 %>%
  mutate(Pix = seq.int(nrow(dat14))) %>% # pixel # (sanity check)
  gather(key = IndivYr, value = nIndPix, # make longform
         -c(Pix, Year, HabSuit, HabAd, DE, nElkPix)) %>%
  filter(nElkPix > 0) %>% # only incl used pixels
  group_by(IndivYr) %>%
  mutate(nIndTot = sum(nIndPix)) %>% # calculate offset per indiv
  ungroup()
long14$IndivYr <- gsub("\\.", "-", long14$IndivYr)
long14$IndivYr <- gsub("e", "", long14$IndivYr)

 
de15.250 <- resample(rawde15, refraster, method='bilinear')
stack15 <- stack(locstack15, de15.250)
dat15 <- data.frame(getValues(stack15)) %>%
  mutate(HabSuit= ifelse(DE2015 >= 2.90, "Excellent", 
                 ifelse(DE2015 >= 2.75 & DE2015 < 2.90, "Good",
                 ifelse(DE2015 < 2.75, "Marginal", "Poor"))),
         HabAd = ifelse(DE2015 >= 2.75, 1, 0),
         Year = 2015) %>%
  rename(DE = DE2015)
dat15$nElkPix <- rowSums(dat15[1:nelk15]) # total locs per pixel
long15 <- dat15 %>%
  mutate(Pix = seq.int(nrow(dat15))) %>% # pixel # (sanity check)
  gather(key = IndivYr, value = nIndPix, # make longform
         -c(Pix, Year, HabSuit, HabAd, DE, nElkPix)) %>%
  filter(nElkPix > 0) %>% # only incl used pixels
  group_by(IndivYr) %>%
  mutate(nIndTot = sum(nIndPix)) %>% # calculate offset per indiv
  ungroup()
long15$IndivYr <- gsub("\\.", "-", long15$IndivYr)
long15$IndivYr <- gsub("e", "", long15$IndivYr)

rufdat <- rbind(long14, long15)
write.csv(rufdat, file = "ruf-indiv.csv")

rufdat <- read.csv("ruf-indiv.csv")

rufdat$HabAd <- as.factor(rufdat$HabAd)



#### MODELS ####


#### adequate ####
ad <- filter(rufdat, nIndPix > 0 & HabAd == 1)

# poisson
p <- glm(nIndPix ~ offset(log(nIndTot)) + nElkPix, 
         family = poisson, data = ad) 
par(mfrow=c(2,2))
plot(p)
summary(p)
test <- ad$nIndPix/ad$nIndTot
hist(test)
hist(log(test))
summary(test)


# quasipoisson
q <- glm(nIndPix ~ offset(log(nIndTot)) + nElkPix, 
         family = quasipoisson, data = ad) 
summary(q)


# zero-truncated poisson
z <- vglm(nIndPix ~ offset(log(nIndTot)) + nElkPix, 
         family = pospoisson(), data = ad) 
summary(z)


# negbin
n <- glm.nb(nIndPix ~ offset(log(nIndTot)) + nElkPix, 
         link = log, data = ad) 
summary(n)


# zero-truncated negbin
zn <- vglm(nIndPix ~ offset(log(nIndTot)) + nElkPix, 
         family = posnegbinomial(), data = ad)
summary(zn)



#### log-likehihood tests ####

pchisq(2 * (logLik(n) - logLik(p)), df = 1, lower.tail = FALSE)
  # negbin > poisson

pchisq(2 * (logLik(n) - logLik(zn)), df = 1, lower.tail = FALSE)
  # zero-truncated negbin ~ negbin 

pchisq(2 * (logLik(z) - logLik(n)), df = 1, lower.tail = FALSE)
  # zero-truncated poisson ~ negbin 

plot(n)


#### shape of relationship ####

n1 <- glm.nb(nIndPix ~ offset(log(nIndTot)) + nElkPix, 
         link = log, data = ad) 
n2 <- glm.nb(nIndPix ~ offset(log(nIndTot)) + nElkPix + I(nElkPix^2), 
         link = log, data = ad) 
n3 <- glm.nb(nIndPix ~ offset(log(nIndTot)) + nElkPix + I(nElkPix^2)+ I(nElkPix^3), 
         link = log, data = ad) 
AIC(n1, n2, n3)
# n3 ftw



#### inadequate ####
inad <- rufdat %>%
  mutate(RelFreq = nIndPix/nIndTot) %>%
  filter(nIndPix > 0 & HabAd == 0 & RelFreq < 0.4) 


ni <- glm.nb(nIndPix ~ offset(log(nIndTot)) + nElkPix, 
         link = log, data = inad) 
summary(ni)

ni1 <- glm.nb(nIndPix ~ offset(log(nIndTot)) + nElkPix, 
         link = log, data = inad) 
ni2 <- glm.nb(nIndPix ~ offset(log(nIndTot)) + nElkPix + I(nElkPix^2), 
         link = log, data = inad) 
ni3 <- glm.nb(nIndPix ~ offset(log(nIndTot)) + nElkPix + I(nElkPix^2)+ I(nElkPix^3), 
         link = log, data = inad) 
AIC(ni1, ni2, ni3)


#### excellent ####
exc <- rufdat %>%
  mutate(RelFreq = nIndPix/nIndTot) %>%
  filter(nIndPix > 0 & HabSuit == "Excellent") 


ne <- glm.nb(nIndPix ~ offset(log(nIndTot)) + nElkPix, 
         link = log, data = exc) 
summary(ne)

ne1 <- glm.nb(nIndPix ~ offset(log(nIndTot)) + nElkPix, 
         link = log, data = exc) 
ne2 <- glm.nb(nIndPix ~ offset(log(nIndTot)) + nElkPix + I(nElkPix^2), 
         link = log, data = exc) 
ne3 <- glm.nb(nIndPix ~ offset(log(nIndTot)) + nElkPix + I(nElkPix^2)+ I(nElkPix^3), 
         link = log, data = exc) 
AIC(ne1, ne2, ne3)


#### plotting ####

# adequate #
dens <- 0:70
pred <- predict(n3, 
                newdata = data.frame(nElkPix = dens, nIndTot = 1), 
                type = "response")
adnew <- ad %>%
  mutate(RelFreq = nIndPix/nIndTot)
par(mfrow=c(1,1))
plot(adnew$nElkPix, adnew$RelFreq,
     main = "Adequate Baseline Suitability",
     ylab = "Relative frequency of use",
     xlab = expression(paste("Conspecific Density (n/250",
                            m^2, ")", sep="")))
lines(dens, pred, type = "l")

# inadequate #
dens <- 0:max(inad$nElkPix)
predi <- predict(ni3, 
                newdata = data.frame(nElkPix = dens, nIndTot = 1), 
                type = "response")
par(mfrow=c(1,1))
plot(inad$nElkPix, inad$RelFreq,
     main = "Inadequate Baseline Suitability",
     ylab = "Relative frequency of use",
     xlab = expression(paste("Conspecific Density (n/250",
                            m^2, ")", sep="")))
lines(dens, predi, type = "l")



# excellent #
dens <- 0:max(exc$nElkPix)
predi <- predict(ne3, 
                newdata = data.frame(nElkPix = dens, nIndTot = 1), 
                type = "response")
par(mfrow=c(1,1))
plot(exc$nElkPix, exc$RelFreq,
     main = "Excellent Baseline Suitability",
     ylab = "Relative frequency of use",
     xlab = expression(paste("Conspecific Density (n/250",
                            m^2, ")", sep="")))
lines(dens, predi, type = "l")
