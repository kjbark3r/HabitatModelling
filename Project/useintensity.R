# # # # # # # #  # # # # # # # # # # # # # # # # 
# INTENSITY OF ELK USE AND AVAILABLE NUTRITION #
#            WILD562 CLASS PROJECT             #
#               KRISTIN BARKER                 #
#                 APRIL 2017                   #
# # # # # # # #  # # # # # # # # # # # # # # # # 



#### SETUP ####

## set working directory ##
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\HabitatModelling\\Project"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\HabitatModelling\\Project"
ifelse(file.exists(wd_workcomp), setwd(wd_workcomp), setwd(wd_laptop))
rm(wd_workcomp, wd_laptop)

## load packages ##
library(VGAM) #vglm(), zero-truncated poisson/negbin
library(MASS) #glm.nb(), negbin
library(AICcmodavg) 
library(raster)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)

## read in "raw" data ##
rawlocs <- read.csv("../../ElkDatabase/collardata-locsonly-equalsampling.csv")
rawde14 <- raster("../../Vegetation/DE2014.tif")
rawde15 <- raster("../../Vegetation/DE2015.tif")
rawhbm14 <- raster("../../Vegetation/gherb2014.tif")
rawhbm15 <- raster("../../Vegetation/gherb2015.tif")
rawgdm14 <- raster("GDMherb2014.tif")
rawgdm15 <- raster("GDMherb2015.tif")
migstatus <- read.csv("../../Nutrition/migstatus.csv")
mig <- dplyr::select(migstatus, c(IndivYr, MigStatus))

## define projection ##
latlong <- CRS("+init=epsg:4326") # WGS84 projection



#### DATA PREP ####

## GDM prelim estimation ##


## format dates and times; add IndivYr and MigStatus ##
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

    
## reference raster - 250m2 resolution ##
refraster <- raster(extent(rawde14), crs = rawde14@crs,
                    res = c(250, 250))
 
       
## XY coordinates from 2014 and 2015 data ##
# also subset by MigStatus  #

coords14 <- data.frame(sp14@coords)
r14 <- subset(l14, MigStatus == "Resident")
    rxy14 <- data.frame("x" = r14$Long, "y" = r14$Lat)
    rll14 <- SpatialPointsDataFrame(rxy14, r14,proj4string = latlong)
    rsp14 <- spTransform(rll14, rawde14@crs)
    rcoords14 <- data.frame(rsp14@coords)
i14 <- subset(l14, MigStatus == "Intermediate")
    ixy14 <- data.frame("x" = i14$Long, "y" = i14$Lat)
    ill14 <- SpatialPointsDataFrame(ixy14, i14,proj4string = latlong)
    isp14 <- spTransform(ill14, rawde14@crs)
    icoords14 <- data.frame(isp14@coords)
m14 <- subset(l14, MigStatus == "Migrant")
    mxy14 <- data.frame("x" = m14$Long, "y" = m14$Lat)
    mll14 <- SpatialPointsDataFrame(mxy14, m14,proj4string = latlong)
    msp14 <- spTransform(mll14, rawde14@crs)
    mcoords14 <- data.frame(msp14@coords)

coords15 <- data.frame(sp15@coords)
r15 <- subset(l15, MigStatus == "Resident")
    rxy15 <- data.frame("x" = r15$Long, "y" = r15$Lat)
    rll15 <- SpatialPointsDataFrame(rxy15, r15,proj4string = latlong)
    rsp15 <- spTransform(rll15, rawde15@crs)
    rcoords15 <- data.frame(rsp15@coords)
i15 <- subset(l15, MigStatus == "Intermediate")
    ixy15 <- data.frame("x" = i15$Long, "y" = i15$Lat)
    ill15 <- SpatialPointsDataFrame(ixy15, i15,proj4string = latlong)
    isp15 <- spTransform(ill15, rawde15@crs)
    icoords15 <- data.frame(isp15@coords)
m15 <- subset(l15, MigStatus == "Migrant")
    mxy15 <- data.frame("x" = m15$Long, "y" = m15$Lat)
    mll15 <- SpatialPointsDataFrame(mxy15, m15,proj4string = latlong)
    msp15 <- spTransform(mll15, rawde15@crs)
    mcoords15 <- data.frame(msp15@coords)
    
    
## nlocs/250m2 pixel ##
n14 <- rasterize(coords14, refraster, fun='count')
rn14 <- rasterize(rcoords14, refraster, fun='count')
in14 <- rasterize(icoords14, refraster, fun='count')
mn14 <- rasterize(mcoords14, refraster, fun='count')
n15 <- rasterize(coords15, refraster, fun='count')
rn15 <- rasterize(rcoords15, refraster, fun='count')
in15 <- rasterize(icoords15, refraster, fun='count')
mn15 <- rasterize(mcoords15, refraster, fun='count')

## resampled nute/250m2 pixel ##
de14.250 <- resample(rawde14, refraster, method='bilinear')
de15.250 <- resample(rawde15, refraster, method='bilinear')
hbm14.250 <- resample(rawhbm14, refraster, method='bilinear')
hbm15.250 <- resample(rawhbm15, refraster, method='bilinear')
gdm14.250 <- resample(rawgdm14, refraster, method='bilinear')
gdm15.250 <- resample(rawgdm15, refraster, method='bilinear')
gdm14.250[gdm14.250 < 0] <- 0
gdm15.250[gdm15.250 < 0] <- 0

## combine nlocs with underlying veg data ##

brick14 <- brick(n14, rn14, in14, mn14,
                 de14.250, hbm14.250, gdm14.250)
locs14 <- data.frame(getValues(brick14))
locs14 <- locs14 %>%
  rename(nLocs = layer.1,
         nRes = layer.2,
         nInt = layer.3,
         nMig = layer.4,
         DE = DE2014,
         Biomass = gherb2014,
         GDM = GDMherb2014) %>%
  mutate(Year = "2014")
locs14[is.na(locs14)] <- 0
locs14$tRes <- sum(locs14$nRes)
locs14$tInt <- sum(locs14$nInt)
locs14$tMig <- sum(locs14$nMig)
locs14$UseIntensity <- locs14$nLocs/sum(locs14$nLocs)
locs14$Total <- sum(locs14$nLocs)

brick15 <- brick(n15, rn15, in15, mn15,
                 de15.250, hbm15.250, gdm15.250)
locs15 <- data.frame(getValues(brick15))
locs15 <- locs15 %>%
  rename(nLocs = layer.1,
         nRes = layer.2,
         nInt = layer.3,
         nMig = layer.4,
         DE = DE2015,
         Biomass = gherb2015,
         GDM = GDMherb2015) %>%
  mutate(Year = "2015",
         tRes = sum(nRes),
         tInt = sum(nInt),
         tMig = sum(nMig)) 
locs15[is.na(locs15)] <- 0
locs15$tRes <- sum(locs15$nRes)
locs15$tInt <- sum(locs15$nInt)
locs15$tMig <- sum(locs15$nMig)
locs15$UseIntensity <- locs15$nLocs/sum(locs15$nLocs)
locs15$Total <- sum(locs15$nLocs)
locsnute <- bind_rows(locs14, locs15)
locsnute.no0 <- filter(locsnute, nLocs > 0)

# updated df w 
# UseIntensity and per cap nute per mig behav
# scaled GDM (10-g units now)
uidat <- locsnute.no0 %>%
  mutate(ResUI = nRes/tRes,
         IntUI = nInt/tInt,
         MigUI = nMig/tMig,
         GDM10 = GDM/10,
         ResGDM10 = nRes/nLocs*GDM10,
         IntGDM10 = nInt/nLocs*GDM10,
         MigGDM10 = nMig/nLocs*GDM10)
write.csv(uidat, file = "uidat.csv", row.names=F)

#### VISUALS - DATA AND RELATIONSHIPS ####
uidat <- read.csv("uidat.csv")

# Use-intensity distribution
hist(uidat$UseIntensity, breaks = 100)

# use-intensity ~ available nutrition
plot(UseIntensity ~ GDM10, data = uidat)
ggplot(uidat, aes(x = GDM10, y = UseIntensity)) +
  stat_smooth(method = "loess") +
  geom_point(size = 1)


# trying the above all on one plot
ggplot(uidat, aes(GDM10)) +
  stat_smooth(aes(y = ResUI, colour = "ResUI"), method = "loess") +
  stat_smooth(aes(y = IntUI, colour = "IntUI"), method = "loess") +
  stat_smooth(aes(y = MigUI, colour = "MigUI"), method = "loess") +
  stat_smooth(aes(y = UseIntensity, colour = "UIpopn"), method = "loess") 
# well shit, that was easy. thanks hadley.


# per capita nutrition per migstatus
uidatsub <- uidat
uidatsub$ResGDM10 <- ifelse(uidatsub$ResGDM10 == 0, NA, uidatsub$ResGDM10)
uidatsub$IntGDM10 <- ifelse(uidatsub$IntGDM10 == 0, NA, uidatsub$IntGDM10)
uidatsub$MigGDM10 <- ifelse(uidatsub$MigGDM10 == 0, NA, uidatsub$MigGDM10)

q <- ggplot(uidatsub, aes(ResGDM10)) +
  geom_histogram()
w <- ggplot(uidatsub, aes(IntGDM10)) +
  geom_histogram()
e <- ggplot(uidatsub, aes(MigGDM10)) +
  geom_histogram()
grid.arrange(q,w,e)


#### USE/NUTRITION MODELS ####

#1. determine best type of model
par(mfrow=c(2,2))

    # poisson
    mp <- glm(nLocs ~ offset(log(Total)) + GDM10,
                family = poisson, data = uidat)
    summary(mp)
    plot(mp)
    
    # quasipoisson
    mqp <- glm(nLocs ~ offset(log(Total)) + GDM10,
                family = quasipoisson, data = uidat)
    summary(mqp)
    plot(mqp) # essentially same as above
    
    # zero-truncated poisson
    mzp <- vglm(nLocs ~ offset(log(Total)) + GDM10,
                family = pospoisson(), data = uidat)
    summary(mzp)
    #plot(mzp) #can't
    
    # negative binomial
    mnb <- glm.nb(nLocs ~ offset(log(Total)) + GDM10,
                data = uidat, link = log)
    summary(mnb)
    plot(mnb) #very similar to mp, mqp
    # assumption: dispersion parameter that changes
    # whereas in poisson it is held constant
      # so poisson is a type of negbin model
    
    # zero-truncated negative binomial
    mznb <- vglm(nLocs ~ offset(log(Total)) + GDM10,
                data = uidat, 
                family = posnegbinomial())
    warnings()
    summary(mznb)


#### assessing relative model fit ####

## poisson vs negbin ##
# likelihood ratio test #
pchisq(2 * (logLik(mnb) - logLik(mp)), df = 1, 
       lower.tail = FALSE)
# can do this bc poisson is nested in negbin
# very small p-value suggests negbin is better
# based on this i'm throwing out all the p stuff
# and based on mznb warnings i'm throwing that out 
# so, negbin ftw!!


#2. determine whether relationship is linear
m1 <- glm.nb(nLocs ~ offset(log(Total)) + GDM10,
            data = uidat, link = log)
summary(m1)
estm1 <- cbind(Estimate = coef(m1), confint(m1))
estm1 # model coefficients
exp(estm1) # estimated use-intensity ratios
  # interp: ~23% increase in use-intensity rate for
  # every 10-g increase in GDM

## compare fit of quadratic and cubic terms
m2 <- glm.nb(nLocs ~ offset(log(Total)) + GDM10 + I(GDM10^2),
            data = uidat, link = log)
m3 <- glm.nb(nLocs ~ offset(log(Total)) + GDM10 + I(GDM10^2)  + I(GDM10^3),
            data = uidat, link = log)
AIC(m1, m2, m3)
BIC(m1, m2, m3)
# cubic comes out best, even when more highly penalized




#3. density as habitat component?
#### kristin you left off here ####
#### need to think about how to incl density as habitat component ####
m4 <- glm.nb(nLocs ~ offset(log(Total)) + GDM10 + I(GDM10^2)  + I(GDM10^3),
            data = uidat, link = log)
m5 <- glm.nb(nLocs ~ offset(log(Total)) + GDM10 + I(GDM10^2)  + I(GDM10^3) + nLocs,
            data = uidat, link = log)
AIC(m4, m5)
BIC(m4, m5)


#### VISUALS - MODEL PREDICTIONS ####

pp <- ggplot(uidat, aes(x = GDM10, y = UseIntensity)) +
  stat_smooth(method="glm",
              se = TRUE,
              formula = y ~ poly(x, 3, raw = TRUE)) +
  geom_point(size=1)
pp

