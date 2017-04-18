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
library(VGAM) #vglm(), zero-truncated poisson/negbin
library(MASS) #glm.nb(), negbin
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
mig <- dplyr::select(migstatus, c(IndivYr, MigStatus))

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
locs <- left_join(locs, mig, by = "IndivYr")

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
    
    

## reference raster - 250m2 resolution ##
refraster <- raster(extent(rawde14), crs = rawde14@crs,
                    res = c(250, 250))


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
gdm14.250 <- resample(gdm14, refraster, method='bilinear')
gdm15.250 <- resample(gdm15, refraster, method='bilinear')


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
         GDM = layer.5) %>%
  mutate(Year = "2014") 
locs14[is.na(locs14)] <- 0
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
         GDM = layer.5) %>%
  mutate(Year = "2015") 
locs15[is.na(locs15)] <- 0
locs15$UseIntensity <- locs15$nLocs/sum(locs15$nLocs)
locs15$Total <- sum(locs15$nLocs)
locsnute <- bind_rows(locs14, locs15)
locsnute.no0 <- filter(locsnute, nLocs > 0)

# new df incl's UseIntensity per ea mig behav
uidat <- locsnute.no0 %>%
  mutate(ResUI = nRes/Total,
         IntUI = nInt/Total,
         MigUI = nMig/Total,
         NutePC = UseIntensity*GDM,
         ResPC = nRes/Total*GDM,
         IntPC = nInt/Total*GDM,
         MigPC = nMig/Total*GDM)


#### VISUALS - DATA AND RELATIONSHIPS ####

# Use-intensity distribution
hist(locsnute.no0$UseIntensity, breaks = 100)

# use-intensity ~ available nutrition
plot(UseIntensity ~ GDM, data = locsnute.no0)

# Useintensity by GDM, all popn and ea migstatus #
a <- ggplot(locsnute.no0, aes(x = GDM, y = UseIntensity)) +
  stat_smooth(method="loess")
b <- ggplot(uidat, aes(x = GDM, y = ResUI)) +
  stat_smooth(method="loess")
c <- ggplot(uidat, aes(x = GDM, y = IntUI)) +
  stat_smooth(method="loess")
d  <- ggplot(uidat, aes(x = GDM, y = MigUI)) +
  stat_smooth(method="loess")
grid.arrange(a,b,c,d, nrow=2)



#### USE/NUTRITION MODELS ####

#1. determine best type of model
par(mfrow=c(2,2))

    # poisson
    mp <- glm(nLocs ~ offset(log(Total)) + GDM,
                family = poisson, data = locsnute.no0)
    summary(mp)
    plot(mp)
    
    # quasipoisson
    mqp <- glm(nLocs ~ offset(log(Total)) + GDM,
                family = quasipoisson, data = locsnute.no0)
    summary(mqp)
    plot(mqp) # essentially same as above
    
    # zero-truncated poisson
    mzp <- vglm(nLocs ~ offset(log(Total)) + GDM,
                family = pospoisson(), data = locsnute.no0)
    summary(mzp)
    plot(mzp) #can't
    
    # negative binomial
    mnb <- glm.nb(nLocs ~ offset(log(Total)) + GDM,
                data = locsnute.no0, link = log)
    summary(mnb)
    plot(mnb) #very similar to mp, mqp
    # assumption: dispersion parameter that changes
    # whereas in poisson it is held constant
      # so poisson is a type of negbin model
    
    # zero-truncated negative binomial
    mznb <- vglm(nLocs ~ offset(log(Total)) + GDM,
                data = locsnute.no0, 
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


m1 <- glm.nb(nLocs ~ offset(log(Total)) + GDM,
            data = locsnute.no0, link = log)
m2 <- glm.nb(nLocs ~ offset(log(Total)) + GDM + I(GDM^2),
            data = locsnute.no0, link = log)
m3 <- glm.nb(nLocs ~ offset(log(Total)) + GDM + I(GDM^2)  + I(GDM^3),
            data = locsnute.no0, link = log)
AIC(m1, m2, m3)
BIC(m1, m2, m3)
# cubic still comes out best even when more highly penalized


# model estimates from top population-level model #
estm3 <- cbind(Estimate = coef(m3), confint(m3))
estm3
# estimated use-intensity ratios
exp(estm3)
# not sure how to interpret due to cubic
# if linear only, would interp as a 2% decrease in
  # use-intensity rate for every unit increase in GDM
  # bc multiplicative effect when exponentiated 
  # (when on y scale rather tyhan ln(y) scale)




#### VISUALS - MODEL PREDICTIONS ####

pp <- ggplot(locsnute.no0, aes(x = DE, y = UseIntensity)) +
  stat_smooth(method="glm",
              se = TRUE,
              formula = y ~ poly(x, 3, raw = TRUE)) +
  geom_point(size=1)
pp

