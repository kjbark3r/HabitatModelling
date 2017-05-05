# # # # # # # #  # # # # # # # # # # # # # # # # 
#  RESOURCE SELECTION AND CONSPECIFIC DENSITY  #
#            WILD562 CLASS PROJECT             #
#                 TAKE 297                     #
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
library(adehabitatHR)
library(AICcmodavg) 
library(raster)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)

## read in data (from useintensity.R)
ruf <- read.csv("ruf-data.csv")


#### CREATE USED-AVAIL DFs ####

gdm14 <- raster("GDMherb2014.tif")
gdm15 <- raster("GDMherb2015.tif")
latlong <- CRS("+init=epsg:4326") # WGS84 projection
ruf14 <- filter(ruf, Year == 2014)
ruf15 <- filter(ruf, Year == 2015)

# 2014 #
ruf14xy <- data.frame("x" = ruf14$Long, "y" = ruf14$Lat)
ruf14ll <- SpatialPointsDataFrame(ruf14xy, ruf14, proj4string = latlong)
ruf14sp <- spTransform(ruf14ll, gdm14@crs) # match raster projection
kud14 <- kernelUD(ruf14sp, h = "href") # popn-level HR
vert14 <- getverticeshr(kud14) # polygon to pull avail from
avail14 <- spsample(vert14, nrow(ruf14), "random") # =n used locs
plot(vert14); plot(avail14, add = T); plot(ruf14sp, color = "red", add = T)
avail14df <- data.frame(avail14)
avail14df$Used <- 0
used14df <- data.frame(ruf14sp@coords)
used14df$Used <- 1
ua14 <- bind_rows(used14df, avail14df)
ua14xy <- data.frame("x"=ua14$x,"y"=ua14$y)
ua14sp <- SpatialPointsDataFrame(ua14xy, ua14, proj4string = gdm14@crs)


# 2015 #
ruf15xy <- data.frame("x" = ruf15$Long, "y" = ruf15$Lat)
ruf15ll <- SpatialPointsDataFrame(ruf15xy, ruf15, proj4string = latlong)
ruf15sp <- spTransform(ruf15ll, gdm15@crs) # match raster projection
kud15 <- kernelUD(ruf15sp, h = "href") # popn-level HR
vert15 <- getverticeshr(kud15) # polygon to pull avail from
avail15 <- spsample(vert15, nrow(ruf15), "random") # =n used locs
plot(vert15); plot(avail15, add = T); plot(ruf15sp, color = "red", add = T)
avail15df <- data.frame(avail15)
avail15df$Used <- 0
used15df <- data.frame(ruf15sp@coords)
used15df$Used <- 1
ua15 <- bind_rows(used15df, avail15df)
ua15xy <- data.frame("x"=ua15$x,"y"=ua15$y)
ua15sp <- SpatialPointsDataFrame(ua15xy, ua15, proj4string = gdm15@crs)


#### READ IN AND STACK RASTERS ####

# read in nute data #
rawde14 <- raster("../../Vegetation/DE2014.tif")
rawde15 <- raster("../../Vegetation/DE2015.tif")
nlocs14 <- raster("nlocs14.tif")
nlocs15 <- raster("nlocs15.tif")

# create reference raster - 250m2 resolution #
refraster <- raster(extent(rawde14), crs = rawde14@crs,
                    res = c(250, 250))

# resample nute rasters to 250m2 scale #
de14.250 <- resample(rawde14, refraster, method='bilinear')
de15.250 <- resample(rawde15, refraster, method='bilinear')


# bricks to extract from #
brick14 <- brick(nlocs14, de14.250)
brick15 <- brick(nlocs15, de15.250)


# extract nute and dens for used and avail locs #
ext14 <- data.frame(raster::extract(brick14, ua14sp))
ext14$nlocs14 <- ifelse(is.na(ext14$nlocs14), 0, ext14$nlocs14)
ext15 <- data.frame(raster::extract(brick15, ua15sp))
ext15$nlocs15 <- ifelse(is.na(ext15$nlocs15), 0, ext15$nlocs15)

# combine with used/avail info #
ua14dat <- bind_cols(ua14, ext14) %>%
  rename(nLocs = nlocs14,
         DE = DE2014) %>%
  mutate(Year = 2014)
ua15dat <- bind_cols(ua15, ext15) %>%
  rename(nLocs = nlocs15,
         DE = DE2015) %>%
  mutate(Year = 2015)
uadat <- bind_rows(ua14dat, ua15dat)
any(is.na(uadat)) # sweet
uadat$HabSuit <- ifelse(uadat$DE >= 2.9, "Excellent",
                 ifelse(uadat$DE < 2.9 & uadat$DE >= 2.75, "Good",
                 ifelse(uadat$DE < 2.75 & uadat$DE >= 2.4, "Marginal",
                        "Poor")))
uadat$Adequate <- as.factor(ifelse(uadat$DE >= 2.75, 1, 0))

write.csv(uadat, file = "rsf-data.csv")


#### RSF - DOES SELXN FOR HAB QUAL CHANGE AT DIFF DENSITIES? ####

uadat <- read.csv("rsf-data.csv")
uadat$Adequate <- as.factor(uadat$Adequate)

m1 <- glm(Used ~ DE, data = uadat, family = binomial(logit))
m2 <- glm(Used ~ DE*nLocs, data = uadat, family = binomial(logit))
AIC(m1, m2)

m3 <- glm(Used ~ Adequate, data = uadat, family = binomial(logit))
m4 <- glm(Used ~ Adequate*nLocs, data = uadat, family = binomial(logit))
AIC(m3, m4)

exp(coef(m4))

ggplot(uadat, aes(x=nLocs, 
                     y=Used, linetype=HabSuit)) + 
  stat_smooth(method="glm", 
              method.args = list(family="binomial"), 
              level=0.95) 


plothum <- ggplot(wolfdata2, aes(x=DistFromHumanAccess2, 
                     y=used, linetype=closedFactor)) + 
  labs(x = "Distance From Humans (m)", y = "Pr(Use)") +
  theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Open canopy", "Closed canopy")) +
  stat_smooth(method="glm", 
              method.args = list(family="binomial"), 
              level=0.95) 