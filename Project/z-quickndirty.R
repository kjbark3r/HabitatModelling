#### quick test - r/i/m per cap distns ####


#### RESIDENT ####

# res-only locs
r14 <- l14 %>%
  inner_join(migstatus, by = "IndivYr") %>%
  filter(MigStatus == "Resident")
    rxy14 <- data.frame("x" = r14$Long, "y" = r14$Lat)
    rll14 <- SpatialPointsDataFrame(rxy14, r14,proj4string = latlong)
    rsp14 <- spTransform(rll14, rawde14@crs)
rcoords14 <- data.frame(rsp14@coords)    

# res-only nlocs
rn14.250 <- rasterize(rcoords14, refraster, fun='count')

rbrick14 <- brick(rn14.250, de14.250, hbm14.250, gdm14.250)

rlocs14 <- data.frame(getValues(rbrick14))
rlocs14 <- rlocs14 %>%
  rename(nLocs = layer.1,
         DE = DE2014,
         Biomass = gherb2014,
         GDM = layer.2) %>%
  mutate(Year = "2014") %>%
  filter(!is.na(nLocs))
rlocs14$UseIntensity <- rlocs14$nLocs/sum(rlocs14$nLocs)

rtest <- rlocs14 %>%
  mutate(pcDE = DE/nLocs,
         pcBM = Biomass/nLocs,
         pcGDM = GDM/nLocs)

par(mfrow=c(3,1))
hist(rtest$pcDE)
hist(rtest$pcBM)
hist(rtest$pcGDM)

ggplot(rtest, aes(x=DE, y=UseIntensity)) +
  stat_smooth(method = "auto") +
  geom_point()

#### INTERMEDIATE ####


i14 <- l14 %>%
  inner_join(migstatus, by = "IndivYr") %>%
  filter(MigStatus == "Intermediate")
    ixy14 <- data.frame("x" = i14$Long, "y" = i14$Lat)
    ill14 <- SpatialPointsDataFrame(ixy14, i14,proj4string = latlong)
    isp14 <- spTransform(ill14, rawde14@crs)
icoords14 <- data.frame(isp14@coords)    


in14.250 <- rasterize(icoords14, refraster, fun='count')

ibrick14 <- brick(in14.250, de14.250, hbm14.250, gdm14.250)

ilocs14 <- data.frame(getValues(ibrick14))
ilocs14 <- ilocs14 %>%
  rename(nLocs = layer.1,
         DE = DE2014,
         Biomass = gherb2014,
         GDM = layer.2) %>%
  mutate(Year = "2014") %>%
  filter(!is.na(nLocs))
ilocs14$UseIntensity <- ilocs14$nLocs/sum(ilocs14$nLocs)

itest <- ilocs14 %>%
  mutate(pcDE = DE/nLocs,
         pcBM = Biomass/nLocs,
         pcGDM = GDM/nLocs)

par(mfrow=c(3,1))
hist(itest$pcDE)
hist(itest$pcBM)
hist(itest$pcGDM)

ggplot(itest, aes(x=DE, y=UseIntensity)) +
  stat_smooth(method = "glm") +
  geom_point()

#### MIGRANT ####

m14 <- l14 %>%
  inner_join(migstatus, by = "IndivYr") %>%
  filter(MigStatus == "Migrant")
    mxy14 <- data.frame("x" = m14$Long, "y" = m14$Lat)
    mll14 <- SpatialPointsDataFrame(mxy14, r14,proj4string = latlong)
    msp14 <- spTransform(mll14, rawde14@crs)
mcoords14 <- data.frame(msp14@coords)    


mn14.250 <- rasterize(mcoords14, refraster, fun='count')

mbrick14 <- brick(mn14.250, de14.250, hbm14.250, gdm14.250)

mlocs14 <- data.frame(getValues(mbrick14))
mlocs14 <- mlocs14 %>%
  rename(nLocs = layer.1,
         DE = DE2014,
         Biomass = gherb2014,
         GDM = layer.2) %>%
  mutate(Year = "2014") %>%
  filter(!is.na(nLocs))
mlocs14$UseIntensity <- mlocs14$nLocs/sum(mlocs14$nLocs)

mtest <- mlocs14 %>%
  mutate(pcDE = DE/nLocs,
         pcBM = Biomass/nLocs,
         pcGDM = GDM/nLocs)

par(mfrow=c(3,1))
hist(mtest$pcDE)
hist(mtest$pcBM)
hist(mtest$pcGDM)

ggplot(mtest, aes(x=DE, y=UseIntensity)) +
  stat_smooth(method = "auto") +
  geom_point()



#### shp of locs with migstatus included? ####
dat <- locs %>%
  select(-AnimalID) %>%
  right_join(migstatus, by = "IndivYr")
## rm non-foraging times; subset 1 rndm loc/day; spatialize ##
l14 <- dat %>%
  filter(Sex == "Female") %>%
  subset(between(Date, as.Date("2014-07-15"), as.Date("2014-08-31"))) %>%
  subset(Time < 0300 | Time > 2300) %>%
  group_by(IndivYr, Date) %>%
  sample_n(1) %>%
  ungroup()
    xy14 <- data.frame("x" = l14$Long, "y" = l14$Lat)
    ll14 <- SpatialPointsDataFrame(xy14, l14,proj4string = latlong)
    sp14 <- spTransform(ll14, rawde14@crs)
l15 <- dat %>%
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

datcoords14 <- data.frame(sp14@coords, sp14@data$MigStatus) %>%
      rename(MigStatus = sp14.data.MigStatus)
rcoords14 <- datcoords14 %>%
  filter(MigStatus == "Resident") %>%
  select(-MigStatus)
icoords14 <- datcoords14 %>%
  filter(MigStatus == "Intermediate") %>%
  select(-MigStatus)
mcoords14 <- datcoords14 %>%
  filter(MigStatus == "Migrant") %>%
  select(-MigStatus)

datcoords15 <- data.frame(sp15@coords, sp15@data$MigStatus) %>%
      rename(MigStatus = sp15.data.MigStatus)
rcoords15 <- datcoords15 %>%
  filter(MigStatus == "Resident") %>%
  select(-MigStatus)
icoords15 <- datcoords15 %>%
  filter(MigStatus == "Intermediate") %>%
  select(-MigStatus)
mcoords15 <- datcoords15 %>%
  filter(MigStatus == "Migrant") %>%
  select(-MigStatus)

## nlocs/250m2 pixel ##
rn14.250 <- rasterize(rcoords14, refraster, fun='count')
in14.250 <- rasterize(icoords14, refraster, fun='count')
mn14.250 <- rasterize(mcoords14, refraster, fun='count')
rn15.250 <- rasterize(rcoords15, refraster, fun='count')
in15.250 <- rasterize(icoords15, refraster, fun='count')
mn15.250 <- rasterize(mcoords15, refraster, fun='count')

plot(rn14.250)









#### CUTS AND MISC ####



## VERIFY SUBSETTING WORKED AS EXPECTED ##

length(unique(l14$IndivYr))*length(unique(l14$Date))
length(unique(l15$IndivYr))*length(unique(l15$Date))
 #14 is right but 15 is off by 59, huh
 #im ignoring it bc this is just a class project



## COUNT NUMBER OF POINTS PER PIXEL ##

library(adehabitatMA)
pixels <- as(de14, 'SpatialPixels')
test14 <- count.points(sp14, pixels)
plot(pixels)
plot(sp14, color = "blue")
# error w[[1]] : no [[ method for object without attributes

a <- table(cellFromXY(de14, sp14))

test <- rasterize(sp14, de14, fun='count')
plot(test)
# returns brick with NA values
# it's trying to count occurrence of each column i think


# below (stolen) code works
r <- raster(ncols=36, nrows=18)
n <- 1000
x <- runif(n) * 360 - 180
y <- runif(n) * 180 - 90
xy <- cbind(x, y)
# prensence/absensce (NA) (is there a point or not?)
r1 <- rasterize(xy, r, field=1)
# how many points?
r2 <- rasterize(xy, r, fun=function(x,...)length(x))
r3 <- rasterize(xy, r, fun='count')
plot(r2)
plot(r3)
# those 2 functions do the same thing, good


testlocs <- data.frame(sp14@data$Long, sp14@data$Lat)
test <- rasterize(testlocs, de14, fun='count') 
# still NAs but closer... it's trying to plot now

testlocs <- data.frame(sp14@data$Long, sp14@data$Lat)
x2 <- sp14@data$Long
test <- rasterize(testlocs, de14, 
                  fun=function(x2,...)length(x2)) 
# stilllllll NAs...
# maybe i need a blank raster for this

testrast <- raster(extent(de14), crs = de14@crs)
test <- rasterize(sp14, testrast, fun='count')  
# nope still brick

testrast <- raster(extent(de14), crs = de14@crs)
test <- rasterize(testlocs, testrast, fun='count')
plot(test)
unique(test@data@values)
# fuckkkkkkk
plot(test); plot(testlocs, add=T)
# ohhhh duh, using latlong from @data slot
# rather than stateplace from @coords
# smart, kristin

testlocs <- data.frame(sp14@coords)
test <- rasterize(testlocs, de14, fun='count')
plot(test)
unique(test@data@values)
length(which(!is.na(test@data@values)))
# seems right, but plot doesn't show any data
writeRaster(test, "nlocs-test", format="GTiff")
# ok looks correct in arcmap, just hard to see data
# here because pixels are so small

testlocs <- data.frame(sp14@coords)
testrast <- raster(extent(de14), crs = de14@crs)
test2 <- rasterize(testlocs, testrast, fun='count')
plot(test2) 
# eeeeeee
unique(test2@data@values)
length(unique(test2@data@values))
summary(test2@data@values)
# now the plot does show data
# but pixels are way too big
# next step: choose a resolution between this and above
  # which will require resampling DE raster

coords14 <- data.frame(sp14@coords) # make df of just XYs
resraster <- raster(extent(de14), crs = de14@crs)
res(resraster) <- c(100, 100) # reference raster
de14 <- resample(rawde14, resraster, method='bilinear')
plot(de14)
test <- rasterize(coords14, de14, fun='count')
plot(test)
unique(test@data@values)
summary(test@data@values)
writeRaster(test, "nlocs-test2", format="GTiff")
# seems better but may still need a little coarser

resraster2 <- raster(extent(de14), crs = de14@crs)
res(resraster2) <- c(500, 500)
de14 <- resample(rawde14, resraster2, method='bilinear')
plot(de14)
test <- rasterize(coords14, de14, fun='count')
plot(test)
unique(test@data@values)
summary(test@data@values)
writeRaster(test, "nlocs-test3", format="GTiff")
# wayyyyyyy too coarse; covers shitloads of DE pixels

resraster3 <- raster(extent(de14), crs = de14@crs)
res(resraster3) <- c(250, 250) #justify w ndvi, haha
de14 <- resample(rawde14, resraster3, method='bilinear')
plot(de14)
test <- rasterize(coords14, de14, fun='count')
plot(test)
unique(test@data@values)
summary(test@data@values)
writeRaster(test, "nlocs-test4", format="GTiff")

## biomass ##
bm14 <- resample(rawhbm14, refraster, method='bilinear')
nbm14 <- rasterize(coords14, bm14, fun='count')
bm15 <- resample(rawhbm15, refraster, method='bilinear')
nbm15 <- rasterize(coords15, bm15, fun='count')
# didn't need to do this; it gives exact same info
# in fact could've just used your refraster to count nlocs
# now need to combine nlocs with assoc'd nute vals

## digestible energy ##
de14 <- resample(rawde14, refraster, method='bilinear')
nde14 <- rasterize(coords14, de14, fun='count')
de15 <- resample(rawde15, refraster, method='bilinear')
nde15 <- rasterize(coords15, de15, fun='count')


## creating df of nlocs and nute ##
test <- data.frame(getValues(locsnute))
test <- test %>%
  rename(nLocs14 = layer.1,
         nLocs15 = layer.2) %>%
 filter(!is.na(nLocs14))
# ah right, nas in 2014 not same as in 2015
# make separate dfs to retain as much info as possible



#### zero-inflated model ####
library(pscl)
m1z <- zeroinfl(nLocs ~ offset(log(Total)) + DE,
            family = poisson, data = locsnute.250)
# error, don't actually have any 0s, duh



#### MODEL ATTEMPTS (prelim/premature) ####

# DE poisson #
mod1 <- glm(nLocs ~ offset(log(Total)) + DE,
            family = poisson, data = locsnute.no0)
summary(mod1)
mod2 <- glm(nLocs ~ offset(log(Total)) + DE + I(DE^2),
            family = poisson, data = locsnute.no0)
summary(mod2)
mod3 <- glm(nLocs ~ offset(log(Total)) + DE + I(DE^2) + I(DE^3),
            family = poisson, data = locsnute.no0)
summary(mod3)
Cand.set <- list()
Cand.set[[1]] <- mod1
Cand.set[[2]] <- mod2
Cand.set[[3]] <- mod3
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# strong evidence against linear relationship
# for all elk put together
# now i'm confused because i'm supposed to be
# modeling log(nLocs) but can't bc that's not an
# integer, and if i transform it after the fact
# that confuses the (non)linearity of the relationship
# with the explanatory variables
# oh never mind, poisson is log-linked so the
# response is already being logged


# GDM poisson #
mod1 <- glm(nLocs ~ offset(log(Total)) + GDM,
            family = poisson, data = locsnute.no0)
summary(mod1)
mod2 <- glm(nLocs ~ offset(log(Total)) + GDM + I(GDM^2),
            family = poisson, data = locsnute.no0)
summary(mod2)
mod3 <- glm(nLocs ~ offset(log(Total)) + GDM + I(GDM^2) + I(GDM^3),
            family = poisson, data = locsnute.no0)
summary(mod3)
Cand.set <- list()
Cand.set[[1]] <- mod1
Cand.set[[2]] <- mod2
Cand.set[[3]] <- mod3
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# strong evidence against linear relationship
# for all elk put together


## DE quasipoisson ##
mod1 <- glm(nLocs ~ offset(log(Total)) + DE,
            family = quasipoisson, data = locsnute.no0)
mod2 <- glm(nLocs ~ offset(log(Total)) + DE + I(DE^2),
            family = quasipoisson, data = locsnute.250)
mod3q <- glm(nLocs ~ offset(log(Total)) + DE + I(DE^2) + I(DE^3),
            family = quasipoisson, data = locsnute.no0)
summary(mod3q)
# still overdispersed, plus AIC is now NA, wah wah
par(mfrow=c(2,2))
plot(mod3q)
plot(mod3)
# omfg so terrible


## GDM zero-infl with all data ##
library(pscl)
zer <- zeroinfl(nLocs ~ offset(log(Total)) + GDM,
                data = locsnute)
summary(zer)
# ah fuck, i never did know what to do with these


## GDMs per MigStatus ##
m1 <- glm(nRes ~ offset(log(Total)) + DE,
          family = poisson, data = locsnute.no0)
plot(m1)

m2 <- glm(nRes ~ offset(log(Total)) + DE,
          family = quasipoisson, data = locsnute.no0)
plot(m2)
AIC(m1, m2)


## migstatus ##
mr1 <- glm(nRes ~ offset(log(Total)) + GDM + UseIntensity,
          family = poisson, 
          data = locsnute.no0)
summary(mr1)
mr2 <- glm(nRes ~ offset(log(Total)) + GDM + UseIntensity +
             GDM*UseIntensity,
          family = poisson, 
          data = locsnute.no0)
summary(mr2)
ggplot(locsnute.no0, aes(y = log(nRes/Total), x = GDM)) +
  stat_smooth(method="glm", 
              family="poisson",
              se = TRUE)



## zero-truncated poisson ##
library(VGAM)
test <- vglm(nLocs ~ offset(log(Total)) + GDM,
            family = pospoisson(), data = locsnute.no0)
summary(test)
test2 <- vglm(nLocs ~ offset(log(Total)) + GDM + I(GDM^2),
            family = pospoisson(), data = locsnute.no0)
summary(test2)
test3 <- vglm(nLocs ~ offset(log(Total)) + GDM + I(GDM^2) + I(GDM^3),
            family = pospoisson(), data = locsnute.no0)
summary(test3)
BIC(test)
BIC(test2)
BIC(test3)

# compare this with regular poisson? #
mod1 <- glm(nLocs ~ offset(log(Total)) + GDM,
            family = poisson, data = locsnute.no0)
test <- vglm(nLocs ~ offset(log(Total)) + GDM,
            family = pospoisson(), data = locsnute.no0)
Cand.set <- list()
Cand.set[[1]] <- mod1
Cand.set[[2]] <- test
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# newp #

par(mfrow=c(2,2)); plot(test)
# newp #

## from http://stats.idre.ucla.edu/r/dae/zero-truncated-poisson/
# plot resid vs fitted using ggplot #
output <- data.frame(resid=resid(test), fitted=fitted(test))
ggplot(output, aes (fitted, resid)) +
  geom_jitter(position=position_jitter(width=.25)) +
  stat_smooth(method="loess")
# plot resid vs fitted using quantile regression #
# to determine whether "outliers" have big influence #
ggplot(output, aes(fitted, resid)) +
    geom_jitter(position=position_jitter(width=.25)) +
    stat_quantile(method="rq")
# oh my. so much bad.


## negative binomial model ##
library(MASS)
nb <- glm.nb(nLocs ~ offset(log(Total)) + GDM,
            data = locsnute.no0, link = log)
summary(nb)
AIC(mod1, nb)

## zero-truncated negative binomial ##
library(VGAM)
znb <- vglm(nLocs ~ offset(log(Total)) + GDM,
            data = locsnute.no0, 
            family = posnegbinomial())
warnings()
# these warnings tell me to just fit a positive poisson
# i think due to size of y vals
# which is good bc i may be able to ignore the non-0 thing
summary(znb)
# second intercept is overdispersion parameter
# ditto second linear predictor

#### VISUAL ATTEMPTS (prelim/premature) ####

## DE raster with elk locs in it, 2014 ##
plot(gdm14)
plot(sp14, add=T)


## relationship bt use-intens and DE ##
plot(UseIntensity ~ DE, data=locsnute.250)
ggplot(locsnute.250, aes(x=DE, y=UseIntensity)) +
  stat_smooth(method = glm) +
  geom_point()
# gross
ggplot(locsnute.250, aes(x=DE, y=UseIntensity)) +
  stat_smooth(method = "auto") +
  geom_point()


## freq distn of per cap nute - use-intensity ##
test <- locsnute.250 %>%
  mutate(pcDE = DE/UseIntensity,
         pcBM = Biomass/UseIntensity,
         pcGDM = GDM/UseIntensity)
par(mfrow=c(3,1))
hist(test$pcDE)
hist(test$pcBM)
hist(test$pcGDM)

## poisson requires counts; can't do intensity
testmod <- glm(nLocs ~ pcGDM,
  family = poisson, 
  data = test)
summary(testmod)


## freq distn of per cap nute - nlocs ##
test <- locsnute.250 %>%
  mutate(pcDE = DE/nLocs,
         pcBM = Biomass/nLocs,
         pcGDM = GDM/nLocs)
par(mfrow=c(3,1))
hist(test$pcDE)
hist(test$pcBM)
hist(test$pcGDM)


## mod1 2 and 3 predictions ##
pp1 <- ggplot(locsnute.250, aes(x = DE, y = nLocs)) +
  stat_smooth(method="glm",
              se = TRUE) 
pp2 <- ggplot(locsnute.250, aes(x = DE, y = nLocs)) +
  stat_smooth(method="glm",
              se = TRUE,
              formula = y ~ poly(x, 2, raw = TRUE)) 
pp3 <- ggplot(locsnute.250, aes(x = DE, y = nLocs)) +
  stat_smooth(method="glm",
              se = TRUE,
              formula = y ~ poly(x, 3, raw = TRUE)) 
grid.arrange(pp1, pp2, pp3)


## ditto above but with gdm ##
pp1 <- ggplot(locsnute.250, aes(x = GDM, y = UseIntensity)) +
  stat_smooth(method="glm",
              se = TRUE) 
pp2 <- ggplot(locsnute.250, aes(x = GDM, y = UseIntensity)) +
  stat_smooth(method="glm",
              se = TRUE,
              formula = y ~ poly(x, 2, raw = TRUE)) 
pp3 <- ggplot(locsnute.250, aes(x = GDM, y = UseIntensity)) +
  stat_smooth(method="glm",
              se = TRUE,
              formula = y ~ poly(x, 3, raw = TRUE)) 
grid.arrange(pp1, pp2, pp3)


## aaaand biomass ##
pp1 <- ggplot(locsnute.250, aes(x = Biomass, y = UseIntensity)) +
  stat_smooth(method="glm",
              se = TRUE) 
pp2 <- ggplot(locsnute.250, aes(x = Biomass, y = UseIntensity)) +
  stat_smooth(method="glm",
              se = TRUE,
              formula = y ~ poly(x, 2, raw = TRUE)) 
pp3 <- ggplot(locsnute.250, aes(x = Biomass, y = UseIntensity)) +
  stat_smooth(method="glm",
              se = TRUE,
              formula = y ~ poly(x, 3, raw = TRUE)) 
grid.arrange(pp1, pp2, pp3)


# Useintensity by GDM, all popn and ea migstatus #
# got rid of these bc didn't want points included #
a <- ggplot(locsnute.no0, aes(x = GDM, y = UseIntensity)) +
  stat_smooth(method="loess") +
  geom_point(size=1)
b <- ggplot(uidat, aes(x = GDM, y = ResUI)) +
  stat_smooth(method="loess")+
  geom_point(size=1)
c <- ggplot(uidat, aes(x = GDM, y = IntUI)) +
  stat_smooth(method="loess")+
  geom_point(size=1)
d  <- ggplot(uidat, aes(x = GDM, y = MigUI)) +
  stat_smooth(method="loess")+
  geom_point(size=1)
grid.arrange(a,b,c,d, nrow=2)


# Useintensity by DE, all popn and ea migstatus #
a <- ggplot(locsnute.no0, aes(x = DE, y = UseIntensity)) +
  stat_smooth(method="loess") 
b <- ggplot(uidat, aes(x = DE, y = ResUI)) +
  stat_smooth(method="loess")
c <- ggplot(uidat, aes(x = DE, y = IntUI)) +
  stat_smooth(method="loess")
d  <- ggplot(uidat, aes(x = DE, y = MigUI)) +
  stat_smooth(method="loess")
grid.arrange(a,b,c,d, nrow=2)
# yeesh, super wonky relationships
# going to use GDM for the class project


# per capita GDM, all popn and ea migstatus #
# which doesn't make much sense if you think about it...
e <- ggplot(uidat, aes(x = GDM, y = NutePC)) +
  stat_smooth(method="loess")
f <- ggplot(uidat, aes(x = GDM, y = ResPC)) +
  stat_smooth(method="loess")
g <- ggplot(uidat, aes(x = GDM, y = IntPC)) +
  stat_smooth(method="loess")
h  <- ggplot(uidat, aes(x = GDM, y = MigPC)) +
  stat_smooth(method="loess")
grid.arrange(e,f,g,h, nrow=2)

### loess smoother vs glm smoother from top mode
### just out of curiosity
pp <- ggplot(locsnute.no0, aes(x = DE, y = UseIntensity)) +
  stat_smooth(method="glm",
              se = TRUE,
              formula = y ~ poly(x, 3, raw = TRUE)) 
ppl <- ggplot(locsnute.no0, aes(x = DE, y = UseIntensity)) +
  stat_smooth(method="loess",
              se = TRUE) 
grid.arrange(pp, ppl)


#### model comparison attempts ####

## quasipoisson vs negbin ##
# likelihood ratio test #
pchisq(2 * (logLik(mnb) - logLik(mqp)), df = 1, 
       lower.tail = FALSE)
# not actually sure i can do this
# nope, gives NA
