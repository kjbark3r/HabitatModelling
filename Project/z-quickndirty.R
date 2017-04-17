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

test <- rlocs14 %>%
  mutate(pcDE = DE/nLocs,
         pcBM = Biomass/nLocs,
         pcGDM = GDM/nLocs)
par(mfrow=c(3,1))
hist(test$pcDE)
hist(test$pcBM)
hist(test$pcGDM)



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

itest <- ilocs14 %>%
  mutate(pcDE = DE/nLocs,
         pcBM = Biomass/nLocs,
         pcGDM = GDM/nLocs)
par(mfrow=c(3,1))
hist(itest$pcDE)
hist(itest$pcBM)
hist(itest$pcGDM)



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

mtest <- mlocs14 %>%
  mutate(pcDE = DE/nLocs,
         pcBM = Biomass/nLocs,
         pcGDM = GDM/nLocs)
par(mfrow=c(3,1))
hist(mtest$pcDE)
hist(mtest$pcBM)
hist(mtest$pcGDM)



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


