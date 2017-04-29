# # # # # # # #  # # # # # # # # # # # # # # # # 
#     HERBACEOUS GDM MODEL - CREATED FOR       #
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
library(AICcmodavg) 
library(raster)
library(rgdal) # read/write shp's
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)


#### DATA AND COVARIATE PREP ####

# GDM, plot, and covariate info
moddat <- read.csv("../../Vegetation/GDM-model-data.csv") %>%
  rename(landcov = cover_class) 


# Set factor levels for landcover type #
ref.lev <- moddat %>%
  dplyr::select(c(landcov, class_name, GDMherb)) %>%
  group_by(class_name, landcov) %>%
  summarise(MedianGDMh = median(GDMherb)) %>%
  arrange(MedianGDMh) %>% ## Order from least to most forb GDM 
  ungroup() 
ref.lev # so reference level is landcover type with lowest DE
moddat$landcov <- factor(moddat$landcov, levels = as.vector(ref.lev$landcov))
write.csv(ref.lev, file = "GDMherb-by-landcover.csv", row.names=F)

dat.cor <- moddat %>%
  dplyr::select(cc_std, elev_std, landcov, gsri_std, ndvi_dur_std, 
         ndvi_ti_std, sum_precip_std, slope_std, GDMherb)
source("../../zMisc/pairs-panels.R")
pairs.panels(dat.cor) 
## 0.52 cc-elev
## 0.65 cc-landcov
## 0.57 elev-ti.ndvi
# removing cc in order to keep landcov & elev
# also removed ndvi_dur bc of NAs
# and because it's never been important before...



#### MODEL "SELECTION" ####

m.glob <- glm(GDMherb+0.001 ~ elev_std + landcov + gsri_std + 
                 ndvi_ti_std + sum_precip_std + slope_std,
               family = Gamma, data = moddat)
step.aic <- step(m.glob, direction = "both", k = 2)

m.top <- lm(GDMherb ~ elev_std + landcov + gsri_std + 
              ndvi_ti_std + slope_std, 
             data = moddat)
summary(m.top)



#### RASTER PREDICTION ####

# create predictive model (unstandardized covariates)
m.pred <- lm(GDMherb ~ elev + landcov + gsri + ndvi_ti + slope, 
           data = moddat)

# name and stack rasters
wd <- getwd()
rast.14 <- list.files(path=paste(wd, "writtenrasters/covs2014", sep="/"), 
                      pattern="tif$", full.names=TRUE) #read in 2014 rasters
stack.14 <- stack(rast.14)
names(stack.14) # rename rasters to match covariate names
names(stack.14) <- c("elev", "gsri", "landcov", "ndvi_ti", "precip", "slope")
names(stack.14) # sanity check
rast.15 <- list.files(path=paste(wd, "writtenrasters/covs2015", sep="/"), 
                      pattern="tif$", full.names=TRUE)  #read in 2015 rasters
stack.15 <- stack(rast.15)
names(stack.15) 
names(stack.15) <- c("elev", "gsri", "landcov", "ndvi_ti", "precip", "slope")
names(stack.15)

# function to get both response and SE predictions
predfun <- function(model, data) {
  v <- predict(model, data, se.fit=TRUE) 
  cbind(p=as.vector(v$fit), se=as.vector(v$se.fit))
}

# predict 2014 rasters of DE and StdError (indices 1 and 2, respectively)
de2014 <- predict(stack.14, m.pred, fun=predfun, index=1:2, progress="text")
names(de2014) <- c("GDMherb2014","StdErr14") 
#plot(de2014) # plot both
plot(de2014[["GDMherb2014"]]) # plot one at a time
#plot(de2014[["StdErr14"]])

# predict 2015 rasters of DE and StdError (indices 1 and 2, respectively)
de2015 <- predict(stack.15, m.pred, fun=predfun, index=1:2, progress="text")
names(de2015) <- c("GDMherb2015","StdErr15") 
#plot(de2015) # plot both
plot(de2015[["GDMherb2015"]]) # plot one at a time
#plot(de2015[["StdErr15"]])

# export DE rasters
writeRaster(de2014, names(de2014), bylayer = TRUE, 
            format = "GTiff", overwrite = TRUE)
writeRaster(de2015, names(de2015), bylayer = TRUE, 
            format = "GTiff", overwrite = TRUE)
