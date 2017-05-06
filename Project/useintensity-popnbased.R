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
library(adehabitatHR)
library(AICcmodavg) 
library(raster)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(dplyr)

## read in data ##
uidat <- read.csv("uidat.csv")



#### VISUALS ####


## default-smoothed nLocs ~ GDM w points
ggplot(uidat, aes(x = GDM, y = nLocs)) +
  stat_smooth() +
  geom_point(size = 1)

## default-smoothed UseIntensity ~ GDM
ggplot(uidat, aes(x = GDM, y = UseIntensity)) +
  stat_smooth() +
  geom_point(size = 1)
  
## smoothed nLocs ~ GDM wo points
ggplot(uidat, aes(x = GDM, y = nLocs)) +
  stat_smooth(method = "loess") 
ggplot(uidat, aes(x = GDM, y = nLocs)) +
  stat_smooth() 
ggplot(uidat, aes(x = GDM, y = nLocs)) +
  stat_smooth(method = "glm",
              formula = y ~ poly(x, 3, raw = TRUE)) 

## just points
pt <- ggplot(uidat, aes(y = nLocs, x = GDM)) +
  labs(y=expression(paste("Elk use (# individuals)", sep="")), 
       x="Available nutrition (GDM)") +
  geom_point(size = 1)
pt

pt + stat_smooth()
pt + stat_smooth(method="glm", 
                 formula = y ~ poly(x, 3, raw = TRUE))


#### MODELS ####


# define 5 model types to assess #

    # poisson
    mp <- glm(nLocs ~ GDM + I(GDM^2) + I(GDM^3),
                family = poisson, data = uidat)
    summary(mp) # retains 3
    
    # quasipoisson
    mqp <- glm(nLocs ~ GDM + I(GDM^2) + I(GDM^3),
                family = quasipoisson, data = uidat)
    summary(mqp) # retains 3
    
    # zero-truncated poisson
    mzp <- vglm(nLocs ~ GDM + I(GDM^2) + I(GDM^3),
                family = pospoisson(), data = uidat)
    summary(mzp)
    
    # negative binomial
    mnb <- glm.nb(nLocs ~ GDM + I(GDM^2) + I(GDM^3),
                data = uidat, link = log)
    summary(mnb) # retains 3
    
    # zero-truncated negative binomial
    mznb <- vglm(nLocs ~ GDM + I(GDM^2) + I(GDM^3),
                data = uidat, 
                family = posnegbinomial())
    summary(mznb)
    warnings()
      # warnings say not to use this


## test poisson for overdispersion ##
    library(AER)
    dispersiontest(mp, trafo = 1)
    # tests for diff bt a model of variance=mean and 
    # of variance = mean + some constant*a function of the mean
    # alpha = overdispersion parameter

## test square/cubic covariate significance in negbin
    drop1(mnb, test = "LRT")


#### model fit: variance-mean relationship ####
    
## kristin you stole most of this from the insanely helpful
## http://www.ssc.wisc.edu/sscc/pubs/RFR/RFR_RegressionGLM.html

# create df with model results (residuals, fitted vals, etc)
mnbDiag <- data.frame(uidat,
                     link=predict(mnb, type="link"),
                     fit=predict(mnb, type="response"),
                     pearson=residuals(mnb,type="pearson"),
                     resid=residuals(mnb,type="response"),
                     residSqr=residuals(mnb,type="response")^2
                     )

# visually compare how well negbin and quasipoisson
  # fit expected residual-mean relationship
# idea is you want model line closest to means of loess line
ggplot(data=mnbDiag, aes(x=fit, y=residSqr)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) + # poisson, black
  geom_abline(intercept = 0, slope = summary(mqp)$dispersion,
              color="green") + # quasipoisson
  stat_function(fun=function(fit){fit + fit^2/mnb$theta},
                color = "red") + #negbin
  stat_smooth(method="loess", se = FALSE) #loess, blue
# negbin ftw


# remove those 2 annoying outlier ag pivots & re-plot
    # just for better visual of lines on plot
    # actual relationship/inference doesn't change
mnbDiag1 <- filter(mnbDiag, resid < 20) 

# pretty report plot
ggplot(data=mnbDiag1, aes(x=fit, y=residSqr)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) + # poisson, black
  geom_abline(intercept = 0, slope = summary(mqp)$dispersion,
              color="green") + # quasipoisson
  stat_function(fun=function(fit){fit + fit^2/mnb$theta},
                color = "red") + #negbin
  stat_smooth(method="loess", se = FALSE) + #loess, blue
  labs(x = "Fitted values", y = "Squared residuals")
    


#### RELATIONSHIP PLOT FROM TOP MODEL ####

mplot <- ggplot(mnbDiag, aes(x = GDM, y = nLocs)) +
  labs(y=expression(paste("Elk use (# individuals)", sep="")), 
       x="Available nutrition (GDM)") +
  geom_point() +
  geom_line(aes(x = mnbDiag$GDM, y = mnbDiag$fit))
mplot

pt + stat_smooth()
pt + stat_smooth(method="glm", 
                 formula = y ~ poly(x, 3, raw = TRUE))

plot(inad$nElkPix, inad$RelFreq,
     main = "Inadequate Baseline Suitability",
     ylab = "Relative frequency of use",
     xlab = expression(paste("Conspecific Density (n/250",
                            m^2, ")", sep="")))
lines(dens, predi, type = "l")    

#### PER CAPITA NUTRITION BY MIGSTATUS ####
    
## violin plots ##
    
    # new df, longform #
    vdf <- uidat %>%
      mutate(ResGDM = ResGDM10*10,
              IntGDM = IntGDM10*10,
             MigGDM = MigGDM10*10) %>%
      dplyr::select(ResGDM, IntGDM, MigGDM) %>%
      gather(key = MigStatus, value = GDMpc) %>%
      transform(MigStatus = factor(MigStatus,
                        levels = c("ResGDM",
                                   "IntGDM",
                                   "MigGDM"),
                            ordered = TRUE)) 
    viol <- ggplot(data = vdf, aes(x = MigStatus, 
                                   y = log(GDMpc))) +
      geom_violin(fill="grey") +
        geom_boxplot(width=.1, outlier.colour=NA) +
        geom_hline(yintercept=2.75) +
        stat_summary(fun.y=mean, geom="point", 
                     fill="black", shape=21, size=2.5)
    viol
    

## anova ##
    a <- aov(GDMpc ~ MigStatus, data = vdf)
    summary(a)
    
    t <- TukeyHSD(aov(GDMpc ~ MigStatus, data = vdf))
    t
           

    
    
    
    
    
    
    
    
    
    
    
    
    
    ## ## ## ## ## ## ###
## CUTS AND MISC ####
## ## ## ## ## ## ###
    
    
        
## floundering attempts to assess relative model support ##
    
    # check residuals vs fitted plots
    par(mfrow=c(2,2))
    plot(mp) #poisson
    plot(mqp) #quasipoisson
    plot(mnb) #negbin
      # unclear, these all look similar to me :/

    
    # random stolen code looking at fit
    # for count data specifically
    table(uidat$nLocs)
      # suuuuper r-skewed, obvs
    library(vcd)
    fit <- goodfit(uidat$nLocs)
    summary(fit)
    rootogram(fit)
    Ord_plot(uidat$nLocs)
      # positive slope, negative intercept
    distplot(uidat$nLocs, type = "poisson")
    distplot(uidat$nLocs, type = "nbinomial")
      # both look like shit
    distplot(uidat$nLocs, type = NULL)
    
    
## trying to model zero-truncated negbin another way
    # bc i can't figure out the stupid errors
  install.packages("zerotrunc")
    
        
