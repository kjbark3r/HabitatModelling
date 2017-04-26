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
    mp <- glm(nLocs ~ GDM,
                family = poisson, data = uidat)
    summary(mp)
    
    # quasipoisson
    mqp <- glm(nLocs ~ GDM,
                family = quasipoisson, data = uidat)
    summary(mqp)
    
    # zero-truncated poisson
    mzp <- vglm(nLocs ~ GDM,
                family = pospoisson(), data = uidat)
    summary(mzp)
    
    # negative binomial
    mnb <- glm.nb(nLocs ~ GDM,
                data = uidat, link = log)
    summary(mnb)
    
    # zero-truncated negative binomial
    mznb <- vglm(nLocs ~ GDM,
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

## relative support for zero-truncated poisson or negbin ##
AIC(mzp)
AIC(mnb)
# zero-truncated poisson
# but i'm nervous about the overdispersion


    
## chi-sq tests of relative likelihoods ##
    
    # negative binomial > poisson?
    pchisq(2 * (logLik(mnb) - logLik(mp)), df = 1, lower.tail = FALSE)
      # yes, negbin better
    
    # zero-truncated poisson > poisson?
    pchisq(2 * (logLik(mzp) - logLik(mp)), df = 1, lower.tail = FALSE)
      # yes, zero-truncated better
    
    # zero-truncated poisson > negative binomial?
    pchisq(2 * (logLik(mzp) - logLik(mnb)), df = 1, lower.tail = FALSE)
      # yes, shit
    
    # zero-truncated poisson > zero-truncated negative binomial?
    pchisq(2 * (logLik(mzp) - logLik(mznb)), df = 1, lower.tail = FALSE)
      # no? i don't get what it means when it says 1 
    pchisq(2 * (logLik(mznb) - logLik(mzp)), df = 1, lower.tail = FALSE)
    
    # quasipoisson > negative binomial?
    pchisq(2 * (logLik(mqp) - logLik(mnb)), df = 1, lower.tail = FALSE)
      # can't do this bc quasipoisson doesn't have a likelihood, ugh


    


        
## just playing with the zero-truncated poisson for now
    mzp1 <- vglm(nLocs ~ GDM,
                family = pospoisson(), data = uidat)
    mzp2 <- vglm(nLocs ~ GDM + I(GDM^2),
                family = pospoisson(), data = uidat)
    mzp3 <- vglm(nLocs ~ GDM + I(GDM^2) + I(GDM^3),
                family = pospoisson(), data = uidat)
    AIC(mzp1); AIC(mzp2); AIC(mzp3)
    BIC(mzp1); BIC(mzp2); BIC(mzp3)
      # cubic ftw as usual
    
    
 
    

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
    
        
