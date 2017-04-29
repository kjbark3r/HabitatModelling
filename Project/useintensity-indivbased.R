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

## read in data (from useintensity.R)
ruf <- read.csv("ruf-data.csv")



#### MODELS - PR(SELXN) ####

## poisson, or negative binomial? ##

  
  ### ### ##
  ## nExc ##

  mep <- glm(nExc ~ offset(log(nIndiv)) + nLocs, 
                  family = poisson, data= ruf,
             subset = HabSuit=="Excellent")
  menb <- glm.nb(nExc ~ offset(log(nIndiv)) + nLocs, 
                  link = log, data= ruf,
             subset = HabSuit=="Excellent")
  par(mfrow=c(2,2))
  plot(mep)
  plot(menb)
  # check out weirdo points
  wtfs <- data.frame(hatvalues(menb))
  wtfs <- data.frame(menb$fitted.values)
  # remove outlier indiv, collar malfunction not enuf locs
    mep <- glm(nExc ~ offset(log(nIndiv)) + nLocs, 
                  family = poisson, data= ruf,
             subset = HabSuit=="Excellent" & IndivYr != "141160-15")
  menb <- glm.nb(nExc ~ offset(log(nIndiv)) + nLocs, 
                  link = log, data= ruf,
             subset = HabSuit=="Excellent" & IndivYr != "141160-15")
  plot(mep)
  plot(menb)
  # ahhh, that's better
  summary(mep)
    # yeeeep overdispersed
  summary(menb)
    # much better, only ~1.1 now
  # negative binomial > poisson?
  pchisq(2 * (logLik(menb) - logLik(mep)), df = 1, lower.tail = FALSE)
    # duh

  
  
  ### ### ###
  ## nGood ##
  
  mgp <- glm(nGood ~ offset(log(nIndiv)) + nLocs, 
                  family = poisson, data= ruf,
             subset = HabSuit=="Good")
  mgnb <- glm.nb(nGood ~ offset(log(nIndiv)) + nLocs, 
                  link = log, data= ruf,
             subset = HabSuit=="Good")
  par(mfrow=c(2,2))
  plot(mgp)
  plot(mgnb)
  # id weirdo points
  wtfs <- data.frame(mgnb$fitted.values)
  # same indiv; removed. bummer, i liked her.
  mgp <- glm(nGood ~ offset(log(nIndiv)) + nLocs, 
                  family = poisson, data= ruf,
             subset = HabSuit=="Good" & IndivYr != "141160-15")
  mgnb <- glm.nb(nGood ~ offset(log(nIndiv)) + nLocs, 
                  link = log, data= ruf,
             subset = HabSuit=="Good" & IndivYr != "141160-15")
  plot(mgp)
  plot(mgnb)
  # lovely
  summary(mgp)
    # holy balls, that's terrible
  summary(mgnb)
    # way better
  # negative binomial > poisson?
  pchisq(2 * (logLik(menb) - logLik(mep)), df = 1, lower.tail = FALSE)


  
  ### ### ### #
  ## Ad/Inad ##
  # trying this bc good has super narrow range of predictions
  
  ## ad ##
  madp <- glm(nAd ~ offset(log(nIndiv)) + nLocs, 
                  family = poisson, data= ruf,
             subset = DEcat =="Adequate" & IndivYr != "141160-15")
  madnb <- glm.nb(nAd ~ offset(log(nIndiv)) + nLocs, 
                  link = log, data= ruf,
             subset = DEcat =="Adequate" & IndivYr != "141160-15")
  plot(madp)
  plot(madnb)
  wtfs <- data.frame(madnb$fitted.values)
  View(wtfs)
  summary(madp)
  summary(madnb)  
  mad1 <- glm.nb(nAd ~ offset(log(nIndiv)) + nLocs, 
                  link = log, data= ruf,
             subset = DEcat =="Adequate" & IndivYr != "141160-15")
  mad2 <- glm.nb(nAd ~ offset(log(nIndiv)) + nLocs + I(nLocs^2), 
                  link = log, data= ruf,
             subset = DEcat=="Adequate" & IndivYr != "141160-15")
  mad3 <- glm.nb(nAd ~ offset(log(nIndiv)) + nLocs + I(nLocs^2) + I(nLocs^3), 
                  link = log, data= ruf,
             subset = DEcat=="Adequate" & IndivYr != "141160-15")
  AIC(mad1, mad2, mad3) # cubed ftw
  
  
  ## inad ##
  minadp <- glm(nInad ~ offset(log(nIndiv)) + nLocs, 
                  family = poisson, data= ruf,
             subset = DEcat =="Inadequate" & IndivYr != "141160-15" & IndivYr != "140120-15")
  minadnb <- glm.nb(nInad ~ offset(log(nIndiv)) + nLocs, 
                  link = log, data= ruf,
             subset = DEcat =="Inadequate" & IndivYr != "141160-15" & IndivYr != "140120-15")
  plot(minadp)
  plot(minadnb)
  wtfs <- data.frame(madnb$fitted.values)
  View(wtfs)
  summary(minadp)
  summary(minadnb)  
  # shape of relationship
  minad1 <- glm.nb(nInad ~ offset(log(nIndiv)) + nLocs, 
                  link = log, data= ruf,
             subset = DEcat =="Inadequate" & IndivYr != "141160-15" & IndivYr != "140120-15")
  minad2 <- glm.nb(nInad ~ offset(log(nIndiv)) + nLocs + I(nLocs^2), 
                  link = log, data= ruf,
             subset = DEcat=="Inadequate" & IndivYr != "141160-15" & IndivYr != "140120-15")
  minad3 <- glm.nb(nInad ~ offset(log(nIndiv)) + nLocs + I(nLocs^2) + I(nLocs^3), 
                  link = log, data= ruf,
             subset = DEcat=="Inadequate" & IndivYr != "141160-15" & IndivYr != "140120-15")
  AIC(minad1, minad2, minad3) # lin/squ ftw
  
  
  
  ## marginal only, bc nothing really in poor hab ##
  mmp <- glm(nMarg ~ offset(log(nIndiv)) + nLocs, 
                  family = poisson, data= ruf,
             subset = HabSuit =="Marginal" & IndivYr != "141160-15" & IndivYr != "140120-15")
  mmnb <- glm.nb(nMarg ~ offset(log(nIndiv)) + nLocs, 
                  link = log, data= ruf,
             subset = HabSuit =="Marginal" & IndivYr != "141160-15" & IndivYr != "140120-15")
  plot(mmp)
  plot(mmnb)
  wtfs <- data.frame(mmnb$fitted.values)
  View(wtfs)
  summary(mmp)
  summary(mmnb)  
  # shape of relationship
  mm1 <- glm.nb(nMarg ~ offset(log(nIndiv)) + nLocs, 
                  link = log, data= ruf,
             subset = HabSuit =="Marginal" & IndivYr != "141160-15" & IndivYr != "140120-15")
  mm2 <- glm.nb(nMarg ~ offset(log(nIndiv)) + nLocs + I(nLocs^2), 
                  link = log, data= ruf,
             subset = HabSuit =="Marginal" & IndivYr != "141160-15" & IndivYr != "140120-15")
  mm3 <- glm.nb(nMarg ~ offset(log(nIndiv)) + nLocs + I(nLocs^2) + I(nLocs^3), 
                  link = log, data= ruf,
             subset = HabSuit =="Marginal" & IndivYr != "141160-15" & IndivYr != "140120-15")
  AIC(mm1, mm2, mm3) # lin/squ ftw, same as for inadequate
  
  

## Pr(Use) ~ DE category , NegBin ####

  # excellent DE #
  mexc1 <- glm.nb(nExc ~ offset(log(nIndiv)) + nLocs, 
                  link = log, data= ruf,
             subset = HabSuit=="Excellent" & IndivYr != "141160-15")
  mexc2 <- glm.nb(nExc ~ offset(log(nIndiv)) + nLocs + I(nLocs^2), 
                  link = log, data= ruf,
             subset = HabSuit=="Excellent" & IndivYr != "141160-15")
  mexc3 <- glm.nb(nExc ~ offset(log(nIndiv)) + nLocs + I(nLocs^2) + I(nLocs^3), 
                  link = log, data= ruf,
             subset = HabSuit=="Excellent" & IndivYr != "141160-15")
  AIC(mexc1, mexc2, mexc3) # cubed ftw

  # good DE #
  mgd1 <- glm.nb(nGood ~ offset(log(nIndiv)) + nLocs, 
                  link = log, data= ruf,
             subset = HabSuit=="Good" & IndivYr != "141160-15")
  mgd2 <- glm.nb(nGood ~ offset(log(nIndiv)) + nLocs + I(nLocs^2), 
                  link = log, data= ruf,
             subset = HabSuit=="Good" & IndivYr != "141160-15")
  mgd3 <- glm.nb(nGood ~ offset(log(nIndiv)) + nLocs + I(nLocs^2) + I(nLocs^3), 
                  link = log, data= ruf,
             subset = HabSuit=="Good" & IndivYr != "141160-15")
  AIC(mgd1, mgd2, mgd3) # squared/cubic indistinguishable
 
  
  
  # marginal DE #
  mmrg1 <- glm.nb(nMarg ~ offset(log(nIndiv)) + nLocs, 
                  link = log, data= ruf)
  mmrg2 <- glm.nb(nMarg ~ offset(log(nIndiv)) + nLocs + I(nLocs^2), 
                  link = log, data= ruf)
  mmrg3 <- glm.nb(nMarg ~ offset(log(nIndiv)) + nLocs + I(nLocs^2) + I(nLocs^3), 
                  link = log, data= ruf)
  BIC(mmrg1, mmrg2, mmrg3) # cubic ftw
  
  # poor DE #
  mpr1 <- glm.nb(nPoor ~ offset(log(nIndiv)) + nLocs, 
                  link = log, data= ruf)
  mpr2 <- glm.nb(nPoor ~ offset(log(nIndiv)) + nLocs + I(nLocs^2), 
                  link = log, data= ruf)
  mpr3 <- glm.nb(nPoor ~ offset(log(nIndiv)) + nLocs + I(nLocs^2) + I(nLocs^3), 
                  link = log, data= ruf)
  BIC(mpr1, mpr2, mpr3) # linear ftw


## top models ##
mexc2 <- glm.nb(nExc ~ offset(log(nIndiv)) + nLocs + I(nLocs^2), 
                  link = log, data= ruf)
summary(mexc2)  


#### VISUALS - RELATIONSHIP PLOTS ####


## no no these are bad loess-smoothed plots
## Pr(USE) ~ DE categories, split by MigStatus ####
a <- ggplot(ruf, aes(x = nLocs, y = nExc/nIndiv,
                     linetype = MigStatus)) +
  stat_smooth(method = loess) +
  geom_point(size = 1)
b <- ggplot(ruf, aes(x = nLocs, y = nGood/nIndiv,
                     linetype = MigStatus)) +
  stat_smooth(method = loess) +
  geom_point(size = 1)
c <- ggplot(ruf, aes(x = nLocs, y = nMarg/nIndiv,
                     linetype = MigStatus)) +
  stat_smooth(method = loess) +
  geom_point(size = 1)
d <- ggplot(ruf, aes(x = nLocs, y = nPoor/nIndiv,
                     linetype = MigStatus)) +
  stat_smooth(method = loess) +
  geom_point(size = 1)
grid.arrange(a, b, c, d, nrow = 2)


## Pr(USE) ~ DE categories, NOT split by MigStatus ####
e <- ggplot(ruf, aes(x = nLocs, y = nExc/nIndiv)) +
  stat_smooth(method = loess) +
  geom_point(size = 1)
f <- ggplot(ruf, aes(x = nLocs, y = nGood/nIndiv)) +
  stat_smooth(method = loess) +
  geom_point(size = 1)
g <- ggplot(ruf, aes(x = nLocs, y = nMarg/nIndiv)) +
  stat_smooth(method = loess) +
  geom_point(size = 1)
h <- ggplot(ruf, aes(x = nLocs, y = nPoor/nIndiv)) +
  stat_smooth(method = loess) +
  geom_point(size = 1)
grid.arrange(e,f,g,h, nrow = 2)


#### UPDATED VISUALS - properly subsetted models ####

pp <- ggplot(ruf, aes(nLocs)) +
    labs(x=expression(paste("Conspecific Density (n/250",
                            m^2, ")", sep="")), 
       y="Relative frequency of use",
       colour = "Habitat Type") 

# excellent/good #
pexc <- stat_smooth(aes(y = nExc/nIndiv, colour = "nExc"),
                    data = subset(ruf, HabSuit == "Excellent"),
                    method = "glm",  
                    formula = y ~ poly(x, 2, raw = TRUE))
pgd <- stat_smooth(aes(y = nGood/nIndiv, colour = "nGood"),
                    data = subset(ruf, HabSuit == "Good"),
                    method = "glm",  
                    formula = y ~ poly(x, 2, raw = TRUE))

# adequate #
pad <-   stat_smooth(aes(y = nAd/nIndiv, colour = "nAd"),
                    data = subset(ruf, DEcat == "Adequate"),
                    method = "glm",  
                    formula = y ~ poly(x, 3, raw = TRUE))
pinad <-   stat_smooth(aes(y = nAd/nIndiv, colour = "nInd"),
                    data = subset(ruf, DEcat == "Inadequate"),
                    method = "glm",  
                    formula = y ~ poly(x, 2, raw = TRUE))



#### VISUALS - PREDICTED PLOTS ####

# all together
pp <- ggplot(ruf, aes(nLocs))  +
  labs(x=expression(paste("Conspecific Density (n/250",
                            m^2, ")", sep="")), 
       y="Relative frequency of use",
       colour = "Habitat Type") +
  scale_color_hue(labels = c("Marginal", 
                             "Poor"))
pexc <- stat_smooth(aes(y = nExc/nIndiv, colour = "nExc"),
                    method = "glm",  
                    formula = y ~ poly(x, 2, raw = TRUE))
pgd <- stat_smooth(aes(y = nGood/nIndiv, colour = "nGood"),
                    method = "glm",  
                    formula = y ~ poly(x, 3, raw = TRUE))
pp+pexc+pgd

pmrg <- stat_smooth(aes(y = nMarg/nIndiv, colour = "nMarg"),
                    method = "glm",  
                    formula = y ~ poly(x, 3, raw = TRUE))
ppoor <- stat_smooth(aes(y = nPoor/nIndiv, colour = "nPoor"),
                    method = "glm")
pp+pmrg+ppoor



# split by adequate/inadequate
z <- pp+pexc+pgd
y <- pp+pmrg+ppoor
grid.arrange(z, y, nrow=2)
z



