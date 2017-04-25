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


## Pr(Use) ~ DE category ####

  # excellent DE #
  mexc1 <- glm.nb(nExc ~ offset(log(nIndiv)) + nLocs, 
                  link = log, data= ruf)
  mexc2 <- glm.nb(nExc ~ offset(log(nIndiv)) + nLocs + I(nLocs^2), 
                  link = log, data= ruf)
  mexc3 <- glm.nb(nExc ~ offset(log(nIndiv)) + nLocs + I(nLocs^2) + I(nLocs^3), 
                  link = log, data= ruf)
  BIC(mexc1, mexc2, mexc3) # squared ftw

  # good DE #
  mgd1 <- glm.nb(nGood ~ offset(log(nIndiv)) + nLocs, 
                  link = log, data= ruf)
  mgd2 <- glm.nb(nGood ~ offset(log(nIndiv)) + nLocs + I(nLocs^2), 
                  link = log, data= ruf)
  mgd3 <- glm.nb(nGood ~ offset(log(nIndiv)) + nLocs + I(nLocs^2) + I(nLocs^3), 
                  link = log, data= ruf)
  BIC(mgd1, mgd2, mgd3) # squared/cubic indistinguishable
 
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


#### VISUALS - PREDICTED PLOTS ####

# all together
pp <- ggplot(ruf, aes(nLocs)) 
pexc <- stat_smooth(aes(y = nExc/nIndiv, colour = "nExc"),
                    method = "glm",  
                    formula = y ~ poly(x, 2, raw = TRUE))
pgd <- stat_smooth(aes(y = nGood/nIndiv, colour = "nGood"),
                    method = "glm",  
                    formula = y ~ poly(x, 3, raw = TRUE))
pmrg <- stat_smooth(aes(y = nMarg/nIndiv, colour = "nMarg"),
                    method = "glm",  
                    formula = y ~ poly(x, 3, raw = TRUE))
ppoor <- stat_smooth(aes(y = nPoor/nIndiv, colour = "nPoor"),
                    method = "glm")
pp+pexc+pgd+pmrg+ppoor


# split by adequate/inadequate
z <- pp+pexc+pgd
y <- pp+pmrg+ppoor
grid.arrange(z, y, nrow=2)

