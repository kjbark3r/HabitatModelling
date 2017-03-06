###
## special MCP version (shouldn't be onerous my ass)

### WILD 562 LAb 5 - Multiple Logistic Regression and Model Selection


#function to install and load required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#load or install these packages:
packages <- c("car", "lattice", "ggplot2", "MASS", "AICcmodavg", "MuMIn", "effects", "corrgram", "GGally")

#run function to install packages
ipak(packages)
rm(ipak, packages)

library(dplyr)
library(stats)
library(gridExtra)

##### 0.1 Preliminaries: importing data #######################################################################

##SET WD
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\HabitatModelling\\Lab5_MultLogRgrsn-ModSlxn"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\HabitatModelling\\Lab5_MultLogRgrsn-ModSlxn"

ifelse(file.exists(wd_workcomp), setwd(wd_workcomp), setwd(wd_laptop))
getwd()
rm(wd_workcomp, wd_laptop)



####### MANUALLY SET DATA SOURCE ##
#wolfdata <- read.csv("wolfkde.csv", header=TRUE, sep = ",", na.strings="NA", dec=".")
#wolfdata2 <- na.omit(wolfdata)
#wolfdata2 <- na.omit(wolfdata) %>%
#  subset(pack == "Red Deer")

wolfdata <- read.csv("wolfmcp.csv", header=TRUE, sep = ",", na.strings="NA", dec=".")
wolfdata2 <- na.omit(wolfdata)

wolfdata2 <- wolfdata2 %>%
  rename(deer_w2 = deerwin,
         elk_w2 = elkwint,
         moose_w2 = moosewi,
         sheep_w2 = sheepwi,
         goat_w2 = goatwin,
         wolf_w2 = wolfwin,
         Elevation2 = elevati,
         DistFromHumanAccess2 = distacc,
         DistFromHighHumanAccess2 = disthha,
         habitatType = habitattyp,
         closedConif = ClosedConifer,
         modConif = ModerateConifer,
         openConif = OpenConifer,
         mixed = MixedForest,
         herb = Herbaceous,
         shrub = Shrub,
         water = Water,
         alpineHerb = AlpineHerb,
         alpineShrub = AlpineShrub) %>%
  mutate(alpine = alpineHerb+alpineShrub)
         

#### ASSESSING CONFOUNDING VARIABLES ####

## full model ##
model.full = glm(used~deer_w2 + elk_w2 +moose_w2 +sheep_w2+goat_w2+
                   Elevation2+DistFromHumanAccess2+DistFromHighHumanAccess2, 
                 data =wolfdata2, family= binomial(logit))

## UNIVARIATE MODELS ##
md = glm(used~deer_w2,
                 data =wolfdata2, family= binomial(logit))
melk = glm(used~elk_w2,
                 data =wolfdata2, family= binomial(logit))
mm = glm(used~moose_w2, 
                 data =wolfdata2, family= binomial(logit))
ms = glm(used~sheep_w2, 
                 data =wolfdata2, family= binomial(logit))
mg = glm(used~goat_w2, 
                 data =wolfdata2, family= binomial(logit))
melev = glm(used~Elevation2, 
                 data =wolfdata2, family= binomial(logit))
mh = glm(used~DistFromHumanAccess2, 
                 data =wolfdata2, family= binomial(logit))
mhh = glm(used~DistFromHighHumanAccess2, 
                 data =wolfdata2, family= binomial(logit))
ml = glm(used~landcov.f, 
                 data =wolfdata2, family= binomial(logit))

## extract coeffs from all models
coefs = data.frame(rbind(
  summary(md)$coefficients[,1:2], 
  summary(melk)$coefficients[,1:2],
  summary(mm)$coefficients[,1:2],
  summary(ms)$coefficients[,1:2],
  summary(mg)$coefficients[,1:2],
  summary(melev)$coefficients[,1:2],
  summary(mh)$coefficients[,1:2],
  summary(mhh)$coefficients[,1:2]))
coefs$Model = c("Deer Int", "Deer", 
                "Elk Int", "Elk", 
                "Moose Int", "Moose", 
                "Sheep Int", "Sheep", 
                "Goat Int", "Goat",
                "Elevation Int",  "Elevation",
                "DistHuman Int", "DistHuman",  
                "DistHighHuman Int", "DistHighHuman")
#write.csv(coefs, "coeffs-univariate.csv", row.names=F)

## extract coeffs from full model
coef(model.full)
coef2 <- data.frame(summary(model.full)$coefficients[,1:2]) %>%
  rename(Estfullmod = Estimate, SEfullmod = Std..Error)
coef2$Model <- c("Intercept", "Deer", 
                "Elk", "Moose","Sheep","Goat",
                "Elevation", "DistHuman", "DistHighHuman")
#write.csv(coef2, "coeffs-fullmodel.csv", row.names=F)

## combine and look for sign changes
coefcomp <- full_join(coefs, coef2, by = "Model")

## add "closed" canopy cover covariate
wolfdata2$closed <- wolfdata2$closedConif + wolfdata2$modConif + 
                    wolfdata2$openConif + 
                    wolfdata2$mixed + wolfdata2$burn
wolfdata2$closedFactor <-as.factor(wolfdata2$closed)
wolfdata2$closedFactor <- ifelse(wolfdata2$closedFactor == 0, 
                                 "Open", "Closed")


### ASSESSING COLLINEARITY ###

# VIF - get idea of biggest potential issues
a <- vif(model.full)
View(a)
## elev>elk>deer>highhuman


# correlation coeffs from bassing's prof's code
dat.cor <- wolfdata2 %>%
  dplyr::select(deer_w2, moose_w2, elk_w2, sheep_w2, goat_w2,
                Elevation2, DistFromHumanAccess2,
                DistFromHighHumanAccess2, closed)
source("../../zMisc/pairs-panels.R")
pairs.panels(dat.cor) # bassing's prof's code to handle factors


### DETERMINING WHICH COVARIATES CONFOUND MOOSE ###

mmd <- glm(used~moose_w2+ deer_w2, 
                 data =wolfdata2, family= binomial(logit))
coef(mm); coef(mmd)
# deer

mme <- glm(used~moose_w2+ elk_w2, 
                 data =wolfdata2, family= binomial(logit))
coef(mm); coef(mme)
# and elk

mmev <- glm(used~moose_w2+ Elevation2, 
                 data =wolfdata2, family= binomial(logit))
coef(mm); coef(mmev)
# and elev

## ok prob shouldn't incl moose with other prey species
## and won't be using elev anyway


### DETERMINING WHICH COVARIATES CONFOUND HIGH HUMAN ###
mhhev = glm(used~DistFromHighHumanAccess2+Elevation2, 
                 data =wolfdata2, family= binomial(logit))
coef(mhhev); coef(mh)
#  elev, shocker


### ASSESSING LANDCOVER CORRELATIONS ###

# collapsing some types 
wolfdata2 <- wolfdata2 %>%
  mutate(alpinerock = alpine+RockIce)
# adding combined deer/elk
wolfdata2$prefprey <- (wolfdata2$deer_w2 + wolfdata2$elk_w2)/2
wolfdata2$prefpreys <- (wolfdata2$deer_w2 + wolfdata2$elk_w2 + 
                          wolfdata2$moose_w2)/3


#### MODEL SELECTION ####


## first test prefprey vs deer-elk
mods <- list()
mods[[1]] <- glm(used ~ prefprey, 
                 data = wolfdata2, family = binomial(logit))
mods[[2]] <- glm(used ~ elk_w2, 
                 data = wolfdata2, family = binomial(logit))
mods[[3]] <- glm(used ~ deer_w2, 
                 data = wolfdata2, family = binomial(logit))
modnms <- c("prefprey", "elk", "deer")
aictab(cand.set = mods, modnames = modnms)
# prefprey ftw


## including moose as potential prefprey
mods <- list()
mods[[1]] <- glm(used ~ prefpreys, 
                 data = wolfdata2, family = binomial(logit))
mods[[2]] <- glm(used ~ elk_w2, 
                 data = wolfdata2, family = binomial(logit))
mods[[3]] <- glm(used ~ deer_w2, 
                 data = wolfdata2, family = binomial(logit))
mods[[4]] <- glm(used ~ moose_w2, 
                 data = wolfdata2, family = binomial(logit))
modnms <- c("prefpreys", "elk", "deer", "moose")
aictab(cand.set = mods, modnames = modnms)
# prefprey ftw


## a priori hypotheses
mods <- list()
mods[[1]] <- glm(used ~ prefprey, 
                 data = wolfdata2, family = binomial(logit))
mods[[2]] <- glm(used ~ DistFromHumanAccess2, 
                 data = wolfdata2, family = binomial(logit))
mods[[3]] <- glm(used ~ prefprey + 
                   DistFromHumanAccess2, 
                 data = wolfdata2, family = binomial(logit))
mods[[4]] <- glm(used ~ prefprey + 
                   closed + 
                   prefprey*closed, 
                 data = wolfdata2, family = binomial(logit))
mods[[5]] <- glm(used ~ DistFromHumanAccess2 + 
                   closed + 
                   DistFromHumanAccess2*closed, 
                 data = wolfdata2, family = binomial(logit))
mods[[6]] <- glm(used ~ prefprey + 
                   closed + prefprey*closed +
                   DistFromHumanAccess2 + 
                   DistFromHumanAccess2*closed, 
                 data = wolfdata2, family = binomial(logit))
mods[[7]] <- glm(used ~ prefprey + 
                   closed +
                   DistFromHumanAccess2 + 
                   DistFromHumanAccess2*closed, 
                 data = wolfdata2, family = binomial(logit))

modnms <- c("prefprey", "human", "prey+hum", "prey*canopy", 
            "human*canopy", "prey*canopy-human*canopy",
            "prey-human*canopy")
aictab(cand.set = mods, modnames = modnms)

## BIC
bictab <- data.frame(mod = c("prefprey", "human", "prey+hum", "prey*canopy", 
            "human*canopy", "prey*canopy-human*canopy",
            "prey-human*canopy"),
            res = c(BIC(mods[[1]]), BIC(mods[[2]]), BIC(mods[[3]]), 
                    BIC(mods[[4]]), BIC(mods[[5]]), BIC(mods[[6]]),
                    BIC(mods[[7]])))
arrange(bictab, res)
write.csv(bictab, "bic-reddeer.csv", row.names=F)



# intrxn with  human
topmod <- glm(used ~ prefprey + closed + 
                   DistFromHumanAccess2 + DistFromHumanAccess2*closed, 
                 data = wolfdata2, family = binomial(logit))
summary(topmod)


# prey + human
topmod2 <- glm(used ~ prefprey + DistFromHumanAccess2,
               data = wolfdata2, family = binomial(logit))
summary(topmod2)





## full model plot ##
plothum <- ggplot(wolfdata2, aes(x=DistFromHumanAccess2, 
                     y=used, linetype=closedFactor)) + 
  labs(x = "Distance From Humans (m)", y = "Pr(Use)") +
  theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Open canopy", "Closed canopy")) +
  stat_smooth(method="glm", 
              method.args = list(family="binomial"), 
              level=0.95) 
plotprey <- ggplot(wolfdata2, aes(x=prefprey, 
                     y=used)) + 
  stat_smooth(method="glm", 
              method.args = list(family="binomial"), 
              level=0.95) 
grid.arrange(plothum, plotprey, nrow=2)



#### INTERPRETATIONS ####

ORprey <- exp(1.12)
ORhum <- exp(-0.0032082)
ORclosedhum <- exp(0.0019875)


#### Attempting to understand plots ####

# how dist to humans changes with canopy cover
ggplot(wolfdata2, aes(x=DistFromHumanAccess2, 
                     y=used, fill=closedFactor)) + 
  stat_smooth(method="glm", 
              method.args = list(family="binomial"), 
              level=0.95) 

# how prey selxn changes with canopy cover
ggplot(wolfdata2, aes(x=prefprey, 
                     y=used, fill=closedFactor)) + 
  stat_smooth(method="glm", 
              method.args = list(family="binomial"), 
              level=0.95) 


#### OBJECTIVE 8.0 - Caterpillar plots of coefficients ####


## or check out the cool GGally package
require(GGally)
ggcoef(topmod)
## super cool!


require(ggplot2)


B<-summary(topmod)$coefficient[1:length(summary(topmod)$coefficient[,1]),1]
#create margin of error (ME) for 95% CI
ME <- summary(topmod)$coefficient[1:length(summary(topmod)$coefficient[,1]),2]*1.96
lower<-B - ME
upper<-B + ME


# bundle into data frame
logisData<-data.frame(B, lower, upper, names(summary(topmod)$coefficient[,2]))
names(logisData) <- c("Coefficient", "lower.ci", "upper.ci", "Variable")
levels(logisData$Variable)[1] <- "Intercept"
logisData$Variable <- relevel(logisData$Variable, ref="Intercept")



# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.6) # move them .05 to the left and right
x1<-ggplot(data=logisData, aes(x=Variable,y=Coefficient)) +
  geom_errorbar(data=logisData,aes(ymin=lower.ci, ymax=upper.ci), width=.4,position=pd,size=1) +
  geom_point(size=3, col="blue") 

p6<-x1+theme(axis.text.y = element_text(size=14, family="Times"),axis.text.x = element_text(size=14, family="Times"),text = element_text(size=16, family="Times"),axis.title.x=element_text(size=16, family="Times"),axis.title.y=element_text(size=16, family="Times",vjust=1))
p7<-p6+theme(axis.line.x = element_line(color="black", size = 0.25),
             axis.line.y = element_line(color="black", size = 0.25),legend.title=element_blank(),legend.text=element_text(size=16, family="Times"))+ylab("Estimate") + xlab("Coefficient")

p7

tiff("coefPlot.tiff", res=600, compression = "lzw", height=5, width=7, units="in")
p7
dev.off()


