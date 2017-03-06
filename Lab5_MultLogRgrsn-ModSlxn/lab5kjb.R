
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

wolfdata <- read.csv("wolfkde.csv", header=TRUE, sep = ",", na.strings="NA", dec=".")
wolfdata2 <- na.omit(wolfdata) #%>%
#  subset(pack == "Red Deer")



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
                    wolfdata2$openConif + wolfdata2$decid + 
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
# not deer

mme <- glm(used~moose_w2+ elk_w2, 
                 data =wolfdata2, family= binomial(logit))
coef(mm); coef(mme)
# not elk

mmev <- glm(used~moose_w2+ Elevation2, 
                 data =wolfdata2, family= binomial(logit))
coef(mm); coef(mmev)
# not elev


### DETERMINING WHICH COVARIATES CONFOUND  HUMAN ###
mhhev = glm(used~DistFromHumanAccess2+Elevation2, 
                 data =wolfdata2, family= binomial(logit))
coef(mhhev); coef(mh)
# not elev, weird

mhuh <- glm(used~DistFromHumanAccess2+goat_w2, 
                 data =wolfdata2, family= binomial(logit))
coef(mhuh); coef(mh)
# no clue, tried all


### ASSESSING LANDCOVER CORRELATIONS ###

# collapsing some types 
wolfdata2 <- wolfdata2 %>%
  mutate(populus = decid+mixed,
         alpinerock = alpine+rockIce,
         shrubreg = shrub + regen)
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
BIC(mods[[1]]); BIC(mods[[2]]); BIC(mods[[3]])
# prefprey ftw


## now include moose as potential prefprey (specific to RD)
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
BIC(mods[[1]]); BIC(mods[[2]]); BIC(mods[[3]]); BIC(mods[[4]])
aictab(cand.set = mods, modnames = modnms)
# deer wins here
## so using prefprey bc already know that's better than deer


## below line renames prefpreys to prefprey 
  ## to keep below code same for red deer specifically
#wolfdata2$prefprey <- wolfdata2$prefpreys

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



# prey + human
topmod <- glm(used ~ prefprey + DistFromHumanAccess2,
               data = wolfdata2, family = binomial(logit))
summary(topmod)

# intrxn with just human
topmod2 <- glm(used ~ prefprey + closed + 
                   DistFromHumanAccess2 + DistFromHumanAccess2*closed, 
                 data = wolfdata2, family = binomial(logit))
summary(topmod2)

# intrxn with just human
topmod3 <- glm(used ~ closed + 
                   DistFromHumanAccess2 + DistFromHumanAccess2*closed, 
                 data = wolfdata2, family = binomial(logit))
summary(topmod3)






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



#########################################

#### CUTS ####

## additional options (prob delete these)

# ## using the ggcorr package
# #~# gives actual numbers
# ggcorr(wolfdata[1:9], label = TRUE)

# ## GGally package with ggpairs()
# # slow but beautiful
# ggpairs(wolfdata[1:9])
# 
# # Function for calculating correlations and probabilities, https://stat.ethz.ch/pipermail/r-help/2001-November/016201.html
# # Correlations appear below the diagonal and significance probabilities above the diagonal 
# 
# #~# calculate and store correlation info
# cor.prob <- function(X, dfr = nrow(X) - 2) {
#   R <- cor(X, use="complete.obs")
#   above <- row(R) < col(R)
#   r2 <- R[above]^2
#   Fstat <- r2 * dfr / (1 - r2)
#   R[above] <- 1 - pf(Fstat, 1, dfr)
#   R
# }
# 
# cor.prob(as.matrix(wolfdata[,c("deer_w2","elk_w2", "moose_w2", "sheep_w2", "goat_w2", "Elevation2", "DistFromHumanAccess2", "DistFromHighHumanAccess2")]))
# 
# ## to add *'s for P=0.05 significant correlations try this
# #~# redo above with statistical significance added
#   #but that's bio'l bullshit so not necessary
# cor.prob2 <- function(X, dfr = nrow(X) - 2) {
#   R <- cor(X, use="complete.obs")
#   above <- row(R) < col(R)
#   r2 <- R[above]^2
#   Fstat <- r2 * dfr / (1 - r2)
#   R[above] <- 1 - pf(Fstat, 1, dfr)
#   Rstar = ifelse(R[above]<0.05, "***", "NS")
#   R[above]=paste(R[above],Rstar)
#   R
# }
# 
# cordat <- cor.prob2(as.matrix(wolfdata[,c("deer_w2","elk_w2", "moose_w2", 
#                                "sheep_w2", "goat_w2", "Elevation2", 
#                                "DistFromHumanAccess2", "DistFromHighHumanAccess2")]))
# write.csv(cordat, file = "correlations.csv")
# 


### assesing collinearity of continuous and categorical variables
cor.test(wolfdata$alpine, wolfdata$Elevation2)
cor.test(wolfdata$burn, wolfdata$Elevation2)
cor.test(wolfdata$closedConif, wolfdata$Elevation2)
cor.test(wolfdata$herb, wolfdata$Elevation2)
cor.test(wolfdata$goat_w2, wolfdata$Elevation2)
## probably shouldnt consider goat and elevation in the same model

## lets plot the correlations between Elevation [7] and landcover types [18:29]
corrgram(wolfdata[c(7, 18:29)], order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Landcover Correlations with Elevation")
## so nothing too egregious 

## next human access[8]
corrgram(wolfdata[c(8, 18:29)], order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Landcover Correlations with Distance from Human Access")

## again, only issue is Rock and Ice but even then its not a huge effect. 
#~# few areas in rock and ice close to human access

## we can essentially see this here: 
boxplot(Elevation2~landcov.f, ylab="Elevation (m)", data=wolfdata, las=3)
#~# tight spread means you can't eval in each other
# can't eval elev & alpine bc alpine only ccurs in high elev
boxplot(DistFromHumanAccess2~landcov.f, ylab="Dist Human Access (m)", 
        data=wolfdata, las=3)
## so collinearity is not as important for categorical variables but it becomes important if we start to assess cagetorical interactions with continuous factors. 

##### 2.0 Interaction between categorical factor and continuous ###



#~# logistic regression fit to a model with an interaction
## interactions are ways to break apart collinearity or confounding
ggplot(wolfdata, aes(x=DistFromHighHumanAccess2, y=used, fill=closedFactor)) + 
  stat_smooth(method="glm", method.args = list(family="binomial"), level=0.95) 
#+ facet_grid(closed~.)
## this shows the effect of distance from high human access varies a lot with whether wolves are in closed cover or not
## But does there look to be an interaction?

boxplot(DistFromHighHumanAccess2~closed, ylab="Distance from High Human (m)", data=wolfdata)
# they can only get away from humans in open landcover types
# so yes, you can only get far away from humans evidently in open landcover types (rock / ice) 
## but this isnt that big a problem. 

## lets fit the model now
disthha.cover <-  glm(used~closed + DistFromHighHumanAccess2 + 
                        closed*DistFromHighHumanAccess2, data =wolfdata, 
                      family= binomial(logit))
summary(disthha.cover)
boxplot(DistFromHighHumanAccess2~closedFactor, ylab="Distance From High Human Access (m)", data=wolfdata)
## so yes, you can only get far away from humans evidently in open landcover types (rock / ice) but this isnt that big a problem. 

## lets try again with distance to human access
ggplot(wolfdata, aes(x=DistFromHumanAccess2, y=used, fill=closedFactor)) + 
  stat_smooth(method="glm", method.args = list(family="binomial"), level=0.95)
#+ facet_grid(closed~.)
## bit more evidence of an interaction here (the lines cross)
boxplot(DistFromHumanAccess2~closedFactor, ylab="Distance From High Human Access (m)", data=wolfdata)
distha.cover <-  glm(used~closed + DistFromHumanAccess2 + closed*DistFromHumanAccess2, data =wolfdata, family= binomial(logit))
summary(distha.cover)
#~# closed was positive but now it's negative
## now can't interp main effects of model without considering
#effects of intrxn at same time
#at low levels of humandist closed is loer
# as dist to human inc, pr(use) in closed will become bigger
## in presence of intrxn you can't interp main effects as closed in isolation
# intrxns heelp us break apart colinearity by accounting for
# biology that explains why they're correlated to begin with
#~# negative intrxn
## very close to humans, wolves like to be in the open, weird
## geneally tho, close dist to human, closed (1) better than open


## How best to proceed?

## Can't really consider models with moose, deer and elk together
## should not consider models with distance to high human use and 
## elevation together


## 3.1 Manual Model Selection Using AIC
## after we have fit ANY model in the str() we can see that there is AIC information stored
cover <-  glm(used~closed, data =wolfdata2, family= binomial(logit))
cover$aic
      
## Lets use using AIC to select interactions...
distha <-  glm(used~DistFromHumanAccess2, data =wolfdata2, family= binomial(logit))
distha.cover <-  glm(used~closed + DistFromHumanAccess2, 
                     data =wolfdata2, family= binomial(logit)) 
## Main effects only
disthaXcover <-  glm(used~closed + DistFromHumanAccess2 + 
                       closed*DistFromHumanAccess2, data =wolfdata2, 
                     family= binomial(logit))
#~# don't worry about error message      
AIC(cover, distha, distha.cover, disthaXcover)
## so STRONG evidence that model disthhaXcover is much better than disthhaXcover
      
## lets redo with distance to high human access
disthha <-  glm(used~DistFromHighHumanAccess2, data =wolfdata2, family= binomial(logit))
disthha.cover <-  glm(used~closed + DistFromHighHumanAccess2, data =wolfdata2, family= binomial(logit)) ## Main effects only
disthhaXcover <-  glm(used~closed + DistFromHighHumanAccess2 + closed*DistFromHighHumanAccess2, data =wolfdata2, family= binomial(logit))
      
AIC(cover, disthha, disthha.cover, disthhaXcover)
## Again, STRONG evidence that model disthhaXcover is much better than disthhaXcover

#### 3.2 Stepwise model selection ###

# Full model
full.model = glm(used~deer_w2 + elk_w2 +moose_w2 +sheep_w2+goat_w2+
                   Elevation2+DistFromHumanAccess2+DistFromHighHumanAccess2 +
                   closed + closed*DistFromHighHumanAccess2, data =wolfdata2, 
                 family= binomial(logit))
      
# Backwards selection
stepAIC(full.model, direction = "backward")
#~# model with everything but the interaction is the "best" model

top.backwards = glm(used ~ deer_w2 + elk_w2 + moose_w2 + sheep_w2 + 
                      goat_w2 + Elevation2 + DistFromHumanAccess2 + 
                      DistFromHighHumanAccess2, data=wolfdata2,
                    family=binomial(logit))
summary(top.backwards)


# Forwards selection
null.model = glm(used~1,data=wolfdata2,
                 family=binomial(logit)) # null model is just intercept
stepAIC(null.model, scope=list(upper=full.model, lower= null.model),
        direction="forward")
## lots of output supressed in Rmarkdown
#~# order in which variables are added kind tells how important they are
## Ok - Note the best model selected with stepwise forward selection was the same
top.forward = glm(used ~ deer_w2 + elk_w2 + moose_w2 + sheep_w2 + 
                    goat_w2 + Elevation2 + DistFromHumanAccess2 + 
                    DistFromHighHumanAccess2 + closed, data=wolfdata2,
                  family=binomial(logit))
summary(top.forward)
vif(top.forward)

## but there are a bunch of collinear variables in the model, moose/elk/deer, human/human/human...
## basically everything is being retained, not much kicked out. 

## now what about landcover (rock Ice as intercept)
full.model.landcov = glm(used~ closedConif +modConif+openConif+decid+regen+mixed+herb+shrub+water+burn+alpine, data =wolfdata2, family= binomial(logit))
stepAIC(full.model.landcov, direction = "backward")

top.model.landcov = glm(used~openConif+modConif+closedConif+mixed+herb+shrub+water+burn, data =wolfdata2, family= binomial(logit))
summary(top.model.landcov)
#~# intercept is the categories that got dropped
vif(top.model.landcov)
## lets use this combination of Landcover covariates next as the BEST top model
#~# "vifs on a categorical variable, y'know, meh"



## Ok - we are going to take 2 competing sets of models. 
## Model 1 set - JUST biotic covariates, prey species and humans
## model 2 set - JUST environmental covariate models
## excercise on the board
################## FIT CANDIDATE MODELS with the AICcmodavg package


## Model set 1: Biotic
m.biotic[[1]] <- glm(used ~ 1, family=binomial(logit), data=wolfdata2)
m.biotic[[2]] <- glm(used ~ elk_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[3]] <- glm(used ~ deer_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[4]] <- glm(used ~ moose_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[5]] <- glm(used ~ sheep_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[6]] <- glm(used ~ goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[7]] <- glm(used ~ moose_w2 + sheep_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[8]] <- glm(used ~ deer_w2 + sheep_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[9]] <- glm(used ~ elk_w2 + sheep_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[10]] <- glm(used ~ elk_w2 + goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[11]] <- glm(used ~ deer_w2 + goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[12]] <- glm(used ~ moose_w2 + goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[13]] <- glm(used ~ sheep_w2 + goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[14]] <- glm(used ~ DistFromHighHumanAccess2, family=binomial(logit), data=wolfdata2)
m.biotic[[15]] <- glm(used ~ DistFromHighHumanAccess2+deer_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[16]] <- glm(used ~ DistFromHighHumanAccess2+moose_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[17]] <- glm(used ~ DistFromHighHumanAccess2+sheep_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[18]] <- glm(used ~ DistFromHighHumanAccess2+goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[19]] <- glm(used ~ DistFromHighHumanAccess2+moose_w2 + sheep_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[20]] <- glm(used ~ DistFromHighHumanAccess2+deer_w2 + sheep_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[21]] <- glm(used ~ DistFromHighHumanAccess2+elk_w2 + sheep_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[22]] <- glm(used ~ DistFromHighHumanAccess2+elk_w2 + goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[23]] <- glm(used ~ DistFromHighHumanAccess2+deer_w2 + goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[24]] <- glm(used ~ DistFromHighHumanAccess2+moose_w2 + goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[25]] <- glm(used ~ DistFromHighHumanAccess2+sheep_w2 + goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[26]] <- glm(used ~ DistFromHighHumanAccess2, family=binomial(logit), data=wolfdata2)
m.biotic[[27]] <- glm(used ~ DistFromHighHumanAccess2+deer_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[28]] <- glm(used ~ DistFromHighHumanAccess2+moose_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[29]] <- glm(used ~ DistFromHighHumanAccess2+sheep_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[30]] <- glm(used ~ DistFromHighHumanAccess2+goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[31]] <- glm(used ~ DistFromHighHumanAccess2+moose_w2 + sheep_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[32]] <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolfdata2)
m.biotic[[33]] <- glm(used ~ DistFromHumanAccess2+deer_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[34]] <- glm(used ~ DistFromHumanAccess2+moose_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[35]] <- glm(used ~ DistFromHumanAccess2+sheep_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[36]] <- glm(used ~ DistFromHumanAccess2+goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[37]] <- glm(used ~ DistFromHumanAccess2+moose_w2 + sheep_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[38]] <- glm(used ~ DistFromHumanAccess2+deer_w2 + sheep_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[39]] <- glm(used ~ DistFromHumanAccess2+elk_w2 + sheep_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[40]] <- glm(used ~ DistFromHumanAccess2+elk_w2 + goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[41]] <- glm(used ~ DistFromHumanAccess2+deer_w2 + goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[42]] <- glm(used ~ DistFromHumanAccess2+moose_w2 + goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[43]] <- glm(used ~ DistFromHumanAccess2+sheep_w2 + goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[44]] <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolfdata2)
m.biotic[[45]] <- glm(used ~ DistFromHumanAccess2+deer_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[46]] <- glm(used ~ DistFromHumanAccess2+moose_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[47]] <- glm(used ~ DistFromHumanAccess2+sheep_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[48]] <- glm(used ~ DistFromHumanAccess2+goat_w2, family=binomial(logit), data=wolfdata2)
m.biotic[[49]] <- glm(used ~ DistFromHumanAccess2+moose_w2 + sheep_w2, family=binomial(logit), data=wolfdata2)

## then name our models .
## note you can name your models with a command like this
# model.names <-  ("null", "disthha", "distacc", "sheepwi", "goatwin", "elkwint", "moosewin", "deerwin") but in this case there were 49 models
model.names.biotic <-c("m0","m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12","m13","m14","m15","m16","m17","m18","m19","m20","m21","m22","m23","m24","m25","m26","m27","m28","m29","m30","m31","m32","m33","m34","m35","m36","m37","m38","m39","m40","m41","m42","m43","m44", "m45","m46","m47","m48")
model.names.biotic <-1:49

aictab(cand.set = m.biotic, modnames = model.names.biotic)

## OK so the top model was model 41

top.biotic <- glm(used ~ DistFromHumanAccess2+deer_w2 + goat_w2, family=binomial(logit), data=wolfdata2)
summary(top.biotic)

## and the 2nd ranked top biotic model was  model 40
second.biotic <- glm(used ~ DistFromHumanAccess2+elk_w2 + goat_w2, family=binomial(logit), data=wolfdata2)
summary(second.biotic)

## Model set 2: Environmental Covariates Only
## 

m.env <- list()
head(m.env)

## Model set 1: Biotic
m.env[[1]] <- glm(used ~ 1, family=binomial(logit), data=wolfdata2)
m.env[[2]] <- glm(used ~ Elevation2, family=binomial(logit), data=wolfdata2)
m.env[[3]] <- glm(used ~ DistFromHighHumanAccess2, family=binomial(logit), data=wolfdata2)
m.env[[4]] <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolfdata2)
m.env[[5]] <- glm(used ~ openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfdata2)
m.env[[6]] <- glm(used ~ Elevation2 + DistFromHumanAccess2, family=binomial(logit), data=wolfdata2)
m.env[[7]] <- glm(used ~ DistFromHighHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfdata2)
m.env[[8]] <- glm(used ~ DistFromHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfdata2)
m.env[[9]] <- glm(used ~ Elevation2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfdata2)
m.env[[10]] <- glm(used ~ Elevation2 + DistFromHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfdata2)
m.env[[11]] <- glm(used ~ Elevation2 + DistFromHighHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfdata2)
m.env[[12]] <- glm(used ~ Elevation2 + DistFromHighHumanAccess2 + closed + closed*DistFromHighHumanAccess2, family=binomial(logit), data=wolfdata2)
m.env[[13]] <- glm(used ~ Elevation2 + DistFromHumanAccess2 + closed + closed*DistFromHumanAccess2, family=binomial(logit), data=wolfdata2)
m.env[[14]] <- glm(used ~ DistFromHighHumanAccess2 + closed + closed*DistFromHighHumanAccess2, family=binomial(logit), data=wolfdata2)
m.env[[15]] <- glm(used ~ DistFromHumanAccess2 + closed + closed*DistFromHumanAccess2, family=binomial(logit), data=wolfdata2)


model.names.env <-1:15

aictab(cand.set = m.env, modnames = model.names.env)

#OK - top model is model 11
top.env <- glm(used ~ Elevation2 + DistFromHighHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfdata2)
summary(top.env)

## now - which 'set' of covariates is best? Env? or Biotic?

AIC(top.env, top.biotic)

## Environmental model HANDS DOWN. 

## now go back and compare 'top' model to top model selected by AIC

AIC(top.forward, top.biotic, second.biotic, top.env)
## AIC will overfit models and does not penalize for collinearity. 



### OBJECTIVE 5.0 - model selection using the MuMIn Package

## Also explore the use of package MuMIn - Mutlimodel inference
# http://cran.r-project.org/web/packages/MuMIn/MuMIn.pdf

# re-run FULL logistic regression model
top.forward = glm(used ~ deer_w2 + elk_w2 + moose_w2 + sheep_w2 + goat_w2 + Elevation2 + DistFromHumanAccess2 + 
                    closed + DistFromHighHumanAccess2*closed, data=wolfdata2,family=binomial(logit), na.action ="na.fail")
summary(top.forward)

#install and load MuMIn package
require(MuMIn)

#use dredge function to get all possible models
x1<-dredge(top.forward)

## x1 - looking at all 
## there are over 1000 models fit! 10! models = ? models
## note dredge has fit XX models in total out of this candidate set of 19 candidate variables

head(x1, n = 10) ## only shows top 10 models fit

#get top models with AICc <2
top.models<-get.models(x1, subset=delta<2)
#top.models

#model average covariate effects
x6<-model.avg(top.models)
summary(x6)

top.dredge.lc = glm(used ~ openConif+modConif+closedConif+mixed+
                      herb+shrub+water+burn+decid+regen+alpine, 
                    data=wolfdata2,family=binomial(logit), 
                    na.action ="na.fail")
x2<-dredge(top.dredge.lc)
head(x2, n=10)


#### Objective 6. Model Selection using BIC ###

##### Note we will use the function BIC() in the base {stats4} pacakge
##### This generic function calculates the Bayesian information criterion, 
## also known as Schwarz's Bayesian criterion (SBC), for one or several fitted model 
## objects for which a log-likelihood value can be obtained, according to 
## the formula -2*log-likelihood + npar*log(nobs), where npar represents the number of 
## parameters and nobs the number of observations in the fitted model.
##### In my estimation, BIC tends to be more conservative in preventing model overfitting 
## because it does not consider K the 'penalty' function, but instead, considers K*log(n) 
## where n is the number of rows of data, as the penalty function. 
## Thus, it calculates a bigger penalty for larger datasets, which gaurds against overfitting. 
##### there are not as many functions out there to calculate model selection using BIC 
##### today we will use BIC in the package MuMIn

#```{r}
## First manually
AIC(top.forward, top.biotic, second.biotic, top.env)
BIC(top.forward, top.biotic, second.biotic, top.env)
## OK - so not much difference in top models using BIC and AIC in this dataset

## Now lets use the dredge function with BIC

x1.bic<-dredge(top.forward, rank=BIC) ## note this now ranks using BIC

## x1.bic - look at all 

head(x1.bic, n = 10) ## only shows top 10 models fit
# lets compare the top model from AIC and BIC
head(x1.bic, n = 1) ## only shows top 1 models fit with BIC
head(x1, n = 1) ## only shows top 1 models fit with AIC

## So AIC is overfitting here potentially. 

#get top models with BIC <2
top.models.bic<-get.models(x1.bic, subset=delta<2)
#top.models.bic ## note there is only 1 top model here using BIC

#model average covariate effects
##x.top.bic<-model.avg(top.models.bic) ## only 1 top model, so this doesnt work

## Lets run the 'top' model selected using BIC for next week
top.model.bic = glm(used ~ DistFromHighHumanAccess2 + DistFromHumanAccess2+Elevation2+elk_w2+goat_w2+moose_w2, data=wolfdata2,family=binomial(logit), na.action ="na.fail")
summary(top.model.bic)
## compare to top AIC model
summary(top.forward)
#```



#### OBJECTIVE 7.0 - Variable reduction using PCA ###

head(wolfdata2)

pcawolf <-princomp(na.omit(wolfdata2[1:9]), cor=TRUE)
summary(pcawolf)
#~# component 1 is explaining 55% of the variation
  ## this is only on the X variables, deer-humans
# looking in multi-variable way in 9 or 11 dimensions
  #which axes - looking at niche of all envtal covs and saying
  #what's best way to seperate these in mutually orthogonal way
  # orthogonal essentially = indep
loadings(pcawolf)
#~# component 1 is fcn of -1.w*deer + 0.42*elk
#component1 is linear combination of those component coeffs times the covariates
#"component 1 is impossible to understand"
#in a non-duimensional way, c1 is explainig like 50-60% of the variation
#in your x variables
plot(pcawolf, type="lines")

biplot(pcawolf, xlim =c(-0.06, 0.04))
# takes all varn in 9-dimensional cloud of (9) x-variables
# and "schmooshes" that into this figure showing correlation of c1 and c2
# and shows you hwere data are (these are datapoints and where they line up
# w the diff covs on your axes)
# c2 seems to align more with sheep and goats
#this linear combo of predictors,
#i'm gonna create new vrbl called c1 that is fcn
#of linear combo of prredictors
# bc have done pcs,
#comp1 is a linear indep fcn of these covs
# that maximizes separation along this x axis

# get below #s from above output
#THIS is the x-axis in below plot ($Comp.1)
wolfdata2$Comp.1 <- -0.406*wolfdata2$deer_w2 - 
  0.370*wolfdata2$moose_w2 - 0.402*wolfdata2$elk_w2 +
  0.182*wolfdata2$goat_w2 - 0.415*wolfdata2$wolf_w2 + 
  0.408*wolfdata2$Elevation2 + 0.318*wolfdata2$DistFromHumanAccess2 +
  0.233*wolfdata2$DistFromHighHumanAccess2

wolf_comp1 <- glm(used ~ Comp.1, family=binomial (logit), data=wolfdata2)
wolfdata2$fitted1 <- fitted(wolf_comp1)
hist(wolfdata2$fitted1)
#~# hist of predicted probabilities using c1
plot(wolfdata2$Comp.1, wolfdata2$fitted1)
#pr(use) as fcn of comp1

# how can you actually use this information? 
# like, hey manager, you need more of component 1

figPCA <- ggplot(wolfdata2, aes(x=Comp.1, y=used)) + stat_smooth(method="glm", method.args = list(family="binomial"))
x.axis = "-0.406*deer - 0.370*moose - 0.402*elk +0.182*goat - 0.415*wolf + 0.408*Elev + 0.318*DistHumans + 0.233*DistHighHumans"
figPCA2 <- figPCA + xlab(x.axis)
figPCA2

# ok so we've done something, but it makes no sense.

## of course the problem is that figPCA2's x-axis is unintelligible from a practical viewpoint. 

#~# use for variable redxn
# we've boiled our 9 covariates down to 1
# mark uses this to understand variance
# but we already did this in collinearity
