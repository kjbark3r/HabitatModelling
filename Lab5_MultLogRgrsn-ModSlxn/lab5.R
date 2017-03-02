##########################################################################################

### WILD 562 LAb 5 - Multiple Logistic Regression and Model Selection

##########################################################################################
## Preliminaries: 0.0 Installing Packages

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

##### 0.1 Preliminaries: importing data #######################################################################

##SET WD
wd_workcomp <- "C:\Users\kristin.barker\Documents\GitHub\HabitatModelling\Lab5_MultLogRgrsn-ModSlxn"
wd_laptop <- "C:\Users\kjbark3r\Documents\GitHub\HabitatModelling\Lab5_MultLogRgrsn-ModSlxn"

ifelse(file.exists(wd_workcomp), setwd(wd_workcomp), setwd(wd_laptop))
rm(wd_workcomp, wd_laptop)

wolfkde <- read.csv("wolfkde.csv", header=TRUE, sep = ",", na.strings="NA", dec=".")
head(wolfkde)



## Objective 1.0 Multiple Logistic Regression & Collinearity ####

## We will first evaluate collinearity between Distance to High Human Use and Elevation

## First lets fit Univariate models of these 2 covariates
elev <- glm(used~Elevation2, data =wolfkde, family= binomial(logit))
disthhacc <-  glm(used~DistFromHighHumanAccess2, data =wolfkde, family= binomial(logit))

# Next, fit both in our first multiple logistic regression model
elev.disthhacc <- glm(used~Elevation2 +DistFromHighHumanAccess2 , data =wolfkde, family= binomial(logit))
summary(elev.disthhacc)
# now lets extract coefficients

summary(elev)$coefficients[,1:2]
summary(disthhacc)$coefficients[,1:2]
summary(elev.disthhacc)$coefficients[,1:2]

### what just happened to the coefficient for Distance to High Human Access?

## lets visually explore differences
disthumanBnp = 0:7000 # range of distances in study area
prDisthhacc <- predict(disthhacc, newdata=data.frame(DistFromHighHumanAccess2=disthumanBnp), type="response")
head(prDisthhacc)

plot(wolfkde$DistFromHighHumanAccess2, wolfkde$used)
lines(disthumanBnp, prDisthhacc, type="l", ylab= "Pr(Used)")

## now lets do for the Multiple Logistic regression model
## but now we have 2 sets of covariates to consider, elevation and distance from high human access
## lets determine the 'median' elevation

summary(wolfkde$Elevation2)
## ok - lets evaluate the probability of use at 1931 meters from the elev.disthhacc model
meanElev = 1931
## predict dist to human access holding elev constant at its mean
prElevMedian.Disthhacc <- predict(elev.disthhacc, 
                                  newdata=data.frame(DistFromHighHumanAccess2=disthumanBnp, 
                                                     Elevation2=meanElev), type="response")
plot(wolfkde$DistFromHighHumanAccess2, wolfkde$used, xlim=(c(0,10000)))
lines(disthumanBnp, prElevMedian.Disthhacc, type="l", ylab= "Pr(Used)")
lines(disthumanBnp, prDisthhacc, type="l", ylab= "Pr(Used)")



## what is going on?? how did the coefficient switch sign?
## Partial Regression Coefficients - in multiple linear or logistic regression the B's now change interpretation to the effects of X2 on Y while holding effects of X1 constant at their mean.

## Previous plot was only plotted for 1 level of Elevation 
  #at the median elevation of 1900m
## lets create a new data framew with elevations and distance 
  #to high human access varying in 10 levels
## using the pretty() function 
#? pretty

#~# doing the above again over a variety of elevs
  # pretty() picks pretty breakpoints
  # fcn of elev, cut into 5 categories, and 
  # humanaccess split into 10
#expland.grid could just as well be a df
newdata <- expand.grid(Elevation2 = pretty(wolfkde$Elevation2, 5), 
                       DistFromHighHumanAccess2 = pretty(wolfkde$DistFromHighHumanAccess2, 10))
head(newdata)
newdata$prElev.Disthha <-predict(elev.disthhacc, newdata, type="response")

ggplot(newdata, aes(x = DistFromHighHumanAccess2, 
                    y = prElev.Disthha)) + 
  geom_line() + 
  facet_wrap(~Elevation2)
#~#- diff prob of use of wolves as fcn of dist to humanacc over diff ranges of elevs
## linear in logit link but aren't linear without going thru link

## Can we hold effects of X1 constant while varying X1?

## why are Elevation and DistHighHumanUse changing?
## they are CORRELATED!
cor.test(wolfkde$Elevation2, wolfkde$DistFromHighHumanAccess2)
elev_disthha <- lm(DistFromHighHumanAccess2~Elevation2, data=wolfkde)
summary(elev_disthha)
cor.test(wolfkde$Elevation2, wolfkde$DistFromHighHumanAccess2)

plot(wolfkde$Elevation2,wolfkde$DistFromHighHumanAccess2, type="p")
abline(lm(DistFromHighHumanAccess2~Elevation2, data=wolfkde), col="red")
## Can we hold effects of X1 constant while varying X1?

## showing it both ways
pairs(~Elevation2+DistFromHighHumanAccess2, data=wolfkde, main="Scatterplot Matrix")

## Elevation2 and distance to High Human Access are correlated AND confounded 
## i.e., there are no areas far from high human access at LOW elevations
#~# so we can't say anything about how wolves would respond to humans at high elev

## lets test 2 other variables, elk and deer...
deer <- glm(used~deer_w2, data =wolfkde, family= binomial(logit))
elk <-  glm(used~elk_w2, data =wolfkde, family= binomial(logit))

# Next, fit both in our first multiple logistic regression model
deer.elk <- glm(used~deer_w2 + elk_w2, data =wolfkde, family= binomial(logit))

# now lets extract coefficients
summary(deer)$coefficients[,1:2]
summary(elk)$coefficients[,1:2]
summary(deer.elk)$coefficients[,1:2]

## note this time the sign's didnt flip, but, they significantly changed, weakening in the presence of each other. 
## this is because they are so correlated
cor.test(wolfkde$deer_w2, wolfkde$elk_w2)



#### Objective 2.0 Screening for Collinearity ####

### Plotting pairwise correlations one at a time.
plot(wolfkde$Elevation2 ,wolfkde$goat_w2, type="p")
abline(lm(goat_w2~Elevation2, data=wolfkde), col="red")


## graphically examining collinearity
# using different methods pairs() in base package
pairs(~deer_w2+moose_w2+elk_w2+sheep_w2+goat_w2+Elevation2, data=wolfkde, main="Scatterplot Matrix")
pairs(~Elevation2+DistFromHumanAccess2+DistFromHighHumanAccess2, 
      data=wolfkde, main="Scatterplot Matrix")

## using car library
scatterplotMatrix(~deer_w2+moose_w2+elk_w2+sheep_w2+goat_w2+Elevation2, data=wolfkde, main="Scatterplot Matrix")
scatterplotMatrix(~Elevation2+DistFromHumanAccess2+DistFromHighHumanAccess2, data=wolfkde, main="Scatterplot Matrix")

scatterplotMatrix(~deer_w2+moose_w2+elk_w2+sheep_w2+goat_w2+Elevation2+DistFromHumanAccess2+DistFromHighHumanAccess2, data=wolfkde, main="Scatterplot Matrix")

## using corrgram
#~# red is negative relnship, blue is positive
corrgram(wolfkde[1:9], order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations in the Wolf Data")
#~# pie shoes how much of varn in one is explained by the other
  #ie how correlated they are

corrgram(wolfkde[1:9], order=TRUE, lower.panel=panel.pts,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations in the Wolf Data")

corrgram(wolfkde[1:9], order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations in the Wolf Data")

## note there are a billion ways to explore scatter plot matrices
## https://www.r-bloggers.com/multiple-regression-lines-in-ggpairs/
## and in R Graphics Cookbook   Section 5.13. 

## using the ggcorr package
#~# gives actual numbers
ggcorr(wolfkde[1:9], label = TRUE)

## GGally package with ggpairs()
ggpairs(wolfkde[1:9])

# Function for calculating correlations and probabilities, https://stat.ethz.ch/pipermail/r-help/2001-November/016201.html
# Correlations appear below the diagonal and significance probabilities above the diagonal 

#~# calculate and store correlation info
cor.prob <- function(X, dfr = nrow(X) - 2) {
  R <- cor(X, use="complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R
}

cor.prob(as.matrix(wolfkde[,c("deer_w2","elk_w2", "moose_w2", "sheep_w2", "goat_w2", "Elevation2", "DistFromHumanAccess2", "DistFromHighHumanAccess2")]))

## to add *'s for P=0.05 significant correlations try this
#~# redo above with statistical significance added
  #but that bio'l bullshit so not necessary
cor.prob2 <- function(X, dfr = nrow(X) - 2) {
  R <- cor(X, use="complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  Rstar = ifelse(R[above]<0.05, "***", "NS")
  R[above]=paste(R[above],Rstar)
  R
}

cor.prob2(as.matrix(wolfkde[,c("deer_w2","elk_w2", "moose_w2", 
                               "sheep_w2", "goat_w2", "Elevation2", 
                               "DistFromHumanAccess2", "DistFromHighHumanAccess2")]))

## So which covariates have the highest correlations?? 
## Deer, Elk, and Moose all have correlation coefficients > 0.65
## Sheep and Goats are correlated > 0.4
## elevation is inversely correlated with an R of -0.75 with deer, elk , moose

## Screening for multicollinearity using Variance Inflation Factors
full.model = glm(used~deer_w2 + elk_w2 +moose_w2 +sheep_w2+goat_w2+Elevation2+DistFromHumanAccess2+DistFromHighHumanAccess2, data =wolfkde, family= binomial(logit))
#?vif()
vif(full.model)
#~# elev has highest value so it's most correlated w the others

##
## The square root of the variance inflation factor indicates how much larger 
## the standard error is, compared with what it would be if that variable 
## were uncorrelated with the other predictor variables in the model.
## If the variance inflation factor of a predictor variable were 
## 5.27 (√5.27 = 2.3) this means that the standard error for the coefficient 
## of that predictor variable is 2.3 times as large as it would be if that 
## predictor variable were uncorrelated with the other predictor variables.
## The definition of ‘high’ is somewhat arbitrary but values 
## in the range of 5-10 are commonly used.
## so in this case, we are really concerned with Elevation

#~#~ but remember it's humanaccess that's changing the most
  # ugh science iso so non-straightforward
summary(full.model)

## but in the final model, sheep nor deer are significant any more, 
## but they probably shouldnt have been in the model in the first place



### assesing collinearity of continuous and categorical variables
cor.test(wolfkde$alpine, wolfkde$Elevation2)
cor.test(wolfkde$burn, wolfkde$Elevation2)
cor.test(wolfkde$closedConif, wolfkde$Elevation2)
cor.test(wolfkde$herb, wolfkde$Elevation2)
cor.test(wolfkde$goat_w2, wolfkde$Elevation2)
## probably shouldnt consider goat and elevation in the same model

cor.prob(as.matrix(wolfkde[,c("Elevation2", "openConif", "closedConif", "modConif", "burn", "herb", "decid", "burn", "alpine")]))

## lets plot the correlations between Elevation [7] and landcover types [18:29]
corrgram(wolfkde[c(7, 18:29)], order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Landcover Correlations with Elevation")
## so nothing too egregious 

## next human access[8]
corrgram(wolfkde[c(8, 18:29)], order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Landcover Correlations with Distance from Human Access")

## again, only issue is Rock and Ice but even then its not a huge effect. 
#~# few areas in rock and ice close to human access

## we can essentially see this here: 
boxplot(Elevation2~landcov.f, ylab="Elevation (m)", data=wolfkde, las=3)
#~# tight spread means you can't eval in each other
# can't eval elev & alpine bc alpine only ccurs in high elev
boxplot(DistFromHumanAccess2~landcov.f, ylab="Dist Human Access (m)", 
        data=wolfkde, las=3)
## so collinearity is not as important for categorical variables but it becomes important if we start to assess cagetorical interactions with continuous factors. 


##### 2.0 Interaction between categorical factor and continuous ####

## Relationship between whether wolves are responding to human use differently in open and closed cover types.
wolfkde$closed = 0
wolfkde$closed <- wolfkde$closedConif + wolfkde$modConif + wolfkde$openConif + 
                  wolfkde$decid + wolfkde$mixed + wolfkde$burn
## note I considered burn here as 'closed' - could change. 
#~# hypo whether wolves respond to human diff depending whether
  # in open or closed landcover

wolfkde$closedFactor <-as.factor(wolfkde$closed)

#~# logistic regression fit to a model with an interaction
## interactions are ways to break apart collinearity or confounding
ggplot(wolfkde, aes(x=DistFromHighHumanAccess2, y=used, fill=closedFactor)) + 
  stat_smooth(method="glm", method.args = list(family="binomial"), level=0.95) 
#+ facet_grid(closed~.)
## this shows the effect of distance from high human access varies a lot with whether wolves are in closed cover or not
## But does there look to be an interaction?

boxplot(DistFromHighHumanAccess2~closed, ylab="Distance from High Human (m)", data=wolfkde)
# they can only get away from humans in open landcover types
# so yes, you can only get far away from humans evidently in open landcover types (rock / ice) but this isnt that big a problem. 

## lets fit the model now
disthha.cover <-  glm(used~closed + DistFromHighHumanAccess2 + 
                        closed*DistFromHighHumanAccess2, data =wolfkde, 
                      family= binomial(logit))
summary(disthha.cover)
boxplot(DistFromHighHumanAccess2~closedFactor, ylab="Distance From High Human Access (m)", data=wolfkde)
## so yes, you can only get far away from humans evidently in open landcover types (rock / ice) but this isnt that big a problem. 

## lets try again with distance to human access
ggplot(wolfkde, aes(x=DistFromHumanAccess2, y=used, fill=closedFactor)) + 
  stat_smooth(method="glm", method.args = list(family="binomial"), level=0.95)
#+ facet_grid(closed~.)
## bit more evidence of an interaction here (the lines cross)
boxplot(DistFromHumanAccess2~closedFactor, ylab="Distance From High Human Access (m)", data=wolfkde)
distha.cover <-  glm(used~closed + DistFromHumanAccess2 + closed*DistFromHumanAccess2, data =wolfkde, family= binomial(logit))
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


#### OBJECTIVE 3.0 - Model building ####

## How best to proceed?

## Can't really consider models with moose, deer and elk together
## should not consider models with distance to high human use and 
## elevation together

## one option is Stepwise model selection

## but first we have to clean up msising data
length(wolfkde$Elevation2)
wolfkde2 <- na.omit(wolfkde)
length(wolfkde2$Elevation2)
## note there were 252 NA's for some covariates

## 3.1 Manual Model Selection Using AIC
## after we have fit ANY model in the str() we can see that there is AIC information stored
cover <-  glm(used~closed, data =wolfkde2, family= binomial(logit))
cover$aic
      
## Lets use using AIC to select interactions...
distha <-  glm(used~DistFromHumanAccess2, data =wolfkde2, family= binomial(logit))
distha.cover <-  glm(used~closed + DistFromHumanAccess2, 
                     data =wolfkde2, family= binomial(logit)) 
## Main effects only
disthaXcover <-  glm(used~closed + DistFromHumanAccess2 + 
                       closed*DistFromHumanAccess2, data =wolfkde2, 
                     family= binomial(logit))
#~# don't worry about error message      
AIC(cover, distha, distha.cover, disthaXcover)
## so STRONG evidence that model disthhaXcover is much better than disthhaXcover
      
## lets redo with distance to high human access
disthha <-  glm(used~DistFromHighHumanAccess2, data =wolfkde2, family= binomial(logit))
disthha.cover <-  glm(used~closed + DistFromHighHumanAccess2, data =wolfkde2, family= binomial(logit)) ## Main effects only
disthhaXcover <-  glm(used~closed + DistFromHighHumanAccess2 + closed*DistFromHighHumanAccess2, data =wolfkde2, family= binomial(logit))
      
AIC(cover, disthha, disthha.cover, disthhaXcover)
## Again, STRONG evidence that model disthhaXcover is much better than disthhaXcover
      
      
#### 3.2 Stepwise model selection ####

# Full model
full.model = glm(used~deer_w2 + elk_w2 +moose_w2 +sheep_w2+goat_w2+
                   Elevation2+DistFromHumanAccess2+DistFromHighHumanAccess2 +
                   closed + closed*DistFromHighHumanAccess2, data =wolfkde2, 
                 family= binomial(logit))
      
# Backwards selection
stepAIC(full.model, direction = "backward")
#~# model with everything but the interaction is the "best" model

top.backwards = glm(used ~ deer_w2 + elk_w2 + moose_w2 + sheep_w2 + 
                      goat_w2 + Elevation2 + DistFromHumanAccess2 + 
                      DistFromHighHumanAccess2, data=wolfkde2,
                    family=binomial(logit))
summary(top.backwards)


# Forwards selection
null.model = glm(used~1,data=wolfkde2,
                 family=binomial(logit)) # null model is just intercept
stepAIC(null.model, scope=list(upper=full.model, lower= null.model),
        direction="forward")
## lots of output supressed in Rmarkdown
#~# order in which variables are added kind tells how important they are
## Ok - Note the best model selected with stepwise forward selection was the same
top.forward = glm(used ~ deer_w2 + elk_w2 + moose_w2 + sheep_w2 + 
                    goat_w2 + Elevation2 + DistFromHumanAccess2 + 
                    DistFromHighHumanAccess2 + closed, data=wolfkde2,
                  family=binomial(logit))
summary(top.forward)
vif(top.forward)

## but there are a bunch of collinear variables in the model, moose/elk/deer, human/human/human...
## basically everything is being retained, not much kicked out. 

## now what about landcover (rock Ice as intercept)
full.model.landcov = glm(used~ closedConif +modConif+openConif+decid+regen+mixed+herb+shrub+water+burn+alpine, data =wolfkde2, family= binomial(logit))
stepAIC(full.model.landcov, direction = "backward")

top.model.landcov = glm(used~openConif+modConif+closedConif+mixed+herb+shrub+water+burn, data =wolfkde2, family= binomial(logit))
summary(top.model.landcov)
#~# intercept is the categories that got dropped
vif(top.model.landcov)
## lets use this combination of Landcover covariates next as the BEST top model
#~# "vifs on a categorical variable, y'know, meh"


#### OBJECTIVE 4.0 - model selection using the AICcmodavg package ####

## Ok - we are going to take 2 competing sets of models. 
## Model 1 set - JUST biotic covariates, prey species and humans
## model 2 set - JUST environmental covariate models
## excercise on the board
################## FIT CANDIDATE MODELS with the AICcmodavg package

m.biotic <- list()
head(m.biotic)

#lets fit our a-priori list of models 

## Model set 1: Biotic interactions, deer/elk/moose all too correlated to put in the same model
## sheep and goat are OK

## Model set 1: Biotic
m.biotic[[1]] <- glm(used ~ 1, family=binomial(logit), data=wolfkde2)
m.biotic[[2]] <- glm(used ~ elk_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[3]] <- glm(used ~ deer_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[4]] <- glm(used ~ moose_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[5]] <- glm(used ~ sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[6]] <- glm(used ~ goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[7]] <- glm(used ~ moose_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[8]] <- glm(used ~ deer_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[9]] <- glm(used ~ elk_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[10]] <- glm(used ~ elk_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[11]] <- glm(used ~ deer_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[12]] <- glm(used ~ moose_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[13]] <- glm(used ~ sheep_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[14]] <- glm(used ~ DistFromHighHumanAccess2, family=binomial(logit), data=wolfkde2)
m.biotic[[15]] <- glm(used ~ DistFromHighHumanAccess2+deer_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[16]] <- glm(used ~ DistFromHighHumanAccess2+moose_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[17]] <- glm(used ~ DistFromHighHumanAccess2+sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[18]] <- glm(used ~ DistFromHighHumanAccess2+goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[19]] <- glm(used ~ DistFromHighHumanAccess2+moose_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[20]] <- glm(used ~ DistFromHighHumanAccess2+deer_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[21]] <- glm(used ~ DistFromHighHumanAccess2+elk_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[22]] <- glm(used ~ DistFromHighHumanAccess2+elk_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[23]] <- glm(used ~ DistFromHighHumanAccess2+deer_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[24]] <- glm(used ~ DistFromHighHumanAccess2+moose_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[25]] <- glm(used ~ DistFromHighHumanAccess2+sheep_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[26]] <- glm(used ~ DistFromHighHumanAccess2, family=binomial(logit), data=wolfkde2)
m.biotic[[27]] <- glm(used ~ DistFromHighHumanAccess2+deer_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[28]] <- glm(used ~ DistFromHighHumanAccess2+moose_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[29]] <- glm(used ~ DistFromHighHumanAccess2+sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[30]] <- glm(used ~ DistFromHighHumanAccess2+goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[31]] <- glm(used ~ DistFromHighHumanAccess2+moose_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[32]] <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolfkde2)
m.biotic[[33]] <- glm(used ~ DistFromHumanAccess2+deer_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[34]] <- glm(used ~ DistFromHumanAccess2+moose_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[35]] <- glm(used ~ DistFromHumanAccess2+sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[36]] <- glm(used ~ DistFromHumanAccess2+goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[37]] <- glm(used ~ DistFromHumanAccess2+moose_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[38]] <- glm(used ~ DistFromHumanAccess2+deer_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[39]] <- glm(used ~ DistFromHumanAccess2+elk_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[40]] <- glm(used ~ DistFromHumanAccess2+elk_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[41]] <- glm(used ~ DistFromHumanAccess2+deer_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[42]] <- glm(used ~ DistFromHumanAccess2+moose_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[43]] <- glm(used ~ DistFromHumanAccess2+sheep_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[44]] <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolfkde2)
m.biotic[[45]] <- glm(used ~ DistFromHumanAccess2+deer_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[46]] <- glm(used ~ DistFromHumanAccess2+moose_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[47]] <- glm(used ~ DistFromHumanAccess2+sheep_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[48]] <- glm(used ~ DistFromHumanAccess2+goat_w2, family=binomial(logit), data=wolfkde2)
m.biotic[[49]] <- glm(used ~ DistFromHumanAccess2+moose_w2 + sheep_w2, family=binomial(logit), data=wolfkde2)
                   

## then name our models .
## note you can name your models with a command like this
# model.names <-  ("null", "disthha", "distacc", "sheepwi", "goatwin", "elkwint", "moosewin", "deerwin") but in this case there were 49 models
model.names.biotic <-c("m0","m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12","m13","m14","m15","m16","m17","m18","m19","m20","m21","m22","m23","m24","m25","m26","m27","m28","m29","m30","m31","m32","m33","m34","m35","m36","m37","m38","m39","m40","m41","m42","m43","m44", "m45","m46","m47","m48")
model.names.biotic <-1:49

aictab(cand.set = m.biotic, modnames = model.names.biotic)

## OK so the top model was model 41

top.biotic <- glm(used ~ DistFromHumanAccess2+deer_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
summary(top.biotic)

## and the 2nd ranked top biotic model was  model 40
second.biotic <- glm(used ~ DistFromHumanAccess2+elk_w2 + goat_w2, family=binomial(logit), data=wolfkde2)
summary(second.biotic)

## Model set 2: Environmental Covariates Only
## 

m.env <- list()
head(m.env)

## Model set 1: Biotic
m.env[[1]] <- glm(used ~ 1, family=binomial(logit), data=wolfkde2)
m.env[[2]] <- glm(used ~ Elevation2, family=binomial(logit), data=wolfkde2)
m.env[[3]] <- glm(used ~ DistFromHighHumanAccess2, family=binomial(logit), data=wolfkde2)
m.env[[4]] <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolfkde2)
m.env[[5]] <- glm(used ~ openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde2)
m.env[[6]] <- glm(used ~ Elevation2 + DistFromHumanAccess2, family=binomial(logit), data=wolfkde2)
m.env[[7]] <- glm(used ~ DistFromHighHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde2)
m.env[[8]] <- glm(used ~ DistFromHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde2)
m.env[[9]] <- glm(used ~ Elevation2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde2)
m.env[[10]] <- glm(used ~ Elevation2 + DistFromHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde2)
m.env[[11]] <- glm(used ~ Elevation2 + DistFromHighHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde2)
m.env[[12]] <- glm(used ~ Elevation2 + DistFromHighHumanAccess2 + closed + closed*DistFromHighHumanAccess2, family=binomial(logit), data=wolfkde2)
m.env[[13]] <- glm(used ~ Elevation2 + DistFromHumanAccess2 + closed + closed*DistFromHumanAccess2, family=binomial(logit), data=wolfkde2)
m.env[[14]] <- glm(used ~ DistFromHighHumanAccess2 + closed + closed*DistFromHighHumanAccess2, family=binomial(logit), data=wolfkde2)
m.env[[15]] <- glm(used ~ DistFromHumanAccess2 + closed + closed*DistFromHumanAccess2, family=binomial(logit), data=wolfkde2)


model.names.env <-1:15

aictab(cand.set = m.env, modnames = model.names.env)

#OK - top model is model 11
top.env <- glm(used ~ Elevation2 + DistFromHighHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde2)
summary(top.env)

## now - which 'set' of covariates is best? Env? or Biotic?

AIC(top.env, top.biotic)

## Environmental model HANDS DOWN. 

## now go back and compare 'top' model to top model selected by AIC

AIC(top.forward, top.biotic, second.biotic, top.env)
## AIC will overfit models and does not penalize for collinearity. 


########################################################################################################################
### OBJECTIVE 5.0 - model selection using the MuMIn Package

## Also explore the use of package MuMIn - Mutlimodel inference
# http://cran.r-project.org/web/packages/MuMIn/MuMIn.pdf

# re-run FULL logistic regression model
top.forward = glm(used ~ deer_w2 + elk_w2 + moose_w2 + sheep_w2 + goat_w2 + Elevation2 + DistFromHumanAccess2 + DistFromHighHumanAccess2 + closed + DistFromHighHumanAccess2*closed, data=wolfkde2,family=binomial(logit), na.action ="na.fail")
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

top.dredge.lc = glm(used ~ openConif+modConif+closedConif+mixed+herb+shrub+water+burn+decid+regen+alpine, data=wolfkde2,family=binomial(logit), na.action ="na.fail")
x2<-dredge(top.dredge.lc)
head(x2, n=10)


#### Objective 6. Model Selection using BIC ####

##### Note we will use the function BIC() in the base {stats4} pacakge
##### This generic function calculates the Bayesian information criterion, also known as Schwarz's Bayesian criterion (SBC), for one or several fitted model objects for which a log-likelihood value can be obtained, according to the formula -2*log-likelihood + npar*log(nobs), where npar represents the number of parameters and nobs the number of observations in the fitted model.
##### In my estimation, BIC tends to be more conservative in preventing model overfitting because it does not consider K the 'penalty' function, but instead, considers K*log(n) where n is the number of rows of data, as the penalty function. Thus, it calculates a bigger penalty for larger datasets, which gaurds against overfitting. 
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
top.model.bic = glm(used ~ DistFromHighHumanAccess2 + DistFromHumanAccess2+Elevation2+elk_w2+goat_w2+moose_w2, data=wolfkde2,family=binomial(logit), na.action ="na.fail")
summary(top.model.bic)
## compare to top AIC model
summary(top.forward)
#```



#### OBJECTIVE 7.0 - Variable reduction using PCA ####

head(wolfkde2)

pcawolf <-princomp(na.omit(wolfkde2[1:9]), cor=TRUE)
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
wolfkde2$Comp.1 <- -0.406*wolfkde2$deer_w2 - 
  0.370*wolfkde2$moose_w2 - 0.402*wolfkde2$elk_w2 +
  0.182*wolfkde2$goat_w2 - 0.415*wolfkde2$wolf_w2 + 
  0.408*wolfkde2$Elevation2 + 0.318*wolfkde2$DistFromHumanAccess2 +
  0.233*wolfkde2$DistFromHighHumanAccess2

wolf_comp1 <- glm(used ~ Comp.1, family=binomial (logit), data=wolfkde2)
wolfkde2$fitted1 <- fitted(wolf_comp1)
hist(wolfkde2$fitted1)
#~# hist of predicted probabilities using c1
plot(wolfkde2$Comp.1, wolfkde2$fitted1)
#pr(use) as fcn of comp1

# how can you actually use this information? 
# like, hey manager, you need more of component 1

figPCA <- ggplot(wolfkde2, aes(x=Comp.1, y=used)) + stat_smooth(method="glm", method.args = list(family="binomial"))
x.axis = "-0.406*deer - 0.370*moose - 0.402*elk +0.182*goat - 0.415*wolf + 0.408*Elev + 0.318*DistHumans + 0.233*DistHighHumans"
figPCA2 <- figPCA + xlab(x.axis)
figPCA2

# ok so we've done something, but it makes no sense.

## of course the problem is that figPCA2's x-axis is unintelligible from a practical viewpoint. 

#~# use for variable redxn
# we've boiled our 9 covariates down to 1
# mark uses this to understand variance
# but we already did this in collinearity



#### OBJECTIVE 8.0 - Caterpillar plots of coefficients ####

require(ggplot2)

# run logistic regression model
summary(full.model)

B<-summary(full.model)$coefficient[1:length(summary(full.model)$coefficient[,1]),1]
#create margin of error (ME) for 95% CI
ME <- summary(full.model)$coefficient[1:length(summary(full.model)$coefficient[,1]),2]*1.96
lower<-B - ME
upper<-B + ME


# bundle into data frame
logisData<-data.frame(B, lower, upper, names(summary(full.model)$coefficient[,2]))
names(logisData) <- c("Coefficient", "lower.ci", "upper.ci", "Variable")
levels(logisData$Variable)[1] <- "Intercept"
logisData$Variable <- relevel(logisData$Variable, ref="Intercept")

############################################################################################

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


################################################
## or check out the cool GGally package
require(GGally)
ggcoef(full.model)
## super cool!

