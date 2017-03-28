
#### Lab 7: WILD 562 R code for Mixed Effects ######

##### In today's lab we will explore the addition of random effects to generalized linear models in the sense of RSF models using the logit link function. Here, we consider now Generalized Linear Mixed-Models. Most models will be fit with the standard package lme4, and we will learn about i) centering/scaling covariates to ease estimation, ii) alternative approaches to mixed-effects models including two-stage models, and their challenges, and iii) the challenge of prediction using glmm's. 

##### Note there are number of resources for fitting GLMM's in R, and a number of useful packages and websites. Some here are:
##### In Particular, ANYTHING by Ben Bolker such as here: http://ms.mcmaster.ca/~bolker/R/misc/foxchapter/bolker_chap.html
##### and here http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html


#####################################################################################
## Preliminaries: 0.0 Installing Packages

#function to install and load required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#load or install these packages:
packages <- c("ggplot2", "sandwich", "lme4", "lmtest", "plotrix", "merTools", "ResourceSelection", "Hmisc", "GGally")
## Note lme4 is the workhorse of fitting generalized linear mixed effects models, but also see the following packages
# glmm
# MCMCglmm
# glmmADB
# glmmPQL
## Also note that merTools are a set of post-estimation commands written to interface with objects of type merMod, which are created by packages like lme4 (i.e., a mixed-effect model) 

#run function to install packages
ipak(packages)

##### 0.1 Preliminaries: setting working directory #######################################################################

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\HabitatModelling\\Lab7_MixedEffects"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\HabitatModelling\\Lab7_MixedEffects"

## automatically set working directory depending which computer you're on
ifelse(file.exists(wd_laptop), setwd(wd_laptop), setwd(wd_desktop)) 
rm(wd_workcomp, wd_laptop)
getwd()
list.files() ## handy command to see what is inside the working directory


################################################################################################################################################################
#### 1.0 Revisit Wolf data with random effect for wolf packs

wolfkde2 <- read.csv("wolfkde.csv", header=TRUE, sep = ",", na.strings="NA", dec=".")
wolfkde3 <-na.omit(wolfkde2)
wolfkde3$usedFactor <-as.factor(wolfkde3$usedFactor)

head(wolfkde3)
table(wolfkde2$pack, wolfkde2$used)
## unbalanced sample sizes between packs

top.env <- glm(used ~ Elevation2 + DistFromHighHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde3)
summary(top.env)
## but subset by packs
top.env.bv <- glm(used ~ Elevation2 + DistFromHighHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde3, subset=pack== "Bow Valley")
summary(top.env.bv)

## but subset by packs
top.env.rd <- glm(used ~ Elevation2 + DistFromHighHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde3, subset=pack== "Red Deer")
summary(top.env.rd)

## how to fit with one model?
top.env.mixed <- glmer(used~Elevation2 + DistFromHighHumanAccess2 + openConif+modConif+closedConif+mixed+herb+shrub+water+burn+(1|pack), data=wolfkde3,family=binomial(link="logit"))
summary(top.env.mixed)

#fit warnings:
#  Some predictor variables are on very different scales: consider rescaling
#convergence code: 0
# Model failed to converge with max|grad| = 0.912625 (tol = 0.001, component 1)
#Model is nearly unidentifiable: very large eigenvalue
#- Rescale variables?
#Model is nearly unidentifiable: large eigenvalue ratio
#- Rescale variables?

#### 1.1 Rescaling variables
##### The error messages from the fit of the wolf data with a random effect for pack tells us that we need to rescale our variables. 

#? scale ## note the defaults are to center to 0 and scale by dividing the centered x values by their standard deviation.
## The new variable has a mean = 0 and are now expressed in units +/- of standard deviations. 
## This is a very common/important step in the fitting of GLMM's
## This also has the direct advantage of being able to now directly compare the coefficients of continuous covariates in terms of SDs. 
## Finally, not we do not really ever scale categorical variables. 
head(wolfkde3)

wolfkde3$Elevation2_sc <-scale(wolfkde3$Elevation2)
hist(wolfkde3$Elevation2)
hist(wolfkde3$Elevation2_sc)
plot(wolfkde3$Elevation2, wolfkde3$Elevation2_sc)
summary(wolfkde3$Elevation2)
summary(wolfkde3$Elevation2_sc)

wolfkde3$DistFromHighHumanAccess2_sc <- scale(wolfkde3$DistFromHighHumanAccess2)
plot(wolfkde3$DistFromHighHumanAccess2_sc, wolfkde3$DistFromHighHumanAccess2)

### Now refit top model with standardized continuous data
top.env2 <- glm(used ~ Elevation2_sc + DistFromHighHumanAccess2_sc + openConif+modConif+closedConif+mixed+herb+shrub+water+burn, family=binomial(logit), data=wolfkde3)
summary(top.env2)

## Now you can directly compare the coefficients of Elevation and Distance from high human access?
## Which one has a 'stronger' effect? 
## Comparing the categorical landcover coefficients, which one is the strongest ? (burn)

## how to fit with one model?
top.env.mixed2 <- glmer(used~Elevation2_sc + DistFromHighHumanAccess2_sc + openConif+modConif+closedConif+mixed+herb+shrub+water+burn+(1|pack), data=wolfkde3,family=binomial(link="logit"))
summary(top.env.mixed2)
## see - nasty error messages are gone. 

##### Now we can also discuss how to interpret the random effects terms. We are now given a variance and standard deviation for the Random effect (here, pack). This is reported in units of standard deviations, which is another nice reason to standardize continuous (and categorical) covariates.  Here, we can interpre that there is a substantially STRONGER response by wolves to elevation than individual level variability. i.e., Beta-elevation = -3.1523, and St.Dev wolf = 0.5472, or about 5.7 times stronger response to elevation than individual wolf pack variation. 

##### Contrast that with the comparison of St.Dev (pack) to the coefficient for Distance from High Human Access = 0.4316.  This tells us that while most wolf packs avoided high human activity, there was more variation between packs in this response than the response. This tells us something about the variability in pack-level responses to human activity. 

##### Which is best from an AIC perspective?
AIC(top.env2, top.env.mixed2, top.env.rd, top.env.bv)
# Warning message:
# In AIC.default(top.env, top.env.mixed, top.env.rd, top.env.bv) :
#  models are not all fitted to the same number of observations

## Note that we get this error message because top.env.rd and top.env.bv are fit with only subsets of the data. 
## However, recall that for 1 dataset, AIC and LL is additive. Thus, the 'additive' AIC of the two 'separate' models is
## 396.4251 + 851.1538 = 1247.58 which is substantively better than the mixed effect model

##### Either way, comparing the fixed-effect versus the mixed-effect model, we can see that the glmm is far better. However, a simpler model with two separate models, one for each wolf pack, explained the datset better. 
##### But in practice, we would almost never fit a model with just 2 levels of a random effect, so now we will go to a more 'sensible' dataset. 

########################################################################################################
###### Objective 2.  Mixed-effets Models with Migrant Elk

##### These data are based on: Hebblewhite, M., and E. H. Merrill. 2009. Trade-offs between predation risk and forage differ between migrant strategies in a migratory ungulate. Ecology 90:3445-3454.
################################################################################
##### 2.1 Exploring and managing data

# Bring in data
elk <- read.table("lab7_elk_migrant.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
head(elk)
elk$elkuidF <- as.factor(elk$elkuid)


#### Get to know our data graphically
ggplot(elk, aes(x=utmx, y = utmy, color = elkuidF)) + geom_point()

## how about 'just' the telemetry locations?
elk.used <- subset(elk, elk$used == 1)
ggplot(elk.used, aes(x=utmx, y = utmy, color = elkuidF)) + geom_point()

##### What kind of sampling design is this?

# write.csv(elk.used, "lab8_elk_migrant_used.csv")  ## we might want to use this later, like in Lab 8

# get to know our data
table(elk$elkuid, elk$year)
table(elk$elkuid, elk$used)

##### OK, so there are some data from 2003, most from 2004, 1 elk from 2002.  And there is wide variation in the number of used and available locations for each elk from 52 (elk 90 0's) to 3711 (elk 29 0's)
##### Any guess why there is variation in the number of 0's?  Home range size: # of locations was based on an areal basis.

###### Based on Hebblewhite, M., and E. H. Merrill. 2009. Trade-offs between predation risk and forage differ between migrant strategies in a migratory ungulate. Ecology 90:3445-3454.
##### We are going to be conducting an RSF as a function of Forage Biomass (totalherb) and wolf predation risk (ctotrisk).  Lets look at these two variables and get to know them. 
hist(elk$ctotrisk)
hist(log(elk$ctotrisk))

summary(elk[,30:31]) ## So 1579 NA's predation risk values, and 11 NA's for total herbaceous vegetation
length(elk$ctotrisk)

#### Subset dataset for complete.cases where there are no NA data - why is this important?  How can we compare models using AIC with different number of rows?

elk2 <- elk[complete.cases(elk[30:31]), ]
summary(elk2)
length(elk2$ctotrisk)
## Still need to clean up predation risk data being predicted > 1
elk2$ctotrisk[elk2$ctotrisk>1]=1

table(elk2$elkuid, elk2$year)
table(elk2$elkuid, elk2$used)

# Compute sample size for each elk
n = tapply(elk$idn, elk$elkuid,length)
n

# Calculate mean wolf predation risk
wolf = tapply(na.omit(elk2$ctotrisk), elk2$elkuid[which((elk2$ctotrisk!="NA")==TRUE)],mean)
wolf
hist(wolf)
### this shows a wide variation in the exposure of elk to wolf predation risk

forage = tapply(na.omit(elk2$totalherb), elk$elkuid[which((elk2$totalherb!="NA")==TRUE)],mean)
forage
hist(forage)
################################################################################
##### 2.1 Scaling risk and forage
str(elk2)

elk2$totalherb_sc <- scale(elk2$totalherb)
elk2$ctotrisk_sc <- scale(elk2$ctotrisk)
elk2$ctotrisk2_sc <- scale(elk2$ctotrisk2)
elk2$riskforage_sc <- scale(elk2$riskforage)
elk2$for2_sc <- scale(elk2$for2)
elk2$risk2_sc <- scale(elk2$risk2)

##### AGain, just to double check what scale is doing

plot(elk2$ctotrisk_sc, elk2$ctotrisk)

################################################################################
#### 2.2 Fitting Standard Fixed-Effects Model and Understanding Ecology

##### Fitting best model(s)

forage = glm(used~totalherb, data=elk2,family=binomial(link="logit"))
risk = glm(used~ctotrisk, data=elk2,family=binomial(link="logit"))
forANDrisk = glm(used~totalherb+ctotrisk, data=elk2,family=binomial(link="logit"))
forrisk = glm(used~totalherb+ctotrisk+ctotrisk*totalherb, data=elk2,family=binomial(link="logit"))

AIC(forage, risk, forANDrisk, forrisk)

## Best model from AIC perspective is forrisk

summary(forrisk)

## Refit top model with random effects
forrisk_sc = glm(used~totalherb_sc+ctotrisk_sc+ctotrisk_sc*totalherb_sc, data=elk2,family=binomial(link="logit"))
summary(forrisk_sc)

################################################################################
##### 2.3 Visualizing the interaction from the unstandardized Fixed-effects model

# Calculate some summary statistics for forage
hist(elk$totalherb)
hist(log(elk$totalherb))
quantile(elk$totalherb,na.rm=TRUE)
mean(elk$totalherb,na.rm=TRUE)

herb.lo = 5
herb.med = 15
herb.hi = 50

# Make predictions
predrisk = seq(0,1,0.01)
pred.lo = predict(forrisk,type="response", newdata = data.frame(totalherb=herb.lo, ctotrisk=predrisk))
pred.med = predict(forrisk,type="response", newdata = data.frame(totalherb=herb.med, ctotrisk=predrisk))
pred.hi = predict(forrisk,type="response", newdata = data.frame(totalherb=herb.hi, ctotrisk=predrisk))

# Make plot

plot(elk2$ctotrisk,elk2$used, xlab="Risk", ylab="Pr(Use)")
lines(predrisk,pred.lo, lty=1)
lines(predrisk,pred.med, lty=2)
lines(predrisk,pred.hi, lty=3)
legend(x=0.7,y=0.95, legend=c("Observed","Low Forage","Medium Forage","High Forage"), pch=c(1,rep(-1,3)),lty=c(-1,1:3),bty="n")

##### 2.4 Visualizing the interactions with ggplot2
elk2$usedF <- as.factor(elk2$used)

ggplot(elk2, aes(x=ctotrisk, y = used)) + stat_smooth(method="glm", method.args = list(family="binomial"))
ggplot(elk2, aes(x=totalherb, y = used)) + stat_smooth(method="glm", method.args = list(family="binomial"))
ggplot(elk2, aes(x=riskforage, y = used)) + geom_rug() + stat_smooth(method="glm", method.args = list(family="binomial"))
#### But what does this last plot mean? Need to make a categorical variable at 3 different levels of forage

## Note I use the Hmisc package here to split into categories
elk2$forage.cat  <- as.factor(as.numeric(cut2(elk2$totalherb, g=3)))
elk2$forage.cat  <- cut2(elk2$totalherb, g=3)
ggplot(elk2, aes(x=ctotrisk, y = used, fill = forage.cat)) + stat_smooth(method="glm", method.args = list(family="binomial"))

elk2$risk.cat  <- cut2(elk2$ctotrisk, g=3)
ggplot(elk2, aes(x=totalherb, y = used, fill = risk.cat)) + stat_smooth(method="glm", method.args = list(family="binomial"))

##### Both graphs show that the effect of the interaction is only really present at low forage levels when elk select for high predation risk, but otherwise they select high forage biomass at all times, just less when under high predation risk.  

##### 2.5 Visualizing the marginal effects with Resource Selection mep
mep(forrisk_sc)
### But note it does not do main effects


################################################################################################################################################################
#### Objective 3.0  Robust standard errors clustering for non-independence with the Newey-West Sandwhich Estimator

##### This is a demonstration of the Newey-West Sandwhich Estimator - i.e., clustering the standard errors taking into account autocorrelation within an individual elk, Heterskedasticity. 

##### The relevant paper demonstrating it with RSF models is: Nielsen, S. E., M. S. Boyce, G. B. Stenhouse, and R. H. Munro. 2002. Modeling grizzly bear habitats in the Yellowhead ecosystem of Alberta: taking autocorrelation seriously. Ursus 13:45-56.

## This next command vcovHC generates the default heteroskedastic robust standard errors in R

forrisk2 <-vcovHC(forrisk, elk$elkuid, type = "const", sandwhich = TRUE)
forrisk2

coeftest(forrisk, forrisk2)

##### compare the SE's that are robust and not robust for comparison but without a cluster variable.  This reports a z-test statistic and associated P-value for whether there is a difference in the estimate of the two coefficients from the two models. This is a handy function in general to test the coefficients from 2 models of any type with the same structure. Here it tells us that there is a difference in the intercept (not really that ecologically interesting) and a difference in forage between the two models.  Because the Z-statistic is positive, 0.04 for totalherb, that tells us that failing to take into account the 'clustering' within individuals leads us to overesimate the effect of forage. 

################################################################################################################################################################
##### Objective 4.0 Fixed-effects Models with a Fixed-Effect for Each Individual Elk

##### Analysis with individual elk ID as a fixed factor, with 1 intercept per elk. Note we will use elkuidF as the factor

## now fit model with fixed effect for each individual elk

forriskFI = glm(used~totalherb+ctotrisk+ctotrisk*totalherb+elkuidF, data=elk,family=binomial(link="logit"))
summary(forriskFI)

##### Discussion - what does each coefficient mean in a used-available design? 
##### What is the correspondance between the sampling ratio of used to available locations and the coefficient?
table(elk2$elkuid, elk2$used)

##### Check for elk 196, 141/(141+1120) = 0.557; B is 0.502 - close enough in multiple logit model with continuous covariates

##### Why not just use this fixed effect?

AIC(forriskFI, forage, risk, forANDrisk, forrisk)

##### Main issue is the greater number of parameters, here, 1 for each individual elk. But, from an AIC perspective, the fixed-effects of each individual elk provide a better 'fit'. 

#### Objective 5. Two-stage modeling

##### Next we will explore the 'poor-mans' version of mixed-effects models, somewhere between fitting a fixed-effect for each individual elk and a full mixed-effect model

##### The best papers to read here are: Fieberg, J., J. Matthiopoulos, M. Hebblewhite, M. S. Boyce, and J. L. Frair. 2010. Correlation and studies of habitat selection: problem, red herring or opportunity? Philosophical Transactions of the Royal Society B: Biological Sciences 365:2233-2244., and Murtaugh, P. A. 2007. Simplicity and complexity in ecological data analysis. Ecology 88:56-62. andSawyer, H., R. M. Nielson, F. Lindzey, and L. L. Mcdonald. 2006. Winter Habitat Selection of Mule Deer Before and During Development of a Natural Gas Field. Journal of Wildlife Management 70:396-403, and 

#### Fixed effects model for each individual

elkids = sort(unique(elk2$elkuid))
modelnames = paste("elk",elkids)
models = list()

for (i in 1:length(elkids)){
	models[[i]]=glm(used~totalherb_sc+ctotrisk_sc+ctotrisk_sc*totalherb_sc, data=elk2,subset = elkuid==elkids[i], family=binomial(link="logit"))

}

names(models)=modelnames
# lapply(models,summary) #### Note I supressed this because it just spits out 17 models, 1 for each elk

# This creates a dataframe with the beta coefficients for each model/elk
coefs = as.data.frame(lapply(models, coef))
coefs

#Calculate means for each of the coefficients
mean(as.numeric(coefs[1,]))
mean(as.numeric(coefs[2,]))
mean(as.numeric(coefs[3,]))
mean(as.numeric(coefs[4,]))

##### Therefore, the linear part of the two-staged model would be:

##### -2.15 - 1.417*totalherb_sc -6.006*ctotrisk_sc + 6.657*totalherb_sc:ctotrisk_sc


##### LEts make some graphical displays of the Beta coefficients across individuals

par(mfrow=c(2,2))
hist(as.numeric(coefs[1,]), main="intercept",breaks=10)
hist(as.numeric(coefs[2,]), main="Forage",breaks=10)
hist(as.numeric(coefs[3,]), main ="Risk",breaks=10)
hist(as.numeric(coefs[4,]), main="Forage*Risk",breaks=10)

##### So a biological conclusions is that there is substantial variation between individuals in their coefficients for forage, wolf predation risk and the interaction. 

################################################################################################################################################################
################# Objective 6.0 Mixed Effects modeling! #######################
##### 6.1 Mixed-effects model with random intercept
##### Discuss in class what a random intercept means in the context of a Used-Available Design

fr.ri = glmer(used~totalherb_sc+ctotrisk_sc+ctotrisk_sc*totalherb_sc+(1|elkuid), data=elk2,family=binomial(link="logit"), verbose=FALSE)
summary(fr.ri)

################################################################################
###### Objective 5.2 Learning about how GLMM's are being fit

#### default is fitting the GLMM by Laplace approximation to the MLE estimator, which by default is 1 decimal place/point (numerical integration points) at which the model is fit with Adaptive Gauss-Hermite Quadrature.  The recommended # of nip points is >> 5 for most binomial GLMM's. 
##### we set this by changing the nACQ - integer scalar's. 
##### nAGQ- integer scalar - the number of points per axis for evaluating the adaptive Gauss-Hermite approximation to the log-likelihood. Defaults to 1, corresponding to the Laplace approximation. Values greater than 1 produce greater accuracy in the evaluation of the log-likelihood at the expense of speed. A value of zero uses a faster but less exact form of parameter estimation for GLMMs by optimizing the random effects and the fixed-effects coefficients in the penalized iteratively reweighted least squares step. (See Details.)

fr.ri2 = glmer(used~totalherb_sc+ctotrisk_sc+ctotrisk_sc*totalherb_sc+(1|elkuid), data=elk2,family=binomial(link="logit"), verbose=FALSE, nAGQ = 10)
summary(fr.ri2)

##### Note there really is no major effect in this dataset.  But - good to know. 

##### AGain, get used to interpreting the Random Effects. Here, they are saying we have 17 elk with 34390 rows of data, and the Std. Dev = 0.7899, which, compared to our standardized coefficient estimates for total herb and wolf risk are relatively huge. That is, another way of saying there is substantial individual-level variation in resource selection between individuals, equal to or greater than the variation in the actual strength of selection.  This is an important insight, and something we already learned from our Two-stage modeling above. 

# Useful functions
fixef(fr.ri) # This is the fixed effects coefficients
ranef(fr.ri) # These are the random effects, which in this model is just (1|elkuid), so, one coefficient for each individual elk
##### Compare these to the fixed effect coefficients estimated with an intercept for each individual elk? How do they compare?
coef(fr.ri) ### Note this shows you the entire coefficient matrix for all individual elk - note that the rest of the model is fixed for each individual elk. 

# Compare intercepts from two step and mixed-effects model
# Put everything in one place; I add the fixed intercept to the random effects here.
# But you could also get this from coef(fr.ri)
B0 <- cbind(as.numeric(coefs[1,]),ranef(fr.ri)$elkuid[,1]+fixef(fr.ri)[1])
rownames(B0)=rownames(ranef(fr.ri)$elkuid)
colnames(B0) = c("Two-Step","Random Effects")
str(B0)
B0
par(mfrow=c(1,1))
plot(B0[,1], B0[, 2])
abline(lm(B0[,1] ~ B0[, 2]))

#### This plot shows the Two-step coefficients for each individual elk on the X axix, and the random coefficient for each individual elk from the GLMM on the Y-axis. What do you notice? What are the axes of Y and X here?  Should there be a relationship between these two?

# Make a histogram
multhist(list(B0[,1],B0[,2]), xlab = "Intercept Coefficient",ylab="Frequency (# Elk)", col = c("gray","tan"))
legend("topright", legend = colnames(B0), fill = c("gray","tan"), bty = "n")

### This histogram shows us that the mixed-effects model is estimating the distribution of random intercepts here as 'more' gaussian/normally distributed than the Two-stage data. 

################################################################################
#### Obective 5.1 Random Coefficients - Mixed effects model with random coefficient for ctotrisk

fr.rc = glmer(used~totalherb_sc+ctotrisk_sc+totalherb_sc*ctotrisk_sc+(ctotrisk_sc|elkuid), data=elk2,family=binomial(link="logit"), verbose=FALSE)

fixef(fr.rc) # This is the fixed effects coefficients
ranef(fr.rc) # These are the random effects, which in this model is just (1|elkuid), so, one coefficient for each individual elk
##### Compare these to the fixed effect coefficients estimated with an intercept for each individual elk? How do they compare?
coef(fr.rc)

##### Note here that the coefficient for predation risk is allowed to vary

summary(fr.rc)

##### Again, look at the Std.Dev of the random effects for elkuid (intercept) and wolfpredation risk, 8.533. That is DOUBLE the strength of the coefficient for wolf predation risk in the model. Also note that the coefficient for wolf predation risk has dramatically changed, becoming much stronger now. 
##### NExt, we now see the correlation between the two random effects here, which is 0.97.  This means that there is a positive covariance between animals with a high value for a random intercept, and animals with a larger coefficient (more positive) for wolf predation risk. Interpreting this is tricky.  Think of what a positive intercept means in a used-available design - this means that they have more 1's than 0's, so, a smaller home range the way that we sampled availability based on areal extent. Why would animals with small home ranges be 'selecting' areas of higher predation risk compared to animals with larger home ranges? This is a real ecologically interesting question to ponder with this dataset. 
###### We also see below the output the Correlation of Fixed Effects: This is a bit tricky to interpret. The "correlation of fixed effects" output doesn't have the intuitive meaning that most would ascribe to it. Specifically, is not about the correlation of the variables (as OP notes). It is in fact about the expected correlation of the regression coefficients. Although this may speak to multicollinearity it does not necessarily. In this case it is telling you that if you did the experiment again and it so happened that the coefficient for forage got smaller, it would not be correlated with the coefficient for wolf predation risk. In fact, in these Correlations of fixed effects, there is nothing too alarming here; the fact that wolf predation risk and the intercept values were positively correlated was already noted above with the random effects, and discussed above. 
##### But, if these fixed-effect coefficients were highly correlated, and the real coefficients, not just the intercepts, that would be more problematic and might suggest problems with confounding or collinearity. 

##### Compare parameters between two-stage and mixed-models models
# Note that I use the coef function here
B0.rc = cbind(as.numeric(coefs[1,]),coef(fr.rc)$elkuid[,1])
rownames(B0.rc)=rownames(ranef(fr.ri)$elkuid)
colnames(B0.rc) = c("Two-Step","Random Effects")
B.risk = cbind(as.numeric(coefs[3,]),coef(fr.rc)$elkuid[,3])
rownames(B.risk)=rownames(ranef(fr.ri)$elkuid)
colnames(B.risk) = c("Two-Step","Random Effects")

## lets look at the Intercepts
B0.rc

## Next, lets look at the risk coefficients

B.risk
##### So, quite correlated from the 2 different models
plot(B.risk[,1], B.risk[,2])
abline(lm(B.risk[,1] ~ B.risk[, 2]))
##### So here there is some correlation between the coefficients for predation risk from the Two-stage models (X-axis) and the glmm (Y axis)


# Make histogram of betas
par(mfrow = c(1,2))
multhist(list(B0.rc[,1],B0.rc[,2]), xlab = "Intercept Coefficient",ylab="Frequency (# Elk)", col = c("gray","tan"), ylim=c(0,10))
legend(2.4,10, legend = colnames(B0), fill = c("gray","tan"), bty = "n")
multhist(list(B.risk[,1],B.risk[,2]), xlab = "Risk Coefficient",ylab="Frequency (# Elk)", col = c("gray","tan"),ylim = c(0,10))
legend(2.4,10, legend = colnames(B0), fill = c("gray","tan"), bty = "n")

##### What is the assumption about the distribution of the random effects doing to the modeled responses here?

################################################################################
###### 6.4 Model Selection with Random Effects

AIC(forriskFI, forage, risk, forANDrisk, forrisk, fr.ri, fr.rc)
##### So hands down, the top model here is a random effect of wolf predation risk, beating even the fixed-effect of each individual elk as an intercept (forriskFI)
##### Note that model selection with random effects models gets a lot more complicated... here we are doing model selection on the Fixed effects only. 

################################################################################
###### 7.0  Predictions from GLMM RSF models. 

##### Obtaining predictions from GLMM Models is tricky. What are you trying to predict is the challening question? Marginal or population-averaged responses? Conditional or subjet specific responses?  

##### Moreover, a challenge remains in how best to actually use the models with random effects in it to make predictions, so there are a number of packages and discussions that essentially recommend bootrapping or simulation approaches. All of this ends up taking us to a Bayesian framework eventually..

##### 7.1 Plotting predictions from random coefficient model using the fixed-effect coefficients (i.e., a marginal level prediction)

##### First lets refit the top model with out scaled coefficients
fr.rc2 = glmer(used~totalherb+ctotrisk+totalherb*ctotrisk+(ctotrisk|elkuid), data=elk2,family=binomial(link="logit"), verbose=FALSE)

# Plotting predictions from random coefficient model
# First remove NAs from the data
elk.c = elk2[which((elk2$ctotrisk!="NA"&elk2$totalherb!="NA")==TRUE),]

#Plot observed use
par(mfrow = c(1,1))
plot(elk.c$ctotrisk, elk.c$used,xlab="Risk",ylab="Pr(Use)")

# Set some variables up for nice plotting in a loop over each elk
elkids = sort(unique(elk.c$elkuid))
ltypes = rep(1:6,each=3)
lwide = rep(seq(1,3,1),each = 6)
colors = rep(c("red","black","seagreen", "violet", "tan", "orange"),3)

# Begin loop
for (i in elkids){
  # To plot predictions you need to create new data for just that elk
  dat = as.data.frame(model.matrix(terms(fr.rc2),elk.c)[elk.c$elkuid==i,])
  dat$totalherb = mean(dat$totalherb) # Use the mean forage for an elk
  dat[,colnames(dat)=="totalherb:ctotrisk"] = dat$totalherb*dat$ctotrisk # Recalculate the interaction term with the mean forage
  dat$pred.prob = plogis(as.matrix(dat)%*%t(as.vector(coef(fr.rc2)$elkuid[which(elkids==i),]))) # Use matrix algebra to get prediction based on coefficients for each individual elk
  ord = order(dat$ctotrisk) # store the order we want to plot in
  # Draw a line for you prediction
  lines(dat$ctotrisk[ord], dat$pred.prob[ord], lty=ltypes[which(elkids==i)],lwd = lwide[which(elkids==i)],col=colors[which(elkids==i)])
}

legend("right", legend = c("Observed", paste("Elk ",elkids)), pch=c(1,rep(-1,length(elkids))),lty = c(-1,ltypes[1:length(elkids)]),lwd = c(-1,lwide[1:length(elkids)]),col = c(1,colors[1:length(elkids)]), bty = "n")

##### 7.1 Plotting with 95% CI's
###### We can use a handy function in ggplot to fill by each individual elk like this:
ggplot(elk2, aes(x=ctotrisk, y = used, colour = elkuidF)) + stat_smooth(method="glm", method.args = list(family="binomial"))

###### Although a few things are wrong with this model. First, the model predictions are based on the fixed-effects, and fixed-effects variance (only) not taking into account the random effects variance.  So the 95% CI's are almost certainly overly precise compared to if they had included the random effects variance. 

###### This is a challenge in mixed-effects models! And there are HUGE amounts of pages dedicated to it
###### Here are just a few pages

##### https://cran.r-project.org/web/packages/merTools/vignettes/Using_predictInterval.html

##### 7.2 Predicting with predict() function

?predict.merMod

##### First we do the basic predictions which are the fixed-effects ignoring random effects

elk2$naive.pred <-predict(forrisk, type = "response")
elk2$fr.rc.pred <- predict(fr.rc, type = "response")
hist(elk2$fr.rc.pred)
# note this is the same as specifying the full random effects 
# elk2$fr.rc.pred3 <- predict(fr.rc, re.form = ~(ctotrisk_sc|elkuid), type = "response")
summary(elk2$fr.rc.pred)
##### First we do the basic predictions which are the fixed-effects unconditional on the random effects (i.e., Naive logit)
elk2$fr.rc.pred2 <- predict(fr.rc, re.form = NA, type = "response")
summary(elk2$fr.rc.pred2)

##### But note now we can make predictions for JUST individual elk ignoring the variation between individuals in predation risk responses
elk2$fr.rc.pred3 <- predict(fr.rc, re.form = ~(1|elkuid) , type = "response")
summary(elk2$fr.rc.pred3)
hist(elk2$fr.rc.pred3)

##### Explore relationships between different predictions

plot(elk2$fr.rc.pred2, elk2$fr.rc.pred)
##### This is the plot of the predictions from the unconditional predictions (X) versus the fully-specified random effects of risk|elkid (y)

ggpairs(elk2[46:49])
##### This plot compares the predictions from the full mixed-effect model (fr.rc.pred), the unconditional predictions ignoring the RE's (fr.rc.pred2), the predictions from the model only considering the random effect of individual elk (not response to wolves, fr.rc.pred3), and the naive logistic regression results ignoring everything. 



#### Objective 7.4 Comparing spatial predictions from fixed and mixed-effect models. 

##### First, lets plot the same kind of predictions starting with the 'Naive' RSF model
ggplot(elk2, aes(utmx, utmy, col = naive.pred)) + geom_point(size=5) + coord_equal() +  scale_colour_gradient(low = 'yellow', high = 'red')

##### Now the random effects model with ctotrisk|elkuid
ggplot(elk2, aes(utmx, utmy, col = fr.rc.pred)) + geom_point(size=5) + coord_equal() +  scale_colour_gradient(low = 'yellow', high = 'red')

##### Now the predictions unconditioned on any random effects 
ggplot(elk2, aes(utmx, utmy, col = fr.rc.pred2)) + geom_point(size=5) + coord_equal() +  scale_colour_gradient(low = 'yellow', high = 'red')

##### And finally, the spaital predictions holding effects of predation risk constant andonly considering vairation between elk (not as sensible in this example)
ggplot(elk2, aes(utmx, utmy, col = fr.rc.pred3)) + geom_point(size=5) + coord_equal() +  scale_colour_gradient(low = 'yellow', high = 'red')



##### Objective 7.5 Predicting using the bootMer() command  - To be Continued...

##### As the documentation for lme4::predict.merMod() notes:
##### There is no option for computing standard errors of predictions because it is difficult to define an efficient method that incorporates uncertainty in the variance parameters; we recommend lme4::bootMer() for this task.

#boot.fr.rc <- bootMer(fr.rc, FUN = function(x) as.numeric(logLik(x), nsim = 100))
#boot.fr.rc$mle
#### These are the MLE estimates of your beta coefficients given the model structure 

##### Or in the cases where the model is too big or complex, you can extract the prediction intervals using predictInterval() from the package merTools
##### http://stats.stackexchange.com/questions/147836/prediction-interval-for-lmer-mixed-effects-model-in-r

#preds <- predictInterval(fr.rc, newdata = newDat, n.sims = 99)
