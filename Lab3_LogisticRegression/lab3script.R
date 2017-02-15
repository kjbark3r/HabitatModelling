### WILD 562 : Lab 3 Logistic Regression
#~#  = mynotes

#function to install and load required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#load or install these packages:
packages <- c("ggplot2","foreign", "lattice", "psych", "effects", "plyr")

#run function to install packages
ipak(packages)

#####
setwd("C:\\Users\\kjbark3r\\Documents\\GitHub\\HabitatModelling\\Lab3_LogisticRegression\\")


#### 0.1 Merging the wolf availabiltiy sample from the KDE's from last week. 
# Note last week we merged the wolf USED data, but not the availability data. Here is the code to do that
#### merging the availabiltiy data. Note I did this for you, skip to 1.0 below. 
#rdavail <- as.data.frame(cov.availRD)
#str(rdavail)
#rdavail$pack <- c("Red Deer")
#str(rdused)

## repeat for Bow Valley pack
#bvavail <- as.data.frame(cov.availBV)
#str(bvavail)
#bvavail$pack <- c("Bow Valley")
#str(bvavail)

## merge the two availability samples together
#wolfavail <- rbind(rdavail, bvavail)
#str(wolfavail)

## and for next week, lets add a new column for a 1=used 0 = avail
#wolfavail$used <- 0

#write.table(wolfavail, file = "/Users/mark.hebblewhite/Dropbox/WILD 562/Spring2017/lab2/new/wolfavail.csv", row.names=FALSE, na="", col.names=TRUE, sep=",")

## 1.0 Merging wolf USED and wolf AVAIL datasets
# first read them back into active memory. 

wolfused <-read.csv("wolfused.csv", header = TRUE)
wolfavail <-read.csv("wolfavail.csv", header = TRUE)
str(wolfavail)

wolfkde <- rbind(wolfused, wolfavail)
str(wolfkde)
table(wolfkde$used, wolfkde$pack)
table(wolfkde$used, wolfkde$deer_w2)

## next we will create a new variable called usedFactor and graphically compare USED and AVAIL locations for prey
wolfkde$usedFactor <- factor(wolfkde$used, labels=c('0','1'))
str(wolfkde)


par(mfrow = c(2,3))
boxplot(deer_w2~usedFactor, data=wolfkde, main = "Deer Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(elk_w2~usedFactor, main = "Elk Used-Avail", ylab="elk_w2", xlab="usedFactor", data=wolfkde)
boxplot(moose_w2~usedFactor, main = "Moose Used-Avail", ylab="moose_w2", xlab="usedFactor", data=wolfkde)
boxplot(goat_w2~usedFactor, main = "Goat Used-Avail", ylab="goat_w2", xlab="usedFactor", data=wolfkde)
boxplot(sheep_w2~usedFactor, main = "Sheep Used-Avail", ylab="sheep_w2", xlab="usedFactor", data=wolfkde)
## Now lets do for Elevation and Distance from Human Access2
par(mfrow = c(1,2))
boxplot(Elevation2~usedFactor, data=wolfkde, main = "Elevation Used-Avail", xlab="usedFactor", ylab="elev")
boxplot(DistFromHumanAccess2~usedFactor, data=wolfkde, main = "Human Access Used-Avail", ylab="elk_w2", xlab="usedFactor")

## subset for Bow Valley Pack
bvkde<- subset(wolfkde, subset=pack =="Bow Valley")
par(mfrow = c(2,3))
boxplot(deer_w2~usedFactor, data=bvkde, main = "Deer Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(elk_w2~usedFactor, data=bvkde, main = "Elk Used-Avail", ylab="elk_w2", xlab="usedFactor")
boxplot(moose_w2~usedFactor, data=bvkde, main = "Moose Used-Avail", ylab="moose_w2", xlab="usedFactor")
boxplot(goat_w2~usedFactor, data=bvkde, main = "Goat Used-Avail", ylab="goat_w2", xlab="usedFactor")
boxplot(sheep_w2~usedFactor, data=bvkde, main = "Sheep Used-Avail", ylab="sheep_w2", xlab="usedFactor")
## Now lets do for Elevation and Distance from Human Access2
par(mfrow = c(1,2))
boxplot(Elevation2~usedFactor, data=bvkde, main = "Elevation Used-Avail", xlab="usedFactor", ylab="elev")
boxplot(DistFromHumanAccess2~usedFactor, data=bvkde, main = "Human Access Used-Avail", ylab="elk_w2", xlab="usedFactor")

## subset for Red Deer Wolf
rdkde <- subset(wolfkde, subset=pack=="Red Deer")
table(rdkde$used, rdkde$pack)
par(mfrow = c(2,3))
boxplot(deer_w2~usedFactor, data=rdkde, main = "Deer Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(elk_w2~usedFactor, data=rdkde, main = "Elk Used-Avail", ylab="elk_w2", xlab="usedFactor")
boxplot(moose_w2~usedFactor, data=rdkde, main = "Moose Used-Avail", ylab="moose_w2", xlab="usedFactor")
boxplot(goat_w2~usedFactor, data=rdkde, main = "Goat Used-Avail", ylab="goat_w2", xlab="usedFactor")
boxplot(sheep_w2~usedFactor, data=rdkde, main = "Sheep Used-Avail", ylab="sheep_w2", xlab="usedFactor")
## Now lets do for Elevation and Distance from Human Access2
par(mfrow = c(1,2))
boxplot(Elevation2~usedFactor, data=rdkde, main = "Elevation Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(DistFromHumanAccess2~usedFactor, data=rdkde, main = "Human Access Used-Avail", ylab="elk_w2", xlab="usedFactor")


## Can make more complex box plots
par(mfrow = c(1,1))
boxplot(Elevation2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer Elevation Used-Avail")
boxplot(DistFromHumanAccess2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer Human Access Used-Avail")
boxplot(deer_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer DEER Used-Avail")
boxplot(moose_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer MOOSE Used-Avail")
boxplot(elk_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer ELK Used-Avail")
boxplot(goat_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer GOAT Used-Avail")
boxplot(sheep_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer SHEEP Used-Avail")

## using lattice package
bwplot(sheep_w2+ goat_w2 + elk_w2+moose_w2+ deer_w2~as.factor(usedFactor)|pack, data = wolfkde, layout = c(2,5), pch = "|", outer = TRUE)

## numerical summary statistics
aggregate(wolfkde, by=list(wolfkde$pack, wolfkde$used), FUN=mean, na.rm=TRUE)


# psych package
describeBy(wolfkde, wolfkde$pack)
#~# the above doesn't look at used/avail, but could add in as list like above (see help)
tapply(wolfkde$pack, wolfkde$used, summary)

## introduction to the plyr package
## http://stat545.com/block013_plyr-ddply.html
## and
## http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
## and Hadley Wickhams original paper here: plyr paper: The split-apply-combine strategy for data analysis, Hadley Wickham, Journal of Statistical Software, vol. 40, no. 1, pp. 1–29, 2011.   
#~# ddply(data, variable(s), functions)
ddply(wolfkde, c("pack", "used"), summarize, mean=mean(elk_w2, na.rm=TRUE), sd=sd(elk_w2, na.rm=TRUE))
ddply(wolfkde, c("pack", "used"), summarize, mean=mean(deer_w2, na.rm=TRUE), sd=sd(deer_w2, na.rm=TRUE))
ddply(wolfkde, c("pack", "used"), summarize, mean=mean(goat_w2, na.rm=TRUE), sd=sd(goat_w2, na.rm=TRUE))
ddply(wolfkde, c("pack", "used"), summarize, mean=mean(moose_w2, na.rm=TRUE), sd=sd(moose_w2, na.rm=TRUE))
ddply(wolfkde, c("pack", "used"), summarize, mean=mean(sheep_w2, na.rm=TRUE), sd=sd(sheep_w2, na.rm=TRUE))
ddply(wolfkde, c("pack", "used"), summarize, mean=mean(Elevation2, na.rm=TRUE), sd=sd(Elevation2, na.rm=TRUE))
ddply(wolfkde, c("pack", "used"), summarize, mean=mean(DistFromHumanAccess2, na.rm=TRUE), sd=sd(DistFromHumanAccess2, na.rm=TRUE))
## how could we do this all in one step?

#~# fixing margins and graphical parameters
#par(mfrow=c(1,2), mar=c(5,4,4,3))

## Objective 1.3 ###################
## First - revisiting plotting functions in ggplot2 - lets bring in old shapefile data with X and Y
wolfyht <-read.csv("wolfyht.csv", header = TRUE)
str(wolfyht)
# download the ggplot2 pdf manual and make sure you have the R Graphics Cookbook open too
# chapter 6 in R Graphics cookbook
# Data exploration
ggplot(wolfyht, aes(x=EASTING, y = NORTHING)) + 
  geom_point() + 
  stat_density2d() + 
  facet_grid(Pack ~ ., scales="free")
#~# facet is how you split separate packs in diff plots

# lets subset the data
ggplot(wolfyht, aes(x=EASTING, y = NORTHING)) + 
  geom_point() + 
  stat_density2d(aes(alpha=..density..), 
                 geom="tile", contour=FALSE) + 
  facet_grid(Pack ~ .)
#~# remove geom_points from above => essentially a kde
#~# or check out the cool raster stuff
ggplot(wolfyht, aes(x=EASTING, y = NORTHING)) + 
  geom_point() + 
  stat_density2d(aes(fill=..density..), 
                 geom="raster", contour=FALSE) + 
  facet_grid(Pack ~ .)


## Objective 2 ######################################
## Second - Univariate Logistic Regression
## Objective 2.1 first simulating data to understand the model
x = c(-50:50) ## just creating a uniform vector from -50 to 50. 
y = rbinom(length(x), 1, plogis(1+0.07*x) )
## unpack this line by ?rbinom
## and ? plogis

#~# rbinom creates random normal distn
  # for every vector of c's from -50 to 50,
    #run one trial
#rbinom(100, 1, 0.5) simulates one trial with one coin flip per trial with fair coin
#rbinom(100, 1, 0.07) simulates diff odds of event occurring
##plogis is distn fcn for logistic link fcn
# simulate prob with intercept val of 1 and coeff of 0.07
# ie here's a distn and i'm gonna simulate it with some noise
#plogis(1+0.07*-50)
#x as vector that affects odds of occurrence

plot( y ~ x)
abline(lm((y~x)))
wrong = lm(y~x)
summary(wrong)
plot(wrong)
#~# simulated with Bo of 1, but estimate Bo is wrong
# extrapolating limear model fit outside range where it can occur
# to see, plot( y ~ x, xlim=c(-60,60), ylim = c(2,2))
res = glm( y~x, family=binomial(link="logit"))
summary(res)
#~# much closer to real values we simulated
yLogit=predict(res)
plot( yLogit ~ x ) #~# this defaults to linear
yhat=predict(res, type="response")
plot( y ~ x)
lines(yhat~x) #~# real predicted probabilities put through link fcn
#~#ie through the logit link fcn

?glm

#### Univariate analysis of factors affecting wolf used locations
### Now, we are going to conduct univariate logistic regression following advice from (Hosmer and Lemeshow 2000) who advocate such an approach for understanding the effect of each covariate on the probability of use. 

elev <- glm(used ~ Elevation2, family=binomial(logit), data=wolfkde)
summary(elev)
#~# no 95% confidence interval here
#~# 
str(elev)
## exploring univarite logistic regression
## how to obtain 95% confidence intervals? Where are they in the output?
## CI's using profile log-likelihood's
confint(elev)
## CI's using standard errors
confint.default(elev)
#~# statisticians say to use profile log-likelihood
# which are slightly diff than the confint.default ones

## odds ratio's
exp(coefficients(elev))
#~#bc in orig log-oggs formula, "y" is ln(odds ratio), ln(p/1-p)
#~#this transforms response back into terms of odds ratio
# for very 1m inc in elev, odds ratio of any 1 wolf use locn 
  # being at that elev decs by odds ratio of .994
## how to obtain 95% CI's on odds ratio's
exp(cbind(OR=coef(elev), confint(elev)))

## rescaling beta coefficients and odds ratio's 
## note that for elevation, the change in odds is for every 1 meter change in elevation. Perhaps this is not useful.

#~# the above was boring bc wolves wouldn't really care about a 1m change in elev
#~# so check out in units of 100m instead
wolfkde$elev100 <- wolfkde$Elevation2 / 100
elev100 <- glm(used ~ elev100, family=binomial(logit), data=wolfkde)
summary(elev100)
exp(coef(elev100)) #~# gives odds ratio
## therefore the interpretation of the odds ratio is scale dependent
# for every 100m inc in elev, odds of wolf using that area is 43% less likely
# bc odds of using it is 0.56
#get confidence intervals to see that even tho it doesn't look that diff from 1 it is
exp(cbind(OR=coef(elev100), confint(elev100)))

## Unpacking logistic regression - interpreting coefficients
## excercise in EXCEL (argh!)
## repeat EXCEL excercise in R. 
elevBnp = 0:3000 ## creates a new vector elevBnp with ranges from 0 - 3000 in it. 
str(elevBnp)
elevPred = predict(elev, newdata=data.frame(Elevation2=elevBnp), type = "response") ## uses the predict function to predict Y values given the model object elev
hist(elevPred)
#~# elev discriminates between used/unused very well
plot(elevBnp, elevPred, type="l", ylim = c(0,1.0), ylab= "Pr(Used)")
# but were there elevations from 0 - 1300m in Banff?
plot(wolfkde$Elevation2, wolfkde$used)
lines(elevBnp, elevPred, type="l", ylab= "Pr(Used)", add = TRUE)

## next human use
distHuman <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolfkde)
summary(distHuman)
exp(coefficients(distHuman)) #~# GIVES ODDS RATIO
hist(wolfkde$DistFromHumanAccess2)
#~# most predictions are of low use
disthumanBnp = 0:7000
disthumanPred = predict(distHuman, newdata=data.frame(DistFromHumanAccess2=disthumanBnp), type="response")
hist(disthumanPred)
plot(disthumanBnp, disthumanPred, type="l", ylab= "Pr(Used)")
plot(wolfkde$DistFromHumanAccess2, wolfkde$used)
lines(disthumanBnp, disthumanPred, type="l", ylab= "Pr(Used)", add = TRUE)
#~# entire logistic regression is there, but range of observed vals isn't
# so you only see part of the full regression

# now lets do all at once for ungulate HSI models
sheep <- glm(used ~ sheep_w2, family=binomial(logit), data=wolfkde)
summary(sheep)
#~# use sheep < expected
exp(coefficients(sheep))
#1-odds ratio - 12% declien in odds of wolf using area
#[for every unit inc in sheep i think]
habvalues = 0:7
deer <- glm(used ~ deer_w2, family=binomial(logit), data=wolfkde)
summary(deer)
elk <- glm(used ~ elk_w2, family=binomial(logit), data=wolfkde)
summary(elk)
# positive selection coefficient for the above
moose <- glm(used ~ moose_w2, family=binomial(logit), data=wolfkde)
summary(moose)
goat <- glm(used ~ goat_w2, family=binomial(logit), data=wolfkde)
summary(goat)


habvalues = 0:7 #imaginary variable we created for regression
sheeppred = predict(sheep, newdata = data.frame(sheep_w2 = habvalues), type = "response")
goatpred = predict(goat, newdata = data.frame(goat_w2 = habvalues), type = "response")
moosepred = predict(moose, newdata = data.frame(moose_w2 = habvalues), type = "response")
elkpred = predict(elk, newdata = data.frame(elk_w2 = habvalues), type = "response")
deerpred = predict(deer, newdata = data.frame(deer_w2 = habvalues), type = "response")

plot(habvalues, elkpred, type ="l", ylim = c(0,1.0), ylab = "Pr(Used)", col = "green")
lines(habvalues, goatpred, col = "blue")
lines(habvalues, moosepred, col = "red") 
lines(habvalues, sheeppred, col = "black") 
lines(habvalues, deerpred, col = "gray") 
legend(x="topleft", legend= c("Elk","Mountain Goat", "Moose", "Sheep", "Deer"), lty=1, col = c("green", "blue", "red", "black", "gray"), bty = "n")
#~# shows selection coefficients

## back to elevation
elev <- glm(used ~ Elevation2, family=binomial(logit), data=wolfkde)
summary(elev)
wolfkde$fitted.Elev <- fitted(elev)
#~# fitted()extracts fitted vals from our glm model
head(wolfkde)
hist(wolfkde$fitted.Elev)
plot(wolfkde$Elevation2, wolfkde$fitted.Elev)

# ggplot 2 explore basic histogram function
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev)) + geom_histogram()
# lets explore faceting
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev)) + geom_histogram(binwidth=0.05, fill="gray70", colour="black") + facet_grid(used ~ .)
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev)) + geom_histogram(binwidth=0.05, fill="gray70", colour="black") + facet_grid(used ~ ., scales = "free")
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev, fill=usedFactor)) + geom_histogram(binwidth=0.05, position="identity", alpha=0.7) + xlab("Predicted Probability of Wolf Use") + theme(axis.title.x=element_text(size=16))
#~# he likes the above one
# orig data were 1s and 0s, but predictions are continuous
# this screws with your r2 val - how can you evaluate the model??
# so splitting out by 0 and 1 helpd visualize the relationship or something
# ponder this

# lets redo this graph using faceting by pack
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev, y=..density.., fill=usedFactor)) + geom_histogram(binwidth=0.05, position="identity", alpha=0.7) + xlab("Predicted Probability of Wolf Use") + theme(axis.title.x=element_text(size=16)) + facet_grid(pack ~ ., scales="free")
#looks at model that doesn't consider effect of pack
#and visualizes it by pack
#tells you the red deer pack is existing at higher elevations
#and this is 75% bow valley wolf model bc more data from them

# Now lets explore fitting functions to the distributions
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev)) + geom_density()
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev), fill=usedFactor) + geom_density(alpha=0.5) + xlim(0,1)+xlab("Predicted Probability of Wolf Use") + theme(axis.title.x=element_text(size=16)) 
# kernel lines
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev, y=..density.., fill=usedFactor)) + geom_histogram(binwidth=0.05) + geom_density(alpha = 0.5) + facet_grid(pack ~ .)

#############################################################################
# Exploring Predictions as a function of covariates
#___________
# this fits a univariate glm as a function of elevation and predicts
ggplot(wolfkde, aes(x=Elevation2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial"))

#
ggplot(wolfkde, aes(x=DistFromHumanAccess2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial"))
ggplot(wolfkde, aes(x=elk_w2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial"))
## but whats up with the dots? - lets jitter and see
ggplot(wolfkde, aes(x=elk_w2, y=used)) + geom_point() +geom_jitter(aes(colour = used), width=0.25, height = 0.05) + stat_smooth(method="glm", method.args = list(family="binomial"))
#~# jitter shows where classn is breaking up by 1s nd 0s
# "more confusion in the 0s," apparently
#supposedly helps visualize what's going on in the ogistic regression

## lets redo elevation jittered by used
ggplot(wolfkde, aes(x=Elevation2, y=used)) + geom_point() +geom_jitter(aes(colour = used), width=0.25, height = 0.05)+ stat_smooth(method="glm", method.args = list(family="binomial"))

# Splitting by wolf pack and change the confidence interval to 95th
#~# fit separate linear regression per pack
ggplot(wolfkde, aes(x=Elevation2, y=used, colour=pack)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial"), level=0.95)
ggplot(wolfkde, aes(x=Elevation2, y=used, colour=pack)) + geom_point() + geom_jitter(width=0.25, height = 0.05) +stat_smooth(method="glm", method.args = list(family="binomial"), level=0.90)
#~# diff steepness tells coeff for packs is different

# versus  faceting by wolf pack
ggplot(wolfkde, aes(x=Elevation2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial"), level=0.90) + facet_grid(pack~.)

ggplot(wolfkde, aes(x=sheep_w2, y=used)) + geom_point() + stat_smooth(method="glm", method.args = list(family="binomial"), level=0.90) + facet_grid(pack~.)
#~# they switch - bow valley avoids; red deer selects

# this function plots predictions from the previously fitted best model
ggplot(wolfkde, aes(x=Elevation2, y=fitted.Elev)) + geom_point() + stat_smooth(method=lm) + ylim(0, 0.8)
## this plot shows the problem with fitting linear models with binary data
#stupid straight line is the linear model

#
#Printing PDFs from R
pdf("wolf_elev.pdf", width=4, height=4)
print(ggplot(wolfkde, aes(x=Elevation2, y=used)) + 
        geom_point(colour="gray") + 
        stat_smooth(method="glm", method.args = list(family="binomial")) + 
        xlab("Prey H.S.I") + 
        ylab("Predicted Probability of Wolf Use"))
dev.off()
# then go and look in the active directory for wolf_elev.pdf
#or
ggsave("elev_wolf2.pdf", width=4, height=4)
#~# prob better; allowsmanual specification of dpi which journals have requirements about
#
# Now - how to make a figure of all 5 prey species predictions
## code figured out from Latham et al. (2013) in Ecography
fig3<-ggplot(wolfkde, aes(x=elk_w2, y=used)) + 
  geom_smooth(data = wolfkde, aes(x=elk_w2, y=used, col="Elk"),
              method="glm", method.args = list(family="binomial")) + 
  geom_smooth(data = wolfkde, aes(x=deer_w2, y=used, col="Deer"),
              method="glm", method.args = list(family="binomial"))+ 
  geom_smooth(data = wolfkde, aes(x=moose_w2, y=used, col="Moose"),
              method="glm", method.args = list(family="binomial"))+ 
  geom_smooth(data = wolfkde, aes(x=sheep_w2, y=used, col="Sheep"),
              method="glm", method.args = list(family="binomial"))+ 
  geom_smooth(data = wolfkde, aes(x=goat_w2, y=used, col="Goat"),
              method="glm", method.args = list(family="binomial")) + 
  xlab("Relative probability of summer prey resource selection") + 
  ylab("Relative probability of summer wolf resource selection") + 
  theme(axis.title.y=element_text(size=18), axis.text.y=element_text(size=18)) + 
  theme(axis.title.x=element_text(size=18), axis.text.x=element_text(size=18))+ 
  labs(fill="Prey Species")
fig3


## lastly, I will illustrate our first use of the effects package which is especially nice later for quickly visualizing interactions
plot(effect("Elevation2", elev), grid=TRUE) 
#~# predicted probability plot
# not as good as ggplot for linear regression; "slight overkill" but cool for interactions
plot(effect("deer_w2", deer), grid=TRUE)
## but note the scales are stretched to linearize the response