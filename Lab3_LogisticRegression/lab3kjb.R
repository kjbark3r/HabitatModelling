## WILDLIFE HABITAT MODELING LAB feb2017 ##
##  LAB 3 - Feb.2017 - Use/Availability  ##


#### SETUP ####

# function to install and load required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#load or install these packages:
packages <- c("ggplot2","foreign", "lattice", "psych", "effects", "plyr")

# run function to install packages
ipak(packages)

# set working directory
setwd("C:\\Users\\kjbark3r\\Documents\\GitHub\\HabitatModelling\\Lab3_LogisticRegression\\")

# read in data
wolfused <-read.csv("wolfused.csv", header = TRUE)
wolfavail <-read.csv("wolfavail.csv", header = TRUE)

# combine data into used/available dataframe
wolfkde <- rbind(wolfused, wolfavail)
table(wolfkde$used, wolfkde$pack)
wolfkde$usedFactor <- factor(wolfkde$used, labels=c('0','1'))


#### KDE SELECTION ####


#### All wolves ####

outputs <- data.frame(
	Model = character(),
	OddsRatio = numeric(),
	LowCI = numeric(),
	HighCI = numeric(),
	Intercept = integer(),
	stringsAsFactors = FALSE)

modnames <- c("elev100",
			  "distHuman",
			  "sheep",
			  "goat",
			  "moose",
			  "elk ",
			  "deer",
			  stringsAsFactors=FALSE)
			  
for(i in 1:7) {
	mod = modnames[i]
	outputs[i,1] <- paste(mod)
	}


## elevation (m) ##
elev <- glm(used ~ Elevation2, family=binomial(logit), data=wolfkde)
summary(elev)
str(elev)
## CI's using profile log-likelihood's
confint(elev)
## odds ratio's
exp(coefficients(elev))
## how to obtain 95% CI's on odds ratio's
exp(cbind(OR=coef(elev), confint(elev)))



## elevation (100m) ##
wolfkde$elev100 <- wolfkde$Elevation2 / 100
elev100 <- glm(used ~ elev100, family=binomial(logit), data=wolfkde)
summary(elev100)
exp(coef(elev100)) #~# gives odds ratio (rather than log-odds)
# for every 100m inc in elev, odds of wolf using that area is 43% less likely
#get confidence intervals to see that even tho it doesn't look that diff from 1 it is
exp(cbind(OR=coef(elev100), confint(elev100)))
elevBnp = 0:3000 ## creates a new vector elevBnp with ranges from 0 - 3000 in it. 
str(elevBnp)
## use predict function to predict Y values using the model object elev
 # predicts probability of use of elev values based on modeled relnship
elevPred = predict(elev, newdata=data.frame(Elevation2=elevBnp), type = "response")
hist(elevPred)
#~# elev discriminates between used/unused very well
plot(elevBnp, elevPred, type="l", ylim = c(0,1.0), ylab= "Pr(Used)")
# but were there elevations from 0 - 1300m in Banff?
plot(wolfkde$Elevation2, wolfkde$used)
lines(elevBnp, elevPred, type="l", ylab= "Pr(Used)", add = TRUE)
# extract fitted values from our model (Pr(use))
wolfkde$fitted.Elev <- fitted(elev)
# store model results
outputs[1,2] <- exp(coef(elev100)[2])
outputs[1,3] <- confint(elev100)[2,1]
outputs[1,4] <- confint(elev100)[2,2]
outputs[1,5] <- exp(coef(elev100)[1])


## human use ##
distHuman <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=wolfkde)
summary(distHuman)
exp(coefficients(distHuman)) #~# GIVES ODDS RATIO
hist(wolfkde$DistFromHumanAccess2) #~# most predictions are of low use
disthumanBnp = 0:7000
disthumanPred = predict(distHuman, newdata=data.frame(DistFromHumanAccess2=disthumanBnp), type="response")
hist(disthumanPred)
plot(disthumanBnp, disthumanPred, type="l", ylab= "Pr(Used)")
plot(wolfkde$DistFromHumanAccess2, wolfkde$used)
lines(disthumanBnp, disthumanPred, type="l", ylab= "Pr(Used)", add = TRUE)
wolfkde$fitted.distHuman <- fitted(distHuman)
# store model results
outputs[2,2] <- exp(coef(distHuman)[2])
outputs[2,3] <- confint(distHuman)[2,1]
outputs[2,4] <- confint(distHuman)[2,2]
outputs[2,5] <- exp(coef(distHuman)[1])


## hsi vals for predictions ##
habvalues = 0:7

## sheep ##
sheep <- glm(used ~ sheep_w2, family=binomial(logit), data=wolfkde)
summary(sheep) 
exp(coefficients(sheep))
sheeppred = predict(sheep, newdata = data.frame(sheep_w2 = habvalues), type = "response")
wolfkde$fitted.Sheep <- fitted(sheep)
# store model results
outputs[3,2] <- exp(coef(sheep)[2])
outputs[3,3] <- confint(sheep)[2,1]
outputs[3,4] <- confint(sheep)[2,2]
outputs[3,5] <- exp(coef(sheep)[1])

## goat ##
goat <- glm(used ~ goat_w2, family=binomial(logit), data=wolfkde)
summary(goat) 
exp(coefficients(goat))
goatpred = predict(goat, newdata = data.frame(goat_w2 = habvalues), type = "response")
wolfkde$fitted.Goat <- fitted(goat)
# store model results
outputs[4,2] <- exp(coef(goat)[2])
outputs[4,3] <- confint(goat)[2,1]
outputs[4,4] <- confint(goat)[2,2]
outputs[4,5] <- exp(coef(goat)[1])

## moose ##
moose <- glm(used ~ moose_w2, family=binomial(logit), data=wolfkde)
summary(moose) 
exp(coefficients(moose))
moosepred = predict(moose, newdata = data.frame(moose_w2 = habvalues), type = "response")
wolfkde$fitted.Moose <- fitted(moose)
# store model results
outputs[5,2] <- exp(coef(moose)[2])
outputs[5,3] <- confint(moose)[2,1]
outputs[5,4] <- confint(moose)[2,2]
outputs[5,5] <- exp(coef(moose)[1])

## elk ##
elk <- glm(used ~ elk_w2, family=binomial(logit), data=wolfkde)
summary(elk) 
exp(coefficients(elk))
elkpred = predict(elk, newdata = data.frame(elk_w2 = habvalues), type = "response")
wolfkde$fitted.Elk <- fitted(elk)
# store model results
outputs[6,2] <- exp(coef(elk)[2])
outputs[6,3] <- confint(elk)[2,1]
outputs[6,4] <- confint(elk)[2,2]
outputs[6,5] <- exp(coef(elk)[1])

## deer ##
deer <- glm(used ~ deer_w2, family=binomial(logit), data=wolfkde)
summary(deer) 
exp(coefficients(deer))
deerpred = predict(deer, newdata = data.frame(deer_w2 = habvalues), type = "response")
wolfkde$fitted.Deer <- fitted(deer)
# store model results
outputs[7,2] <- exp(coef(deer)[2])
outputs[7,3] <- confint(deer)[2,1]
outputs[7,4] <- confint(deer)[2,2]
outputs[7,5] <- exp(coef(deer)[1])



#### Red Deer ####

rdkde <- wolfkde[wolfkde$pack=="Red Deer",]
rdkde <- na.omit(rdkde)

outputsrd <- data.frame(
	Model = character(),
	OddsRatio = numeric(),
	LowCI = numeric(),
	HighCI = numeric(),
	Intercept = integer(),
	stringsAsFactors = FALSE)

modnamesrd <- c("elevrd100",
			  "distHumanrd",
			  "sheeprd",
			  "goatrd",
			  "mooserd",
			  "elkrd",
			  "deerrd",
			  stringsAsFactors=FALSE)
			  
for(i in 1:7) {
	mod = modnamesrd[i]
	outputsrd[i,1] <- paste(mod)
	}

## elevation (m) ##

elevrd <- glm(used ~ Elevation2, family=binomial(logit), data=rdkde)
summary(elevrd)
str(elevrd)
## CI's using profile log-likelihood's
confint(elevrd)
## odds ratio's
exp(coefficients(elevrd))
## how to obtain 95% CI's on odds ratio's
exp(cbind(OR=coef(elevrd), confint(elevrd)))


## elevation (100m) ##
rdkde$elev100 <- rdkde$Elevation2 / 100
elevrd100 <- glm(used ~ elev100, family=binomial(logit), data=rdkde)
summary(elevrd100)
exp(coef(elevrd100)) #~# gives odds ratio
# for every 100m inc in elev, odds of wolf using that area is 43% less likely
#get confidence intervals to see that even tho it doesn't look that diff from 1 it is
exp(cbind(OR=coef(elevrd100), confint(elevrd100)))
elevBnp = 0:3000 ## creates a new vector elevBnp with ranges from 0 - 3000 in it. 
str(elevBnp)
## use predict function to predict Y values using the model object elev
 # predicts probability of use of elev values based on modeled relnship
elevrdPred = predict(elevrd, newdata=data.frame(Elevation2=elevBnp), type = "response")
hist(elevrdPred)
#~# elev discriminates between used/unused very well
plot(elevBnp, elevrdPred, type="l", ylim = c(0,1.0), ylab= "Pr(Used)")
# but were there elevations from 0 - 1300m in Banff?
plot(rdkde$Elevation2, rdkde$used)
lines(elevBnp, elevrdPred, type="l", ylab= "Pr(Used)", add = TRUE)
# extract fitted values from our model (Pr(use))
rdkde$fitted.elevrd <- fitted(elevrd)
# store model results
outputsrd[1,2] <- exp(coef(elevrd100)[2])
outputsrd[1,3] <- confint(elevrd100)[2,1]
outputsrd[1,4] <- confint(elevrd100)[2,2]
outputsrd[1,5] <- exp(coef(elevrd100)[1])


## human use ##
distHumanrd <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=rdkde)
summary(distHumanrd)
exp(coefficients(distHumanrd)) #~# GIVES ODDS RATIO
hist(rdkde$DistFromHumanAccess2) #~# most predictions are of low use
distHumanrdBnp = 0:7000
distHumanrdPred = predict(distHumanrd, newdata=data.frame(DistFromHumanAccess2=distHumanrdBnp), type="response")
hist(distHumanrdPred)
plot(distHumanrdBnp, distHumanrdPred, type="l", ylab= "Pr(Used)")
plot(rdkde$DistFromHumanAccess2, rdkde$used)
lines(distHumanrdBnp, distHumanrdPred, type="l", ylab= "Pr(Used)", add = TRUE)
rdkde$fitted.distHumanrd <- fitted(distHumanrd)
# store model results
outputsrd[2,2] <- exp(coef(distHumanrd)[2])
outputsrd[2,3] <- confint(distHumanrd)[2,1]
outputsrd[2,4] <- confint(distHumanrd)[2,2]
outputsrd[2,5] <- exp(coef(distHumanrd)[1])

## hsi vals for predictions ##
habvalues = 0:7

## sheep ##
sheeprd <- glm(used ~ sheep_w2, family=binomial(logit), data=rdkde)
summary(sheeprd) 
exp(coefficients(sheeprd))
sheeprdpred = predict(sheeprd, newdata = data.frame(sheep_w2 = habvalues), type = "response")
rdkde$fitted.sheeprd <- fitted(sheeprd)
# store model results
outputsrd[3,2] <- exp(coef(sheeprd)[2])
outputsrd[3,3] <- confint(sheeprd)[2,1]
outputsrd[3,4] <- confint(sheeprd)[2,2]
outputsrd[3,5] <- exp(coef(sheeprd)[1])

## goat ##
goatrd <- glm(used ~ goat_w2, family=binomial(logit), data=rdkde)
summary(goatrd) 
exp(coefficients(goatrd))
goatrdpred = predict(goatrd, newdata = data.frame(goat_w2 = habvalues), type = "response")
rdkde$fitted.goatrd <- fitted(goatrd)
# store model results
outputsrd[4,2] <- exp(coef(goatrd)[2])
outputsrd[4,3] <- confint(goatrd)[2,1]
outputsrd[4,4] <- confint(goatrd)[2,2]
outputsrd[4,5] <- exp(coef(goatrd)[1])

## moose ##
mooserd <- glm(used ~ moose_w2, family=binomial(logit), data=rdkde)
summary(mooserd) 
exp(coefficients(mooserd))
mooserdpred = predict(mooserd, newdata = data.frame(moose_w2 = habvalues), type = "response")
rdkde$fitted.mooserd <- fitted(mooserd)
# store model results
outputsrd[5,2] <- exp(coef(mooserd)[2])
outputsrd[5,3] <- confint(mooserd)[2,1]
outputsrd[5,4] <- confint(mooserd)[2,2]
outputsrd[5,5] <- exp(coef(mooserd)[1])

## elk ##
elkrd <- glm(used ~ elk_w2, family=binomial(logit), data=rdkde)
summary(elkrd) 
exp(coefficients(elkrd))
elkrdpred = predict(elkrd, newdata = data.frame(elk_w2 = habvalues), type = "response")
rdkde$fitted.elkrd <- fitted(elkrd)
# store model results
outputsrd[6,2] <- exp(coef(elkrd)[2])
outputsrd[6,3] <- confint(elkrd)[2,1]
outputsrd[6,4] <- confint(elkrd)[2,2]
outputsrd[6,5] <- exp(coef(elkrd)[1])


## deer ##
deerrd <- glm(used ~ deer_w2, family=binomial(logit), data=rdkde)
summary(deerrd) 
exp(coefficients(deerrd))
deerrdpred = predict(deerrd, newdata = data.frame(deer_w2 = habvalues), type = "response")
rdkde$fitted.deerrd <- fitted(deerrd)
# store model results
outputsrd[7,2] <- exp(coef(deerrd)[2])
outputsrd[7,3] <- confint(deerrd)[2,1]
outputsrd[7,4] <- confint(deerrd)[2,2]
outputsrd[7,5] <- exp(coef(deerrd)[1])


#### Bow Valley ####

bvkde <- wolfkde[wolfkde$pack=="Bow Valley",]
bvkde <- na.omit(bvkde)

outputsbv <- data.frame(
	Model = character(),
	OddsRatio = numeric(),
	LowCI = numeric(),
	HighCI = numeric(),
	Intercept = integer(),
	stringsAsFactors = FALSE)

modnamesbv <- c("elevbv100",
			  "distHumanbv",
			  "sheepbv",
			  "goatbv",
			  "moosebv",
			  "elkbv",
			  "deerbv",
			  stringsAsFactors=FALSE)
			  
for(i in 1:7) {
	mod = modnamesbv[i]
	outputsbv[i,1] <- paste(mod)
	}


## elevation (m) ##
elevbv <- glm(used ~ Elevation2, family=binomial(logit), data=bvkde)
summary(elevbv)
str(elevbv)
## CI's using profile log-likelihood's
confint(elevbv)
## odds ratio's
exp(coefficients(elevbv))
## how to obtain 95% CI's on odds ratio's
exp(cbind(OR=coef(elevbv), confint(elevbv)))


## elevation (100m) ##
bvkde$elev100 <- bvkde$Elevation2 / 100
elevbv100 <- glm(used ~ elev100, family=binomial(logit), data=bvkde)
summary(elevbv100)
exp(coef(elevbv100)) #~# gives odds ratio
exp(cbind(OR=coef(elevbv100), confint(elevbv100)))
elevBnp = 0:3000 ## creates a new vector elevBnp with ranges from 0 - 3000 in it. 
str(elevBnp)
## use predict function to predict Y values using the model object elev
 # predicts probability of use of elev values based on modeled relnship
elevbvPred = predict(elevbv, newdata=data.frame(Elevation2=elevBnp), type = "response")
hist(elevbvPred)
#~# elev discriminates between used/unused very well
plot(elevBnp, elevbvPred, type="l", ylim = c(0,1.0), ylab= "Pr(Used)")
# but were there elevations from 0 - 1300m in Banff?
plot(bvkde$Elevation2, bvkde$used)
lines(elevBnp, elevbvPred, type="l", ylab= "Pr(Used)", add = TRUE)
# extract fitted values from our model (Pr(use))
bvkde$fitted.elevbv <- fitted(elevbv)
# store model results
outputsbv[1,2] <- exp(coef(elevbv100)[2])
outputsbv[1,3] <- confint(elevbv100)[2,1]
outputsbv[1,4] <- confint(elevbv100)[2,2]
outputsbv[1,5] <- exp(coef(elevbv100)[1])

## human use ##
distHumanbv <- glm(used ~ DistFromHumanAccess2, family=binomial(logit), data=bvkde)
summary(distHumanbv)
exp(coefficients(distHumanbv)) #~# GIVES ODDS RATIO
hist(bvkde$DistFromHumanAccess2) #~# most predictions are of low use
distHumanbvBnp = 0:7000
distHumanbvPred = predict(distHumanbvrd, newdata=data.frame(DistFromHumanAccess2=distHumanbvBnp), type="response")
hist(distHumanbvPred)
plot(distHumanbvBnp, distHumanbvPred, type="l", ylab= "Pr(Used)")
plot(bvkde$DistFromHumanAccess2, bvkde$used)
lines(distHumanbvBnp, distHumanbvPred, type="l", ylab= "Pr(Used)", add = TRUE)
bvkde$fitted.distHumanbv <- fitted(distHumanbv)
# store model results
outputsbv[2,2] <- exp(coef(distHumanbv)[2])
outputsbv[2,3] <- confint(distHumanbv)[2,1]
outputsbv[2,4] <- confint(distHumanbv)[2,2]
outputsbv[2,5] <- exp(coef(distHumanbv)[1])


## hsi vals for predictions ##
habvalues = 0:7

## sheep ##
sheepbv <- glm(used ~ sheep_w2, family=binomial(logit), data=bvkde)
summary(sheepbv) 
exp(coefficients(sheepbv))
sheepbvpred = predict(sheepbv, newdata = data.frame(sheep_w2 = habvalues), type = "response")
bvkde$fitted.sheepbv <- fitted(sheepbv)
# store model results
outputsbv[3,2] <- exp(coef(sheepbv)[2])
outputsbv[3,3] <- confint(sheepbv)[2,1]
outputsbv[3,4] <- confint(sheepbv)[2,2]
outputsbv[3,5] <- exp(coef(sheepbv)[1])

## goat ##
goatbv <- glm(used ~ goat_w2, family=binomial(logit), data=bvkde)
summary(goatbv) 
exp(coefficients(goatbv))
goatbvpred = predict(goatbv, newdata = data.frame(goat_w2 = habvalues), type = "response")
bvkde$fitted.goatbv <- fitted(goatbv)
# store model results
outputsbv[4,2] <- exp(coef(goatbv)[2])
outputsbv[4,3] <- confint(goatbv)[2,1]
outputsbv[4,4] <- confint(goatbv)[2,2]
outputsbv[4,5] <- exp(coef(goatbv)[1])

## moose ##
moosebv <- glm(used ~ moose_w2, family=binomial(logit), data=bvkde)
summary(moosebv) 
exp(coefficients(moosebv))
moosebvpred = predict(moosebv, newdata = data.frame(moose_w2 = habvalues), type = "response")
bvkde$fitted.moosebv <- fitted(moosebv)
# store model results
outputsbv[5,2] <- exp(coef(moosebv)[2])
outputsbv[5,3] <- confint(moosebv)[2,1]
outputsbv[5,4] <- confint(moosebv)[2,2]
outputsbv[5,5] <- exp(coef(moosebv)[1])

## elk ##
elkbv <- glm(used ~ elk_w2, family=binomial(logit), data=bvkde)
summary(elkbv) 
exp(coefficients(elkbv))
elkbvpred = predict(elkbv, newdata = data.frame(elk_w2 = habvalues), type = "response")
bvkde$fitted.elkbv <- fitted(elkbv)
# store model results
outputsbv[6,2] <- exp(coef(elkbv)[2])
outputsbv[6,3] <- confint(elkbv)[2,1]
outputsbv[6,4] <- confint(elkbv)[2,2]
outputsbv[6,5] <- exp(coef(elkbv)[1])


## deer ##
deerbv <- glm(used ~ deer_w2, family=binomial(logit), data=bvkde)
summary(deerbv) 
exp(coefficients(deerbv))
deerbvpred = predict(deerbv, newdata = data.frame(deer_w2 = habvalues), type = "response")
bvkde$fitted.deerbv <- fitted(deerbv)
# store model results
outputsbv[7,2] <- exp(coef(deerbv)[2])
outputsbv[7,3] <- confint(deerbv)[2,1]
outputsbv[7,4] <- confint(deerbv)[2,2]
outputsbv[7,5] <- exp(coef(deerbv)[1])

## combine data ##

results <- rbind(outputs, outputsrd, outputsbv)
write.csv(results, file = "kde-results.csv", row.names=F)


#### MCP SELECTION - ALL WOLVES ####





#### MCP SELECTION - BY PACK ####




#### MCP/KDE COMPARISONS ####



#### STATS SHIT - for report ####



#### VISUALIZATIONS - for report ####

# pr(used) ~ prey HSI

par(mfrow=c(3,1))
# all wolves
plot(habvalues, elkpred, 
     type ="l", 
     ylim = c(0,1.0), 
     ylab = "Pr(Used)",
     xlab = "Habitat Suitability Index (HSI)",
     lty = 1,
     main = "All Wolves")
lines(habvalues, goatpred, lty = 2)
lines(habvalues, moosepred, lty = 3) 
lines(habvalues, sheeppred, lty = 5) 
lines(habvalues, deerpred, lty = 6) 
legend(x="topleft", 
       legend= c("Elk","Mountain Goat", "Moose", "Sheep", "Deer"), 
       lty=c(1,2,3,5,6), bty = "n")
# red deer
plot(habvalues, elkrdpred, 
     type ="l", 
     ylim = c(0,1.0), 
     ylab = "Pr(Used)",
     xlab = "Habitat Suitability Index (HSI)",
     lty = 1,
     main = "Red Deer pack")
lines(habvalues, goatrdpred, lty = 2)
lines(habvalues, mooserdpred, lty = 3) 
lines(habvalues, sheeprdpred, lty = 5) 
lines(habvalues, deerrdpred, lty = 6) 
legend(x="topleft", 
       legend= c("Elk","Mountain Goat", "Moose", "Sheep", "Deer"), 
       lty=c(1,2,3,5,6), bty = "n")
# bow valley
plot(habvalues, elkbvpred, 
     type ="l", 
     ylim = c(0,1.0), 
     ylab = "Pr(Used)",
     xlab = "Habitat Suitability Index (HSI)",
     lty = 1,
     main = "Bow Valley pack")
lines(habvalues, goatbvpred, lty = 2)
lines(habvalues, moosebvpred, lty = 3) 
lines(habvalues, sheepbvpred, lty = 5) 
lines(habvalues, deerbvpred, lty = 6) 
legend(x="topleft", 
       legend= c("Elk","Mountain Goat", "Moose", "Sheep", "Deer"), 
       lty=c(1,2,3,5,6), bty = "n")


#### VISUALIZATIONS - prelim for me ####

# boxplots - used vs avail for each pack on same plot
par(mfrow = c(1,1))
boxplot(Elevation2~used+pack, data = wolfkde, main = "Elevation")
boxplot(DistFromHumanAccess2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer Human Access Used-Avail")
boxplot(deer_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer DEER Used-Avail")
boxplot(moose_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer MOOSE Used-Avail")
boxplot(elk_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer ELK Used-Avail")
boxplot(goat_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer GOAT Used-Avail")
boxplot(sheep_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer SHEEP Used-Avail")


library(gridExtra)
eplot <- ggplot(data = wolfkde,
       aes(y = Elevation2,
           x = pack,
           fill = usedFactor)) +
  labs(title = "Elevation",
            x = "", y = "Meters") +
  theme(legend.position="none") +
  scale_fill_manual(labels = c("Available", "Used"),
                    values=c("grey40","white")) +
  geom_boxplot()
hplot <- ggplot(data = wolfkde,
       aes(y = DistFromHumanAccess2,
           x = pack,
           fill = usedFactor)) +
  labs(title = "Distance Human Access",
            x = "", y = "Meters") +
              theme(legend.position="none") +
  scale_fill_manual(labels = c("Available", "Used"),
                    values=c("grey40","white")) +
  geom_boxplot()
elkplot <- ggplot(data = wolfkde,
       aes(y = elk_w2,
           x = pack,
           fill = usedFactor)) +
    labs(title = "Elk",
            x = "", y = "HSI") +
              theme(legend.position="none") +
  scale_fill_manual(labels = c("Available", "Used"),
                    values=c("grey40","white")) +
  geom_boxplot()
dplot <- ggplot(data = wolfkde,
       aes(y = deer_w2,
           x = pack,
           fill = usedFactor)) +
      labs(title = "Deer",
            x = "", y = "HSI") +
              theme(legend.position="none") +
  scale_fill_manual(labels = c("Available", "Used"),
                    values=c("grey40","white")) +
  geom_boxplot()
mplot <- ggplot(data = wolfkde,
       aes(y = moose_w2,
           x = pack,
           fill = usedFactor)) +
      labs(title = "Moose",
            x = "", y = "HSI") +
              theme(legend.position="none") +
  scale_fill_manual(labels = c("Available", "Used"),
                    values=c("grey40","white")) +
  geom_boxplot()
gplot <- ggplot(data = wolfkde,
       aes(y = goat_w2,
           x = pack,
           fill = usedFactor)) +
      labs(title = "Goat",
            x = "", y = "HSI") +
              theme(legend.position="none") +
  scale_fill_manual(labels = c("Available", "Used"),
                    values=c("grey40","white")) +
  geom_boxplot()
splot <- ggplot(data = wolfkde,
       aes(y = sheep_w2,
           x = pack,
           fill = usedFactor)) +
      labs(title = "Sheep",
            x = "", y = "HSI") +
    scale_fill_manual(labels = c("Available", "Used"),
                    values=c("grey40","white")) +
  theme(legend.title = element_blank()) +
  geom_boxplot()
grid.arrange(eplot, hplot,
             elkplot, dplot, mplot,
             gplot, splot,
             ncol = 2)


## used:avail ##

# all wolves
par(mfrow=c(4,2))
#par(mfrow = c(2,3))
boxplot(deer_w2~usedFactor, data=wolfkde, main = "Deer Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(elk_w2~usedFactor, main = "Elk Used-Avail", ylab="elk_w2", xlab="usedFactor", data=wolfkde)
boxplot(moose_w2~usedFactor, main = "Moose Used-Avail", ylab="moose_w2", xlab="usedFactor", data=wolfkde)
boxplot(goat_w2~usedFactor, main = "Goat Used-Avail", ylab="goat_w2", xlab="usedFactor", data=wolfkde)
boxplot(sheep_w2~usedFactor, main = "Sheep Used-Avail", ylab="sheep_w2", xlab="usedFactor", data=wolfkde)
## Now lets do for Elevation and Distance from Human Access2
#par(mfrow = c(1,2))
boxplot(Elevation2~usedFactor, data=wolfkde, main = "Elevation Used-Avail", xlab="usedFactor", ylab="elev")
boxplot(DistFromHumanAccess2~usedFactor, data=wolfkde, main = "Human Access Used-Avail", ylab="elk_w2", xlab="usedFactor")

# red deer
par(mfrow=c(4,2))
#par(mfrow = c(2,3))
boxplot(deer_w2~usedFactor, data=rdkde, main = "Deer Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(elk_w2~usedFactor, main = "Elk Used-Avail", ylab="elk_w2", xlab="usedFactor", data=rdkde)
boxplot(moose_w2~usedFactor, main = "Moose Used-Avail", ylab="moose_w2", xlab="usedFactor", data=rdkde)
boxplot(goat_w2~usedFactor, main = "Goat Used-Avail", ylab="goat_w2", xlab="usedFactor", data=rdkde)
boxplot(sheep_w2~usedFactor, main = "Sheep Used-Avail", ylab="sheep_w2", xlab="usedFactor", data=rdkde)
## Now lets do for Elevation and Distance from Human Access2
#par(mfrow = c(1,2))
boxplot(Elevation2~usedFactor, data=rdkde, main = "Elevation Used-Avail", xlab="usedFactor", ylab="elev")
boxplot(DistFromHumanAccess2~usedFactor, data=rdkde, main = "Human Access Used-Avail", ylab="elk_w2", xlab="usedFactor")

# bow valley
par(mfrow=c(4,2))
#par(mfrow = c(2,3))
boxplot(deer_w2~usedFactor, data=bvkde, main = "Deer Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(elk_w2~usedFactor, main = "Elk Used-Avail", ylab="elk_w2", xlab="usedFactor", data=bvkde)
boxplot(moose_w2~usedFactor, main = "Moose Used-Avail", ylab="moose_w2", xlab="usedFactor", data=bvkde)
boxplot(goat_w2~usedFactor, main = "Goat Used-Avail", ylab="goat_w2", xlab="usedFactor", data=bvkde)
boxplot(sheep_w2~usedFactor, main = "Sheep Used-Avail", ylab="sheep_w2", xlab="usedFactor", data=bvkde)
## Now lets do for Elevation and Distance from Human Access2
#par(mfrow = c(1,2))
boxplot(Elevation2~usedFactor, data=bvkde, main = "Elevation Used-Avail", xlab="usedFactor", ylab="elev")
boxplot(DistFromHumanAccess2~usedFactor, data=bvkde, main = "Human Access Used-Avail", ylab="elk_w2", xlab="usedFactor")



#### VISUALIZATIONS - from lab ####


# awesome plot of selection coefficients for each species
plot(habvalues, elkpred, type ="l", ylim = c(0,1.0), ylab = "Pr(Used)", col = "green")
lines(habvalues, goatpred, col = "blue")
lines(habvalues, moosepred, col = "red") 
lines(habvalues, sheeppred, col = "black") 
lines(habvalues, deerpred, col = "gray") 
legend(x="topleft", legend= c("Elk","Mountain Goat", "Moose", "Sheep", "Deer"), lty=1, col = c("green", "blue", "red", "black", "gray"), bty = "n")

# quick plots of predicted use ~ each covariate (separate)
hist(wolfkde$fitted.Elev)
plot(wolfkde$Elevation2, wolfkde$fitted.Elev)

# better plots of predicted use - elev only
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
# Now lets explore fitting functions to the distributions
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev)) + geom_density()
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev), fill=usedFactor) + geom_density(alpha=0.5) + xlim(0,1)+xlab("Predicted Probability of Wolf Use") + theme(axis.title.x=element_text(size=16)) 
# kernel lines
ggplot(wolfkde, aes(x=wolfkde$fitted.Elev, y=..density.., fill=usedFactor)) + geom_histogram(binwidth=0.05) + geom_density(alpha = 0.5) + facet_grid(pack ~ .)



# boxplots - KDE - used/avail - all wolves
par(mfrow=c(4,2))

#par(mfrow = c(2,3))
boxplot(deer_w2~usedFactor, data=wolfkde, main = "Deer Used-Avail", xlab="usedFactor", ylab="deer")
boxplot(elk_w2~usedFactor, main = "Elk Used-Avail", ylab="elk_w2", xlab="usedFactor", data=wolfkde)
boxplot(moose_w2~usedFactor, main = "Moose Used-Avail", ylab="moose_w2", xlab="usedFactor", data=wolfkde)
boxplot(goat_w2~usedFactor, main = "Goat Used-Avail", ylab="goat_w2", xlab="usedFactor", data=wolfkde)
boxplot(sheep_w2~usedFactor, main = "Sheep Used-Avail", ylab="sheep_w2", xlab="usedFactor", data=wolfkde)
## Now lets do for Elevation and Distance from Human Access2
#par(mfrow = c(1,2))
boxplot(Elevation2~usedFactor, data=wolfkde, main = "Elevation Used-Avail", xlab="usedFactor", ylab="elev")
boxplot(DistFromHumanAccess2~usedFactor, data=wolfkde, main = "Human Access Used-Avail", ylab="elk_w2", xlab="usedFactor")

## boxplots - kde- used vs avail - Bow Valley 
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

## boxplots - kde- used vs avail - Red Deer
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


# boxplots - used vs avail for each pack on same plot
par(mfrow = c(1,1))
boxplot(Elevation2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer Elevation Used-Avail")
boxplot(DistFromHumanAccess2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer Human Access Used-Avail")
boxplot(deer_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer DEER Used-Avail")
boxplot(moose_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer MOOSE Used-Avail")
boxplot(elk_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer ELK Used-Avail")
boxplot(goat_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer GOAT Used-Avail")
boxplot(sheep_w2~used+pack, data = wolfkde, main = "Bow Valley vs. Red Deer SHEEP Used-Avail")


### SUMMARIES, STATS, ETC ####
library(dplyr)
library(tidyr)
esum <- data.frame(ddply(wolfkde, c("pack", "used"), summarize, 
                   mean=mean(Elevation2, na.rm=TRUE))) %>%
  mutate(Covariate = "Elevation")
hsum <- ddply(wolfkde, c("pack", "used"), summarize, 
              mean=mean(DistFromHumanAccess2, na.rm=TRUE)) %>%
  mutate(Covariate = "Dist Human")
elksum <- ddply(wolfkde, c("pack", "used"), summarize, 
                mean=mean(elk_w2, na.rm=TRUE)) %>%
  mutate(Covariate = "Elk")
dsum <- ddply(wolfkde, c("pack", "used"), summarize, 
              mean=mean(deer_w2, na.rm=TRUE)) %>%
  mutate(Covariate = "Deer")
gsum <- ddply(wolfkde, c("pack", "used"), summarize, 
              mean=mean(goat_w2, na.rm=TRUE)) %>%
  mutate(Covariate = "Goat")
msum <- ddply(wolfkde, c("pack", "used"), summarize, 
              mean=mean(moose_w2, na.rm=TRUE)) %>%
  mutate(Covariate = "Moose")
ssum <- ddply(wolfkde, c("pack", "used"), summarize, 
              mean=mean(sheep_w2, na.rm=TRUE)) %>%
  mutate(Covariate = "Sheep")

slxn <- rbind(esum,  hsum, elksum, dsum, gsum, msum, ssum) %>%
  group_by(Covariate, pack) %>%
  summarise(SI = log(mean[2]/mean[1])) %>%
  ungroup() %>%
  arrange(desc(abs(SI)))
slxnrd <- filter(slxn, pack == "Red Deer") %>%
  arrange(desc(SI))
slxnbv <- filter(slxn, pack == "Bow Valley") %>%
  arrange(desc(SI))

# plotting wolf locns and densities, faceted by pack
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

