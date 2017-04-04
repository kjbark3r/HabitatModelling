#'---
#'title: "Step selection functions"
#'author: Björn Reineking 
#'email: bjoern.reineking@irstea.fr
#'date: 19 September 2016
#'---
#' 

#' Step selection functions: Literature
#' ========================================================
#' - Forester et al. (2009) Ecology 90: 3554-3565
#' - Potts et al. (2014) Ecology and Evolution 4: 4578-4588
#' - Potts et al. (2014) J. R. Soc. Interface 11: 20140333
#' - Avgar et al. (2016) Methods in Ecology and Evolution 7: 619-630
#' 

#' Step selection functions
#' ========================================================
#' 
#' For each step (i.e. pair of successive locations),
#' the environmental conditions at the chosen location are compared to those at other available locations.
#' 
#' For one environmental predictor x (e.g. NDVI), 
#' the probability $p_i$ of observing the chosen location $i$ at a distance $d_i$ from the current location is given by:
#' $$ p_i = \frac{exp(\alpha d_i + \beta x_i)}{\sum_{j=1}^{N} exp(\alpha d_j + \beta x_j)} $$
#'  
#' where 
#' - $\alpha$ is the regression coefficient quantifying the effect of distance, 
#' - $\beta$ is the regression coefficient quantifying the environmental selection, 
#' - $N$ is the total number of alternative locations considered for the step.
#' 
#' The exponential function ensures that the individual weights for each alternative location are positive, 
#' and the sum in the denominator ensures that the individual weights sum to one.
#' 
#' The values for $\alpha$ and $\beta$ are chosen such that the probability (likelihood) of the observed loations is maximized.
#' 

#' Step selection functions: A receipe
#' ==================================
#' - Problem formulation
#' - Data collection
#' - Data pre-processing
#' - Modelling
#' - Interpretation
#' - Iterate process
#'
#' We will use the buffalo data set
#' 

#' Loading packages
#' ===============================================
#'
library(raster)
library(rgdal)
library(ggmap)
library(survival)
library(TwoStepCLogit)
library(pbs)
library(dplyr)
library(lubridate)
library(move)
library(MASS)
library(fdrtool)
library(circular)
library(CircStats)


#'Setting up the working directory
#'=================================
#' Add in your working directory that contains the function file below 

#' and sourcing some functions (a poor man's package):
source("ssf_fun_2016_final.R")

#'Load buffalo data
#'==========================================
#'See Kami's slides for how we got here...
load("buffalo_cleaned.Rdata")

#'Always inspect your data: summary statistics
#'============================================
summary(buffalo)

#'Always inspect your data: plots
#'===============================
plot(buffalo)

#'Environmental data: topography, waterways, and NDVI
#'========================================
#'

env <- brick("env_buffalo.grd")   # creating a multi layer raster object from a multi layer band
plot(env)


#' Thin movement data and split to bursts
#' ===================================
step_duration <- 180   # three hours (180 minutes)
#' - We reduce the data set to observations that are within a certain time step range. The SSF assumes Brownian motion,
#' so we should thin sufficiently. See presentation by Chris tomorrow. Here: we go for roughly `r step_duration/60` hours. 
#' - When two observations are separated by less than the threshold, the second observation is removed
#' - When two observations are separated by more than the upper threshold, the observations are assigned to different bursts
#' if you can get away with an exponential step length distribution, USE THAT
#'

#' Thin movement data and split to bursts
#' ===================================


thinned_buf <- moveStack(lapply(split(buffalo), 
  thin_split, step_duration, unit = "mins"))

tbuf_df <- as(thinned_buf, "data.frame")
tbuf_df

#' - First, we *split(buffalo_data)*, i.e. we create a list of move objects, with one list element for each animal
#' - Second, we use *lapply* to loop through the list, and apply the function *thin_split_new* to each animal, 
#'   with the two additional arguments target_interval = `r step_duration`, and units = "mins"
#' - splits the data where the time gap is too long; in this case, if the gap is longer than the 3 hour period 
#' - Third, we re-create a moveStack from the thinned data of each animal 
#'
 
#' Remove observations without movement (optional)
#' ====================================
#' We remove observations where the animal did not move - this is only necessary if we want
#' to use step length distributions such as lognormal that do not allow for zero displacement.
#' Here, we have only one observation that has a distance of zero, so removing it is not likely to
#' bias the results
#' - be cautious doing this; should be done rarely; only if animal moves a LOT between steps
#' 
which(unlist(distance(thinned_buf)) == 0)
if (any(unlist(distance(thinned_buf)) == 0)) {
  thinned_buf <- thinned_buf[-which(unlist(distance(thinned_buf)) == 0),  ]
  thinned_buf <- moveStack(lapply(split(thinned_buf), 
                                   thin_split, step_duration, unit = "mins"))
}

#' Create control locations for each case
#' ==========================
#' - Prepare an object "angle_dist", which contains for each step in the thinned data
#'     * the relative angle (in radians) of the step and 
#'     * the distance of the step
#' - This information is used to estimate parameters for the distribution of proposed steps
#'
 
#' Create control locations for each case cont.
#' ==========================
angle_dist_buf <- prepare_angle_dist(thinned_buf)
# Inspect the data
summary(angle_dist_buf)


#' Empirical distances and turning angles
#' ==========================
#' 
par(mfrow = c(1, 2))
hist(angle_dist_buf[,"dist"], breaks = 20, main = "", 
  xlab = "Distance (m)")
hist(angle_dist_buf[, "rel.angle"], breaks = seq(-pi, pi,
  len=11), main="", xlab="Relative angle (radians)")  

# most of the time, these animals moved less than 5 km
# these animals generally continues in a straightish line
# this histogram would be flat if it was a random walk 


#' Fit distributions to distances
#' =============================
#' 
fexp <- fitdistr(angle_dist_buf[, "dist"], "exponential")
fgam <- fitdistr(angle_dist_buf[, "dist"], "gamma")
flogn <- fitdistr(angle_dist_buf[, "dist"], "lognormal")

fexp <- fitdistr(angle_dist_buf[, "dist"], "exponential")
fgam <- fitdistr(angle_dist_buf[, "dist"], "gamma")
flogn <- fitdistr(angle_dist_buf[, "dist"], "lognormal")

par(mfrow = c(1,1))
hist(angle_dist_buf[,"dist"], breaks = 50, prob = TRUE, xlim = c(0, 4000), ylim = c(0, 2e-3),
     xlab = "Step length (m)", main = "")
plot(function(x) dexp(x, rate = fexp$estimate), add = TRUE, from = 0, to = 5000)
plot(function(x) dgamma(x, shape = fgam$estimate["shape"], rate = fgam$estimate["rate"]), add = TRUE, 
     from = 0, to = 5000, col = "red")
plot(function(x) dlnorm(x, meanlog = flogn$estimate["meanlog"], 
                        sdlog = flogn$estimate["sdlog"]), add = TRUE, 
     from = 0, to = 5000, col = "blue")
plot(function(x) dhalfnorm(x, theta = 1/mean(angle_dist_buf[, "dist"])), add = TRUE, 
     from = 0, to = 5000, col = "green")
legend("topright", lty = 1, col = c("black", "red", "blue", "green"),
       legend = c("exp", "gamma", "lnorm", "halfnorm"))

# if you were to do model selection here to find out which is best fit, 
# you would need to include the environmental data
# because they should be influencing the step selection
# Exponential is best, as it's the simplest and looks to fit the best


#' Fit distribution to turning angles
#' =============================
#' 
fkappa <- est.kappa(angle_dist_buf[,"rel.angle"])
fkappa
hist(angle_dist_buf[, "rel.angle"], prob = TRUE, main = "", xlab = "Turning angle")
plot(function(x) dvonmises(x, circular(0), kappa=fkappa), add = TRUE, 
     from = -pi, to = pi, col = "red")

# von mises distribution is a normal distribution about 0

#' Create control locations for each case
#' ==========================
#' Currently, the code for generating alternative steps; much easier to calculate this stuff with flat projections
#' assumes a suitable "flat" projection, so we pick the appropriate UTM projection for the study area

utm_crs_buf <- CRS("+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

#' Create control locations for each case cont.
#' ==========================
#' Control steps: sample with one of the following distance distributions: exponential, gamma, lognormal, halfnormal,
#' potentially drawing turning angles from a von Mises distribution.
#' We will use an exponential distribution here because it is the simplest and it seems to fit the data well.
#' We will use a radially symmetric proposal distribution because it is easier to interpret parameter estimates for cos(rel.angle), and
#' because in an earlier analysis on these data it showed less residual autocorrelation.
#' 

my_K <- 100  # tells it how many alternative locations to sample

set.seed(3)    # setting the seed here ensure that the next time you do the analysis, you'll get the same results
ssf_data_buf <- prepare_ssf_steps(thinned_buf,    # create alternative steps from the "thinned_data" object
                                  method = "exponential", K = my_K,       # again, exponential is best
                                  theta = fexp$estimate / 2,              # theta is rate parameter; inverse of mean distance
                                  crs = utm_crs_buf)

#' - We set the random number generator seed to get reproducible results
#' - K = `r my_K` control points is a rather low number. More should give less variable results, 
#'   but at the cost of additional computing effort
#' - We use a wider proposal distribution than the observed steps (Forester et al. 2009)
#' 

#' Sanity check: plot the choice set for a given step
#' make sure alternative positions are really around the real positions
#' ==========
#' 
#' 

head(ssf_data_exp_buf, n=500L)
ssf_data_buf

dat_buf <- subset(ssf_data_buf, stratum == "Cilla_0_9")
dat0_buf <- subset(ssf_data_buf, stratum == "Cilla_0_7" & used == 1)
plot(rbind(dat_buf, dat0_buf), axes = TRUE)
points(subset(dat_buf, used == 1), col ="red")
points(subset(ssf_data_buf, stratum == "Cilla_0_8" &
                used == 1), col ="blue")
points(subset(ssf_data_buf, stratum == "Cilla_0_7" &
                used == 1), col ="green")
dat2_buf <- as.data.frame(subset(ssf_data_buf, stratum %in% c("Cilla_0_7", "Cilla_0_8") &
                               used == 1))
lines(dat2_buf$x, dat2_buf$y, col ="green")
legend("topright", col = c("green", "blue", "red"), pch = 1, 
       legend = c("t-1", "t", "t+1"))

#' Extract the raster information at step locations
#' ==================================
envmat <- extract(env, spTransform(ssf_data_buf, env@crs))
## matrix with the environmental information

#' Add the information to the existing data set; basically appending the env-DATA
#' ==================================
#' Caution - do the cbind on the @data part only
ssf_data_buf@data <- cbind(ssf_data_buf@data, envmat)

head(ssf_data_buf)   # burst means it's in the first movement burst
                     # separate by 3 hours (ish)


#' Add variable hour
#' =================
#' Allows to model variation in step lengths, turning angles,
#' and preference for environmental conditions as a function of time of day
#' if we want to make selection of NDVI dependent on time of day
 
ssf_data_buf$hour <- hour(ssf_data_buf$date) + 
  minute(ssf_data_buf$date) / 60                         # first need to transform data to hour of day
par(mfrow = c(1, 2))
boxplot(dist ~ floor(hour), data=subset(ssf_data_buf, 
  used == 1), xlab = "Time of day", ylab = 
    "Distance (m)")
boxplot(cos(rel.angle) ~ floor(hour), data = 
  subset(ssf_data_buf, used == 1), 
  xlab = "Time of day", ylab = "cos(turning angle)")
par(mfrow = c(1, 1))

## shows crepuscular movement

#' Selecting Cilla (only one animal) to speed up things
#' =================================
ssf_cilla <- subset(ssf_data_buf, id == "Cilla")

# Remove NA's
ssf_cilla <- ssf_cilla[complete.cases(ssf_cilla@data),]

#' Collinearity
#' ============= 
#' 
round(cor(ssf_cilla@data[, c("slope", "elev", 
  "water_dist", "mean_NDVI", "var_NDVI")]), 2)

#'elev and water_dist are positively correlated > 0.7
#' check how our explanatory variables (covariates) are correlated with each other
#' positive correlation between elevation and distance to water --- this makes sense!
#' if you have strong correlation between two variables, makes it difficult to detect effect of each of these variables on the dependant variable
#' so, should just pick one of these variables

#' Which collinear variable to pick? 
#' ===================
#' - The one that is more relevant
#' - The one that is by itself a better predictor
#' 

#' Collinearity
#' ============= 
m1_exp_water <- clogit(used ~ cos(rel.angle) + dist + water_dist +   # use cosine of rel. angle because it makes straight ahead = 1, as cos 0 = 1
   strata(stratum), data = ssf_cilla)
m1_exp_elev <- clogit(used ~ cos(rel.angle) + dist + elev + 
  strata(stratum), data = ssf_cilla)
AIC(m1_exp_water)
AIC(m1_exp_elev)

# clogit regression used because a GROUP of alternative positions has a 
# sum of probility equal to one, rather than individual alternative locations

#' So we pick elev, because it by itself explains the movement better
#' 

#' Calculate step selection function
#' ==========================
#'
m_1 <- clogit(used ~ cos(rel.angle) + dist + slope + elev + mean_NDVI +
  var_NDVI + strata(stratum), 
  data = ssf_cilla)
summary(m_1)

#' slope and var_NDVI do not contribute significantly to the fit
#' the negative distance implies that animals are picking shorter step lengths than what we proposed (which were long ones)
#' seem to select areas with higher NDVI
#' if NDVI_var was significant and negative, it would mean the animal would avoid areas with highly variable NDVI    

#' Model selection
#' ==================
#' Model selection is a vast topic. Here just use stepwise backward selection
#' based on AIC
m_2 <- update(m_1, .~. - slope)
AIC(m_1)
AIC(m_2)
summary(m_2)

#' Model selection
#' ==================
m_3 <- update(m_2, .~. - var_NDVI)
AIC(m_3)
summary(m_3)


#' Model selection
#' ==================
m_4 <- update(m_3, .~. - dist)
AIC(m_4)
# So we stick with m_3


#' Model checking: serial autocorrelation
#' ======================
#' Forester et al. 2009 Ecology 90:3554-3565.
#' 
#' Calculate the deviance residuals for each stratum (i.e., the sum of the residuals 
#' for the case and all associated controls).
ssf_cilla$m_3_residuals <- residuals(m_3, type = 
  "deviance")
resid_df <- group_by(ssf_cilla@data, date)
resid_df <- summarise(resid_df, residuals = 
  sum(m_3_residuals))
resid_df$group <- 1
#' Fit an intercept-only mixed-effects model using lme() from the nlme package.
library(nlme)
rm1 <- lme(residuals ~ 1, random = ~ 1 | group, 
           data = resid_df)

summary(rm1)

#' Model checking: serial autocorrelation cont.
#' ======================
plot(ACF(rm1), alpha = 0.05)


#' So there is some residual temporal autocorrelation; especially at time lag 1; we violate basic assumptions here
#' to get rid of this, must increase step length
#' One effect of residual temporal autocorrelation is too extreme p-values, but it may also cause bias in parameter estimates.
#' 


#' Model evaluation
#' ================
#' - R2 is low. Always is. Why?  If it was 1, we would have to predict exactly where the animal moves
#' - Not yet clear what a good performance index would be.
#' - Consider this an exercise. 
#' Here: we contrast for used and alternative steps the 
#' "Proportion of values less than or equal to the current value"
#' 
#' - Cross-validation
#'     - split steps in e.g. 5 folds (long stretches better)
#'     - leave each fold out, refit and predict to left-out fold
#'

#' Model evaluation
#' ================
#' "Proportion of values less than or equal to the current value"
#' AUC - area under the receiver operating characteristic curve



ssf_cilla$pred_m3 <- predict(m_3)
by_stratum <- group_by(ssf_cilla@data, stratum)
by_stratum <- mutate(by_stratum, pred_rank = 
                       cume_dist(pred_m3))

#' Model evaluation
#' ================

hist(subset(by_stratum, used == 1)$pred_rank, xlab = "AUC", main = "", prob = TRUE)
#' this shows that most of the time, the animal actually goes to a better site; i.e., few crappy steps
#' Mean AUC across steps
mean(subset(by_stratum, used == 1)$pred_rank)   # our mean AUC is 0.75
# to really evaluate this, we should split our data into training and test sets

#' Interpretation
#' ===============
#' - Map preference
#' - Variable importance (see SDM this afternoon)
#' - Response functions (only interesting for time varying parameters)
#' 

#' Map habitat preference
#' ======================
#' The raster prediction function assumes that all environmental layers are
#' respresented in one raster stack

# required for prediction by clogit model, but does 
# not affect result
stratum <- extract_strata(m_3)[1] 

pred_df_map <- data.frame(getValues(env))
pred_df_map$stratum <- stratum
pred_df_map$dist <- 0
pred_df_map$rel.angle <- 0

#' Map habitat preference
#' ======================
m3_pred <- predict(m_3, newdata = pred_df_map)
m3_pred_raster <- raster(env, 1) # Create raster layer
m3_pred_raster[] <- m3_pred
plot(m3_pred_raster)      # green areas should be attractive areas; white should be avoided

#' Overlay cilla
#' =============
cilla <- subset(buffalo,individual.local.identifier == 
          "Cilla")
plot(m3_pred_raster)
points(spTransform(cilla, utm_crs))

#' Same, but zoom in
#' =================
plot(crop(m3_pred_raster, spTransform(cilla, utm_crs)))
lines(spTransform(cilla, utm_crs))   # you'll notice that it doesn't cross the river, even though there is probably good habitat south of it
                                     # might want to include a variable that takes the river into account; binary variable

#' Iterate
#' =======
#' Here: a model with time-varying preference for mean_NDVI
#' 
boxplot(mean_NDVI ~ floor(hour), data = 
  subset(ssf_cilla, used == 1),xlab = "Time of day", 
  ylab = "mean NDVI")

m_time <- clogit(used ~ cos(rel.angle) + dist +  elev + mean_NDVI + 
                   mean_NDVI:pbs(hour, df = 5, Boundary.knots = c(0,   # what is preference for NDVI is dependent on time of day?
                                                                       # pbs is periodic smooth function
                                                                       # 0 to 24 hours
                                                                       # 5 degrees of freedom means it's pretty flexible
                  24)) + strata(stratum), 
                 data = ssf_cilla)
summary(m_time)

#' Predictions with the model: response function
#' ==========================
pred_data <- data.frame("stratum" = stratum, dist = 0,
  elev = 0, mean_NDVI = 1, rel.angle = 0, hour = 
  seq(0, 24, len = 101))
pred_time <- predict(m_time, newdata = pred_data, 
                     se.fit = TRUE)
upper <- pred_time$fit + 1.96 * pred_time$se.fit
lower <- pred_time$fit - 1.96 * pred_time$se.fit

#' Predictions with the model: response function; this is still the preference for NDVI vs. time of day
#' ==========================
par(mfrow = c(1, 1))
plot(pred_data$hour, pred_time$fit, type = "l",               # seems to prefer green areas very early in the day, but avoid around 5 pm
                                                              # very wide confidence intervals though
                                                              # maybe this is because it's going to the water in the morning, which 
                                                              # is in the river valley and is greener (higher NDVI)
  ylim = range(c(upper, lower)), xlab = "Time of day",
  ylab = "Preference mean_NDVI")
lines(pred_data$hour, upper, lty = 2)
lines(pred_data$hour, lower, lty = 2)
abline(h = 0, lty = 3)

#' Simulating with the model; using the the model you created, you can create an artificial "Cilla"
#' =========================
#' We start at the first recorded position of the individual "Cilla"
coordinates(spTransform(subset(ssf_data_exp, id == 
  "Cilla" & used == 1), utm_crs))[1, ]                 
start_xy <- c("x" = 384885.8, "y" = 7240399.0)
#' We assume that the first step was a move eastwards 
#' need to vary the initial trajectory as you do simulations
start_angle <- 0


#' Simulating with the model
#' =========================
#' We arbitrarily pick the first stratum in the model - which one we pick does not affect the 
#' results, but we need to supply a stratum for the predict.coxph function

stratum <- extract_strata(m_3)[1]       # doesn't matter which stratum you pick, but you must pick one to start
set.seed(2)
sim_1 <- simulate_ssf(m_3, map = env, nsteps = 50,                 #need to provide model, env conditions, 
  K = 200, method = "exponential", theta = fexp$estimate / 2,
  start_xy = start_xy, start_angle = start_angle, 
  stratum = stratum, crs = utm_crs)
# this model will tell us where the animal would show up if we made certain changes to the landscape
# need to use the same settings that you used to create your original model (i.e. "exponential, theta, crs)
# we increased K because here it's even more important that we give animal lots of choices; need to sample full environmental conditions;  
# earlier, we at least had the point where it went, but now we don't have anything about trajectory, since we have no known points


#' Plotting the simulated steps
#' =====================================================
sim_1 <- spTransform(sim_1, env@crs)
plot(crop(raster(env, "elev"), extent(sim_1) + 5000))
lines(coordinates(sim_1), col ="red")                 # why the fuck did it go right over the mountain?
                                                      # using elevation by itself is dangerous
                                                      # the points on each side of the mountain where the animal went were both low elevations
                                                      # however, it still went over an unrealistic elevation
                                                      # need to take the amount of work required to get from low elevation point
                                                      # to next low elevation point (energy required, not just elevation difference)
                                                      # somehow must come up with covariate that captures this energy required
                                                      # this shows the inherent weakness of step selection function
                                                      # we thinned the data and said that animal could make the 3 hour leaps between locations
                                                      # however, animals are making decisions one different scales at the same time
                                                      # so later on, when we simulate the data using the decision we made earlier to thin
                                                      # the data, we see that the simulated step is totally unrealistic, in that it travels over
                                                      # the mountain, when it probably wouldn't do so in real life;
                                                      # instead, it would make its decision at a smaller scale, at least at this step

#' Comparing with the real first 50 steps of Cilla
#' =====================================================
cilla_steps <- subset(ssf_data_exp, id == "Cilla" & 
                        used == 1)
cilla_steps <- spTransform(cilla_steps, utm_crs)
elev <- raster(env, "elev")
elev_crop <- crop(elev, extent(cilla_steps[1:50, ]) +
                    15000)

#' Comparing with the real first 50 steps of Cilla
#' =====================================================
plot(elev_crop)
lines(coordinates(cilla_steps)[1:50, ], col = "red")
lines(coordinates(sim_1))

#' Test method with simulated data
#' ====================================
#' Question: Is the method able to recover the "true" parameter values
#' from data?
#'  
#' Approach: 
#' 
#' - Simulate data with a known model
#' - Fit method to simulated data
#' - Compare fitted parameters to those of the known model
#'

#' Prepare parameters and environmental data
#' ==========================================
#' We pick the model m_3 to represent Truth
generating_model <- m_3
#' Setting of parameter values
#' 
start_xy <- c("x" = 384885.8, "y" = 7240399.0)
start_angle <- 0
stratum <- extract_strata(m_3)[1]

#' Simulate data with a known model
#' =====================================================
set.seed(4)
if(file.exists(paste0("simulated_path_", step_duration, "_", my_K, ".rds"))) {
  simulated_path <- readRDS(paste0("simulated_path_", step_duration, "_", my_K, ".rds"))
} else {
  simulated_path <- simulate_ssf(generating_model, 
  map = env, nsteps = 500, K = 1000, method = 
  "exponential", start_xy = start_xy, start_angle = 
  start_angle, stratum = stratum, theta = fexp$estimate / 2,
  crs = utm_crs, verbose = FALSE)
  saveRDS(simulated_path, file = paste0("simulated_path_", step_duration, "_", my_K, ".rds"))
}

#' Make a move object from the simulated data
#' ===========================================
start_date <- min(timestamps(buffalo))
simulated_data <- move(
  x = coordinates(simulated_path)[, 1],                       
  y = coordinates(simulated_path)[, 2],
  time = seq(start_date, len = NROW(simulated_path), 
    by = paste(step_duration, "mins")),
 proj = simulated_path@proj4string)

#' Plot simulated data
#' =====================================================
simulated_data_utm <- spTransform(simulated_data, 
                                  env@crs)
elev_crop <- crop(elev, extent(simulated_data_utm) + 
                    5000)

#' Plot simulated data
#' =====================================================
plot(elev_crop)
lines(coordinates(simulated_data_utm))

#' Analyse simulated data: thinning and splitting
#' =====================================================
thinned_data_sim <- thin_split(simulated_data, step_duration, 
                               units = "mins")
angle_dist_sim <- prepare_angle_dist(thinned_data_sim)

hist(angle_dist_sim[, "dist"], prob = TRUE)

fexp_sim <- fitdistr(angle_dist_sim[, "dist"], "exponential")
fkappa_sim <- est.kappa(angle_dist_sim[, "rel.angle"])

#' Analyse simulated data: prepare SSF steps and add environmental information
#' =====================================================
set.seed(2)
ssf_test_data <- prepare_ssf_steps(thinned_data_sim, 
  method = "exponential", K = my_K, theta = fexp_sim$estimate / 2,
  crs = utm_crs)
envmat <- extract(env, ssf_test_data)
ssf_test_data@data <- cbind(ssf_test_data@data, 
                            envmat)

#' Analyse simulated data: fit the model
#' =====================================================
m_test_exp <- clogit(used ~ cos(rel.angle) + dist + elev + 
                       mean_NDVI + strata(stratum), 
                     data = ssf_test_data)
coef(generating_model)
coef(m_test_exp)       # we see that the parameters are quite similar between the two models, which is good

summary(m_test_exp)
#' For the movement-related parameters, we need to take the parameters of
#' the proposal steps into account
#' 

#' True rate
true_rate <- fexp$estimate / 2 - coef(generating_model)["dist"]
#' Estimated rate
est_rate <- fexp_sim$estimate / 2 - coef(m_test_exp)["dist"]

#' True mean distance
1/true_rate
#' Estimated mean distance
1/est_rate


#' From preference to utilisation maps
#' ==================================
#' There is a fast method if we have a symmetric jump kernel, e.g. exponential, and no effect of step angles.
#' Barnett, A. & Moorcroft, P. (2008) Analytic steady-state space use patterns and rapid computations in mechanistic home range analysis. Journal of Mathematical Biology, 57, 139–159.
#' 
