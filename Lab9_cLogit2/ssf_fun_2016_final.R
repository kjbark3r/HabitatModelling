#' Movement data analysis functions
#' ==============================
#' author: Björn Reineking
library(circular)

thin_split <- function(object, target_interval, upper_threshold = 1.5 * target_interval, units = "min",
                           minimum_burst_length = 3) {
      # target interval can be specified as an integer, it is 
      # pasted with units to provide a value for the seq-algorithm: e.g. as "60 mins"
      # TODO: currently we throw away more observations than necessary
      time <- timestamps(object)
      interval_id <- cut(time, seq(min(time), max(time), by = paste(target_interval, units)), include.lowest = TRUE, labels = FALSE)
      object <- object[!duplicated(interval_id)]
      dt <- timeLag(object, units = units)
      burstId <- c(0, cumsum(dt > upper_threshold)) # burstId is incremented whenever the time lag to the next observation is larger than upper_threshold
      # Remove bursts that are shorter than the minimum_burst_length
      remove <- which(burstId %in% names(which(table(burstId) < minimum_burst_length)))
      if (length(remove) > 0) {
        object <- object[-remove, ]  
        burstId <- burstId[-remove]
      }
      object$burstId <- factor(burstId)
      object
    }

rdist_2d <- function(n, dist = "exponential", theta, kappa = NULL, start_angle = NULL) {
  # n: number of samples to draw
  # lambda: rate parameter, mean distance is 1/lambda
  r <- switch(dist,
              exponential = rexp(n, theta),
              gamma = rgamma(n, theta[1], theta[2]),
              lognormal = rlnorm(n, theta[1], theta[2]),
              halfnormal = abs(rnorm(n, 0, sqrt(pi/2)/theta)))
  if (is.null(kappa)) {
    theta <- runif(n, 0, 2*pi)  
  } else {
    if(is.null(start_angle)) stop("A start_angle is required when kappa is not NULL")
    theta <- rvonmises(n, circular(0), kappa) + start_angle
  }
  dx <- r * cos(theta)
  dy <- r * sin(theta)
  cbind(dx, dy)  
}



wrap_rel_angle <- function(x) {
  # wraps relative angles to the range from -pi to pi
  ifelse(abs(x) > pi, sign(x) * (abs(x) - 2 * pi), x)
}

relative_control_locations <- function(K, method = c("exponential", "gamma", "lognormal", "halfnormal"), 
                                       start_angle = NULL, theta = NULL, kappa = NULL) {
    dxy <- rdist_2d(K, dist = method, theta, kappa, start_angle)
  dxy
}

degrees2radians <- function(x) {
  -2*pi*(x-90)/360
}

move2spdf <- function(object) {
  # Transform a move object to a SpatialPointsDataFrame
  # date, id, burst, abs.angle
  if (!is.null(object$individual.local.identifier)) {
    id <- factor(as.character(object$individual.local.identifier))
  } else {
    id <- "unknown"
  }
  res <- data.frame(coordinates(object), 
               "date" = timestamps(object),
               "id" = id,
               "burst" = object$burstId,
               "dist" = c(distance(object), NA),
               "abs.angle" = c(degrees2radians(angle(object)), NA)                    
               )
  colnames(res)[1:2] <- c("x", "y")
  coordinates(res) <- ~ x + y
  proj4string(res) <- proj4string(object)
  res  
}

fast_sample_path <- function(path, angle_dist = NULL, K, method = c("exponential", "gamma", "halfnormal", "lognormal", "empirical"), theta = NULL, kappa = NULL) {
  # For each step in a path, create K alternative steps according to method ("exponential" or "empirical")
  xy_path <- coordinates(path)  # xy coordinates 
  # abs_angles <- atan2(diff(xy_path[, "y"]), diff(xy_path[, "x"]))
  abs_angles <- path$abs.angle
  start_angles <- rep(abs_angles[-c(length(abs_angles)-1, length(abs_angles))], each = K)
  path$used <- 1
  path$rel.angle <- wrap_rel_angle(c(NA, diff(abs_angles)))
  path$stratum <- 1:NROW(path)
  
  # The values in the path object for distance and angle are prospective.
  # In the analysis we need retrospective values (how far did we need to move to get to the new position?)
  dxy <- relative_control_locations(K * (NROW(xy_path) - 2), method, 
                                    start_angle = start_angles, theta, kappa)
  start_i <- rep(2:(NROW(xy_path)-1), each = K) # So we ignore the first and last position
  stop_i <- rep(3:(NROW(xy_path)), each = K)
  # the first because we do not know its relative angle, the last because we do not know where the animal moved to further
  xy <- data.frame("x" = dxy[, 1] + xy_path[start_i, 1], "y" = dxy[, 2] + xy_path[start_i, 2])
  xy$date <- path$date[start_i]
  xy$used <- 0
  xy$dist <- sqrt((xy$x - xy_path[start_i, 1])^2 + (xy$y - xy_path[start_i, 2])^2)  
  xy$id <- path$id[start_i]
  xy$burst <- path$burst[start_i]
  abs.angle <- atan2(xy$y - xy_path[start_i, 2], xy$x - xy_path[start_i, 1])
  xy$rel.angle <- wrap_rel_angle(abs.angle - path$abs.angle[start_i - 1])
  xy$stratum <- path$stratum[start_i]
  realized_path <- data.frame(xy_path[-c(1:2), , drop = FALSE])
  #  realized_path <- cbind(realized_path, path[2:(NROW(path)-1), c("date", "used", "id", "stratum", "burst", "dist", "rel.angle")])  
  # tst <- as.data.frame(path)[2:(NROW(path)-1), c("date", "used", "id", "stratum", "burst", "dist", "rel.angle")]
  realized_path <- cbind(realized_path, as.data.frame(path)[2:(NROW(path)-1), c("date", "used", "id", "stratum", "burst", "dist", "rel.angle"), drop = FALSE])  
  cols <- c("x", "y", "date", "used", "id", "stratum", "burst", "dist", "rel.angle")
  res <- rbind(realized_path[, cols], xy[, cols])
  coordinates(res) <- ~ x + y
  proj4string(res) <- proj4string(path)
  res
}

extract_angle_dist <- function(path) {
  dist <- distance(path)
  abs.angle <- angle(path)
  abs.angle <- degrees2radians(abs.angle)
  rel.angle <- wrap_rel_angle(c(NA, diff(abs.angle)))
  burstIds <- path$burstId
  # we remove "steps" across bursts
  remove <- c(1, which(diff(as.numeric(burstIds)) != 0) + 1)
  remove <- c(remove, remove + 1)
  angle_dist <- cbind("dist" = dist[-remove], "rel.angle" = rel.angle[-remove])
  angle_dist
}

prepare_angle_dist <- function(path_object) {
  if (class(path_object) == "MoveStack") {
    path_list <- split(path_object)
  }
  if (class(path_object) == "Move") {
    path_list <- list(path_object)
  }
  angle_dist <- lapply(path_list, extract_angle_dist)
  angle_dist <- do.call("rbind", angle_dist)
  na.omit(angle_dist)    
}

prepare_ssf_steps <- function(path_object, method, K, angle_dist = NULL, theta = NULL, kappa = NULL, disc_radius = NULL, 
                             crs, verbose = FALSE) {  
  # TODO: Rethink whether to do everything in lat-lon to keep projection as in move objects
  orig_proj4string <- proj4string(path_object)
  path_object <- spTransform(path_object, crs)    
  if (class(path_object) == "MoveStack") { # convert move stact to list 
    path_list <- split(path_object)
  }
  if (class(path_object) == "Move") {
    path_list <- list(path_object)
  }
  
  ssf_path_list <- list(length = length(path_list))
  for(animal in seq(path_list)) {      # for each individual 
    animal_path <- path_list[[animal]]
    if(is.null(animal_path$burstId)) {
      animal_path$burstId <- factor(0)
    }
    burstIds <- animal_path$burstId
    burst_path_list <- lapply(unique(as.character(burstIds)), function(burst) {
      if (verbose) cat("Processing animal", animal, "burst", burst, "\n")
      path <- animal_path[animal_path$burstId == burst, ]
      path <- move2spdf(path)
      path <- spTransform(path, crs)    
      ssf_path <- fast_sample_path(path, angle_dist, K = K, method = method, 
                                   theta = theta, kappa = kappa)
      ssf_path      
    })
    ssf_path_list[[animal]] <- do.call("rbind", burst_path_list)
  }
  ssf_data <- do.call("rbind", ssf_path_list)  
  ssf_data$stratum <- paste(ssf_data$id, ssf_data$burst, ssf_data$stratum, sep="_")
  ssf_data <- spTransform(ssf_data, CRS(orig_proj4string))
  ssf_data
}

extract_strata <- function(object) {
  # This code is largely taken from predict.coxph.R
  Terms <- object$terms
  has.strata <- !is.null(attr(Terms, 'specials')$strata)
  mf <- model.frame(object)  
  if (has.strata) {
    if (!is.null(object$strata)) oldstrat <- object$strata
    else {
      stemp <- untangle.specials(Terms, 'strata')
      if (length(stemp$vars)==1) oldstrat <- mf[[stemp$vars]]
      else oldstrat <- strata(mf[,stemp$vars], shortlabel=TRUE)
    }
  }
  gsub("^stratum=", "", unique(oldstrat))
}

simulate_ssf <- function(object, map, nsteps = 100, K, method,
                         start_xy = NULL, start_angle = NULL, theta = NULL, kappa = NULL,
                         stratum, 
                         crs, 
                         verbose = FALSE) {
  # TODO: add date information (i.e. we need starting date and the timestep length)
  # This information is necessary for e.g. models with time-varying coefficients
  xy_path <- matrix(NA, nrow = nsteps + 1, ncol = 2)
  # if(is.null(lambda)) lambda <- 1 / (2 * mean(angle_dist[,"dist"])) # Forester et al. 2009 Ecology
  xy_path[1, ] <- start_xy
  for (i in 1:nsteps) {
    if(verbose) {
      cat(i, "\n")
    }
    # Create proposal steps
    dxy <- relative_control_locations(K, method, 
                                      start_angle, theta, kappa)
    xy <- data.frame("x" = dxy[, 1] + xy_path[i, 1],
                     "y" = dxy[, 2] + xy_path[i, 2])    
    # Add step information on distance and relative angle
    xy$dist <- sqrt((xy$x - xy_path[i, 1])^2 + (xy$y - xy_path[i, 2])^2)
    abs.angle <- atan2(xy$y - xy_path[i, 2], xy$x - xy_path[i, 1])
    xy$rel.angle <- wrap_rel_angle(abs.angle - start_angle)
    # Add information on which stratum is used (should not affect results,
    # but is required by the predict function)
    xy$stratum <- stratum  
    xy$abs.rel.angle <- abs(xy$rel.angle)
    coordinates(xy) <- ~ x + y
    proj4string(xy) <- crs@projargs  
    
    # Add environmental information 
    env <- extract(map, spTransform(xy, map@crs))
    xy@data <- cbind(xy@data, env)
    
    # Use model object to calculate the selection weights for each
    # alternative
    weights <- predict(object, newdata = xy, type = "risk")
    # Set the weights of locations with NA to 0
    weights[is.na(weights)] <- 0  
    # Randomly select one of the alternatives, using the weights for each alternative
    selected <- sample.int(nrow(xy), 1, prob = weights)
    xy_path[i + 1, ] <- coordinates(xy)[selected, ]
    start_angle <- abs.angle[selected]
  }
  res <- as.data.frame(xy_path)
  colnames(res) <- c("x", "y")
  coordinates(res) <- ~ x + y
  proj4string(res) <- crs@projargs  
  res 
}
