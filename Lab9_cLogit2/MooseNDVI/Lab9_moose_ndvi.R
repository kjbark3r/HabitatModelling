---
  title: 'WILD 562 Lab 9 - Moose migration'
author: "Elie Gurarie, with modifications by Mark Hebblewhite"
date: "February 15, 2017"
output: html_document
---
  
  #### WILD 562 - Wildlife Habitat Modeling Lab 9 - Dynamic Habitat Variables and NDVI
  
  #### Today we will use the MODISTools package and elevatr package to annotate tracks from 4 moose in Alaska. 
  #### To find out more about the MODISTools package, read
  
  ### Tuck, S.L., Phillips, H.R.P., Hintzen, R.E., Scharlemann, J.P.W., Purvis, A. and Hudson, L.N. (2014) MODISTools -- downloading and processing MODIS remotely sensed data in R. Ecology and Evolution, 4 (24), 4658--4668. DOI: 10.1002/ece3.1273.
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, cache=TRUE)
```

```{r, eval=FALSE}
rm(list=ls())
#require("rmarkdown")
#require("knitr")
```

0.1 Load Package 
```{r, message = FALSE, warning = FALSE}
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("plyr", "ggplot2", "ggmap", "ggthemes", "magrittr", "lubridate", "scales", "elevatr", "devtools")

ipak(packages)
```

#### 0.2 Preliminaries: setting working directory
```{r}
## define working directory on each computer
wd_laptop <- "/Users/mark.hebblewhite/Dropbox/WILD 562/Spring2017/lab9/moose/"
wd_desktop <- "/Users/mark.hebblewhite/Documents/Teaching/UofMcourses/WILD562/Spring2017/Labs/lab9/moose" 

## automatically set working directory depending which computer you're on
ifelse(file.exists(wd_laptop), setwd(wd_laptop), setwd(wd_desktop)) 
getwd()
list.files() ## handy command to see what is inside the working directory

```

### 1.0 Loading and Plotting Moose Data (with elevation)
```{r}
load(file = "/Users/mark.hebblewhite/Dropbox/WILD 562/Spring2017/Lab9/moose/moose_daily.robj")

str(moose.daily)
head(moose.daily)
levels(moose.daily$id)
table(moose.daily$id)
```
#### Note - to get the elevations I used the new `elevatr` package - doing something like this to the raw moose data:

```{r}
require(elevatr)
proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
moose.daily <- mutate(moose.daily, elevation = get_elev_point(data.frame(x = lon, y = lat), prj = proj, src = "mapzen", api_key = "mapzen-91tYY4s")@data[,1])
```
#### This took some time (to access and download the elevation models.


#### 1.1 A useful function to visualize the migrations of five moose:
```{r}
scan.track.z <- function(time, x, y, z, title = "",...)
{
  par(mar = c(0,3,0,0), oma = c(4,4,2,2))
  layout(rbind(c(1,2), c(1,3), c(1,4))) 
  
  MakeLetter <- function(a, where="topleft", cex=2)
    legend(where, pt.cex=0, bty="n", title=a, cex=cex, legend=NA)
  
  plot(x,y,asp=1, type="o", pch=19, col=rgb(z/max(z),(1-z/(max(z))),0,.5), cex=0.5, ...); MakeLetter(title)
  plot(time,x, type="o", pch=19, col=rgb(0,0,0,.5), xaxt="n", xlab="", cex=0.5, ...); MakeLetter("X")
  plot(time,y, type="o", pch=19, col=rgb(0,0,0,.5), xaxt="n", xlab="", cex=0.5, ...); MakeLetter("Y")
  plot(time,z, type="o", pch=19, col=rgb(0,0,0,.5), cex=0.5, ...); MakeLetter("Z")
}
```

#### Plot 4 moose in X, Y, Z space now that we have elevation
```{r scan.track.z, fig.height = 3}
par(las= 0)
for(me in levels(moose.daily$id))
  with(subset(moose.daily, id == me), scan.track.z(time, x/1e3, y/1e3, elevation, me))
```

#### 2.0 Playing with MODIS

#### An important warning ... the CRAN MODISTools does NOT work because of outdated / unsupported dependencies (see this thread: https://groups.google.com/forum/#!topic/predicts_project/FctDowcYgx0).  You have to install if from GIT-hub: 

```{r, eval = FALSE}
library(devtools)
install_git("http://github.com/seantuck12/MODISTools")
```
#### It might ask you to install a bunch of dependent packages (like `XML` - it did for me). Then load the package.

```{r}
require(MODISTools)
```

#### Class disussion of MODIS products. For now we'll just get the NDVI value for "MOD13Q1" at just two locations - one in the summer range and one at the winter range. 

```{r}
summer <- c(-152, 66.65) 
## This is defining a single 'point' for the centroid (approximately) of the home range
winter <- c(-150.75, 67.3)
## This is doing likewise for the center of the 'winter' range
points <- rbind(summer, winter)

points
## Note that here in this excercise we have just done this for NDVI for 2 points, the centroids of the summer and winter range, but this general approach would work for any number of points, it just takes a long time. 

par(mfrow=c(1,1))
with(subset(moose.daily, id == "C52"), plot(lon, lat))
points(points, col=2, pch = 4, cex=3, lwd=2)
```

The available bands are:
  
  ```{r}
product <- "MOD13Q1"
GetBands(product)
```

Let's go with NDVI

```{r}
band <- c("250m_16_days_NDVI")
pixel <- c(0,0) 
```

You have to specify the x/y/time range of the desired data:

```{r}
period <- data.frame(lat=points[,2], long=points[,1], start.date=2008, end.date=2012, id=1)
```

#### The key function (and this takes a while) is `MODISSubsets`.  This will download a file and save it in the specified `SaveDir`.  This takes a while ... multiple minutes on my computer. 

```{r, eval = FALSE}
saveDir = "/Users/mark.hebblewhite/Dropbox/WILD 562/Spring2017/Lab9/"

MODISSubsets(LoadDat = period, FileSep = "", Products = product, Bands = band, Size = pixel, SaveDir = saveDir, StartDate = TRUE)
```
#### Note this is a very flaky downloading process, and I got these kinds of error messages many times before it worked 

> MODISSubsets(LoadDat = period, FileSep = "", Products = product, Bands = band, Size = pixel, SaveDir = saveDir, StartDate = TRUE)
Files downloaded will be written to /Users/mark.hebblewhite/Dropbox/WILD 562/Spring2017/Lab9/.
Found 2 unique time-series to download.
Error in GetProducts() : System overloaded. Please again later

> MODISSubsets(LoadDat = period, FileSep = "", Products = product, Bands = band, Size = pixel, SaveDir = saveDir, StartDate = TRUE)
Files downloaded will be written to /Users/mark.hebblewhite/Dropbox/WILD 562/Spring2017/Lab9/.
Found 2 unique time-series to download.
Error in function (type, msg, asError = TRUE)  : 
Failed to connect to daacmodis.ornl.gov port 443: Operation timed out

##### Files downloaded will be written to ./data.
##### Found 2 unique time-series to download.
##### Getting subset for location 1 of 2...
##### Getting subset for location 2 of 2...
##### Full subset download complete. Writing the subset download file...
##### Done! Check the subset download file for correct subset information and download messages.

##### This function downloaded two files: `Lat66.65000Lon-152.00000Start2008-01-01End2012-12-31___MOD13Q1.asc` and `Lat67.30000Lon-150.75000Start2008-01-01End2012-12-31___MOD13Q1.asc`.

##### These are difficult files to parse, but there are some functions that simplify their analysis.  `MODISTimeSeries` extracts the time series from all the available `.asc` files in the directory as a matrix:

##### 3.0 Exploring Dynamic MODIS data
```{r, warning=FALSE}
mooseNDVI.ts <- MODISTimeSeries(Dir = "./data", Band = band, Simplify = TRUE)
head(mooseNDVI.ts)
colnames(mooseNDVI.ts) <- c("ndvi.summer", "ndvi.winter")
```

#### The row names of this matrix contain the date of the measurement, as the YEAR-DAYOFYEAR.  This takes some manipulation to turn into a POSIX:

```{r, warning=FALSE, message=FALSE}
require(lubridate)
mooseNDVI.ts <- data.frame(mooseNDVI.ts, date = ymd(paste(substring(row.names(mooseNDVI.ts), 2,5), 1, 1)) + 
ddays(as.numeric(substring(row.names(mooseNDVI.ts), 6))))
```

#### Ok, now we can at least plot these NDVI bands underneath the migration of the moose:

```{r, echo = -1}
par(mfrow = c(2,1), mar = c(0,0,0,0), oma = c(4,4,2,2), mgp = c(2,.25,0), tck = 0.01, las = 1, xpd = NA, cex.axis = 0.75)

with(subset(moose.daily, id == "C52"), 
plot(time, lat, type="o", xlim = range(mooseNDVI.ts$date), xlab = "", xaxt="n", ylab = "latitude", pch = 19, col = rgb(0,0,0,.3), cex = 0.8))

with(mooseNDVI.ts,{
plot(date, ndvi.summer, type="o", pch = 19, col = 2, ylab = "MODIS NDVI")
lines(date, ndvi.winter, type="o", pch = 19, col = 4)
})
legend("topleft", col = c(2,4), legend = c("summer range", "winter range"), pch = 19, lty = 1, cex=0.75)
```

##### There's not a lot of difference in the winter and summer ranges. Interesting that (superficially) it looks like the moose are initiating their winter migration *before* the plummeting in the NDVI actually happens.

##### Excercise 1: Modify the code above to extract and plot data for Moose 27?

#### Excercise 2: Modify the above code to extract time-varying NDVI data for each data point for Moose 27?

