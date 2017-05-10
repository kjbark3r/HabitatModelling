### ### ###### ### ### ### ### ###  
#   WILD562 LAB 8/9 ASSIGNMENT   #
#        KRISTIN BARKER          #
#           MAY 2017             #
### ### ###### ### ### ### ### ###  


#### SETUP ####

# packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("sp","raster","rgdal","ggmap","survival","TwoStepCLogit","pbs","dplyr","lubridate","move","MASS","fdrtool","circular","CircStats", "coxme", "mclogit","data.table","scales")
ipak(packages)

# working directory
wd_desktop <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\HabitatModelling\\Lab9_cLogit2"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\HabitatModelling\\Lab9_cLogit2"
ifelse(file.exists(wd_laptop), setwd(wd_laptop), setwd(wd_desktop)) 
list.files()

# clean workspace
rm(packages, wd_desktop, wd_laptop, ipak)


# custom ssf functions from BjÃ¶rn Reineking 
source("ssf_fun_2016_final.R", verbose = FALSE)

# data
yl29 <- read.csv("yl29.csv") # lab 8 data
yl.all <- read.csv("yl_all.csv",header=T) # lab 9 data

## Create ssf for MoveStack - multiple individuals 

# Read in data for all elk


# look at data
head(yl.all)

yl.all$elkuidF <- as.factor(yl.all$ELKUID)

#### Get to know our data graphically
ggplot(yl.all, aes(x=UTMX, y = UTMY, color = elkuidF)) + 
  geom_point()

#look at elk ids
levels(yl.all$elkuidF)
table(yl.all$elkuidF, yl.all$Year)
## all in 2003! 

#create time stamp
# first format time stamp to bring date and time together
