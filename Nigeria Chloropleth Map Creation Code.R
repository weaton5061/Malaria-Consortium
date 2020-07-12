# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Nigeria new chlorpleth map code
# Author: Will Eaton
# Date created: 7-12-20
# Date modified: 7-12-20
# Local drive location: /Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Methodology/R Code/MC R Code/Malaria-Consortium
# git: https://github.com/weaton5061/Malaria-Consortium.git
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# Load packages
library("spDataLarge")
library(dplyr)
library(GADMTools)
library(tmap)
library(sf)
#library(exactextractr)
library(maptools)
library(raster)
library(sp)
library(readxl)
library(readr)
library(foreign)   ; library(tsModel) ; library("lmtest") ; library("Epi")
library("splines") ; library("vcd")
library(reshape2)  ; library(hablar)
library(tidyr)     
library (viridis)
library(data.table)
library(forecast)  ; library(MASS)
library(tseries)   ; library(scales)
library(tsModel)   ; library(extrafont)
library(lmtest)    ; library(tidyverse)
library(stargazer) ; library(RColorBrewer)
library(readxl)    ; library(olsrr)
library(Hmisc)
library(MASS)
library(ggplot2)
library(dplyr)
library(devEMF)
library(padr)
library(zoo)
library(tidyverse)
library(naniar)
library(GGally)
library(sf)
library(cartogram)
library(mgcv)
library(BAMMtools)

library(raster)
require(rgdal)
require(sp)
require(data.table)
require(RCurl)
require(R.utils)
require(gdalUtils)
require(parallel)

# remove all packages
#lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

#remove all objects from the current workspace (R memory)
rm(list = ls())

#This function closes the specified plot (by default the current device) and if it is an imguR device, uploads the plots for web hosting
dev.off()

#Clear startup screen/console in R / RStudio
cat("\014") 

###########################
#set working directory
###########################
setwd("/Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Nigeria Data")


# Get admin boundaries ----------------------------------------------------
# GADM, the Database of Global Administrative Areas, is a high-resolution database of country administrative
# areas, with a goal of “all countries, at all levels, at any time period.
adm0.nga <- subset(getData("GADM", country = "NGA", level = 0)) # Country
plot(adm0.nga)
adm1.nga <- subset(getData("GADM", country = "NGA", level = 1)) # District
plot(adm1.nga)
# adm3.nga <- subset(getData("GADM", country = "NGA", level = 3))
# plot(adm3.nga)

#----------------------------------------------------------------------------------------
# adm2.nga appears to be LGAs with unique identifiers called NAME_2 ----------------
adm2.nga <- subset(getData("GADM", country = "NGA", level = 2)) # LGAs
plot(adm2.nga)
# View adm2.nga as data frame
adm2.nga.df <- as.data.frame(adm2.nga)
# View adm2.nga.df
View(adm2.nga.df)

# Bring in Nigeria Shapefile with LGAs (n = approx 775. Verify) ----------------------------------------------------------------------------------------------------------------------------------------------
#uga_admin <- st_read(dsn = "/Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/Administrative Boundaries/Uganda/uga_admbnda_adm1_UBOS_v2.shp")
# uga_admin_no_water <- st_read(dsn = "/Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/Shapefiles/uga_adm1_UBOS_clipped.shp")
# Verify shapefile - LOOKS GOOD!
# tm_shape(uga_admin_no_water) + tm_polygons("ADM1_EN") + tm_legend(show=FALSE)
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



# Bring in Nigeria current master dataset NGA_master6.csv ---------------------------------
nga_master6 <- read.csv("//Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Nigeria Data/NGA_master6_2.csv", header = TRUE) # bring in NGA_master6.csv
View(nga_master6)

# Create new variables ----------------------------------------------------
# Create new SMC coverage weight = 0.82
nga_master6$smc_weight_0_82 <- 0.82
nga_master6$smc_score_weighted <- nga_master6$smc_weight_0_82 * nga_master6$SMC_class


# Sort nga_master6 by NAME_2 ----------------------------------------------


# Merge adm2.nga with nga_master6 dataset ---------------------------------



# Create SMC map ----------------------------------------------------------


flood_map_redo.spdf <- merge(adm2.nga, flood_map_redo, by="ADM1_EN", all = TRUE)

tm_shape(adm0.uga) + tm_fill(col="lightblue") + tm_shape(flood_map_redo.spdf) + 
    tm_fill("Flooding", title = "Flooding reported ", n=2, style = "cat", palette = "Reds", labels = c("No", "Yes")) + 
    tm_scale_bar() + tm_compass(position = c("left","top")) + 
    tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")

# Assign flood map to object score_sum_map
flood_map_final <- tm_shape(adm0.uga) + tm_fill(col="lightblue") + tm_shape(flood_map_redo.spdf) + 
    tm_fill("Flooding", title = "Flooding reported ", n=2, style = "cat", palette = "Reds", labels = c("No", "Yes")) + 
    tm_scale_bar() + tm_compass(position = c("left","top")) + 
    tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")

# Save map
tmap_save(flood_map_final, "flood_map_final_6_17_20.png", width =6.78, height=5, units ='in', asp = 0)



# -------------------------------------------------------------------------------------------------------------
# IMPORTANT TASKS 7-10-20: 
# -------------------------------------------------------------------------------------------------------------
# 1) Confirm that classes and scores were created correctly for indicators and don't require inverse values
#    e.g ITN_dist_class (& score)
# COVID_case_class
# Disruptive_event_class (does this require modification or use of mean from UN index?)
# SMC_class (0 or 1) 
# -------------------------------------------------------------------------------------------------------------


# for input chloropleth maps, include the following variables
# SMC_score, ITN_dist_score, COVID_case_score, Disruptive_event_score
# slim down for quick map creation
# NOTE: Covid_case_score is being updated by Alyssa on 7-10-20, don't use until updated by you or her
nga_quick_map_slim <- nga_master6[c(1, 14, 17, 19, 21, 33, 34) ] # slim down NGA_master6
View(nga_quick_map_slim)