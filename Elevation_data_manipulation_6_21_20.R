#--------------------------------------------------------------------------------
# Extract elevation data from SRTM crop boundary layers, map, zonal statistics
# Author: Will Eaton
# Date created 6-16-20
# Date modified 6-21-20
#--------------------------------------------------------------------------------

# Install packages
library("raster")
library("Rtools")

# Load Packages ####################
library("raster")
library("rgdal")
library("sp")
library("sf")
library("tmap")

library("data.table")
library("RCurl")
library("R.utils")
library("gdalUtils")

library(rgeos)
library(rasterVis)
library(shapefiles)
library(viridis)
library(ggplot2)
library(sf)
library(maptools)
library(tmaptools)
library(exactextractr)
library(BAMMtools)
library(shinyjs)
library(shiny)

# We can list the libraries that are actually loaded doing
(.packages())

#####################################

#remove all objects from the current workspace (R memory)
rm(list = ls())

#This function closes the specified plot (by default the current device) and if it is an imguR device, uploads the plots for web hosting
dev.off()

#Clear startup screen/console in R / RStudio
cat("\014") 

##############################
## ELEVATION DATA FROM SRTM ##
##############################

# Load admin boundaries ---------------------------------------------------
adm0.uga <- subset(getData("GADM", country = "UGA", level = 0)) # Country
adm1.uga <- subset(getData("GADM", country = "UGA", level = 1)) # District
adm2.uga <- subset(getData("GADM", country = "UGA", level = 2)) # City Council? District?
# Bring in n= 135 districts shapefile
# uga_2020 <- st_read(dsn = "/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/DataAdministrative Boundaries\\Uganda\\admbnd_2020\\uga_admbnda_adm2_2020.shp")

# Import Data ------------------------------------------------------------------------------
# srtm <- getData('SRTM', lon=30, lat=03)
srtm <-raster("/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/srtm_42_12.tif")
# srtm2 <- getData('SRTM', lon=34, lat=00)
srtm2 <-raster("/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/srtm_42_13.tif")
# srtm3 <- getData('SRTM', lon=35, lat=01)
srtm3 <-raster("/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/srtm_43_12.tif")
# srtm4 <- getData('SRTM', lon=33, lat=04)
srtm4 <-raster("/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/srtm_43_13.tif")
# srtm5 <- getData('SRTM', lon=29, lat=01)
srtm5 <-raster("/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/srtm_44_12.tif")

# Mosaic/merge srtm tiles ------------------------------------------------------------------
srtmmosaic <- mosaic(srtm, srtm2, srtm3, srtm4, srtm5, fun=mean)

# Bring in clipped district boundary created in Arc Gis Pro & imported on 6-21-20
uga_n_135_water_clip <- st_read(dsn = "/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/Shapefiles/Uganda_n_135_districts_water_clip.shp")

###########################################################################
###### 6-21-20 Bring in elevation raster file tif (created previously)
###### Can start from here if have raster file as tif
###########################################################################
DEM_uga <- raster("/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/Elevation Data/uga_elev_no_water.tif")

# Extract mean elevation to district (using arc gis shapefile created on 6-21-20 (uga_n_135_water_clip) -------------------------------------------------------
uganda_elevation_extract <- st_as_sf(uga_n_135_water_clip)
uganda_elevation_extract$mean_elevation <- exact_extract(DEM_uga, uga_n_135_water_clip, 'mean')
uganda_elevation_extract <- as.data.frame(uganda_elevation_extract) # convert to data frame
uga_elev_n_135 <- uganda_elevation_extract[c(5,8)] # only keep ADM2_EN and elevation variable for export
write.csv(uga_elev_n_135,"/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/Elevation Data/uga_elev_n_135.csv", row.names = TRUE) # export table

# View color options  ----------------------------------------------------------------------
# palette_explorer()
# tmap.pal.info

# Create elevation classes  ---------------------------------------------------------------
# go with following elevation categories from Muwanika et al. 2019
# <1200 m, 1200 to 1600m and > 1600 meters, may not have weight, can go in equal weighted group
# found in low densities above 1700 m in Kenya (The Effects of Climatic Factors on the Distribution and Abundance of Malaria Vectors in Kenya)
# make elevation classes binary (within 12-1600 m OR <1200 m or >1600)
uganda_elevation_extract$elev_class[uganda_elevation_extract$mean_elevation > 1600] <- "< 1200 m or > 1600 m"
uganda_elevation_extract$elev_class[uganda_elevation_extract$mean_elevation <=1600 & uganda_elevation_extract$mean_elevation >= 1200] <- "1200 m to 1600 m" #Uganda experiences stable endemic malaria in 95 % of the areas of altitude 1200 to 1600 m (Muwanika et al. 2019)
uganda_elevation_extract$elev_class[uganda_elevation_extract$mean_elevation < 1200] <- "< 1200 m or > 1600 m"
uganda_elevation_extract$elev_class <- as.factor(uganda_elevation_extract$elev_class) # convert to factor variable
uga.elev.sf <- st_sf(uganda_elevation_extract) # convert data frame back to shapefile

# View chloropleth map of result   ---------------------------------------------------------------
tm_shape(adm0.uga) + tm_fill(col="lightblue") + tm_shape(uga.elev.sf) + 
    tm_fill("elev_class", title = "Uganda\nDistrict mean elevation", style="cat", palette = "-viridis") + 
    tm_borders(lwd = 0.2) +
    tm_scale_bar() + tm_compass(position = c("left","top")) +  
    tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")

uga_elev_map_135 <- tm_shape(adm0.uga) + tm_fill(col="lightblue") + tm_shape(uga.elev.sf) +  # assign to obj
    tm_fill("elev_class", title = "Uganda\nDistrict mean elevation", style="cat", palette = "-viridis") + 
    tm_borders(lwd = 0.2) +
    tm_scale_bar() + tm_compass(position = c("left","top")) +  
    tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")

# save image  ---------------------------------------------------------------
setwd("/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/Map outputs/Chloropleth Elevation") # set working drive
tmap_save(uga_elev_map_135, "uga_chloro_elev_class_map_135.png", width =6.78, height=5, units ='in', asp = 0)

# # Calculate mean rf for all months available
# Uganda_dis_new <- cbind(Uganda_dis,mean_rf_all=rowMeans(Uganda_dis[7:30], na.rm=TRUE))
# Uganda_dis_new_2 <- cbind(Uganda_dis_new,mean_rf_2020=rowMeans(Uganda_dis[27:30], na.rm=TRUE))
# Uganda_dis_new_2$rf_anomaly <- Uganda_dis_new_2$mean_rf_2020 - Uganda_dis_new_2$mean_rf_all

# save raster file as geotiff - saved previously 
# uga_elev_raster <- writeRaster(cropped, filename="/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/DataElevation Data/uga_elev.tif", format="GTiff", overwrite=TRUE)
# uga_elv_raster <-writeRaster(cropped2, filename="/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/DataElevation Data/uga_elev_no_water.tif", format="GTiff", overwrite=TRUE)
