#' Skip to content
#' Search or jump to…
#' 
#' Pull requests
#' Issues
#' Marketplace
#' Explore
#' 
#' @weaton5061 
#' Learn Git and GitHub without any code!
#'     Using the Hello World guide, you’ll start a branch, write comments, and open a pull request.
#' 
#' 
#' weaton5061
#' /
#'     Malaria-Consortium
#' 1
#' 00
#' Code
#' Issues 0
#' Pull requests 0 Actions
#' Projects 0
#' Wiki
#' Security 0
#' Insights
#' Settings
#' Malaria-Consortium/Elevation Data Uganda VC.R
#' Eaton recieving memory errors
#' 85e549c 2 hours ago
#' 248 lines (197 sloc)  11.1 KB

#--------------------------------------------------------------------------------
# Extract elevation data from SRTM crop boundary layers, map, zonal statistics
# Author: Will Eaton
# Date created 6-16-20
# Date modified 6-19-20
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
plot(adm0.uga)
adm1.uga <- subset(getData("GADM", country = "UGA", level = 1)) # District
plot(adm1.uga)
adm2.uga <- subset(getData("GADM", country = "UGA", level = 2)) # City Council? District?
plot(adm2.uga)
# Bring in n= 135 districts shapefile
uga_2020 <- st_read(dsn = "/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/DataAdministrative Boundaries\\Uganda\\admbnd_2020\\uga_admbnda_adm2_2020.shp")
tm_shape(uga_2020) + tm_borders(lwd=0.2)+ tm_fill("ADM2_EN") + tm_legend(show=FALSE) # Verify shapefile - LOOKS GOOD!

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
# tm_shape(srtmmosaic) + tm_raster(srtmmosaic)
plot(srtmmosaic)
plot(adm0.uga, add = TRUE) # Verify raster mosic covers admin 0 of uga

# Bring in arc gis clipped district boundary 6-21-20
uga_n_135_water_clip <- st_read(dsn = "/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/Shapefiles/Uganda_n_135_districts_water_clip.shp")

# Mask the raster (requires library("rgdal") package to be loaded) -------------------------
cropped4 <- crop(srtmmosaic, uga_n_135_water_clip)
masked4 <- mask(srtmmosaic, uga_n_135_water_clip)

uga_elev_water_clip) <- writeRaster(cropped4, filename="/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/DataElevation Data/uga_elev_water_clip.tif", format="GTiff", overwrite=TRUE)

# masked <- raster::mask(x = srtmmosaic, mask = uga_admin_no_water) 
# plot(masked)
# cropped <- crop(x=masked, y = extent(uga_admin_no_water)+.1) # Crop the raster
# plot(cropped)
# # Mask and crop the elevation raster again so that water boundary is removed
# masked2 <- mask(x=cropped, mask = uga_admin_no_water)
# plot(masked2)
# cropped2 <- crop(x=masked2, y=extent(uga_admin_no_water))
# plot(cropped2)

# map the raster input for report
# tm_shape(cropped2) + tm_raster(cropped2)

####################################################################
##### LEFT OFF HERE ON 6-20-2020 TRYIN TO CROP WATER BOUNDARY FROM 
# SHAPEFILE ##### IS THIS NECESARRY FOR NEXT STEPS? I DON'T THINK SO
# VERIFY MEAN ELEVATION FOR LAKE BOUNDARY REGION
######################################################################

# Attempt mask and crop of uga_water and uga_2020 shapefiles ------------------------------
attempt.sp <- sf_as_st(uga_water)
clip <- gIntersection(uga_2020, uga_water, byid = TRUE, drop_lower_td = TRUE) #clip polygon 2 with polygon 1
plot(clip, col = "lightblue")

## Clip the map
out <- gIntersection(uga_2020, CP, byid=TRUE)

tm_shape(uga_2020_water_clip) + tm_fill()
#confirm clipping worked
plot(county_clipped)

uga_2020_water_mask <- raster::mask(x=uga_2020, mask = uga_water)
plot(masked2)
cropped2 <- crop(x=masked2, y=extent(uga_admin_no_water))
plot(cropped2)

###########################################################################
###### 6-21-20 Bring in elevation raster file tif (created previously)
###########################################################################
DEM_uga <- raster("/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/Elevation Data/uga_elev_no_water.tif")
tm_shape(adm0.uga) + tm_fill(col="lightblue") + tm_shape(DEM_uga) +                      # create raster elevation map
    tm_raster(palette = 'viridis', title = "Elevation (m)") 
+
    tm_scale_bar() + tm_compass(position = c("left","top")) +  
    tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")

tm_shape(DEM_uga) + tm_raster(title = "Elevation (m)")

# Extract mean elevation to district -------------------------------------------------------
uganda_elevation_extract <- st_as_sf(uga_2020)
uganda_elevation_extract$mean_elevation <- exact_extract(cropped2, uga_2020, 'mean')

# Try exact of mean elevation using arc gis shapefile created on 6-21-20 (uga_n_135_water_clip)
uganda_elevation_extract <- st_as_sf(uga_n_135_water_clip)
uganda_elevation_extract$mean_elevation <- exact_extract(DEM_uga, uga_n_135_water_clip, 'mean')
uganda_elevation_extract <- as.data.frame(uganda_elevation_extract) # convert to data frame
uga_elev_n_135 <- uganda_elevation_extract[c(5,8)] # only keep ADM2_EN and elevation (2 columns of data)
write.csv(uga_elev_n_135,"/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/Elevation Data/uga_elev_n_135.csv", row.names = TRUE) # export table

# View color options  ----------------------------------------------------------------------
palette_explorer()
tmap.pal.info

class(uganda_elevation_extract)
View(uganda_elevation_extract)

# Create elevation classes   ---------------------------------------------------------------
# go with following elevation categories from Muwanika et al. 2019
# <1200 m, 1200 to 1600m and > 1600 meters, may not have weight, can go in equal weighted group
# found in low densities above 1700 m in Kenya (The Effects of Climatic Factors on the Distribution and Abundance of Malaria Vectors in Kenya)
# make elevation classes binary (within 12-1600 m OR <1200 m or >1600)
uganda_elevation_extract$elev_class[uganda_elevation_extract$mean_elevation > 1600] <- "< 1200 m or > 1600 m"
uganda_elevation_extract$elev_class[uganda_elevation_extract$mean_elevation <=1600 & uganda_elevation_extract$mean_elevation >= 1200] <- "1200 m to 1600 m" #Uganda experiences stable endemic malaria in 95 % of the areas of altitude 1200 to 1600 m (Muwanika et al. 2019)
uganda_elevation_extract$elev_class[uganda_elevation_extract$mean_elevation < 1200] <- "< 1200 m or > 1600 m"
# uganda_elevation_extract$elev_class <- as.character(uganda_elevation_extract$elev_class)
uganda_elevation_extract$elev_class <- as.factor(uganda_elevation_extract$elev_class) # convert to factor variable

# convert dataframe back to shapefile
uga.elev.sf <- st_sf(uganda_elevation_extract)


# View chloropleth map of result
tm_shape(adm0.uga) + tm_fill(col="lightblue") + tm_shape(uga.elev.sf) + 
    tm_fill("elev_class", title = "Uganda\nDistrict mean elevation", style="cat", palette = "-viridis") + 
    tm_borders(lwd = 0.2) +
    tm_scale_bar() + tm_compass(position = c("left","top")) +  
    tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")

uga_elev_map_135 <- tm_shape(adm0.uga) + tm_fill(col="lightblue") + tm_shape(uga.elev.sf) + 
    tm_fill("elev_class", title = "Uganda\nDistrict mean elevation", style="cat", palette = "-viridis") + 
    tm_borders(lwd = 0.2) +
    tm_scale_bar() + tm_compass(position = c("left","top")) +  
    tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")

# save image
# set working drive
setwd("/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/Map outputs/Chloropleth Elevation")
tmap_save(uga_elev_map_135, "uga_chloro_elev_class_map_135.png", width =6.78, height=5, units ='in', asp = 0)

# Calculate mean rf for all months available
Uganda_dis_new <- cbind(Uganda_dis,mean_rf_all=rowMeans(Uganda_dis[7:30], na.rm=TRUE))
Uganda_dis_new_2 <- cbind(Uganda_dis_new,mean_rf_2020=rowMeans(Uganda_dis[27:30], na.rm=TRUE))
Uganda_dis_new_2$rf_anomaly <- Uganda_dis_new_2$mean_rf_2020 - Uganda_dis_new_2$mean_rf_all

# system.time(print(levelplot(r, maxpixels = 1e4)))
# set max size of raster, in terms of number of raster cells
tmap_options(max.raster = c(c(plot = 1e7, view = 1e6)))
# reset all options
tmap_options_reset()

# image(cropped, col=inferno(256), bty="n", box=FALSE)
tm_shape(cropped2) + tm_raster(cropped2, col=inferno(256))

tm_shape(cropped2) + tm_raster(col=inferno(256))

tm_shape(cropped2) + tm_raster()
tm_shape(cropped2) + tm_raster(cropped2)

# save raster file as geotiff
uga_elev_raster <- writeRaster(cropped, filename="/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/DataElevation Data/uga_elev.tif", format="GTiff", overwrite=TRUE)
uga_elv_raster <-writeRaster(cropped2, filename="/Volumes/LaCie 5TB/LaCie MacOS Extended/Tulane Research Projects/Malaria Consortium/DataElevation Data/uga_elev_no_water.tif", format="GTiff", overwrite=TRUE)


© 2020 GitHub, Inc.
Terms
Privacy
Security
Status
Help
Contact GitHub
Pricing
API
Training
Blog
About
