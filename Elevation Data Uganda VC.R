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
library("data.table")
library("RCurl")
library("R.utils")
library("gdalUtils")

library(rgeos)
library(rasterVis)
library(shapefiles)
library(viridis)
library(tmap)
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
uga_2020 <- st_read(dsn = "D:\\LACIE MacOS Extended\\Tulane Research Projects\\Malaria Consortium\\Data\\Administrative Boundaries\\Uganda\\admbnd_2020\\uga_admbnda_adm2_2020.shp")
tm_shape(uga_2020) + tm_borders(lwd=0.2)+ tm_fill("ADM2_EN") + tm_legend(show=FALSE) # Verify shapefile - LOOKS GOOD!

# load uganda water boundary
uga_water <- st_read("D:\\LACIE MacOS Extended\\Tulane Research Projects\\Malaria Consortium\\Data\\Uganda Water Bodies\\Ug_Waterbodies\\Ug_Waterbodies.shp") # too much water
uga_water_2 <-st_read("D:\\LACIE MacOS Extended\\Tulane Research Projects\\Malaria Consortium\\Data\\Uganda Water Bodies\\uga_water_areas_dcw\\uga_water_areas_dcw.shp") # right amt of water

plot(adm0.uga)
#plot(uga_water, add=TRUE)
plot(uga_water_2)
plot(uga_water) # this layer looks better

# plot blue boundary country
plot(adm0.uga, col="light blue", border = "light blue")
plot(adm1.uga, add=TRUE)

# Bring in Alyssa's district shapefile then mask and crop to water layer
setwd("D:\\LACIE MacOS Extended\\Tulane Research Projects\\Malaria Consortium\\Data")
uga_admin <- st_read(dsn = "D:\\LACIE MacOS Extended\\Tulane Research Projects\\Malaria Consortium\\Data\\Administrative Boundaries\\Uganda\\uga_admbnda_adm1_UBOS_v2.shp")
plot(uga_admin)

st_is_valid(st_make_valid(uga_admin))  # not sure if I need this code
st_is_valid(st_make_valid(uga_water))


tm_shape(uga_water) + tm_fill() # too much water
tm_shape(uga_water_2) + tm_fill() # Warning message: the shape uga_water is invalid. See sf::st_is_valid
tm_shape(uga_admin) + tm_fill() # good for mapping
uga_water_mask <- mask(x = uga_water_2, mask = uga_admin) # Mask the district admin shapefile # not working?

# Bring in Alyssa's district shapefile with water layer clipped out
uga_admin_no_water <- st_read(dsn = "D:\\LACIE MacOS Extended\\Tulane Research Projects\\Malaria Consortium\\Data\\Shapefiles\\uga_adm1_UBOS_clipped.shp")
tm_shape(uga_admin_no_water) + tm_polygons() # This layer produces image of Uganda with islands, no water shown, but islands are distinguishable
tm_shape(adm0.uga) + tm_fill(col="lightblue", lwd=0.18) + tm_shape(uga_admin_no_water) + tm_polygons(lwd=0.30) # This layer produces image of Uganda with islands and proper amt of H2O

# plot to verify alignment and to see what combined water and admin boundary should look like
#tm_shape(uga_admin) + tm_polygons() + tm_shape(uga_water_2) + tm_polygons(col="lightblue")
# tm_shape(uga_admin) + tm_polygons(lwd=0.30) + tm_shape(uga_water) + tm_polygons(col="lightblue", lwd=0.18)

# # crop the shapefile
# plot(clip, col = "lightblue")
# 
# # Crop the raster
# cropped <- crop(x=masked, y = extent(uga_admin_no_water))
# plot(cropped)


# Import Data ------------------------------------------------------------------------------
# srtm <- getData('SRTM', lon=30, lat=03)
srtm <-raster("D:\\LACIE MacOS Extended\\Tulane Research Projects\\Malaria Consortium\\Data\\srtm_42_12.tif")
# plot(srtm)
# tm_shape(srtm) + tm_raster(srtm)
#Download/bring in two more tiles
# srtm2 <- getData('SRTM', lon=34, lat=00)
srtm2 <-raster("D:\\LACIE MacOS Extended\\Tulane Research Projects\\Malaria Consortium\\Data\\srtm_42_13.tif")
# plot(srtm2)
# srtm3 <- getData('SRTM', lon=35, lat=01)
srtm3 <-raster("D:\\LACIE MacOS Extended\\Tulane Research Projects\\Malaria Consortium\\Data\\srtm_43_12.tif")
# plot(srtm2)
# plot(srtm3)
# srtm4 <- getData('SRTM', lon=33, lat=04)
srtm4 <-raster("D:\\LACIE MacOS Extended\\Tulane Research Projects\\Malaria Consortium\\Data\\srtm_43_13.tif")
# plot(srtm2)
# plot(srtm4)
# srtm5 <- getData('SRTM', lon=29, lat=01)
srtm5 <-raster("D:\\LACIE MacOS Extended\\Tulane Research Projects\\Malaria Consortium\\Data\\srtm_44_12.tif")
# plot(srtm2)
# plot(srtm5)
# srtm6 <- getData('SRTM', lon=29, lat=00)

# Mosaic/merge srtm tiles ------------------------------------------------------------------
srtmmosaic <- mosaic(srtm, srtm2, srtm3, srtm4, srtm5, fun=mean)
# tm_shape(srtmmosaic) + tm_raster(srtmmosaic)
plot(srtmmosaic)
plot(adm0.uga, add = TRUE) # Verify raster mosic covers admin 0 of uga

# Mask the raster (requires library("rgdal") package to be loaded) -------------------------
masked <- raster::mask(x = srtmmosaic, mask = uga_admin_no_water) 
plot(masked)
cropped <- crop(x=masked, y = extent(uga_admin_no_water)+.1) # Crop the raster
plot(cropped)
# Mask and crop the elevation raster again so that water boundary is removed
masked2 <- mask(x=cropped, mask = uga_admin_no_water)
plot(masked2)
cropped2 <- crop(x=masked2, y=extent(uga_admin_no_water))
plot(cropped2)

# map the raster input for report
tm_shape(cropped2) + tm_raster(cropped2)

####################################################################
##### LEFT OFF HERE ON 6-20-2020 TRYIN TO CROP WATER BOUNDARY FROM 
SHAPEFILE ##### IS THIS NECESARRY FOR NEXT STEPS? I DON'T THINK SO
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
DEM_uga <- raster("D:\\LACIE MacOS Extended\\Tulane Research Projects\\Malaria Consortium\\Data\\Elevation Data\\uga_elev_no_water.tif")
tm_shape(adm0.uga) + tm_fill(col="lightblue") + tm_shape(DEM_uga) +                      # create raster elevation map
    tm_raster(palette = 'viridis', title = "Elevation (m)") 
+
    tm_scale_bar() + tm_compass(position = c("left","top")) +  
    tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")

tm_shape(DEM_uga) + tm_raster(title = "Elevation (m)")

# Extract mean elevation to district -------------------------------------------------------
uganda_elevation_extract <- st_as_sf(uga_2020)
uganda_elevation_extract$mean_elevation <- exact_extract(cropped2, uga_2020, 'mean')

# View color options  ----------------------------------------------------------------------
palette_explorer()
tmap.pal.info

# Convert to data frame  -------------------------------------------------------------------
uganda_elevation_extract <- as.data.frame(uganda_elevation_extract)
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
tm_shape(uga_2020) + tm_fill(col="grey80") + tm_shape(uga.elev.sf) + 
    tm_fill("elev_class", title = "Uganda Elevation Classes", style="cat",palette="Paired") + 
    tm_borders(lwd = 0.2) +
    tm_scale_bar() + tm_compass(position = c("left","top")) +  
    tm_layout(frame = FALSE, legend.outside = TRUE, legend.outside.position = "right")


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
uga_elev_raster <- writeRaster(cropped, filename="D:\\LACIE MacOS Extended\\Tulane Research Projects\\Malaria Consortium\\Data\\Elevation Data/uga_elev.tif", format="GTiff", overwrite=TRUE)
uga_elv_raster <-writeRaster(cropped2, filename="D:\\LACIE MacOS Extended\\Tulane Research Projects\\Malaria Consortium\\Data\\Elevation Data/uga_elev_no_water.tif", format="GTiff", overwrite=TRUE)




