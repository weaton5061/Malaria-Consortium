#--------------------------------------------------------------------------------
# Extract elevation data from SRTM crop boundary layers, map, zonal statistics
# Author: Will Eaton
# Date created 6-16-20
# Date modified 6-19-20
#--------------------------------------------------------------------------------

# Load Packages ####################
require(raster)
require(rgdal)
require(sp)
require(data.table)
require(RCurl)
require(R.utils)
require(gdalUtils)

library(rgeos)
library(rasterVis)
library(shapefiles)
library(viridis)
library(tmap)
library(ggplot2)
library(sf)
library(maptools)
library(exactextractr)

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
plot(adm0.uga, add = TRUE)
adm0.uga <- subset(getData("GADM", country = "UGA", level = 0)) # Country
plot(adm0.uga)
adm1.uga <- subset(getData("GADM", country = "UGA", level = 1)) # District
plot(adm1.uga)
adm2.uga <- subset(getData("GADM", country = "UGA", level = 2)) # City Council? District?
plot(adm2.uga)
adm3.uga <- subset(getData("GADM", country = "UGA", level = 3))
plot(adm3.uga)

# load uganda water boundary

uga_water <- st_read("/Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/Uganda Water Bodies/Ug_Waterbodies/Ug_Waterbodies.shp") # this one is better
uga_water_2 <-st_read("/Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/Uganda Water Bodies/uga_water_areas_dcw/uga_water_areas_dcw.shp")

plot(adm0.uga)
#plot(uga_water, add=TRUE)
plot(uga_water_2, add = TRUE)
plot(uga_water) # this layer looks better

# plot blue boundary country
plot(adm0.uga, col="light blue", border = "light blue")
plot(adm1.uga, add=TRUE)

# Bring in Alyssa's district shapefile then mask and crop to water layer
setwd("/Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Data")
uga_admin <- st_read(dsn = "/Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/Administrative Boundaries/Uganda/uga_admbnda_adm1_UBOS_v2.shp")
uga_water_mask <- mask(x = uga_water_2, mask = uga_admin) # Mask the district admin shapefile

# Bring in Alyssa's district shapefile with water layer clipped out
uga_admin_no_water <- st_read(dsn = "/Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/Shapefiles/uga_adm1_UBOS_clipped.shp")
plot(uga_admin_no_water) # ensure correct layer - looks good

# plot to verify alignment and to see what combined water and admin boundary should look like
#tm_shape(uga_admin) + tm_polygons() + tm_shape(uga_water_2) + tm_polygons(col="lightblue")
tm_shape(uga_admin) + tm_polygons(lwd=0.30) + tm_shape(uga_water) + tm_polygons(col="lightblue", lwd=0.18)

# crop the shapefile
plot(clip, col = "lightblue")

# Crop the raster
cropped <- crop(x=masked, y = extent(uga_admin_no_water))
plot(cropped)


# Import Data ---------------------------------------------------
# srtm <- getData('SRTM', lon=30, lat=03, path="/Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/Elevation Data")
srtm <-raster("/Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/srtm_42_12.tif")
plot(srtm)
#Download/bring in two more tiles
# srtm2 <- getData('SRTM', lon=34, lat=00)
srtm2 <-raster("/Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/srtm_42_13.tif")
plot(srtm2)
# srtm3 <- getData('SRTM', lon=35, lat=01)
srtm3 <-raster("/Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/srtm_43_12.tif")
plot(srtm2)
plot(srtm3)
# srtm4 <- getData('SRTM', lon=33, lat=04)
srtm4 <-raster("/Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/srtm_43_13.tif")
plot(srtm2)
plot(srtm4)
# srtm5 <- getData('SRTM', lon=29, lat=01)
srtm5 <-raster("/Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/srtm_44_12.tif")
plot(srtm2)
plot(srtm5)
# srtm6 <- getData('SRTM', lon=29, lat=00)

#Mosaic/merge srtm tiles
srtmmosaic <- mosaic(srtm, srtm2, srtm3, srtm4, srtm5, fun=mean)
plot(srtmmosaic)
plot(adm0.uga, add = TRUE)

# ## crop stack (literally cropping an image. the crop is the extent of haiti with a little buffer)
# RFstack <- crop(RFstack, extent(uga_admin_no_water)+.1)
# RFstack_masked <- mask(RFstack, uga_admin_no_water)

# Mask the raster
masked <- mask(x = srtmmosaic, mask = uga_admin_no_water)
plot(masked)

# Crop the raster
cropped <- crop(x=masked, y = extent(uga_admin_no_water)+.1)
plot(cropped)

# attempt clip of raster layer instead of boundary layer
## crop and mask
## Example RasterLayer

# this almost works - it cropps out the land. I want to crop out the water --------
# r2 <- crop(cropped, extent(uga_water))
# r3 <- mask(r2, uga_water)
# ## Check that it worked
# plot(r3)
# plot(uga_water, add=TRUE, lwd=2)

# mask and crop the elevation raster again so that water boundary is removed
masked2 <- mask(x=cropped, mask = uga_admin_no_water)
plot(masked2)
cropped2 <- crop(x=masked2, y=extent(uga_admin_no_water))
plot(cropped2)

### Extracting mean elevation to district
uganda_elevation_extract <- st_as_sf(uga_admin_no_water)# This is already loaded, but am reloading shapefile/admin units again using this method
uganda_elevation_extract$mean_elevation <- exact_extract(cropped2, uga_admin_no_water, 'mean')

uganda_elevation_extract <- as.data.frame(uganda_elevation_extract)

# go with following elevation categories from Muwanika et al. 2019
# <1200 m, 1200 to 1600m and > 1600 meters, may not have weight, can go in equal weighted group
# found in low densities above 1700 m in Kenya (The Effects of Climatic Factors on the Distribution and Abundance of Malaria Vectors in Kenya)

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
uga_elev_raster <- writeRaster(cropped, filename="/Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/Elevation Data/uga_elev.tif", format="GTiff", overwrite=TRUE)
uga_elv_raster <-writeRaster(cropped2, filename="/Volumes/Samsung T5/LACIE MacOS Extended/Tulane Research Projects/Malaria Consortium/Data/Elevation Data/uga_elev_no_water.tif", format="GTiff", overwrite=TRUE)




