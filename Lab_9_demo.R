#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Wetland delineation
#Coder: Nate Jones (cnjones7@ua.edu)
#Date Created: 3/3/2023
#Purpose: Demonstrate wetland delineation and characterization techniques
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup workspace --------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear workspace (sorry JP!)
remove(list=ls())

#load packages
library(tidyverse)
library(raster)
library(sf)
library(lwgeom)
library(elevatr)
library(whitebox)
library(mapview)

#Create temp dir
temp_dir <- tempdir()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Acquire DEM ---------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create area of interest
aoi <- tibble(
  lat =  47.099776,
  lon = -99.102490) %>% 
  st_as_sf(
    coords = c("lon", "lat"), 
    crs = '+proj=longlat +datum=WGS84 +no_defs')

#Download DEM
dem <- get_elev_raster(aoi, z=14)

#Convert to planar coordinates
p <- "+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
dem <- projectRaster(dem, crs = p)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.0 Delineate wetlands -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#write dem to temp file
writeRaster(dem, paste0(temp_dir,"\\dem.tif"), overwrite=T)

#Smooth DEM
wbt_fast_almost_gaussian_filter(
  input = "dem.tif",
  output = "dem_filter.tif",
  wd = temp_dir)

#Identify depressions
wbt_stochastic_depression_analysis(
  dem = "dem_filter.tif", 
  output = "depressions.tif", 
  rmse = 0.15, 
  range = 100, 
  wd = temp_dir)

#Reclass raster
wbt_reclass(
  input = "depressions.tif", 
  output = "reclass.tif", 
  reclass_vals = '0;0;0.80;1;0.80;1', 
  wd = temp_dir
)

#Identify "connected" inundated areas
wbt_clump(
  input = 'reclass.tif',
  output = 'groups.tif',
  diag = T, 
  zero_back = T,
  wd = temp_dir
)

#Create wetland polygons
wbt_raster_to_vector_polygons(
  input = "groups.tif", 
  output = "wetlands.shp",
  wd = temp_dir)

#read groups into R
wetlands <- st_read(paste0(temp_dir,"//wetlands.shp"))

#Filter for wetland size
wetlands <- wetlands %>% 
  mutate(size_m2 = st_area(.)) %>% 
  mutate(size_m2 = as.numeric(paste(size_m2))) %>% 
  mutate(size_ha = size_m2/10000) %>% 
  filter(size_ha > 0.1) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4.0 Estimate ecologically relevant metrics -----------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Select wetland of interest
n <- 15

#Isolate wetland
wetland <- wetlands[15,]

#Crop DEM to wetland extent
dem_wetland <- mask(dem, wetland)
dem_wetland <- crop(dem_wetland, wetland)

#Wetland size ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wetland_area_m2 <- wetland %>% st_area()

#wetland volume ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create max Raster
wetland_max<-dem_wetland*0+maxValue(dem_wetland)

#estimate depth of each cell
wetland_depth <- wetland_max - dem_wetland 

#estimate volume
wetland_volume_m3 <- cellStats(wetland_depth, sum, na.rm=T)*res(dem_wetland)[1]*res(dem_wetland)[1]

#wetland shoreline length ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wetland_perimeter_m <- wetland %>% st_perimeter(.)

#Export wetland information
output <- tibble(
  fid = wetland$FID, 
  wetland_area_m2, 
  wetland_volume_m3, 
  wetland_perimeter_m
)

