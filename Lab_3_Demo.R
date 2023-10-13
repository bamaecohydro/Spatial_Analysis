#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Lab 3 Demo
# Coder: Nate Jones (cnjones7@ua.edu)
# Date: 8/9/2023
# Purpose: Estimate # of CAFOs, WWTPs, and septics upstream of public water intake
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Setup workspace ----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
remove(list = ls())

#load packages of interest
library(tidyverse)
library(raster)
library(sf)
library(mapview)
library(nhdplusTools)
library(tigris)
library(fasterize)

#Alabama State
states <- states() %>% 
  dplyr::filter(STUSPS == "AL") %>% 
  st_transform(., crs = 4269)

#public water intakes
pwi <- st_read("data\\pwi.shp") %>% 
  st_transform(., crs = 4269)

#Black Warrior AFO/CAFO (https://www.google.com/maps/d/viewer?mid=1bIRTEfDjKj2wplmwj2vRMzcKJ3If8VPK&ll=33.16171093100135%2C-87.82940270239635&z=11)
cafo <- st_read('data\\afo_cafo.shp') %>% 
  st_transform(., crs = 4269)

#USGS Well Locations (proxy for septics)
septics <- raster("data\\septics.tif") %>% 
  projectRaster(., crs = 4269)

#WWTP from HydroSheds
wwtp <- st_read('data\\wwtp.shp') %>% 
  st_transform(., crs = 4269)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Delineate Watershed ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Isolate pwi of interest 
pwi_n <- pwi[1,]

#Identify NHD reach 
start_comid <- discover_nhdplus_id(pwi_n, raindrop = T)
start_comid

#Snag flowline
flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid$comid[1]), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

#get nhdplus files
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = 'download', 
                         flowline_only = FALSE,
                         return_data = TRUE, 
                         overwrite = TRUE)
#Identify catchment
shed <- sf::read_sf(subset_file, "CatchmentSP")
shed <- st_union(shed)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.0 Estimate WWTP, CAFO, and septics upstream in watershed -------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate Area
area_m2 <- shed %>% st_area() %>% paste() %>% as.numeric()
area_km2 <- area_m2/1000000

#Estimate the number of septics
mask <- fasterize(st_as_sf(shed), septics)
n_setpics <- raster::crop(septics, mask)
n_septics <- raster::mask(septics, mask)
n_septics <- cellStats(n_septics, sum, na.rm=T)

#Estiamte number of afo/cafo
n_cafo <- cafo[shed,]
n_cafo <- nrow(n_cafo)

#Estimate number of wwtp
n_wwtp <- wwtp[shed,]
n_wwtp <- nrow(n_wwtp)

#create tibble of metrics
metrics <- tibble(
  ID    = pwi_n$ID, 
  comid = start_comid$comid[1], 
  n_wwtp, 
  n_septics, 
  n_cafo, 
  area_km2)

#print for funzie
metrics




