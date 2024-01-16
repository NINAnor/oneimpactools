# This script extracts the covariates for step selection functions

rm(list=ls()) 

library(sf)
library(raster)
library(terra)
library(plyr)
library(dplyr)
library(parallel)
library(foreach)

###############################################################################################################
# Import lines and end points
###############################################################################################################

dat<-readRDS("03_data/Code_Neri/DataToUseSSFAllInd_Individual_JanFeb.rds")
dat_lines<-readRDS("03_data/Code_Neri/DataToUseSSFIndividualAllLinestring_JanFeb.rds")

# Add stepid, a unique number of step id
dat$stepid<-1:nrow(dat)
dat1<-st_as_sf(dat, coords=c("x1_", "y1_"), crs=32633)
dat2<-st_as_sf(dat, coords=c("x2_", "y2_"), crs=32633)



###############################################################################################################
# Trying to extract at from multiple rasters at once 
###############################################################################################################

# Which covariates should be extracted from raster files? 

# Elevation
# Slope
# TRI
# SR16 
# Clearcuts

dat_lines_vect<-terra::vect(dat_lines[1:20000,])

dat_lines_vect
dem<-terra::rast("/data/R/GeoSpatialData/Elevation/Fenoscandia_DEM_100m/Original/dem_100m_nosefi_float.tif")

# Run seqeunatially, not in parallelle
tictoc::tic()
a<-terra::extract(x=dem, y=dat_lines_vect)
b<-terra::extract(x=dem, y=dat_lines_vect)
tictoc::toc() # 49 secs

# Run the process in parallelle
files<-c("/data/R/GeoSpatialData/Elevation/Fenoscandia_DEM_100m/Original/dem_100m_nosefi_float.tif",
         "/data/R/GeoSpatialData/Elevation/Fenoscandia_DEM_100m/Original/dem_100m_nosefi_float.tif")

doParallel::registerDoParallel(2) 
tictoc::tic()

out<-foreach(i=files) %dopar% {
  library(terra)
  temp<-terra::rast(i)
  terra::extract(x=temp, y=dat_lines_vect)
}
tictoc::toc() # 29 secs, not exactly half the time, but this differences will likely get smaller as the number of lines increase


head(out[[1]])
head(out[[2]])

saveRDS(out, "03_data/Code_Neri/CovsForLinesJanFeb.rds")
