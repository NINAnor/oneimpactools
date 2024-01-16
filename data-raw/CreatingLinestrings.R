# This script extracts the covariates for step selection functions

rm(list=ls()) 

library(sf)
library(raster)
library(plyr)
library(dplyr)
library(parallel)

if(is.null(sessionInfo()$otherPkgs$rgrass7)){
  library(rgrass7)
  library(NinaR)
  grassConnect() 
}



###############################################################################################################
# Import data and convert to spatial lines
###############################################################################################################

dat<-readRDS("03_data/Code_Neri/DataToUseSSFAllInd_Individual_DecMarch.rds")

# Add stepid, a unique number of step id
dat$stepid<-1:nrow(dat)
dat1<-st_as_sf(dat, coords=c("x1_", "y1_"), crs=32633)
dat2<-st_as_sf(dat, coords=c("x2_", "y2_"), crs=32633)

# # Test different approaches to build linestrings
# # Lapply
# tictoc::tic()
# linestrings <- lapply(X = 1:50000, FUN = function(x) {
#   
#   pair <- st_combine(c(dat1[x,]$geometry, dat2[x,]$geometry))
#   line <- st_cast(pair, "LINESTRING")
#   return(line)
# })
# tictoc::toc()
# 
# # Mclapply - parallization
# library(parallel)
# tictoc::tic()
# linestrings <- mclapply(X = 1:1000000, mc.cores=8, FUN = function(x) {
#   
#   pair <- st_combine(c(dat1[x,]$geometry, dat2[x,]$geometry))
#   line <- st_cast(pair, "LINESTRING")
#   return(line)
# })
# tictoc::toc()
# 
# # Parlapply - parallization
# library(snow)
# dat11<-dat1[1:1000000,]
# dat22<-dat2[1:1000000,]
# cl <- snow::makeCluster(8)
# snow::clusterExport(cl, c('dat11', 'dat22'))
# 
# tictoc::tic()
# linestrings <- snow::parLapply(cl=cl, x = 1:100000, fun = function(x) {
#   library(sf)
#   pair <- st_combine(c(dat11[x,]$geometry, dat22[x,]$geometry))
#   line <- st_cast(pair, "LINESTRING")
#   return(line)
# })
# tictoc::toc()
# 
# stopCluster(cl)

# I go for mclapply, even though this is not the fastest one. It does not require the data to be loaded to a cluster,
# like snow and parLapply

# Mclapply - parallization
library(parallel)
tictoc::tic()
linestrings <- mclapply(X = 1:nrow(dat), mc.cores=10, FUN = function(x) {
  
  pair <- st_combine(c(dat1[x,]$geometry, dat2[x,]$geometry))
  line <- st_cast(pair, "LINESTRING")
  return(line)
})
tictoc::toc()

# One MULTILINESTRING object with all the LINESTRINGS
linestrings.all <- st_sfc(do.call("rbind", linestrings))

linestrings_sf<-st_sf(animal_id=dat$animal_id, step_id2=dat$step_id_2,  geom=linestrings.all, crs=32633)

linestrings_sf$stepid_unik<-1:nrow(linestrings_sf)

saveRDS(linestrings_sf, "03_data/Code_Neri/DataToUseSSFIndividualAllLinestring_DecMarch.rds")
st_write(linestrings_sf, dsn="03_data/Code_Neri/DataToUseSSFIndividualAllLinestring_DecMarch.shp", append=FALSE)
