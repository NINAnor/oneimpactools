# This script upload linestrings and extract covariates for lines in POSTGIS

rm(list=ls()) 

library(sf)
library(raster)
library(rgeos)
library(plyr)
library(dplyr)
library(RPostgres)
library(postGIStools)
library(RPostgreSQL)
library(pool)

##############################################################################################
# Import lines into GISDATA 
##############################################################################################
dat_lines<-readRDS("03_data/Code_Neri/DataToUseSSFIndividualAllLinestring_DecMarch.rds")

# Fetching munipalitites from the gisdata server, to remove observations outside Norway
drv<-dbDriver("Postgres") 

# Connect
con<-dbConnect(drv,dbname="gisdata",user="neri.thorsen",
               password="neri",host="GISDATA-DB.NINA.NO", port=5432)


# Update sa.lynx in the postgis DB
write_sf(dsn = con,
         obj = st_transform(dat_lines, 25833),
         Id(schema = "neri.thorsen", table = "lines_moose"),
         temporary = F,
         delete_layer = T)

dbDisconnect(con)

##############################################################################################
# Intersecting against the data on gisdata
##############################################################################################
# Lookup R -- PostGis at GIS wiki

# ----------------------------- Connect to gisdata

# # Set connection parameters
pg_drv <- RPostgreSQL::PostgreSQL()
pg_host <- "gisdata-db.nina.no"
pg_db <- 'gisdata'
pg_user <- 'neri.thorsen'
pg_password <- 'neri'

# PG connection
pool <- dbPool(
  drv = pg_drv,
  dbname = pg_db,
  host = pg_host,
  user = pg_user,
  password = pg_password,
  idleTimeout = 36000000)

con <- poolCheckout(pool)

# ------------ Query the number of road crossings

tictoc::tic()
nroads_query<-"SELECT l1.stepid_unik, SUM(st_numgeometries(st_intersection(l1.geom, p2.geom)))
FROM \"neri.thorsen\".lines_moose AS l1
JOIN \"Topography\".\"Norway_FKB_Veg_polygons\" AS p2 ON st_intersects(l1.geom, p2.geom)
GROUP BY l1.stepid_unik"

nroads<-sf::st_read(dsn = con, query=nroads_query)
colnames(nroads)[2]<-"n_crossings"
nroads
tictoc::toc()

dat_lines2<-merge(as.data.frame(dat_lines)[,-which(colnames(dat_lines)%in%"geom")], nroads, by="stepid_unik", all.x=TRUE)
dat_lines2[is.na(dat_lines2$n_crossings),"n_crossings"]<-0

saveRDS(dat_lines2, "03_data/Code_Neri/DataToUseSSFIndividualAllLinestring_DecMarch_withCovs.rds")

# ------------ Query the proportions of landcover along the path

# Run intersection in POSTGIS takes for ever, something wrong?
#tictoc::tic()
#landcover_query<-"SELECT landcover_int.*
#FROM \"neri.thorsen\".lines_moose AS l1, \"LandCover\".\"Norway_AR50\" AS p2,
#st_intersection(l1.geom, p2.geom) AS landcover_int"
#
#landcover<-sf::st_read(dsn = con, query=landcover_query)
#
#tictoc::toc()

# Trying to do it in parallell outside postgis
ar50<-st_read("/data/R/GeoSpatialData/LandCover/Norway_LandResource_AR50/Original/Arealressurskart - AR50/0000_25833_ar50_gdb.gdb")
dat_lines2<-st_transform(dat_lines, 25833)

tictoc::tic()
out<-mclapply(as.character(unique(dat_lines$animal_id)), mc.cores=8, FUN=function(i){
  st_intersection(dat_lines2[dat_lines2$animal_id%in%i,], ar50)
})
tictoc::toc()

tictoc::tic()
out2<-do.call("rbind", out)
tictoc::toc()

all(unique(out2$stepid_unik)%in%unique(dat_lines2[dat_lines2$animal_id%in%as.character(unique(dat_lines$animal_id)[1:8]),]$stepid_unik))

saveRDS(landcover, "03_data/Code_Neri/IntersectedLinesAgainstLandcover.rds")



