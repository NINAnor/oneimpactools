#-----
# Load and prepare data
# Script adapted to the version of the reindeer models from Renewable Reindeer/ProdChange
# Bram van Moorter and Bernardo Niebuhr
#-----

#' This function reads the data, selects the season, and prepares the data for 
#' analysis or prediction (e.g. standardizing covariates). It returns a dataset
#' with standardized variables, prepared for analysis (in case of 
#' `data = "suit"` or `data = "perm"`) or prediction (in case of `data = "env"`).

# data = suit and perm not implemented
# but could be taken from
#' scenario_rast = list of rasters to be added or removed

data_load_prepare <- function(scenario_table, s_gid,
                              data = c("env", "suit", "perm")[1],
                              model = c("suit", "perm")[1],
                              scenario_rast = NULL,
                              coords = c("x33", "y33"),
                              scripts_folder = "/data/P-Prosjekter/41203800_oneimpact/02_reindoc",
                              verbose = FALSE) {

  #---
  # source previous functions
  if(model == "suit") {
    source(paste0(scripts_folder, "/Analyses/RSF_ResourceSelection/data_prep.r"))
  } else {
    source(paste0(scripts_folder, "/Analyses/SSF_StepSelection/data_prep_ssf.r"))
  }
  source(paste0(scripts_folder, "/Analyses/RSF_ResourceSelection/standardize.r"))
  source(paste0(scripts_folder, "/Analyses/RSF_ResourceSelection/NM_to_1M.r"))

  # get line in scenario table
  i <- which(scenario_table$s_gid == s_gid)

  # get season
  season <- scenario_table[["season"]][i]
  seas <- ifelse(grepl("Vinter|Winter|Win", season, ignore.case = TRUE), "win",
                 ifelse(grepl("Kalving|Calving|Cal", season, ignore.case = TRUE), "cal", "sum"))

  #---
  # read the fitted model

  if(verbose) print("Reading data object...")

  # columns to use, depending on if data is "suit" or "perm"
  d_path_col <- paste0("data_", data, "_object_path")
  d_obj_col <- paste0("data_", data, "_object")

  # read data
  d_fil <- scenario_table[[d_path_col]][i]
  new_env <- new.env() # new environment
  load(file = d_fil, envir = new_env)
  dat <- get(scenario_table[[d_obj_col]][i], new_env) # get data from new environment
  rm(new_env) # close environment to avoid duplicated data
  # filter season
  if(data != "env") dat <- dat[[seas]]

  # also read the original data to get the means and sds if data == "env"
  if(data == "env") {
    d_path_col_orig <- paste0("data_", model, "_object_path")
    d_obj_col_orig <- paste0("data_", model, "_object")

    # read data
    d_fil_orig <- scenario_table[[d_path_col_orig]][i]
    new_env <- new.env()
    load(file = d_fil_orig, envir = new_env)
    dat_orig <- get(scenario_table[[d_obj_col_orig]][i], new_env)
    rm(new_env)
    # filter season
    dat_orig <- dat_orig[[seas]]
  }

  #---
  # prepare data
  
  if(verbose) print("Preparing data...")

  # for env data
  if(data == "env") {

    # compute mean/sf from original data
    # other filters
    dat_orig <- dat_orig[dat_orig$herd != "Sollia",]
    # for suitability model
    if(model == "suit") {
      # prepare dataset
      dat_orig <- data_prep(dat_orig, fixwind = F)
      
      stdf <- dfstandardize_params(dat_orig[dat_orig$use==0, c(6:ncol(dat_orig))], narm = TRUE)
    } else {
      # prepare dataset
      dat_orig <- data_prep(dat_orig)
      
      # for permeability model
      stdf <- dfstandardize_params(dat_orig[dat_orig$use==0, c(7, 10:ncol(dat_orig))], narm = TRUE)
    }
    
    # if we are already applying local changes for a scenario
    # we change the annotated data
    if(!is.null(scenario_rast)) {
      
      # loop over features to be changed - added and removed
      # for (ch in seq_along(scenario_rast)){
      for (ch in c("add", "remove")){
        
        # If there are any layer
        if (length(scenario_rast[[ch]]) > 0) {
          
          # get values
          vals <- terra::extract(scenario_rast[[ch]], dat[, coords], ID = FALSE)  
          # correct variable names
          nam <- colnames(vals)
          nam <- sub("res_", "", nam)
          nam <- sub("log_", "", nam)
          # summary(vals)
          # summary(dat[,nam])
          dat[, nam] <- dat[, nam] + vals
          dat[, nam] <- sapply(1:length(nam), function(x) ifelse(dat[, nam[x]] < 0, 0, dat[, nam[x]]))
        }
      }
      
    }
    
    # on env data
    # run data_prep function:
    if(model == "suit") {
      dat <- data_prep(dat, prediction = T, fixwind = F)
    } else {
      dat$length_water <- 0
      dat$length_water <- ifelse(dat$dist_river < 0.01, 100, dat$length_water)
      dat$length_water <- ifelse(dat$dist_river >= 0.01 & dat$dist_river < 25, 75, dat$length_water)
      dat$length_water <- ifelse(dat$dist_river >= 25 & dat$dist_river < 50, 50, dat$length_water)
      dat$length_water <- ifelse(dat$dist_river >= 50 & dat$dist_river < 75, 25, dat$length_water)
      
      dat$step_length <- 100
      
      dat <- data_prep(dat, prediction = T)
    }
    
    if(model == "perm") {
      if (seas=="win"){
        dat$winter_roads_high_cross <- ifelse(dat$roads_winter_high_100==1, 1, 0)
        dat$winter_roads_low_cross <- ifelse(dat$roads_winter_low_100==1, 1, 0)
      }
      if (seas!="sum"){
        dat$summer_roads_high_cross <- ifelse(dat$roads_summer_high_100==1, 1, 0)
        dat$summer_roads_low_cross <- ifelse(dat$roads_summer_low_100==1, 1, 0)
      }
    }
    
    # standardize the variables:
    sddat <- dfstandardize(dat[,(names(dat) %in% stdf$column_name)], stdf)
    sddat$points_id <- dat$points_id

  } else {
    
    # other filters
    dat <- dat[dat$herd!="Sollia",] # REMOVE Sollia, as we have only 1 animal and we likely have wrong avaialbility
    
    # run data prep and standardize the variables:
    if(data == "suit") {
      # run data_prep function:
      dat <- data_prep(dat, fixwind = F)
      
      # standardize covariates
      stdf <- dfstandardize_params(dat[dat$use==0, c(6:ncol(dat))])
      sddat <- dfstandardize(dat[,c(6:ncol(dat))], stdf)
      sddat <- cbind(dat[,c(1:5)], sddat)
    } else {
      if(data == "perm") {
        # run data_prep function:
        dat <- data_prep(dat)
        
        # standardize covariates
        if (seas=="sum"){
          stdf <- dfstandardize_params(dat[dat$use==0,c(7, 10:ncol(dat))]) # do not standardize the first 9 columns, except step length
          sddat <- dfstandardize(dat[,c(7, 10:ncol(dat))], stdf)
          sddat <- cbind(dat[,c(1:6,8:9)], sddat) # paste back unstandardized var (factors)
        } else{
          stdf <- dfstandardize_params(dat[dat$use==0,c(7, 10:ncol(dat))]) # do not standardize the first 9 columns, except step length
          sddat <- dfstandardize(dat[,c(7, 10:ncol(dat))], stdf)
          sddat <- cbind(dat[,c(1:6,8:9)], sddat) # paste back unstandardized var (factors)
        }
      }
    }
  }

  #---
  # make case-control dataset:
  if(data == "suit") {
    set.seed(0)
    sddat <- NM_to_1M(sddat, M=8)
  }
  
  sddat
}
