#-----
# Predict model in space
# Script adapted to the version of the reindeer models from Renewable Reindeer/ProdChange
# Bram van Moorter and Bernardo Niebuhr
#-----

require(terra)

model_predict <- function(scenario_table, s_gid, mod, dat, dat_orig,
                          model = c("suit", "perm")[1],
                          input_type = c("df", "rast")[1], # only df implemented
                          output_type = c("df", "rast")[2],
                          baseline_max = NULL,
                          gridalign = TRUE, 
                          coords = c("x33", "y33"),
                          crs = NULL,
                          plotit = FALSE,
                          scripts_folder = "/data/P-Prosjekter/41203800_oneimpact/02_reindoc",
                          verbose = FALSE) {

  #---
  # source code
  source(paste0(scripts_folder, "/Analyses/RSF_ResourceSelection/residualize.r"))
  
  # get line in scenario table
  i <- which(scenario_table$s_gid == s_gid)
  
  # get season
  season <- scenario_table[["season"]][i]
  seas <- ifelse(grepl("Vinter|Winter|Win", season, ignore.case = TRUE), "win",
                 ifelse(grepl("Kalving|Calving|Cal", season, ignore.case = TRUE), "cal", "sum"))
  
  #---
  # prepare
  
  if(verbose) print("Preparing data...")
  
  # get correct residual variables
    
  # columns to use, depending on if model is "suit" or "perm"
  m_path_col <- paste0("model_", model, "_object_path")
  m_obj_col <- paste0("model_", model, "_object")
  
  # read model
  m_fil <- scenario_table[[m_path_col]][i]
  new_env <- new.env() # new environment
  load(file = m_fil, envir = new_env)
  old_mod <- get(scenario_table[[m_obj_col]][i], new_env)
  rm(new_env)
  
  (res_formulas <- names(old_mod$coefficients)[grepl("residualize", names(old_mod$coefficients))])
  (res_formulas <- gsub("I(residualize(\"", "", res_formulas, fixed=T))
  (res_formulas <- gsub("\", data = sddat))", "", res_formulas, fixed=T))

  (res_labs <- gsub(" ", "", unlist(lapply(strsplit(res_formulas, "~"), function(x){x[1]}))))
  (res_labs <- paste0("res_", res_labs))

  # get correct residualized variables 
    
  # residuals dat_orig
  for (j in c(1:length(res_formulas))){
    tmp <- residualize(res_formulas[j], data=dat_orig)
    dat_orig <- cbind(dat_orig, tmp)
    names(dat_orig)[ncol(dat_orig)] <- res_labs[j]
  }
  
  # create residuals on env data
  for (j in c(1:length(res_formulas))){
    tmp <- residualize_predict(res_formulas[j], dat_orig, dat)
    dat <- cbind(dat, tmp)
    names(dat)[ncol(dat)] <- res_labs[j]
  }

  if(model == "suit") {
    (formul <- paste(c("strata(strat)", res_labs, names(mod$coefficients)[!grepl("res_", names(mod$coefficients))]), collapse=" + "))
  } else {
    (formul <- paste(c("strata(gps_data_animals_id)", res_labs, names(mod_ssf$coefficients)[!grepl("res_", names(mod_ssf$coefficients))]), collapse=" + "))
  }
  (formul <- paste0("Surv(rep(1, length(use)), use) ~ ", formul))

  # get covariates in the model
  if(model == "suit") cols <- strsplit(formul, split="[+]")[[1]] else
    cols <- strsplit(strsplit(formul, split="~")[[1]][-1], split="[+]")[[1]]
  cols <- gsub(" ", "", cols)[-1]
  cols <- cols[!grepl("^2", cols, fixed=T)]
  if(model == "suit") cols <- ifelse(cols=="I(exp(slope))", "slope", cols) else
    cols <- ifelse(cols=="I(exp(max_slope))", "max_slope", cols)
  cols
  
  # get data for prediction
  d <- dat[,cols]
  d$use <- rep(0, nrow(d))
  d$points_id <- dat$points_id
  nrow(d)

  nrow(d <- na.omit(d))
  
  #---
  # prepare model matrix
  vars <- strsplit(formul, split="[+]")[[1]][-1]
  formul <- paste0(vars, collapse = "+")
  formul <- paste0("use~", formul)
  mm <- model.matrix(as.formula(formul), data=d)
  nrow(mm)

  #---
  # predict
  
  if(verbose) print("Predicting...")
  
  coefs <- c(0, coefficients(mod))
  predvals <- (mm %*% coefs)

  d$linpred <- predvals[,1]
  head(d)

  #---
  # come back to space
  
  # read spatial data again
  # columns to use, depending on if data is "suit" or "perm"
  d_path_col <- paste0("data_env_object_path")
  d_obj_col <- paste0("data_env_object")
  # read data
  d_fil <- scenario_table[[d_path_col]][i]
  new_env <- new.env() # new environment
  load(file = d_fil, envir = new_env)
  grd <- get(scenario_table[[d_obj_col]][i], new_env) # get data from new environment
  rm(new_env)
  # put prediction values in grid
  grd$linpred <- NA
  grd[(grd$points_id %in% d$points_id), c("linpred")] <- d[, c("linpred")]

  #---
  # output object
  out <- list(grid = grd, baseline_max = baseline_max, rast = NULL)
  
  #---
  # rasterize
  if(output_type == "rast"){
  
    if(verbose) print("Rasterizing...")
    
    # align pixels
    if (gridalign) { grd[, coords] <- grd[, coords]-50 }
    # rasterize
    grd_rast <- terra::rast(grd[, c(coords, "linpred")], type = "xyz", crs = crs)
    # exp values
    grd_rast <- exp(grd_rast)
    #quantile(exp(grd$linpred), prob = 0.975, na.rm = T)
    # truncate at 97.5% quantile of the baseline scenario
    if(is.null(baseline_max)) {
      baseline_max <- as.numeric(terra::global(grd_rast, fun = quantile, prob = 0.975, na.rm = T))
      out$baseline_max <- baseline_max
    }
    grd_rast <- grd_rast/baseline_max
    grd_rast <- terra::ifel(grd_rast > 1, 1, grd_rast)
    # grd_rast <- oneimpact::raster_rescale(grd_rast, to = c(0, baseline_max))
    names(grd_rast) <- model
    
    if(plotit) plot(grd_rast)
    
    out$rast <- grd_rast
  }
  
  # return
  out
}
