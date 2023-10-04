#-----
# Pre-process data and fit/re-fit model
# script adapted to the version of the reindeer models from Renewable Reindeer/ProdChange
# Bram van Moorter and Bernardo Niebuhr
#-----

require(survival)

model_refit <- function(scenario_table,
                        s_gid,
                        data,
                        model = c("suit", "perm")[1],
                        scripts_folder = "/data/P-Prosjekter/41203800_oneimpact/02_reindoc",
                        remove_residual = FALSE,
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
  # read the fitted model

  if(verbose) print("Reading model object...")
  
  # columns to use, depending on if model is "suit" or "perm"
  m_path_col <- paste0("model_", model, "_object_path")
  m_obj_col <- paste0("model_", model, "_object")

  # read model
  m_fil <- scenario_table[[m_path_col]][i]
  new_env <- new.env() # new environment
  load(file = m_fil, envir = new_env)
  mod <- get(scenario_table[[m_obj_col]][i], new_env)
  rm(new_env)

  #---
  # Make residuals
  
  sddat <- data
  
  if(model == "suit") {
    if (remove_residual){
      mod <-  update(mod, . ~ . - I(residualize("skitracks_win_high_2500 ~ pub_cabins_winter_high_2500 + pub_cabins_winter_low_5000", data = sddat)) + skitracks_win_high_2500)
    }
  }
    
  (res_formulas <- names(mod$coefficients)[grepl("residualize", names(mod$coefficients))])
  (res_formulas <- gsub("I(residualize(\"", "", res_formulas, fixed = T))
  (res_formulas <- gsub("\", data = sddat))", "", res_formulas, fixed = T))
  
  (res_labs <- gsub(" ", "", unlist(lapply(strsplit(res_formulas, "~"), function(x){x[1]}))))
  (res_labs <- paste0("res_", res_labs))
  
  for (i in c(1:length(res_formulas))){
    tmp <- residualize(res_formulas[i], data=sddat)
    sddat <- cbind(sddat, tmp)
    names(sddat)[ncol(sddat)] <- res_labs[i]
  }
    
  #---
  # Refit model
  
  if(verbose) print("Refitting model...")
    
  if(model == "suit") {
    (formul <- paste(c("strata(strat)", res_labs, names(mod$coefficients)[!grepl("residualize", names(mod$coefficients))]), collapse=" + "))
  } else {
    (formul <- paste(c("strata(gps_data_animals_id)", res_labs, names(mod$coefficients)[!grepl("residualize", names(mod$coefficients))]), collapse=" + "))
  }
  (formul <- paste0("Surv(rep(1, length(use)), use) ~ ", formul))
    
  mod <- survival::coxph(as.formula(formul), data = sddat)
  
  mod
}
