#' Get ZOI radius from covariates in a model
#'
#' @param mod Model.
#' @param variable Vector of variable names or parts of variable names, to be used
#' to select them from the model formula.
#'
#' @export
model_get_zoi <- function(mod, variable, season, model, separator = "_") {

  # get covariates
  covars <- base::all.vars(mod$formula)[-1]

  # variable <- layers
  # season <- ss

  # save the original variables
  variable_orig <- variable

  # check variables
  if(!grepl("sum|summer", season, ignore.case = TRUE)) {
    variable <- sub(pattern = "summer", replacement = "winter", variable)
    variable <- sub(pattern = "roads_low", replacement = "roads_winter_low", variable)
    variable <- sub(pattern = "roads_high", replacement = "roads_winter_high", variable)
    if(grepl("cal|calving", season, ignore.case = TRUE)) {
      variable <- sub(pattern = "skitracks_high", replacement = "skitracks_cal_high", variable)
      variable <- sub(pattern = "skitracks_low", replacement = "skitracks_cal_low", variable)
    }
    if(grepl("win|winter", season, ignore.case = TRUE)) {
      variable <- sub(pattern = "skitracks_high", replacement = "skitracks_win_high", variable)
      variable <- sub(pattern = "skitracks_low", replacement = "skitracks_win_low", variable)
    }
  } else {
    variable <- sub(pattern = "roads_low", replacement = "roads_summer_low", variable)
    variable <- sub(pattern = "roads_high", replacement = "roads_summer_high", variable)
  }

  ############
  # change that later, it is too bad - here and below
  # trick because name of variables changed
  for(ss in c("summer", "winter")) {
    for(tp in c("high", "low")) {
      if(length(ind <- grep(paste0("roads_", ss, "_", tp), variable)) > 0) {
        variable_orig <- c(variable_orig, gsub(pattern = "res_", replacement = "", variable_orig[ind]))
        variable <- c(variable,
                      sub(paste0("roads_", ss), paste0(ss, "_roads"), variable[ind]) |>
                        gsub(pattern = "res_", replacement = ""))
      }
      if(season == "win" & ss == "winter" & model == "suit" & length(ind <- grep(paste0("pub_cabins_", ss, "_", tp), variable)) > 0) {
        variable_orig <- c(variable_orig, gsub(pattern = "", replacement = "", variable_orig[ind]))
        variable <- c(variable,
                      gsub(pattern = paste0(ss, "_", tp), replacement = "win", variable[ind]))
      }
    }
  }

  #cov_sub[ind] <- paste0("roads_", ss, "_", tp, "_cross")
  # if(any(grepl(paste0("roads_", ss, "_", tp), variable))) variable <- c(variable, paste0(ss, "_roads_", tp))

  # # adding pub_cabins_ for winter
  # variable <- c(variable, )

  # subset the ones of interest
  cov_sub <- grep(paste0(variable, collapse = "|"), covars, value = T)
  # order of the variables found
  ord <- lapply(variable, function(x) grep(x, cov_sub))
  # variables found
  found <- sapply(ord, function(x) length(x) > 0)
  # warning if some of the variables was not found
  if(any(found == FALSE)) {
    missing <- variable[!found]
    warning(paste0("The variable(s) '", paste(missing, collapse = ","),"' was not found in the model."))
  }

  ############
  # change that later, it is too bad - here and up there
  # trick because name of variables changed
  # for(ss in c("summer", "winter"))
  #   for(tp in c("high", "low"))
  #     if(length(ind <- grep(paste0(ss, "_roads_", tp), cov_sub)) > 0) cov_sub[ind] <- paste0("roads_", ss, "_", tp, "_cross")

  # get zoi radius
  zoi_radius <- base::strsplit(cov_sub, split = separator) |>
    sapply(function(x) x[length(x)])

  # replace zoi = 100 when it is cross
  zoi_radius <- ifelse(zoi_radius == "cross", "100", zoi_radius)

  # repeat or omit variables
  var_out <- base::rep(variable, sapply(ord, length)) |>
    sub(pattern = "res_", replacement = "") #|>
    # sub(pattern = "log_", replacement = "")
  var_orig_out <- base::rep(variable_orig, sapply(ord, length)) |>
    sub(pattern = "res_", replacement = "")

  #############
  # change that later, it is too bad - here and up there
  # trick because name of variables changed
  for(ss in c("summer", "winter")) {
    for(tp in c("high", "low")) {
      if(length(ind <- grep(paste0(ss, "_roads_", tp), var_out)) > 0)
        var_out[ind] <- sub(paste0(ss, "_roads"), paste0("roads_", ss), var_out[ind])
      if(length(ind <- grep(paste0("pub_cabins_win"), var_out)) > 0)
        for(ii in ind)
          if(season == "win" & model == "suit" & grepl(paste0(ss, "_", tp), var_orig_out[ii]))
            var_out[ii] <- sub(paste0("_win$"), paste0("_winter", "_", tp), var_out[ii])
    }
  }

  # set order
  ord <- unlist(ord[found])

  # get vars with no ZOI
  cov_pre <- sapply(seq_along(zoi_radius), function(i) sub(paste0(zoi_radius[i],"|cross"), "", cov_sub[i]))

  # return tibble
  tibble::tibble(variable_orig = var_orig_out,
                 variable = var_out, covariate_name = cov_sub[ord],
                 prefix = cov_pre[ord], zoi_radius = zoi_radius[ord])
}
