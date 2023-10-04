
#' @export
calc_zoi_local <- function(vects, ref_raster, mod, scenario_table, s_gid = 1,
                           model = c("suit", "perm")[1],
                           baseline = FALSE,
                           where = c("R", "GRASS")[1],
                           cross_chr = "cross", # word fot identifying a crossing variable
                           separator = "_",
                           verbose = FALSE,
                           g_module = c("r.mfilter", "r.resamp.filter", "r.neighbors")[2],
                           output_type = c("cumulative_zoi", "density")[1]) {

  # get values form vectors
  vect_vals <- oneimpactools::scenario_vect_get_values(vects)

  # organize types of features to be processed
  if(baseline) {
    to_process <- vect_vals |>
      dplyr::filter(!is.na(changebase) & changebase != "") |>
      dplyr::group_by(changebase, geom_type, layer) |>
      dplyr::summarise(n = n()) |>
      dplyr::mutate(change_col = "changebase") |>
      dplyr::rename(change_type = changebase)
  } else {
    # baseline features to be corrected
    to_process_base <- vect_vals |>
       dplyr::filter(!is.na(changebase) & changebase != "") |>
       dplyr::group_by(changebase, geom_type, layer) |>
       dplyr::summarise(n = n()) |>
       dplyr::mutate(change_col = "changebase") |>
       dplyr::rename(change_type = changebase)
    # scenario features to be changed
    to_process_scen <- vect_vals |>
        dplyr::filter(!is.na(change) & change != "") |>
        dplyr::group_by(change, geom_type, layer) |>
        dplyr::summarise(n = n()) |>
        dplyr::mutate(change_col = "change") |>
        dplyr::rename(change_type = change)
    # all
    (to_process <- dplyr::bind_rows(to_process_base,
                                    to_process_scen) |>
        dplyr::arrange(change_type, geom_type, layer) |>
        dplyr::select(-c(change_col, n)) |>
        unique())
  }

  # name of output list
  out_rast <- list(add = list(), remove = list())

  # counters
  counter_add <- 1
  counter_remove <- 1

  # for each geom_type, layer, and type of change
  for(i in 1:nrow(to_process)) {

    # geom_type
    gt <- to_process$geom_type[i]
    # layer
    ly <- to_process$layer[i]
    # change
    ch <- to_process$change_type[i]

    # select vector
    if(length(s_gid) == 1) {
      v <- vects[[gt]]
      if(baseline) {
        v <- terra::subset(v, v$layer == ly & v[["changebase"]] == ch)
      } else {
        v <- terra::subset(v, v$layer == ly & ( (v[["changebase"]] == ch & (v$s_gid != s_gid)) | (v[["change"]] == ch & v$s_gid == s_gid) ))
      }
    } else {
      v <- terra::vect()
      for(ss in s_gid) {
        vv <- vects[[gt]]
        if(scenario_info$baseline[scenario_info$s_gid == ss]) {
          vv <- terra::subset(vv, vv$layer == ly & vv[["changebase"]] == ch)
        } else {
          vv <- terra::subset(vv, vv$layer == ly & ( (vv[["changebase"]] == ch & (vv$s_gid != ss)) | (vv[["change"]] == ch & vv$s_gid == ss) ))
        }
        v <- rbind(v, vv)
      }
    }


    # remove repeated features
    duplicated <- sf::st_as_sf(v) |> dplyr::select(geometry) |> duplicated()
    v <- v[!duplicated,]

    # check if there were any features of this type
    if(length(v) > 0) {

      #---
      # get vector info

      # rasterization type
      # it does different things depending on rast_type
      rast_type <- trimws(v$rast_type[1])
      # rasterization column
      rast_col <- trimws(v$rast_col[1])
      # get zoi type
      zoi_metric <- v$zoi_metric[1]
      # get zoi radius
      zoi_radius <- v[[paste0("zoirad_", model)]][[1]][1]
      # reference to get parameter values
      param_val <- v$param_vals[1]

      #---
      # rasterize

      # for 'raw' rasterization
      if(rast_type == "raw") {

        # rasterize directly
        r <- terra::rasterize(v, ref_rast, background = 0)

      }

      if(rast_type == "count") {

        # rasterize through count
        r <- terra::rasterize(v, ref_rast, fun = length)

      }

      if(rast_type == "value") {

        # get column names
        cols <- param_val |>
          strsplit(";") |>
          dplyr::first() |>
          strsplit("=") |>
          sapply(function(x) x[1])

        # separate columns
        terra::values(v) <- terra::values(v) |>
          tidyr::separate_wider_delim(cols = "param_vals", delim = ";",
                                      names = cols) |>
          tidyr::separate_wider_delim(cols = cols, delim = "=",
                                      names = c("__name", ""), names_sep = "") |>
          dplyr::select(-contains("__name")) |>
          mutate(across(all_of(cols), ~ as.numeric(.x)))

        # use column to rasterize
        r <- terra::rasterize(v, ref_rast, field = rast_col, background = 0)
      }

      # check rasterization types
      rast_types <- c("raw", "value", "count")
      if(!(rast_type %in% rast_types))
        stop(paste0("Invalid parameter for the type of rasterization. The value of the column 'rast_type' in your vector layer should be one of: ", paste(rast_types, collapse = ","),"."))

      # upload map if in GRASS
      if(where %in% c("GRASS", "grass", "GRASS GIS", "grass gis")) {
        # upload map
        tmp_name <- "tmp_map_r_compute_zoi"
        rgrass::write_RAST(r, vname = tmp_name, flags = c("o", "overwrite"))
        r <- tmp_name
        # option
        g_input_as_region = TRUE
      } else {
        g_input_as_region = FALSE
      }

      #---
      # get name of the variable within the model
      mod_vars <- model_get_zoi(mod, ly, separator = separator)

      # for each model variable related to this layer
      for(mm in 1:nrow(mod_vars)) {

        zoi_rad <- as.numeric(mod_vars$zoi_radius[mm])

        #---
        # compute zoi
        if(zoi_metric == "cumulative") {
          # cumulative
          zoi <- oneimpact::calc_zoi_cumulative(r, radius = zoi_rad, type = "bartlett",
                                                where = where,
                                                output_type = output_type,
                                                g_input_as_region = g_input_as_region,
                                                g_module = g_module,
                                                g_overwrite = TRUE)
        } else {

          if(zoi_metric == "nearest") {
            # nearest
            zoi <- oneimpact::calc_zoi_nearest(r, radius = zoi_rad, type = "bartlett",
                                               where = where,
                                               g_input_as_region = g_input_as_region,
                                               g_module = g_module,
                                               g_overwrite = TRUE)
          }

        }

        #---
        # set raster to list

        # check if we should add or remove
        if(ch == "add") {
          cc <- counter_add
          counter_add <- counter_add + 1
          signal = 1
        } else {
          cc <- counter_remove
          counter_remove <- counter_remove + 1
          signal = -1
        }

        # if it is a zoi variable (zoi or cross-100m)
        if(!is.na(zoi_rad)) {

          # retrieve zoi map from GRASS
          if(where %in% c("GRASS", "grass", "GRASS GIS", "grass gis")) {
            zoi_name <- zoi
            zoi <- rgrass::read_RAST(zoi)
          }

          names(zoi) <- paste0(mod_vars$variable[mm], separator, mod_vars$zoi_radius[mm])
          out_rast[[ch]][[cc]] <- zoi*signal
          names(out_rast[[ch]])[cc] <- paste0(mod_vars$variable[mm], separator, mod_vars$zoi_radius[mm])

        } else {

          stop("Error: raw vector or raster: to be implemented.")
          # maybe this
          # out_rast[[ch]][[cc]] <- r*signal
          # names(out_rast[[ch]])[cc] <- nm
        }

      }

      # remove in grass
      if(where %in% c("GRASS", "grass", "GRASS GIS", "grass gis")) {
        sapply(c(r, zoi_name), function(x) rgrass::execGRASS("g.remove", type = "raster",
                                                             name = x, flags = "f"))
      }

      # remove rast and vector
      rm(v, r, zoi)

    } else {

      # if v is empty
      warning(paste0("No features of type ", ly, " to be ", ch, "ed in scenario s_gid = ", s_gid, "."))

    }



  }

  # concatenate rasters
  for(i in seq_along(out_rast)) {
    if(length(out_rast[[i]]) > 0) {
      out_rast[[i]] <- terra::rast(out_rast[[i]])
    } else {
      out_rast[[i]] <- NULL
    }

  }

  # return list of rasters to add and remove
  out_rast
}
