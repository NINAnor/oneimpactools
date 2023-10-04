#' Create vector layers to be edited for scenario analyses
#'
#' Create a vector layer with an empty geometry, to be edited in GIS and used to add,
#' remove, of move features, to be used in scenario analyses.
#'
#' @export
vect_table_create <- function(v_gid = NA_integer_,
                              s_gid = NA_integer_,
                              scenset_id = NA_character_,
                              scen_id = NA_character_,
                              type = NA_character_,
                              layer = NA_character_,
                              param_vals = NA_character_,
                              zoi_metric = NA_character_,
                              rast_type = NA_character_,
                              rast_col = NA_character_,
                              zoirad_suit = NA_real_,
                              zoirad_perm = NA_real_,
                              changebase = c("nochange", "remove", "add", "move")[1],
                              change = c("remove", "add", "move")[1],
                              repeated = NA_character_,
                              comment = NA_character_,
                              geom_type = c("POINT", "LINESTRING", "POLYGON",
                                            "MULTIPOINT", "MULTILINESTRING", "MULTIPOLYGON",
                                            "GEOMETRYCOLLECTION")[1],
                              geom_col = "geom",
                              crs = sf::NA_crs_) {


  # wee need at least four lines for polygons
  if(grepl("POLYGON", geom_type)) {
    polygon_id <- multipolygon_id <- rep(1, 4)
  } else {
    polygon_id <- multipolygon_id <- NA
  }

  tb <- data.frame(v_gid = v_gid,
                   s_gid = s_gid,
                   scenset_id = scenset_id,
                   scen_id = scen_id,
                   type = type,
                   layer = layer,
                   param_vals = param_vals,
                   zoi_metric = zoi_metric,
                   rast_type = rast_type,
                   rast_col = rast_col,
                   zoirad_suit = zoirad_suit,
                   zoirad_perm = zoirad_perm,
                   changebase = changebase,
                   change = change,
                   repeated = repeated,
                   comment = comment,
                   x = NA,
                   y = NA,
                   polygon_id = polygon_id,
                   linestring_id = NA,
                   multipolygon_id = multipolygon_id,
                   multilinestring_id = NA)


  if(geom_type == "POINT") {
    sf_func <- sfheaders::sf_pt
  } else {
    if(geom_type == "LINESTRING") {
      sf_func <- sfheaders::sf_line
      # tb <- tb |> dplyr::mutate(linestring_id = 1)
    } else {
      if(geom_type == "POLYGON") {
        sf_func <- sfheaders::sf_poly
        # tb <- tb |> dplyr::mutate(linestring_id  = 1)
      } else {
        if(geom_type == "MULTIPOINT") {
          sf_func <- sfheaders::sf_mpt
        } else {
          if(geom_type == "MULTILINESTRING") {
            sf_func <- sfheaders::sf_mline
          } else {
            if(geom_type == "MULTIPOLYGON") {
              sf_func <- sfheaders::sf_mpoly
            } else {
              if(geom_type == "GEOMETRYCOLLECTION") stop("Parameter type = 'GEOMETRYCOLLECTION' is not supported yet.")
            }
          }
        }
      }
    }
  }

  # create empry layer
  tb_sf <- sf_func(tb, keep = TRUE)
  # set crs
  sf::st_crs(tb_sf) <- crs
  # set geometry name
  sf::st_geometry(tb_sf) <- geom_col

  # remove columns used to create the empty layers
  for(i in c("polygon_id", "linestring_id", "multipolygon_id", "multilinestring_id")) {
    if(i %in% colnames(tb_sf)) {
      tb_sf <- tb_sf[-which(i == colnames(tb_sf))]
    }
  }

  # return
  tb_sf
}

#' Read all vectors from one or all scenarios
#'
#' @param scenario_table `[data.frame]` \cr Table of scenarios (as created
#' through the `scenario_table_create()` function).
#' @param s_gid `[numeric(1)]` \cr Scenario general ID - a numeric ID for the
#' scenario. By default, all scenarios are considered, and all vector are
#' retrieved.
#' @param con `[PqConnection]` \cr Connection to a Postgres database.
#' @param subset_scenarios `[logical(1)=TRUE]` \cr If `TRUE` (default), only the
#' vector features for the scenario defined by `s_gid` are retrieved. If `FALSE`,
#' all features from all scenario are read.
#' @param correct_baseline `[logical(1)=TRUE]` \cr If `TRUE` (default), all
#' features that should be corrected in the baseline scenario are also retrieved.
#' This includes all features for which the column `changebase` in the vector
#' attribute table are not `NULL`.
#' @param return_format `[character(1)="terra"]{"sf", "terra"}` \cr Format of the
#' output vector object in R. So far, only `"terra"` is implemented.
#'
#' @return A `SpatVector` object.
#'
#' @export
scenario_vect_read <- function(scenario_table, con,
                               s_gid = scenario_table$s_gid,
                               subset_scenarios = TRUE,
                               baseline = TRUE,
                               return_format = c("sf", "terra")[2]) {

  # get line in scenario table
  i <- which(scenario_table$s_gid %in% s_gid)

  # for each feature type
  feats <- list()
  feat_type <- c("pts", "lin", "pol")
  feat_cols <- paste0("modified_vector_", feat_type)
  for(j in seq_along(feat_cols)) {

    # check if there are more than one scenario
    f <- scenario_table[[feat_cols[j]]][i]
    if(length(f) > 1 & length(unique(f)) == 1) {
      # take the first if they are all equal
      f <- f[1]
    } else {
      # issue if no row was selected, or if there are multiple different scenarios
      if(length(f) < 1 | length(unique(f)) != 1) {
        stop("More than one scenario was selected, or no scenario was selected.")
      }
    }


    # check if there are changes of this type
    if(is.na(f)) {
      feats[[j]] <- NA
    } else {
      # read vector
      v <- oneimpactools::db_read_vect(con, f, return_format = "terra")

      # subset?
      if(subset_scenarios) {

        # for terra objects
        if(class(v) == "SpatVector") {
          # subset

          # if baseline should be corrected
          if(baseline) {
            feats[[j]] <- terra::subset(v, (v$s_gid %in% s_gid | (v$changebase != "" & !is.na(v$changebase))))
          } else {
            bas <- terra::subset(v, v$changebase != "" & !is.na(v$changebase))
            bas <- terra::subset(bas, !(bas$s_gid %in% s_gid &
                                 ((bas$changebase == "add" & bas$change == "remove") |
                                    (bas$changebase == "remove" & bas$change == "add"))))
            scen <- terra::subset(v, (v$s_gid %in% s_gid))
            feat <- rbind(bas, v)
            feats[[j]] <- feat
          } #else{
          #   # if not
          #   feats[[j]] <- terra::subset(v, (v$s_gid %in% s_gid))
          # }
        } else {
          stop("Parameter `return_format` should be 'terra'. Function for 'sf' not implemented yet.")
        }

      } else {

        # for terra objects
        if(class(v) == "SpatVector") {
          # subset

          feats[[j]] <- v

        } else {
          stop("Parameter `return_format` should be 'terra'. Function for 'sf' not implemented yet.")
        }

      }

    }

  }

  # object names
  names(feats) <- feat_type
  # return lsit of vectors
  feats
}

#' Get attribute table or values from scenario vectors
#'
#' @param vects List of vectors with points, lines, and polygons to be changed in
#' one or more scenarios.
#' @param select Names of columns of the attribute table to be selected/subsetted.
#' For the column names, see [oneimpactools::vect_table_create()].
#'
#' @export
scenario_vect_get_values <- function(vects, select = NULL) {

  # identify NAs
  isthere <- !is.na(vects)

  # get attribute tables and concatenate them
  tab <- lapply(which(isthere),
         function(i) vects[[i]] |>
           sf::st_as_sf() |>
           sf::st_drop_geometry() |>
           dplyr::mutate(geom_type = names(vects)[i])) |>
    dplyr::bind_rows() |>
    dplyr::relocate(geom_type, .before = v_gid) |>
    tibble::as_tibble()

  if(!is.null(select)) {
    tab <- tab |>
      dplyr::select(!!select)
  }

  # return
  tab
}

#' @export
scenario_vect_subset <- function(vects, s_gid = 1,
                                 baseline = TRUE) {

  # for each vector type
  feats <- list()
  for(j in seq_along(vects)) {

    v <- vects[[j]]

    # for terra objects
    if(class(v) == "SpatVector") {
      # subset

      # if it is baseline scenario
      if(baseline) {
        feats[[j]] <- terra::subset(v, (v$s_gid %in% s_gid | (v$changebase != "" & !is.na(v$changebase))))
      } else{
        # if not
        bas <- terra::subset(v, v$changebase != "" & !is.na(v$changebase))
        bas <- terra::subset(bas, !(bas$s_gid %in% s_gid &
                                      ((bas$changebase == "add" & bas$change == "remove") |
                                         (bas$changebase == "remove" & bas$change == "add"))))
        scen <- terra::subset(v, (v$s_gid %in% s_gid))
        feat <- rbind(bas, scen)
        feats[[j]] <- feat
      }
    } else {

      # for NA (non modified) types of features
      if(is.na(v)) {
        feats[[j]] <- NA
      } else {
        # for sf object
        stop("The object `vects` should be a list of 'SpatVector' objects. Function for 'sf' not implemented yet.")
      }

    }
  }

  names(feats) <- names(vects)
  feats
}
