#' Create a new scenario table
#'
#' @description
#' Functions to create and manage tables to organize and describe scenarios and
#' sets of scenarios in R.
#'
#' [oneimpactools::scenario_table_create()] creates a table to organize and
#' describe scenarios and sets of scenarios.
#' If no parameters are given, the table is initialized with NA values, which
#' can be updated later using [oneimpactools::scenario_table_update()].
#'
#' [oneimpactools::scenario_table_update()] updates a table of scenarios, filling
#' or replacing values of existing rows/scenarios in the table. The reference to
#' the scenario(s) to be updated is given by the scenario ID(s)
#' (`s_gid` or `scen_id` columns) and is defined by the `ref_column` parameter.
#' If the scenario already has a non-null value for one of the columns to be
#' updated, the `overwrite` option might be used to replace the existing values.
#'
#' [oneimpactools::scenario_table_add()] adds new lines/scenarios to an existing
#' table of scenarios.
#'
#' [oneimpactools::get_max_s_gid()] returns the maximum scenario gid (s_gid) of
#' an exiting table of scenarios.
#'
#' @details
#' The parameters for region must be re-set for external vectors or bbox. We
#' can possibly add new columns region_path, region_file for external vectors
#' and region_bbox with 4 numbers for bbox.
#'
#' @param s_gid `[numeric(1)]` \cr Scenario general ID - a numeric ID for the
#' scenario. It should be unique between all scenarios and scenario sets.
#' @param scenset_id `[numeric(1)=NA]` \cr Scenario set ID - a numeric ID for the
#' scenario set.
#' @param scenset_name `[character(1)=NA]` \cr Scenario set name. A character name
#' for the scenario set. It should be as informative as possible (e.g. including
#' the project name, study area, sub area, species, what is done in the scenario
#' set).
#' @param scen_id `[numeric(1)=NA]` \cr Scenario ID. It should be unique within a
#' scenario set, but in contrast to s_gid, can be repeated in different scenario
#' sets.
#' @param scen_name `[character(1)=NA]` \cr Scenario name. A character name for the
#' scenario. It should be as informative as possible (e.g. including the project
#' name, study area, sub area, species, what is done in the scenario).
#' @param scen_nickname `[character(1)=NA]` \cr Scenario "nickname". A character
#' name for the scenario which is shorter than the actual name and easier to
#' refer to.
#' @param project_name `[character(1)=NA]` \cr Name of the project.
#' @param study_area `[character(1)=NA]` \cr Name of the study area.
#' @param sub_area `[character(1)=NA]` \cr Name of the subarea, i.e. a spatial
#' reference within the study area where the proposed local changes are made.
#' @param species `[character(1)=NA]` \cr Species name.
#' @param season `[character(1)=NA]` \cr Season(s). It can be either one season
#' if `intra_inter_season = "intra"` or two seasons if `intra_inter_season = "inter"`
#' or `"all"`.
#' @param intra_inter_season `[character(1)="inter"]`{"intra", "inter", "all"} \cr
#' Whether the scenario is looking at connectivity within season (`"intra"`),
#' between seasons (`"inter"`), or both.
#' @param description `[character(1)=NA]` \cr Textual description of the model.
#' @param region_grass_vector `[character(1)=NA]` \cr Polygon in GRASS used to define
#' the region and mask. Should be in the format `"vector_name@mapset_name"`.
#' @param region_grass_raster `[character(1)=NA]` \cr Raster in GRASS used to define
#' the resolution and alignments of the region, and possible the extent as well.
#' Should be in the format `"raster_name@mapset_name"`.
#' @param region_file_vector `[character(1)=NA]` \cr Full path to polygon used to
#' define the region and mask. Should be in the format `"folder_name/file_name.extension"`.
#' @param region_file_raster `[character(1)=NA]` \cr Raster used to define
#' the resolution and alignments of the region, and possible the extent as well.
#' Should be in the format `"folder_name/file_name.extension"`.
#' @param region_postgis_table `[character(1)=NA]` \cr Polygon in PostGIS used to define
#' the region and mas. Should be in the format `"schema.table"`.
#' @param region_postgis_condition = NA
#' @param region_file_path Raster or vector to define the region
#' @param region_file_mask Raster or vector to use as mask
#' @param model_suit_object_path `[character(1)=NA]` \cr Path to the habitat
#' suitability model (RSF/ENM) object with coefficient estimates.
#' @param model_suit_object `[character(1)=NA]` \cr Habitat suitability model
#' object (RSF/ENM) with coefficient estimates.
#' @param model_suit_formula `[character(1)=NA]` \cr Formula for the habitat
#' suitability model (RSF/ENM).
#' @param model_suit_covariates `[character(1)=NA]` \cr Covariates included in the
#' habitat suitability model (RSF/ENM).
#' @param model_perm_object_path `[character(1)=NA]` \cr Path to the habitat
#' permeability model (SSF) object with coefficient estimates.
#' @param model_perm_object `[character(1)=NA]` \cr Habitat permeability model
#' object (SSF) with coefficient estimates.
#' @param model_perm_formula `[character(1)=NA]` \cr Formula for the habitat
#' permeability model (SSF).
#' @param model_perm_covariates `[character(1)=NA]` \cr Covariates included in the
#' habitat permeability model (SSF).
#' @param data_suit_object_path `[character(1)=NA]` \cr
#' @param data_suit_object `[character(1)=NA]` \cr
#' @param data_perm_object_path `[character(1)=NA]` \cr
#' @param data_perm_object `[character(1)=NA]` \cr
#' @param data_env_object_path `[character(1)=NA]` \cr
#' @param data_env_object `[character(1)=NA]` \cr
#' @param modified_vector_pts `[character(1)=NA]` \cr
#' @param modified_vector_lin `[character(1)=NA]` \cr
#' @param modified_vector_pol `[character(1)=NA]` \cr
#' @param modified_raster `[character(1)=NA]` \cr
#' @param modified_env_layers `[character(1)=NA]` \cr
#' @param zoi_variables `[character(1)=NA]` \cr
#' @param zoi_radii `[character(1)=NA]` \cr
#'
#' @examples
#' tb <- scenario_table_create(s_gid = 1,
#'                             scenset_id = "rondane_ss01",
#'                             scen_id = "s01",
#'                             study_area = "Rondane",
#'                             season = "summer")
#'
#' # create table with NA data, to be filled later
#' (tb2 <- scenario_table_create(s_gid = 1:10))
#'
#' @name scenario_table
#' @export
scenario_table_create <- function(s_gid = 1,
                                  scenset_id = NA,
                                  scenset_name = NA,
                                  scen_id = NA,
                                  scen_name = NA,
                                  scen_nickname = NA,
                                  project_name = NA,
                                  study_area = NA,
                                  sub_area = NA,
                                  species = NA,
                                  season = NA,
                                  intra_inter_season = c("intra", "inter", "all")[1],
                                  baseline = NA,
                                  combination = NA,
                                  description = NA,
                                  region_grass_vector = NA,
                                  region_grass_raster = NA,
                                  ### ADD map_align_grass = NA,
                                  region_file_vector = NA,
                                  region_file_raster = NA,
                                  region_postgis_layer = NA,
                                  region_postgis_condition = NA,
                                  grass_mapset = NA,
                                  model_suit_object_path = NA,
                                  model_suit_object = NA,
                                  model_suit_formula = NA,
                                  model_suit_covariates = NA,
                                  model_perm_object_path = NA,
                                  model_perm_object = NA,
                                  model_perm_formula = NA,
                                  model_perm_covariates = NA,
                                  data_suit_object_path = NA,
                                  data_suit_object = NA,
                                  data_perm_object_path = NA,
                                  data_perm_object = NA,
                                  # data_bio_object_path = NA,
                                  # data_bio_object = NA,
                                  data_env_object_path = NA,
                                  data_env_object = NA,
                                  modified_vector_pts = NA,
                                  modified_vector_lin = NA,
                                  modified_vector_pol = NA,
                                  modified_raster = NA,
                                  modified_env_layers = NA,
                                  modified_var_names_suit = NA,
                                  modified_var_names_perm = NA,
                                  zoi_radii_suit = NA,
                                  zoi_radii_perm = NA,
                                  baseline_max_suit = NA,
                                  baseline_max_perm = NA,
                                  scenario_folder_path = NA,
                                  scenario_map_prefix = NA) {

  # conditions

  # season
  season_options <- c("winter", "spring", "calving", "summer", "autumn", "fall", "all") # for reindeer, check it for other species
  if(!is.na(season) & !all(grepl(paste(season_options, collapse = "|"), season, ignore.case = TRUE)))
    stop(paste0("The parameter for 'season' is not valid. Please select on of: ", paste(season_options, collapse = ","), " or NA."))

  # intra-inter
  intra_inter_season_options <- c("intra", "inter", "all")
  if(!all(grepl(paste(intra_inter_season_options, collapse = "|"), intra_inter_season, ignore.case = TRUE)))
    stop(paste0("The parameter for 'intra_inter_season' is not valid. Please select on of: ", paste(intra_inter_season_options, collapse = ",")))

  # create tibble
  tb <- data.frame(s_gid = s_gid,
                   scenset_id = scenset_id,
                   scenset_name = scenset_name,
                   scen_id = scen_id,
                   scen_name = scen_name,
                   scen_nickname = scen_nickname,
                   project_name = project_name,
                   study_area = study_area,
                   sub_area = sub_area,
                   species = species,
                   season = season,
                   intra_inter_season = intra_inter_season,
                   baseline = baseline,
                   combination = combination,
                   description = description,
                   region_grass_vector = region_grass_vector,
                   region_grass_raster = region_grass_raster,
                   region_file_vector = region_file_vector,
                   region_file_raster = region_file_raster,
                   region_postgis_layer = region_postgis_layer,
                   region_postgis_condition = region_postgis_condition,
                   grass_mapset = grass_mapset,
                   model_suit_object_path = model_suit_object_path,
                   model_suit_object = model_suit_object,
                   model_suit_formula = model_suit_formula,
                   model_suit_covariates = model_suit_covariates,
                   model_perm_object_path = model_perm_object_path,
                   model_perm_object = model_perm_object,
                   model_perm_formula = model_perm_formula,
                   model_perm_covariates = model_perm_covariates,
                   data_suit_object_path = data_suit_object_path,
                   data_suit_object = data_suit_object,
                   data_perm_object_path = data_perm_object_path,
                   data_perm_object = data_perm_object,
                   data_env_object_path = data_env_object_path,
                   data_env_object = data_env_object,
                   modified_vector_pts = modified_vector_pts,
                   modified_vector_lin = modified_vector_lin,
                   modified_vector_pol = modified_vector_pol,
                   modified_env_layers = modified_env_layers,
                   modified_var_names_suit = modified_var_names_suit,
                   modified_var_names_perm = modified_var_names_perm,
                   zoi_radii_suit = zoi_radii_suit,
                   zoi_radii_perm = zoi_radii_perm,
                   baseline_max_suit = baseline_max_suit,
                   baseline_max_perm = baseline_max_perm,
                   scenario_folder_path = scenario_folder_path,
                   scenario_map_prefix = scenario_map_prefix) |>
    tibble::as_tibble()

  # return
  tb
}

#' @param scenario_table `[data.frame]` \cr Table of scenarios (as created
#' through the `scenario_table_create()` function).
#' @param ref_column `[character(1)="s_gid"]{"s_gid", "scen_id"}` \cr
#' Column of the scenario table `scenario_table` to be used as a reference to select
#' the rows to be updated. Default is column `"s_gid"`.
#' @param overwrite `[logical(1)=FALSE]` \cr Should a column value be replaced?
#' Default is `FALSE`.
#'
#' @examples
#' # update table
#' scenario_table_update(tb2, s_gid = c(3,6), scen_name = "test")
#'
#' @rdname scenario_table
#' @export
scenario_table_update <- function(scenario_table, s_gid = NA,
                                  ref_column = c("s_gid", "scen_id")[1],
                                  scenset_id = NA,
                                  scenset_name = NA,
                                  scen_id = NA,
                                  scen_name = NA,
                                  scen_nickname = NA,
                                  project_name = NA,
                                  study_area = NA,
                                  sub_area = NA,
                                  species = NA,
                                  season = NA,
                                  intra_inter_season = NA,
                                  baseline = NA,
                                  combination = NA,
                                  description = NA,
                                  region_grass_vector = NA,
                                  region_grass_raster = NA,
                                  region_file_vector = NA,
                                  region_file_raster = NA,
                                  region_postgis_layer = NA,
                                  region_postgis_condition = NA,
                                  grass_mapset = NA,
                                  model_suit_object_path = NA,
                                  model_suit_object = NA,
                                  model_suit_formula = NA,
                                  model_suit_covariates = NA,
                                  model_perm_object_path = NA,
                                  model_perm_object = NA,
                                  model_perm_formula = NA,
                                  model_perm_covariates = NA,
                                  data_suit_object_path = NA,
                                  data_suit_object = NA,
                                  data_perm_object_path = NA,
                                  data_perm_object = NA,
                                  data_env_object_path = NA,
                                  data_env_object = NA,
                                  modified_vector_pts = NA,
                                  modified_vector_lin = NA,
                                  modified_vector_pol = NA,
                                  modified_raster = NA,
                                  modified_env_layers = NA,
                                  modified_var_names_suit = NA,
                                  modified_var_names_perm = NA,
                                  zoi_radii_suit = NA,
                                  zoi_radii_perm = NA,
                                  baseline_max_suit = NA,
                                  baseline_max_perm = NA,
                                  scenario_folder_path = NA,
                                  scenario_map_prefix = NA,
                                  overwrite = FALSE) {

  #----
  # parameters
  # get all the parameters passed to the function
  # from https://stackoverflow.com/questions/66329835/how-to-get-all-parameters-passed-into-a-function-with-their-values
  # vars <- c(mget(ls(environment(), sorted=F)), match.call(expand.dots=F)$...)
  parms <- c(mget(ls(environment(), sorted=F)))
  parms <- parms[!(names(parms) %in% c("scenario_table", "ref_column", "overwrite"))]

  #----
  # conditions

  # key column
  if(all(is.na(s_gid)) & all(is.na(scen_id))) stop("One key/reference column must be provided. Please check the values for 's_gid' and 'scen_id'.")

  # season
  season_options <- c("winter", "spring", "calving", "summer", "autumn", "fall", "all") # for reindeer, check it for other species
  if(!is.na(season) & !all(grepl(paste(season_options, collapse = "|"), season, ignore.case = TRUE)))
    stop(paste0("The parameter for 'season' is not valid. Please select on of: ", paste(season_options, collapse = ","), " or NA."))

  # intra-inter
  intra_inter_season_options <- c("intra", "inter", "all")
  if(!is.na(intra_inter_season) & !all(grepl(paste(intra_inter_season_options, collapse = "|"), intra_inter_season, ignore.case = TRUE)))
    stop(paste0("The parameter for 'intra_inter_season' is not valid. Please select on of: ", paste(intra_inter_season_options, collapse = ",")))

  #----
  # table

  # copy table
  tab_copy <- scenario_table

  # reference column
  if(ref_column == "scen_id" & all(is.na(scen_id)))
    stop("The parameter 'scen_id' must be specified if ref_column == 'scen_id'.")
  if(ref_column == "s_gid") ref <- s_gid else ref <- scen_id

  # check that there are lines selected
  tb_lines <- which(tab_copy[[ref_column]] %in% ref)

  # if there are any lines selected
  if(length(tb_lines) < 1) {
    # error
    stop("No lines in the table were selected. Please check the values for 's_gid' and 'scen_id' and the reference column.")
  } else {
    # remove NAs
    parms <- parms[!is.na(parms) & !is.na(names(parms))]
    # remove reference columns
    parms <- parms[names(parms) != ref_column] # remove ref_column (s_gid or scen_id)
    nms <- names(parms) # refresh names
    # print(nms)

    # update columns
    for(i in seq_along(parms)) { # columns

      # check number of values
      if(length(parms[[i]]) != 1 & length(parms[[i]]) != length(tb_lines)) {
        stop(paste0("The parameter '", nms[i],"' should have length 1 or ", length(tb_lines), ", not ", length(parms[[i]]), "."))
      }

      # update values
      if(length(parms[[i]]) < length(tb_lines)) parms[[i]] <- rep.int(parms[[i]], length(tb_lines))

      for(j in seq_along(tb_lines)) { # lines

        # check if scenario_table value is NA and if should be replaced
        if(!is.na(tab_copy[[nms[i]]][tb_lines[j]]) & overwrite == FALSE) {
          stop(paste0("At least one value in the column '", nms[i], "' is non-null. Set overwrite=TRUE to replace it.\n"))
        } else {

          # if it should be replaced, get a warning
          if(!is.na(tab_copy[[nms[i]]][tb_lines[j]]) & overwrite == TRUE) {
            warning(paste0("The value of '", nms[i], "' for scenario s_gid = ", tab_copy$s_gid[tb_lines[j]], " is non-null and will be replaced."))
          }
          # replace value
          tab_copy[[nms[i]]][tb_lines[j]] <- parms[[i]][j]

        }

      }
    }
  }

  # return
  tab_copy
}

#' Add new lines/scenarios to a table of scenarios
#'
#' @examples
#' # add new scenarios
#' scenario_table_add(tb2, s_gid = 11:12, season = "summer", scen_id = paste0("scenario", c(11, 12)))
#'
#' # error
#' scenario_table_add(tb2, s_gid = 1)
#'
#' @rdname scenario_table
#' @export
scenario_table_add <- function(scenario_table,
                               s_gid = get_max_s_gid(scenario_table) + 1,
                               scenset_id = NA,
                               scenset_name = NA,
                               scen_id = NA,
                               scen_name = NA,
                               scen_nickname = NA,
                               project_name = NA,
                               study_area = NA,
                               sub_area = NA,
                               species = NA,
                               season = NA,
                               intra_inter_season = c("intra", "inter", "all")[1],
                               baseline = NA,
                               combination = NA,
                               description = NA,
                               region_grass_vector = NA,
                               region_grass_raster = NA,
                               region_file_vector = NA,
                               region_file_raster = NA,
                               region_postgis_layer = NA,
                               region_postgis_condition = NA,
                               grass_mapset = NA,
                               model_suit_object_path = NA,
                               model_suit_object = NA,
                               model_suit_formula = NA,
                               model_suit_covariates = NA,
                               model_perm_object_path = NA,
                               model_perm_object = NA,
                               model_perm_formula = NA,
                               model_perm_covariates = NA,
                               data_suit_object_path = NA,
                               data_suit_object = NA,
                               data_perm_object_path = NA,
                               data_perm_object = NA,
                               data_env_object_path = NA,
                               data_env_object = NA,
                               modified_vector_pts = NA,
                               modified_vector_lin = NA,
                               modified_vector_pol = NA,
                               modified_raster = NA,
                               modified_env_layers = NA,
                               modified_var_names_suit = NA,
                               modified_var_names_perm = NA,
                               zoi_radii_suit = NA,
                               zoi_radii_perm = NA,
                               baseline_max_suit = NA,
                               baseline_max_perm = NA,
                               scenario_folder_path = NA,
                               scenario_map_prefix = NA) {

  #----
  # parameters
  # get all the parameters passed to the function
  parms <- c(mget(ls(environment(), sorted=F)))
  parms <- parms[!(names(parms) %in% c("scenario_table"))]

  # create new table
  tab_new <- do.call(scenario_table_create, parms)

  # check that s_gid is unique
  if(all(s_gid %in% scenario_table$s_gid)) {
    stop(paste0("The value(s) of 's_gid' (", paste(s_gid, collapse = ","), ") exists. Please select unique s_gid(s) for the new scenario(s).\n"))
  }

  # return
  rbind(scenario_table, tab_new)
}

#' Get maximum scenario general ID (gid) from a table of scenarios
#'
#' @rdname scenario_table
#' @export
get_max_s_gid <- function(scenario_table) {
  max(scenario_table$s_gid, na.rm = TRUE)
}

# scenario_table_get_gid <- function(scenario_table,
#                                    scenset_id = NA,
#                                    scenset_name = NA,
#                                    scen_id = NA,
#                                    scen_name = NA)
