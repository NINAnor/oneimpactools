#-----
# Get data from the PostGIS database
# Script adapted to the version of the reindeer models from Renewable Reindeer/ProdChange
# Bram van Moorter and Bernardo Niebuhr
#-----

# data = suit and perm not implemented
# but could be taken from

# save_file must be a rda file "path/file.rda".

#' Get spatial/grid data from the PostGIS database
#'
#' This function retrieves the spatial grid for Norway annotated with environmental
#' data. The function is adapted to the version of the reindeer models from
#' Renewable Reindeer/ProdChange and needs to be changed if model/data structure is
#' changed.
#' The function retrieves the spatial grid, including possibly conditions or specific
#' queries and restricting the data retrieved to a spatial bounding box passed as
#' a parameter.
#'
#' @param con `[PqConnection]` \cr Connection to a Postgres database.
#' @param table `[string]` \cr The name of a table within a PostgreSQL/PostGIS database,
#' in the format `"name_of_schema.name_of_table"`.
#' @param condition `[string]` \cr SQL condition passed to a "WHERE" clause
#' (e.g. "country == 'Norway'", where "country" is a column in `table` and 'Norway'
#' is one of the values in this column).
#' @param bbox `[]`
#' @param cols `[string]` \cr Columns to be selected from `table`.
#'
#' @export
db_get_data <- function(con, table,
                        condition = NULL,
                        bbox = NULL,
                        gid_col = "points_id",
                        cols = "*",
                        coords_orig = c("x", "y"),
                        coords = c("x33", "y33"),
                        geom = "geom_e33",
                        reproject = FALSE,
                        crs = 25833,
                        query = NULL,
                        verbose = FALSE,
                        do_not_run = FALSE,
                        save_file = "") {

  if(is.null(query)) {
    # query
    qq <- samtools::db_make_query("SELECT ", gid_col, ", ", # take points_id/gif
                                  ifelse(reproject, paste0("ST_X(ST_Transform(",geom,", ",crs,")) as ",coords[1]), paste0(coords_orig[1], " AS ", coords[1])), ", ", # X coord
                                  ifelse(reproject, paste0("ST_Y(ST_Transform(",geom,", ",crs,")) as ",coords[2]), paste0(coords_orig[2], " AS ", coords[2])), # Y coord
                                  ifelse(cols == "", "", paste0(", ", cols)), # select cols
                                  " FROM ", table)
    # condition
    if(!is.null(condition)) qq <- samtools::db_make_query(qq, " WHERE ", condition)
    # bbox
    if(!is.null(bbox)) {
      bbox_condition <- samtools::db_make_query("a.", coords[1], " > ", bbox[1], " AND ",
                                                "a.", coords[2], " > ", bbox[2], " AND ",
                                                "a.", coords[1], " < ", bbox[3], " AND ",
                                                "a.", coords[2], " < ", bbox[4])
      qq <- samtools::db_make_query("SELECT a.* FROM (", qq, ") a",
                                    " WHERE ", bbox_condition)
    }
    # close query
    qq <- samtools::db_make_query(qq, ";")
  } else {
    qq <- query
  }

  # print query if verbose
  if(verbose) print("This is your query:")
  if(verbose) print(qq)

  # stop before querying if do_not_run
  if(do_not_run) stop("To run the query, select do_not_run = FALSE.")

  # get as table
  dat <- sf::st_read(con, query = qq)
  dat <- sf::st_drop_geometry(dat)

  # rs <- DBI::dbSendQuery(con, qq)
  # dat <- DBI::fetch(rs, -1)
  #dbClearResult(rs)
  #dat$geom_e33 <- NULL
  #str(grd)

  # save
  if(save_file != "") {
    save(dat, file = save_file)
  }

  # return
  dat
}
