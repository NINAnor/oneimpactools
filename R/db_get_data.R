#-----
# Get data from the PostGIS database
# Script adapted to the version of the reindeer models from Renewable Reindeer/ProdChange
# Bram van Moorter and Bernardo Niebuhr
#-----

# data = suit and perm not implemented
# but could be taken from

# save_file must be a rda file "path/file.rda".

#' @export
db_get_data <- function(con, table, condition = NULL, bbox = NULL,
                        coords = c("x33", "y33"),
                        query = NULL,
                        save_file = "") {

  if(is.null(query)) {
    # query
    qq <- oneimpactools::db_make_query("SELECT points_id, ST_X(ST_Transform(geom_e33, 25833)) as x33,
                                       ST_Y(ST_Transform(geom_e33, 25833)) as y33, *
                                       FROM ", table)
    # condition
    if(!is.null(condition)) qq <- oneimpactools::db_make_query(qq, " WHERE ", condition)
    # bbox
    if(!is.null(bbox)) {
      bbox_condition <- db_make_query("a.", coords[1], " > ", bbox[1], " AND ",
                                      "a.", coords[2], " > ", bbox[2], " AND ",
                                      "a.", coords[1], " < ", bbox[3], " AND ",
                                      "a.", coords[2], " < ", bbox[4])
      qq <- oneimpactools::db_make_query("SELECT a.* FROM (", qq, ") a",
                                         " WHERE ", bbox_condition)
    }
    # close query
    qq <- oneimpactools::db_make_query(qq, ";")
  } else {
    qq <- query
  }

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
