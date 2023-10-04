#' Helper function to prepare queries to be sent to PostgreSQL
#'
#' Helper function to easily concatenate strings and prepare queries for
#' PostgreSQL.
#'
#' @param ... `[character]` \cr Strings representing the parts of a query.
#'
#' @return A string with a query, to be used, for instance, within `DBI::dbExecute()`.
#'
#' @examples
#' conditions <- paste0("\"KKOD\" IN ('5011', '5012', '5016', '5017')")
#'
#' query1 <- db_make_query(
#' "CREATE TABLE IF NOT EXISTS sam_env.roads_public_se AS
#' SELECT *
#' FROM
#' \"Topography\".\"Sweden_Vagkartan_Roads_lines\"
#' WHERE ", paste(conditions, collapse = "AND "), ";")
#' query1
#' # to execute, first one needs a connection 'con' to PostgreSQL, then it can be run as
#' # DBI::dbExecute(con, query1)
#'
#' @export
db_make_query <- function(...) {
  strwrap(paste0(...), width = 1e4, simplify = TRUE)
}

#' Helper function to upload vector layers to PostGIS database
#'
#' Helper function to easily upload vector layers to PostGIS, while adding a primary/unique
#' key and possibly a comment to the layer. This function also ensures that the owner
#' of the layer has automatically write to modify the layer as wanted.
#'
#' @param vect Vector object in R, in `sf` format.
#' @param con Connection to PostGIS.
#' @param dsn `[character(1)]` \cr Name of the layer after saved in the PostGIS
#' database. It might contain the name of the schema and the table, in the format
#' `"schema_name.table_name"`.
#' @param pkey_column `[character(1)="pkey"]` \cr Name of the primary key column
#' to be created. Default is `"pkey"`.
#' @param comment `[character=""]` \cr Comment to be added to the layer. Typically a
#' description of the layer, how it was processed, the source, and other relevant
#' information.
#'
#' @export
db_write_vect <- function(vect, con, dsn, pkey_column = "pkey", comment = "") {

  # upload to PostGIS
  dsn_write <- dsn
  if(grepl(".", dsn_write, fixed = TRUE)) {
    sch_tab <- strsplit(dsn_write, ".", fixed = TRUE)[[1]]
    dsn_write <- DBI::Id(schema = sch_tab[1], table = sch_tab[2])
  }
  sf::st_write(vect, con, dsn_write)

  # permission
  qq1 <- db_make_query("GRANT ALL ON TABLE ", dsn, " TO current_user;")
  print(qq1)
  DBI::dbExecute(con, qq1)

  # pkey
  qq2 <- db_make_query("ALTER TABLE ", dsn, " ADD COLUMN ", pkey_column, " SERIAL PRIMARY KEY;")
  qq2
  DBI::dbExecute(con, qq2)

  # comment
  comm <- db_make_query("COMMENT ON TABLE ", dsn, " IS '", comment, "';")
  comm
  DBI::dbSendQuery(con, comm)
}

#' @export
db_read_vect <- function(con, dsn, return_format = c("sf", "terra")) {

  # upload to PostGIS
  dsn_write <- dsn
  if(grepl(".", dsn_write, fixed = TRUE)) {
    sch_tab <- strsplit(dsn_write, ".", fixed = TRUE)[[1]]
    dsn_write <- DBI::Id(schema = sch_tab[1], table = sch_tab[2])
  }
  map <- sf::st_read(con, dsn_write)

  if(return_format == "terra") map <- terra::vect(map)

  map
}
