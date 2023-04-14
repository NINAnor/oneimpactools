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
