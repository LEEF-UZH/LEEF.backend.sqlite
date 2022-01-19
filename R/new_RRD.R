#' Create new RRD database
#'
#' Create a new database following the scheme used for the LEEF.RRD database
#' @param dbname the path and name of the database. Must not exist.
#'
#' @return
#' @export
#'
#' @examples
new_RRD <- function(
  dbname
){
  if (file.exists(dbname)) {
    stop(
      "Database '", dbname, "' exists!\n",
      "  Please delete it before running this command again!")
  }

  conn <- NULL
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname )
  on.exit(
    try(
      DBI::dbDisconnect(conn),
      silent = TRUE
    )
  )

  sql <- readLines(system.file("LEEF.RRD.scheme.sql", package = "LEEF.backend.sqlite"))
  sql <- paste0(sql, collapse = " ")
  sql <- strsplit(sql, ";")

  result <- lapply(
    sql[[1]],
    function(s) {
      DBI::dbExecute(conn, s)
    }
  )
  invisible(result)
}

