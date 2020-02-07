#' Check if coinnection to data source is alive
#'
#' Checks if connection is alive and functional. If not, the function tries
#' to close the connection.
#' @return \code{TRUE} id connection alive, otherwise \code{FALSE}
#' @importFrom DBI dbGetQuery
#' @export
#'
#' @examples
db_isAlive_data <- function() {
  result <- FALSE
  conn <- DATA_options("data_connection")
  try(
    result <- DBI::dbGetQuery(conn, "SELECT 1")[[1]] == 1,
    silent = TRUE
  )
  if (!result & !is.null(conn)) {
      db_disconnect_data()
  }
  invisible(result)
}
