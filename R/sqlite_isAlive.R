#' Check if connection to data source is alive
#'
#' Checks if connection is alive and functional. If not, the function tries
#' to close the connection.
#' @return \code{TRUE} if connection alive, otherwise \code{FALSE}
#' @importFrom DBI dbGetQuery
#' @export
#'
#' @examples
sqlite_isAlive <- function() {
  result <- FALSE
  try(
    {
      result <- DBI::dbGetQuery(
        conn = getOption("LEEF.Data.status")$data_connection,
        statement = "SELECT 1"
      )[[1]] == 1
    },
    silent = TRUE
  )
  if (!result & !is.null(getOption("LEEF.Data.status")$data_connection)) {
      sqlite_disconnect()
  }
  invisible(result)
}
