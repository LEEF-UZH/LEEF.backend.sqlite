#' Disconnect from connected raw data source
#'
#' @return result from \code{DBI::dbDisconnect(conn)}
#'
#' @importFrom DBI dbDisconnect
#'
#' @export
#'
#' @examples
sqlite_disconnect <- function() {
  result <- NULL
  if ( !is.null(getOption("LEEF.status")$data_connection) ) {
    try(
      result <- DBI::dbDisconnect(getOptino("LEEF")$data_connection),
      silent = TRUE
    )
    status <- getOption("LEEF.status")
    status$data_connection <- NULL
    options( LEEF.status = status )
  }
  ##
  invisible(result)
}
