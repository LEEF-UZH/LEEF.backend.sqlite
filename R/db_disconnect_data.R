#' Disconnect from connected raw data source
#'
#' @return result from \code{DBI::dbDisconnect(conn)}
#' @importFrom DBI dbDisconnect
#' @export
#'
#' @examples
db_disconnect_data <- function() {
  result <- NULL
  if (!is.null(DATA_options("data_connection"))) {
    try(
      result <- DBI::dbDisconnect(DATA_options("data_connection")),
      silent = TRUE
    )
    DATA_options( data_connection = NULL)
  }
  invisible(result)
}
