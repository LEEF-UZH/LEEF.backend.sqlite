#' Connect to raw data source
#'
#' The connection is saved in the option \code{data_connection}
#' @return invisibly the connection
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#' @export
#'
#' @examples
sqlite_connect <- function() {
  if (!is.null(getOption("LEEF.Data.status")$data_connection)) {
    warning("Connection already open. To re-open, close firt with `sqlite_disconnect()`!")
  } else {
    if ( is.null(getOption("LEEF.Data")$backend[["dbpath"]] | getOption("LEEF.Data")$backend[["dbname"]])) {
      stop( "Please set dbpath and dbname in config.yml!")
    } else {
      data_connection = DBI::dbConnect(
        drv = RSQLite::SQLite(),
        dbname = file.path(
          getOption("LEEF.Data")$backend[["dbpath"]],
          getOption("LEEF.Data")$backend[["dbname"]]
        )
      )
      status <- getOption("LEEF.Data.status")
      status$data_connection <- data_connection
      options( LEEF.Data.status = status )
    }
  }
  ##
  invisible(TRUE)
}
