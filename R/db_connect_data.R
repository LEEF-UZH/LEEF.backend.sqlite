#' Connect to raw data source
#'
#' The connection is saved in the option \code{data_connection}
#' @return invisibly the connection
#'
#' @importFrom DBI dbConnect
#' @export
#'
#' @examples
db_connect_data <- function() {
  if (!is.null(options()$LEEF.Data$data_connection)) {
    db_disconnect_data()
  }
  if (options()$LEEF.Data$database[["driver"]] == "RSQLite::SQLite()") {
    if (is.null(options()$LEEF.Data$database[["dbpath"]])) {
      stop( "Plese set data -- backend -- dbpath in config.yml!")
    } else {
      dbname = file.path(
        options()$LEEF.Data$database[["dbpath"]],
        options()$LEEF.Data$database[["dbname"]])
    }
    DATA_options(
      data_connection = DBI::dbConnect(
        drv = eval( parse( text = DATA_options("database")[["driver"]] ) ),
        dbname = dbname
      )
    )
  } else {
    stop("Not Implemented yet!")
  }
  invisible(TRUE)
}
