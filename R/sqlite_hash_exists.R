#' Check if hash already exists in table
#'
#' @param table tablename to check against (only one!)
#' @param hash hash to check (can be vector)
#'
#' @return logical vector of the same length as \code{hash}, \code{TRUE} if hash exists in table, otherwise FALSE
#'
#' @importFrom DBI dbGetQuery
#' @export
#'
#' @examples
sqlite_hash_exists <- function(
  table,
  hash
){
  closeAgain <- FALSE

  on.exit(
    {
      if (closeAgain) {
        sqlite_disconnect()
      }
    }
  )

  # Check if connection should be closed again ------------------------------

  if ( is.null(getOption("LEEF.status")$data_connection) ) {
    sqlite_connect()
    closeAgain <- TRUE
  }



  if (length(table) != 1) {
    stop("table has to have a length of one")
  }

  result <- sapply(
    hash,
    function(i) {
      i %in% dbGetQuery( getOption("LEEF.status")$data_connection, paste("SELECT DISTINCT hash FROM",  table ) )
    }
  )
  names(result) <- hash
  ##
  return(result)
}
