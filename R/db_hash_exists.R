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
db_hash_exists <- function(
  table,
  hash
){
  on.exit(
    {
      if (closeAgain) {
        db_disconnect_data()
      }
    }
  )
# Check if connection should be closed again ------------------------------

  closeAgain <- FALSE
  if (is.null(DATA_options("data_connection"))) {
    db_connect_data()
    closeAgain <- TRUE
  }
  if (length(table) != 1) {
    stop("table has to have a length of one")
  }

  result <- sapply(
    hash,
    function(i) {
      i %in% dbGetQuery( DATA_options("data_connection"), paste("SELECT DISTINCT hash FROM",  table ) )
    }
  )
  names(result) <- hash

  return(result)
}
