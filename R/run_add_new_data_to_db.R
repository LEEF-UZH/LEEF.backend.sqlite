#' Write a raw data table
#'
#' Add new data to the database. If a table exist, append it, if not, create a new table with the name \code{name} if \code{create_new_table = TRUE}.
#'
#' @param create_new_table if \code{TRUE}, create a new table if it does not exist. Default \code{FALSE}, i.e. raises error if the table does not exist.
#'
#' @return returns invisible\code{TRUE}
#' @importFrom DBI dbWriteTable dbExistsTable
#' @export
#'
#' @examples
run_add_new_data_to_db <- function(
  create_new_table = FALSE
){
  stop( "TODO to be implemented!" )

  closeAgain <- FALSE

  on.exit(
    {
      if (closeAgain) {
        db_disconnect_data()
      }
    }
  )

# Check if connection should be closed again ------------------------------

  if (is.null(DATA_options("data_connection"))) {
    db_connect_data()
    closeAgain <- TRUE
  }

# Get new_data file names, extension and path -----------------------------

  to_be_imported <-  DATA_options("to_be_imported")
  table_names <- list.files( path = to_be_imported, pattern = new_data_extension )
  table_names <- gsub(new_data_extension, "", table_names)

  if ( !file.exists( file.path( to_be_imported, "hash.sha256") ) ) {
    stop("The new data has not been hashed - please run `hash_new_data() before running this command!")
  }


# Check if table exists ---------------------------------------------------

  for (tbl in table_names) {
    if (
      (!DBI::dbExistsTable( conn = DATA_options("data_connection"), name = tbl ) ) &
      (!create_new_table)
    ) {
      stop("Table '", tbl, "' does not exist!", "\n", "If you want to create the table, set 'create_new_table = TRUE' when calling 'write_new_table!")
    }
  }

  ####
  ## TODO
  ####
  ## Check before writing, that all hashes are new - maybe use the hash for the new_data_set?

# Write data --------------------------------------------------------------
  # timestamp <- format( file.mtime( file.path( to_be_imported, "hash.sha256") ) , "%Y-%m-%d--%H-%M-%S")
  for (i in 1:length(table_names)) {
    x <- read_new_data(table_names[i])
    x[["hash"]] <- read_new_data_hash(table_names[[i]])
    DBI::dbWriteTable(
      conn = DATA_options("data_connection"),
      name = table_names[i],
      value = x,
      overwrite = FALSE,
      append = TRUE
    )
  }

# Finalise stuff ----------------------------------------------------------

  invisible(TRUE)
}


