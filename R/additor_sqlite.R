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
additor_sqlite <- function(
  input,
  output,
  create_new_table = FALSE
){
  new_data_pattern <- "\\.rds"


  stop( "TODO to be implemented!" )

  closeAgain <- FALSE

  on.exit(
    {
      if (closeAgain) {
        sqlite_disconnect()
      }
    }
  )

# Check if connection should be closed again ------------------------------


  if ( is.null(getOption("LEEF.Data.status")$data_connection) ) {
    sqlite_connect()
    closeAgain <- TRUE
  }


# Get new_data file names, extension and path -----------------------------


  table_names <- list.files(
    path = input,
    pattern = new_data_pattern
  )
  table_names <- gsub(new_data_pattern, "", table_names)


# Check if table exists ---------------------------------------------------

  for (tbl in table_names) {
    if (
      (!DBI::dbExistsTable(
        conn = getOption("LEEF.Data.status")$data_connection,
        name = tbl
      ) ) &
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


