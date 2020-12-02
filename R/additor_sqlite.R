#' Write a RRD data table
#'
#' Add new data to the database. If a table exist, append it, if not, create a new table with the name \code{name} if \code{create_new_table = TRUE}.
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data to
#'
#' @return returns invisible\code{TRUE}
#'
#' @importFrom yaml yaml.load_file
#' @importFrom DBI dbWriteTable dbExistsTable dbIsValid
#' @importFrom DBI dbBegin dbCommit dbRollback
#' @importFrom RSQLite SQLite
#' @export
#'
#' @examples
additor_sqlite <- function(
  input,
  output
){

  conn <- NULL

  on.exit(
    {
      # ROLLBACK TRANSACTION ----------------------------------------------------
      if (!is.null(conn)) {
        if (DBI::dbIsValid(conn)) {
          DBI::dbRollback(conn)
          try(
            DBI::dbDisconnect(conn),
            silent = TRUE
          )
        }
      }
    }
  )

  # Some variable definitions -----------------------------------------------

  new_data_pattern <- "\\.csv$"

  smdf <- file.path(input, "sample_metadata.yml")
  smd <- yaml::yaml.load_file( smdf )

  sample_name <- paste(smd$name, smd$timestamp, sep = "_")

  db_name <- "LEEF.RRD.sqlite"
  db <- file.path(output, db_name)

  # create directory structure if it does not exist -------------------------

  if (!dir.exists(output)) {
    dir.create(
      output,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }
  ##
  if (!file.exists( db )) {
    file.create( db )
  }

  # Open sqlite db ----------------------------------------------------------

  conn <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = db
  )


  # BEGIN TRANSACTION -------------------------------------------------------

  DBI::dbBegin(conn)

  # Iterate through measurements --------------------------------------------

  measures <- list.dirs(input, full.names = FALSE, recursive = FALSE)

  for (m in measures) {
    input_files <- list.files(
      path = file.path(input, m),
      pattern = new_data_pattern,
      full.names = FALSE,
      recursive = FALSE
    )
    for (fn_in in input_files) {
      x <- read.csv( file.path(input, m, fn_in) )

      names(x) <- tolower(names(x))

      x <- as.data.frame(x)

      tn <- tolower(gsub("\\.csv", "", fn_in))
      tn <- paste(m, tn, sep = "_")
      tn <- gsub("\\.", "_", tn)

      ## TODO check if data has already been added and only add new data

      skipp_add <- FALSE
      if (DBI::dbExistsTable(conn, tn)){
        ts <- DBI::dbGetQuery(conn, paste("SELECT DISTINCT timestamp FROM", tn))
        tn_exists <- TRUE
        if (unique(x$timestamp) %in% ts) {
          warning("!!! ", tn, " not added as timestamp already present! !!!")
          file.create(file.path(normalizePath(output), paste0("ERROR.ADDING.", tn, ".NOT_ADDED.TIMESTAMP_EXISTS.", ts, ".ERROR")))
        }
      } else {
        tn_exists <- FALSE
      }

      if (!skipp_add) {
        DBI::dbWriteTable(
          conn,
          name = tn,
          value = x,
          overwrite = FALSE,
          append = tn_exists
        )
      }
    }
  }

  # COMMIT TRANSACTION ------------------------------------------------------

  DBI::dbCommit(conn)
  x <- DBI::dbExecute(conn, "VACUUM")
  DBI::dbDisconnect(conn)

  # Finalise stuff ----------------------------------------------------------


  invisible(TRUE)
}


