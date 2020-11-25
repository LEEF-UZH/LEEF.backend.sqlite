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
additor_sqlite_multiple_db <- function(
  input,
  output
){

  conn <- NULL

  progress <- file.path(normalizePath(output), paste0("ADDING.PREPARATION.ADDING"))
  error    <- file.path(normalizePath(output), paste0("ERROR.ADDING.PREPARATION.ERROR"))

  file.create(progress)

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
      if (file.exists(progress)) {
        unlink(progress)
        file.create(error)
      }
    }
  )

  # Some variable definitions -----------------------------------------------

  new_data_pattern <- "\\.rds$"

  smdf <- file.path(input, "sample_metadata.yml")
  smd <- yaml::yaml.load_file( smdf )

  sample_name <- paste(smd$name, smd$timestamp, sep = "_")

  db_base_name <- "LEEF.RRD"

  measures <- list.dirs(input, full.names = FALSE, recursive = FALSE)
  measures <- data.frame(
    measure = measures,
    db_name = paste0(db_base_name, "_", measures)
  )

  # create directory structure if it does not exist -------------------------

  if (!dir.exists(output)) {
    dir.create(
      output,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }
  ##

  unlink(progress)

  # Iterate through measurements --------------------------------------------

  for (i in 1:nrow(measures)) {

    input_files <- list.files(
      path = file.path(input, measures$measure[i]),
      pattern = new_data_pattern,
      full.names = FALSE,
      recursive = FALSE
    )

    for (fn_in in input_files) {

      if (grepl("Master", fn_in)) {
        db_name <- paste0(
          measures$db_name[[i]],
          "_",
          gsub("\\.rds", "", fn_in),
          ".sqlite"
        )
      } else {
        db_name <- paste0(
          db_base_name,
          ".sqlite"
        )
      }

      # BEGIN TRANSACTION -------------------------------------------------------

      progress <- file.path(normalizePath(output), paste0("ADDING.", fn_in, ".TO.", db_name, ".ADDING"))
      error    <- file.path(normalizePath(output), paste0("ERROR.ADDING.", fn_in, ".TO.", db_name, ".ERROR"))
      file.create(progress)

      conn <- DBI::dbConnect(
        drv = RSQLite::SQLite(),
        dbname = file.path( output, db_name )
      )

      DBI::dbBegin(conn)

      tn <- tolower(gsub("\\.rds", "", fn_in))
      tn <- gsub("\\.", "_", tn)

      dat <- readRDS( file.path(input, measures$measure[i], fn_in) )

      names(dat) <- tolower(names(dat))

      dat <- as.data.frame(dat)


      skipp_add <- FALSE
      if (DBI::dbExistsTable(conn, tn)){
        ts <- DBI::dbGetQuery(conn, paste("SELECT DISTINCT timestamp FROM", tn))
        tn_exists <- TRUE
        if (unique(dat$timestamp) %in% ts) {
          warning("!!! ", tn, " not added as timestamp already present! !!!")
          file.create(file.path(normalizePath(output), paste0("ERROR.ADDING.", tn, ".NOT_ADDED.TIMESTAMP_EXISTS.", ts, ".ERROR")))
          skipp_add <- TRUE
        }
      } else {
        tn_exists <- FALSE
      }

      if (!skipp_add) {
        DBI::dbWriteTable(
          conn,
          name = tn,
          value = dat,
          overwrite = FALSE,
          append = tn_exists
        )
      }

      # COMMIT TRANSACTION ------------------------------------------------------

      DBI::dbCommit(conn)
      x <- DBI::dbExecute(conn, "VACUUM")
      DBI::dbDisconnect(conn)

      unlink(progress)
    }
  }


  # Finalise stuff ----------------------------------------------------------


  invisible(TRUE)
}


