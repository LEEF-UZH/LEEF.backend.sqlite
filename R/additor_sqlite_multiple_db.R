#' Write a RRD data table
#'
#' Add new data to the database.
#' If a table exist, append it, if not,
#' create a new table with the name \code{name} if \code{create_new_table = TRUE}.
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data to
#'
#' @return returns invisible\code{TRUE}
#'
#' @importFrom yaml yaml.load_file
#' @importFrom utils read.csv
#' @importFrom DBI dbCreateTable dbWriteTable dbExistsTable dbIsValid
#' @importFrom DBI dbBegin dbCommit dbRollback
#' @importFrom RSQLite SQLite
#' @export
#'
#' @examples
additor_sqlite_multiple_db <- function(input,
                                       output,
                                       tn_postfix = NULL) {

  # Some variable definitions -----------------------------------------------

  new_data_pattern <- "\\.csv$"

  exclude_files <- c(
    ## general
    "compositions.csv", "experimental_design.csv",
    ## flowcytometer
    "gates_coordinates.csv", "metadata_flowcytometer.csv",
    ## flowcam
    "flowcam_dilution.csv",
    # )
    #
    # seperate_db <- c(
    ## bemovi
    "Master.csv",
    "Master_cropped.csv",
    "Master_non_cropped.csv",
    # "Morph_mvt.csv",
    # "Morph_mvt_cropped.csv",
    ## flowcam
    # "algae_traits.csv",
    "algae_metadata.csv",
    "flowcytometer_ungated.csv"
  )

  db_base_name <- "LEEF.RRD"
  db_name <- paste0(db_base_name, ".sqlite")

  # create directory structure if it does not exist -------------------------

  if (!dir.exists(output)) {
    dir.create(
      output,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  conn <- NULL

  progress <- file.path(normalizePath(output), paste0("ADDING.PREPARATION.ADDING"))
  error <- file.path(normalizePath(output), paste0("ERROR.ADDING.PREPARATION.ERROR"))

  file.create(progress)

  on.exit({
    # ROLLBACK TRANSACTION ----------------------------------------------------
    if (!is.null(conn)) {
      if (DBI::dbIsValid(conn)) {
        DBI::dbRollback(conn)
        try(
          disconnect(conn),
          silent = TRUE
        )
      }
    }
    if (file.exists(progress)) {
      unlink(progress)
      file.create(error)
    }
  })


  # Read measures --------------------------------------------------------------------

  measures <- list.dirs(input, full.names = FALSE, recursive = FALSE)

  unlink(progress)

  # Write data in individual transaction --------------------------------------------
  conn <- connect(dbname = file.path(output, db_name))

  for (i in 1:length(measures)) {
    input_files <- list.files(
      path = file.path(input, measures[i]),
      pattern = new_data_pattern,
      full.names = FALSE,
      recursive = FALSE
    )

    input_files <- input_files[!(input_files %in% exclude_files)]

    for (fn_in in input_files) {

      progress <- file.path(normalizePath(output), paste0("ADDING.", fn_in, ".TO.", db_name, ".ADDING"))
      error <- file.path(normalizePath(output), paste0("ERROR.ADDING.", fn_in, ".TO.", db_name, ".ERROR"))
      file.create(progress)

      tn <- tn_from_fn(fn_in, measures[i], tn_postfix = tn_postfix)


      if (!DBI::dbExistsTable(conn, tn)) {
        dat <- utils::read.csv(file.path(input, measures[i], fn_in), nrows = 10)
        dat <- as.data.frame(dat)
        db_create_table(
          conn = conn,
          dat = dat,
          table = tn
        )
      }

      dat <- utils::read.csv(
        file.path(input, measures[i], fn_in)
      )
      dat <- as.data.frame(dat)

      names(dat) <- tolower(names(dat))


      # BEGIN TRANSACTION -------------------------------------------------------

      added <- db_add_data(
        conn = conn,
        data = dat,
        table = tn
      )
      if (!added) {
        file.create(
          file.path(
            normalizePath(output),
            paste0("ERROR.ADDING.", tn, ".NOT_ADDED.TIMESTAMP_EXISTS.", unique(dat$timestamp), ".ERROR")
          )
        )
      }

      # x <- DBI::dbExecute(conn, "VACUUM")

      # END TRANSACTION ------------------------------------------------------


      unlink(progress)
    }
  }

  disconnect(conn)


  # Finalise stuff ----------------------------------------------------------


  invisible(TRUE)
}

# Helper function ---------------------------------------------------------


#' Table name from file name and measure
#'
#' @param fn file name
#' @param measure measure
#' @param tn_postfix postfix for the table name
#'
#' @return table name
#' @export
#'
#' @examples
tn_from_fn <- function(
  fn,
  measure,
  tn_postfix
) {
  tn <- tolower(gsub("\\.csv", "", fn))
  if (is.null(tn_postfix)) {
    tn <- paste0(measure, "__", tn)
  } else {
    tn <- paste0(measure, "_", tn_postfix, "__", tn)
  }
  tn <- gsub("\\.", "_", tn)
  return(tn)
}



#' Get dbname from file name
#'
#' @param fn file name
#' @param dbname default dbname
#' @param seperate_db
#'
#' @return
#' @export
#'
#' @examples
db_name_from_fn <- function(
  fn,
  dbname,
  seperate_db
) {
  stop("Not Tested!!!")
  fn <- tolower(gsub("\\.csv", "", fn))

  if (any(grepl(fn, seperate_db))) {
    dbname <- paste0(
      dbname,
      "_",
      gsub("\\.csv", "", fn),
      ".sqlite"
    )
  } else {
    dbname <- paste0(
      dbname,
      ".sqlite"
    )
    # }
    return(dbname)
  }
}

#' Open connection to db
#'
#' Also create status report file in the dabname directory
#' @param dbname name of database
#'
#' @return connection object
#' @importFrom DBI dbConnect dbGetQuery
#' @export
#'
#' @examples
connect <- function(
  dbname
) {
  conn <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = dbname
  )

  # Set PRAGMAS for minimizing disk use and maximize performance ------------
  ## Use memory instead of discs for temporary store
  DBI::dbGetQuery(conn, "PRAGMA temp_store=2")

  ## Set journal mode to memory. This is slightly unsafer but should not be a problem here
  DBI::dbGetQuery(conn, "PRAGMA journal_mode=MEMORY")

  file.create(
    file.path(
      normalizePath(dirname(dbname)),
      paste0("CONNECTED.", basename(conn@dbname), ".CONNECTED")
    )
  )
  return(conn)
}

#' Title
#'
#' Also delete status report file in the dabname directory
#' @param conn
#'
#' @return
#' @importFrom DBI dbDisconnect
#' @export
#'
#' @examples
disconnect <- function(conn) {
  dbname <- basename(conn@dbname)
  DBI::dbDisconnect(conn)
  unlink(
    file.path(
      normalizePath(dirname(dbname)),
      paste0("CONNECTED.", basename(conn@dbname), ".CONNECTED")
    )
  )
  invisible(TRUE)
}

