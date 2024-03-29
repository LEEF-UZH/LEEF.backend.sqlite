#' Write a RRD data table
#'
#' Add new data to the database.
#' If a table exist, append it, if not,
#' create a new table with the name \code{name} if \code{create_new_table = TRUE}.
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data to
#' @param tn_postfix postfix to the table name
#'
#' @return returns invisible\code{TRUE}
#'
#' @importFrom yaml yaml.load_file
#' @importFrom utils read.csv
#' @importFrom tools file_ext
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

  include_files <- c(
    ## bemovi_mag_16
    "Mean_density_per_ml.csv",
    "Morph_mvt.csv",

    ## bemovi_mag_25
    "Mean_density_per_ml.csv",
    "Mean_density_per_ml_cropped.csv",
    "Mean_density_per_ml_non_cropped.csv",
    "Morph_mvt.csv",
    "Morph_mvt_cropped.csv",
    "Morph_mvt_non_cropped.csv",

    ## conductivity
    "conductivity.csv",

    ## flowcam
    "algae_density.csv",
    "algae_traits.rds",

    ## flowcytometer
    "flowcytometer_density.csv",
    "flowcytometer_traits_bacteria.rds",
    "flowcytometer_traits_algae.rds",

    ## manualcount
    "manualcount_density.csv",

    ## o2meter
    "o2meter.csv"
  )

  traits_data <- c(
    ## bemovi_mag_16
    "Morph_mvt.csv",

    ## bemovi_mag_25
    "Morph_mvt.csv",
    "Morph_mvt_cropped.csv",
    "Morph_mvt_non_cropped.csv",

    ## flowcam
    "algae_traits.rds",

    ## flowcytometer
    "flowcytometer_traits_bacteria.rds",
    "flowcytometer_traits_algae.rds"
  )

  db_base_name <- "LEEF-2.RRD"

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

  for (i in 1:length(measures)) {
    input_files <- get_inputfiles(
      input = input,
      measure = measures[i],
      include_files = include_files
    )

    for (fn_in in input_files) {

      db_name <- db_name_from_fn(fn = fn_in, traits_fns = traits_data, dbname = db_base_name)

      conn <- connect(dbname = file.path(output, db_name))

      progress <- file.path(normalizePath(output), paste0("ADDING.", fn_in, ".TO.", db_name, ".ADDING"))
      error <- file.path(normalizePath(output), paste0("ERROR.ADDING.", fn_in, ".TO.", db_name, ".ERROR"))
      file.create(progress)

      tn <- tn_from_fn(fn_in, measures[i], tn_postfix = tn_postfix)


      dat <- switch(
        tools::file_ext(fn_in),
        "csv" = utils::read.csv(file.path(input, measures[i], fn_in)),
        "rds" = readRDS(file.path(input, measures[i], fn_in)),
        stop("No supported extension!")
      )
      dat <- as.data.frame(dat)
      names(dat) <- tolower(names(dat))

      if (!DBI::dbExistsTable(conn, tn)) {
        db_create_table(
          conn = conn,
          dat = dat[1:2,],
          table = tn
        )
      }


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


      # END TRANSACTION ------------------------------------------------------

      disconnect(conn)

      unlink(progress)
    }
  }



  # Finalise stuff ----------------------------------------------------------


  invisible(TRUE)
}

# Helper function ---------------------------------------------------------

#' Get file names of data files to be added to database
#'
#' @param input input folder
#' @param measure measure (folder name)
#' @param include_files list of files to be included
#'
#' @return file names to be imported
#' @export
get_inputfiles <- function(
    input,
    measure,
    include_files
){
  input_files <- list.files(
    path = file.path(input, measure),
    full.names = FALSE,
    recursive = FALSE
  )


  return(input_files[(input_files %in% include_files)])
}

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
#' @param traits_fns vector of files, which should be in the traits database
#' @param dbname default dbname
#' @param seperate_db
#'
#' @return
#' @export
#'
#' @examples
db_name_from_fn <- function(
  fn,
  traits_fns,
  dbname
) {
  fn <- tolower(gsub("\\.csv", "", fn))
  fn <- tolower(gsub("\\.rds", "", fn))

  if (any(grepl(fn, tolower(traits_fns)))){
    dbname <- paste0(dbname, ".traits")
  }

  dbname <- paste0(
    dbname,
    ".sqlite"
  )

  return(dbname)
}

#' Open connection to db
#'
#' Also create status report file in the dabname directory
#' @param dbname name of database
#'
#' @return connection object
#' @importFrom DBI dbConnect dbExecute
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
  DBI::dbExecute(conn, "PRAGMA temp_store=2")

  ## Set journal mode to memory. This is slightly unsafer but should not be a problem here
  DBI::dbExecute(conn, "PRAGMA journal_mode=MEMORY")

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
      paste0("CONNECTED.", dbname, ".CONNECTED")
    )
  )
  invisible(TRUE)
}

