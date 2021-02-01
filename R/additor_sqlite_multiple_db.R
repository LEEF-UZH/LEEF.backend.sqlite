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
#' @importFrom utils read.csv
#' @importFrom DBI dbCreateTable dbWriteTable dbExistsTable dbIsValid
#' @importFrom DBI dbBegin dbCommit dbRollback
#' @importFrom RSQLite SQLite
#' @export
#'
#' @examples
additor_sqlite_multiple_db <- function(
  input,
  output,
  tn_postfix = NULL
){

  # Some variable definitions -----------------------------------------------

  new_data_pattern <- "\\.csv$"

	exclude_files <- c(
		## general
		"compositions.csv", "experimental_design.csv",
		## flowcytometer
		"gates_coordinates.csv", "metadata_flowcytometer.csv"
	)

  db_base_name <- "LEEF.RRD"

	seperate_db <- c(
		## bemovi
		"master",
		"master_cropped",
		## flowcam
		"algae_traits",
		"algae_metadata"
	)

	# Helper function ---------------------------------------------------------

	get_db_name <- function( db_name, fn_in, seperate_db ) {
	  fn_in <- tolower(gsub("\\.csv", "", fn_in))

	  if (any(grepl(fn_in, seperate_db))) {
	    db_name <- paste0(
	      db_name,
	      "_",
	      gsub("\\.csv", "", fn_in),
	      ".sqlite"
	    )
	  } else {
	    db_name <- paste0(
	      db_base_name,
	      ".sqlite"
	    )
	  }
	}

	connect <- function( dbname ) {
	  conn <- DBI::dbConnect(
	    drv = RSQLite::SQLite(),
	    dbname = dbname
	  )
	  file.create(
	    file.path(
	      normalizePath(output),
	      paste0("CONNECTED.", basename(conn@dbname), ".CONNECTED")
	    )
	  )
	  return(conn)
	}

	disconnect <- function (conn ) {
	  dbname <- basename(conn@dbname)
	  DBI::dbDisconnect(conn)
	  unlink(
	    file.path(
	      normalizePath(output),
	      paste0("CONNECTED.", basename(conn@dbname), ".CONNECTED")
	    )
	  )
	}

  tn_from_fn <- function(fn, measure) {
  	tn <- tolower(gsub("\\.csv", "", fn))
  	if (is.null(tn_postfix)) {
			tn <- paste0(measure, "__", tn)
		} else {
			tn <- paste0(measure, "_", tn_postfix, "__", tn)
		}
    tn <- gsub("\\.", "_", tn)
		return(tn)
  }

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
  error    <- file.path(normalizePath(output), paste0("ERROR.ADDING.PREPARATION.ERROR"))

  file.create(progress)

  on.exit(
    {
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
    }
  )


# Read measures --------------------------------------------------------------------


  measures <- list.dirs(input, full.names = FALSE, recursive = FALSE)
  measures <- data.frame(
    measure = measures,
    db_name = paste0(db_base_name, "_", measures)
  )

  ##

  unlink(progress)

	# Create tables if they do not exist --------------------------------------------

  for (i in 1:nrow(measures)) {

    input_files <- list.files(
      path = file.path(input, measures$measure[i]),
      pattern = new_data_pattern,
      full.names = FALSE,
      recursive = FALSE
    )

    input_files <- input_files[!(input_files %in% exclude_files)]

    for (fn_in in input_files) {

      db_name <- get_db_name(
        measures$db_name[[i]],
        fn_in,
        seperate_db
      )

      progress <- file.path(normalizePath(output), paste0("PREPARING.", fn_in, ".TO.", db_name, ".PREPARING"))
      error    <- file.path(normalizePath(output), paste0("ERROR.PREPARING.", fn_in, ".TO.", db_name, ".PREPARING"))
      file.create(progress)

      conn <- connect( dbname = file.path(output, db_name) )

      tn <- tn_from_fn( fn_in, measures$measure[i] )

      if (!DBI::dbExistsTable(conn, tn)) {
	      dat <- utils::read.csv( file.path(input, measures$measure[i], fn_in), nrows = 10 )
	      dat <- as.data.frame(dat)
	      dat$timestamp <- "TIMESTAMP"

	      if ( db_name == paste0(db_base_name, ".sqlite") ) {
	        names(dat) <- tolower(names(dat))
	      }

				DBI::dbCreateTable(
					conn,
					name = tn,
					fields = dat
				)

				DBI::dbExecute(
					conn,
					paste0("CREATE INDEX idx_", tn, "_timetamp on ", tn, "(timestamp);")
				)
				DBI::dbExecute(
				  conn,
				  paste0("CREATE INDEX idx_", tn, "_bottle on ", tn, "(bottle);")
				)
				DBI::dbExecute(
				  conn,
				  paste0("CREATE INDEX idx_", tn, "_timestamp_bottle on ", tn, "(timestamp, bottle);")
				)
      }
	    disconnect(conn)

      unlink(progress)
    }
  }


	# Write data in one transaction --------------------------------------------

  for (i in 1:nrow(measures)) {

    input_files <- list.files(
      path = file.path(input, measures$measure[i]),
      pattern = new_data_pattern,
      full.names = FALSE,
      recursive = FALSE
    )

    input_files <- input_files[!(input_files %in% exclude_files)]

    for (fn_in in input_files) {

      db_name <- get_db_name(
        measures$db_name[[i]],
        fn_in,
        seperate_db
      )

      progress <- file.path(normalizePath(output), paste0("ADDING.", fn_in, ".TO.", db_name, ".ADDING"))
      error    <- file.path(normalizePath(output), paste0("ERROR.ADDING.", fn_in, ".TO.", db_name, ".ERROR"))
      file.create(progress)

      tn <- tn_from_fn( fn_in, measures$measure[i] )

      dat <- utils::read.csv(
      	file.path(input, measures$measure[i], fn_in)
      )
      dat <- as.data.frame(dat)

      if ( db_name == paste0(db_base_name, ".sqlite") ) {
        names(dat) <- tolower(names(dat))
      }

      conn <- connect( dbname = file.path(output, db_name) )

      # BEGIN TRANSACTION -------------------------------------------------------

      DBI::dbBegin(conn)

      skip_add <- FALSE

      timestamp <- unlist (
        DBI::dbGetQuery(conn, paste("SELECT DISTINCT timestamp FROM", tn))
      )
			if (any(unique(dat$timestamp) %in% timestamp)) {
				warning("!!! ", tn, " not added as timestamp already present! !!!")
				file.create(file.path(normalizePath(output), paste0("ERROR.ADDING.", tn, ".NOT_ADDED.TIMESTAMP_EXISTS.", timestamp, ".ERROR")))
				skip_add <- TRUE
			} else {
        DBI::dbWriteTable(
          conn,
          name = tn,
          value = dat,
          overwrite = FALSE,
          append = TRUE
        )
      }

      DBI::dbCommit(conn)
      x <- DBI::dbExecute(conn, "VACUUM")

      # END TRANSACTION ------------------------------------------------------

      disconnect(conn)

      unlink(progress)
    }
  }


  # Finalise stuff ----------------------------------------------------------


  invisible(TRUE)
}


