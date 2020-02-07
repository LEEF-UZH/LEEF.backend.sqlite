#' Extractor respirometer data
#'
#' Convert all \code{.cvs} files in \code{respirometer} folder to \code{data.frame} and save as \code{.rds} file.
#'
#' This function is extracting data to be added to the database (and therefore make accessible for further analysis and forecasting)
#' from \code{.csv} files.
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom dplyr bind_rows
#' @importFrom readr read_csv
#' @export
#'
extractor_respirometer <- function( input, output ) {
  message("\n########################################################\n")
  message("Extracting respirometer\n")

  # Get csv file names ------------------------------------------------------

  respirometer_path <- file.path( input, "respirometer" )
  respirometer_files <- list.files(
    path = respirometer_path,
    pattern = "*.csv",
    full.names = TRUE,
    recursive = TRUE
  )

  if (length(respirometer_files) == 0) {
    message("nothing to extract\n")
    message("\n########################################################\n")
    return(invisible(FALSE))
  }

# Read file ---------------------------------------------------------------

  res <- lapply(
    respirometer_files,
    readr::read_csv2,
    skip = 1
  )
  # combine intu one large tibble
  res <- dplyr::bind_rows(res)
  res <- dplyr::filter(res, Date != "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

# SAVE --------------------------------------------------------------------

  add_path <- file.path( output, "respirometer" )
  dir.create( add_path, recursive = TRUE, showWarnings = FALSE )
  saveRDS(
    object = res,
    file = file.path(add_path, "respirometer.rds")
  )

# Finalize ----------------------------------------------------------------

  message("done\n")
  message("\n########################################################\n")

  invisible(TRUE)
}
