#' Register the processing of o2meter data in the LEEF package
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @export
#'
register <- function() {
  if (is.null(system.file(package = "LEEF.2"))) {
    stop("This function requres the package to be installed!")
  }
  LEEF.2::add_additor( additor_sqlite_multiple_db )
  ##
  invisible(TRUE)
}

