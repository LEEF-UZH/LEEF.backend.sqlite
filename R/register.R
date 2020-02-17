#' Register the processing of respirometer data in the LEEF.Data package
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @export
#'
register <- function() {
  if (is.null(system.file(package = "LEEF.Data"))) {
    stop("This function requres the package to be installed!")
  }
  LEEF.Data::add_additor( additor_sqlite )
  ##
  invisible(TRUE)
}

