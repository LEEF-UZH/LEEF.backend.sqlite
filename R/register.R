#' Register the processing of respirometer data in the LEEF.Data package
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @importFrom LEEF.Data add_pre_processor add_extractor
#' @export
#'
register <- function() {
  LEEF.Data::add_extractor( pre_processor_respirometer )
  LEEF.Data::add_extractor( extractor_respirometer )
  ##
  invisible(TRUE)
}

