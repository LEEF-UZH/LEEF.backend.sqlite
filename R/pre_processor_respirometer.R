#' Preprocessor respirometer data
#'
#' Just copy from input to output
#'
#' @param input directory from which to read the data
#' @param output directory to which to write the data
#'
#' @return invisibly \code{TRUE} when completed successful
#'
#' @export

pre_processor_respirometer <- function(
  input,
  output
) {
  cat("\n########################################################\n")
  cat("\nProcessing respirometer\n")
  ##
  dir.create(
    file.path(output, "respirometer"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  file.copy(
    from = file.path(input, "respirometer", "."),
    to = file.path(output, "respirometer"),
    recursive = TRUE
  )
  ##
  cat("done\n")
  cat("\n########################################################\n")

  invisible(TRUE)
}
