#' Create folder structure for data import and processing
#'
#' @param config_file config file to use. If none is specified, \code{cofig.yml} in the current working directory will be used.
#' @param tts_api_key api key for getting the Trusted Timestamp from OriginStamp. Default is reading from the environmental variable \code{api_key}.
#'
#' @return invisible \code{TRUE}
#'
#' @importFrom yaml yaml.load_file
#' @importFrom magrittr %>%
#' @importFrom ROriginStamp ROriginStamp_options
#'
#' @export
#'
#' @examples
initialize_db <- function(
  config_file,
  tts_api_key = Sys.getenv("api_key")
){

  # Set api key from environmental variable ---------------------------------

  ROriginStamp::ROriginStamp_options(
    api_key = tts_api_key
  )

  # Define default config file ----------------------------------------------

  if (missing(config_file)) {
    config_file <- file.path( getwd(), "config.yml" )
  }

  # Load Config -------------------------------------------------------------

  cfg <- yaml::yaml.load_file(config_file)

  # Fill in default values if data missing ----------------------------------

  if (is.null(cfg[["root_dir"]])) {
    cfg[["root_dir"]] <- getwd()
  }

  if (is.null(cfg[["to_be_imported"]])) {
    cfg[["to_be_imported"]] <- "ToBeImported"
  }

  if (is.null(cfg[["last_added"]])) {
    cfg[["last_added"]] <- "LastAdded"
  }
  if (is.null(cfg[["archive"]])) {
    cfg[["archive"]] <- "Archive"
  }

  if (is.null(cfg[["database"]][["dbpath"]])) {
    cfg[["database"]][["dbpath"]] <- file.path(
      getwd(),
      strsplit( cfg[["database"]][["dbname"]], "\\.")[[1]][[1]]
    )
  }

  # Set Directories --------------------------------------------------------

  DATA_options(
    root_dir = cfg[["root_dir"]],
    config_name = cfg[["config_name"]],
    to_be_imported = file.path( cfg[["root_dir"]], cfg[["to_be_imported"]] ),
    last_added = file.path( cfg[["root_dir"]], cfg[["last_added"]] ),
    archive = file.path( cfg[["root_dir"]], cfg[["archive"]] )
  )


  # Set Archive Options -----------------------------------------------------

  DATA_options(
    archive_name = cfg[["archive_name"]],
    archive_compression = cfg[["archive_compression"]]
  )



  # Set TTS and DOI ---------------------------------------------------------

  DATA_options(
    tts = cfg[["tts"]],
    tts_info = cfg[["tts_info"]],
    doi = cfg[["doi"]]
  )

  # Set data_connection -----------------------------------------------------

  DATA_options(
    database = cfg[["database"]],
    data_connection = NULL
  )

  # Add preprocessors -------------------------------------------------------

  DATA_options(
    pre_processors = list()
  )

  add_pre_processor( pre_processor_flowcam )
  add_pre_processor( pre_processor_flowcytometer )

  # Add Extractors ----------------------------------------------------------

  DATA_options(
    extractors = list()
  )

  add_extractor( extractor_flowcam )
  add_extractor( extractor_incubatortemp )
  add_extractor( extractor_flowcytometer )
  add_extractor( extractor_manualcount )
  add_extractor( extractor_respirometer )


  # And the end -------------------------------------------------------------



  # ToBeImported folder structure -------------------------------------------

  dir.create( DATA_options("to_be_imported"), showWarnings = FALSE )
  sources <- c(
    DATA_options("pre_processors") %>% names() %>% gsub("pre_processor_", "", .),
    DATA_options("extractors") %>% names() %>% gsub("extractor_", "", .)
  ) %>% unique()
  for (d in sources) {
    dir.create( file.path(DATA_options("to_be_imported"), d), showWarnings = FALSE )
  }

  # Archive folder structure ------------------------------------------------

  dir.create( DATA_options("archive"), showWarnings = FALSE )

  # LastAdded folder structure ----------------------------------------------

  dir.create( DATA_options("last_added"), showWarnings = FALSE )

  # DB folder structure -----------------------------------------------------

  dir.create( DATA_options("database")$dbpath, showWarnings = FALSE )
  file.create( file.path( DATA_options("database")$dbpath, DATA_options("database")$dbname ) )
}
