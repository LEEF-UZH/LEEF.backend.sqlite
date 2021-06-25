#' Remove records from the database
#'
#' Remove from the database all records from the database which are associated
#'   with a measurement method and have the provided timestamp. The records are
#'   moved to a backup database whose name is returned.
#' @param db name of the database from which the records should be removed
#' @param timestamp \bold{required argument!} timestamp which should be removed
#' @param method method(s) from which the records should be removed. This is the first string
#' before a double underscore (\code{__}) in the table name. If not specified, records with the timestamp
#'   are removed from all tables.
#'
#' @return name of the backup database or, if no table for the specified method exists, \code{NULL}.
#'
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect dbDisconnect dbListTables dbGetQuery dbExecute
#' @export
#'
#' @examples
remove_data <- function(
  db,
  timestamp,
  method
) {
  if (missing(timestamp)){
    stop("`timestamp` mssing but required!")
  }
  conn_rdd <- NULL
  conn_bak <- NULL
  result <- NULL
  on.exit({
    if (exists("conn_rdd")) {
      if (class(conn_rdd) == "SQLiteConnection") {
        DBI::dbDisconnect(conn_rdd)
        rm(conn_rdd)
      }
    }
    if (exists("conn_bak")) {
      if (class(conn_bak) == "SQLiteConnection") {
        DBI::dbDisconnect(conn_bak)
        rm(conn_bak)
      }
    }
  })

  conn_rdd <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = db
  )

  tables <- DBI::dbListTables(conn_rdd)

  if (!missing(method)) {
    tables <- grep(
      pattern = paste0("^", method, "__", collapse = "|"),
      tables,
      value = TRUE
      )
  }

  if (length(tables) > 1) {
    if (missing(method)){
      method <- "all"
    }

    db_bak_tmp <- gsub(
      pattern = "\\.sqlite$",
      replacement = paste(
        ".BACKUP",
        paste0(method, collapse = "."),
        timestamp,
        "sqlite",
        sep = "."
      ),
      db
    )

    i <- 0
    while(
      file.exists(db_bak_tmp)
    ) {
      i <- i + 1
      db_bak_tmp <- gsub(
        pattern = "\\.sqlite$",
        replacement = paste0(".", i, ".sqlite"),
        db_bak_tmp
      )
    }
    db_bak <- db_bak_tmp

    conn_bak <- DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = db_bak
    )

    for (table in tables) {
      where <- paste0(
        " FROM",
        "  ", table,
        " WHERE",
        "  timestamp = ", timestamp

      )
      select <- paste0(
        " SELECT",
        "  * ",
        where,
        " ;"
      )
      delete <- paste0(
        " DELETE ",
        where,
        " ;"
      )

      dat <- DBI::dbGetQuery(
        conn_rdd,
        select
      )
      DBI::dbWriteTable(
        conn_bak,
        table,
        dat
      )
      DBI::dbExecute(
        conn_rdd,
        delete
      )
    }
    result <- db_bak
  }
  return(result)
}
