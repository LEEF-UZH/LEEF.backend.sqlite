db_create_table <- function(
  conn,
  dat,
  table
){
  dat$timestamp <- "TIMESTAMP"

  names(dat) <- tolower(names(dat))

  DBI::dbCreateTable(
    conn,
    name = table,
    fields = dat
  )

  DBI::dbExecute(
    conn,
    paste0("CREATE INDEX idx_", table, "_timetamp on ", table, "(timestamp);")
  )
  DBI::dbExecute(
    conn,
    paste0("CREATE INDEX idx_", table, "_bottle on ", table, "(bottle);")
  )
  DBI::dbExecute(
    conn,
    paste0("CREATE INDEX idx_", table, "_timestamp_bottle on ", table, "(timestamp, bottle);")
  )
}
