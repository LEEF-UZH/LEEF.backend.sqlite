db_add_data <- function(
  conn,
  data,
  table
){
  added <- FALSE

  timestamp <- unlist(
    DBI::dbGetQuery(conn, paste("SELECT DISTINCT timestamp FROM", table))
  )

  if (any(unique(data$timestamp) %in% timestamp)) {
    warning("!!! ", table, " not added as timestamp already present! !!!")
    return(FALSE)
  } else {
    DBI::dbBegin(conn)
    DBI::dbWriteTable(
      conn,
      name = table,
      value = data,
      overwrite = FALSE,
      append = TRUE
    )
    DBI::dbCommit(conn)
    return(TRUE)
  }



}
