db_add_data <- function(
  conn,
  data,
  table
){
  added <- FALSE

  if (DBI::dbExistsTable(conn, table)){
    timestamp <- unlist(
      DBI::dbGetQuery(conn, paste("SELECT DISTINCT timestamp FROM", table))
    )
  } else {
    timestamp <- "-99999999999999"
  }

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
