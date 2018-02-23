to_datekey <- function(d){
  str_replace_all(d, "-", "")
}

date_filtered_query <- function(table, date_range, aggregations, where = NULL, group_by = NULL) {
  query <- paste(
    "SELECT",
    aggregations,
    "FROM", table,
    "WHERE datekey>=", to_datekey(date_range[1]), " AND datekey<=", to_datekey(date_range[2]),
    sep = " "
  )
  
  if(!is.null(where)){
    query <- paste(
      query,
      "AND", where,
      sep = " "
    )
  }
  
  if(!is.null(group_by)){
    query <- paste(
      query,
      "GROUP BY", group_by,
      sep = " "
    )
  }
  
  dbGetQuery(con, query)
}

