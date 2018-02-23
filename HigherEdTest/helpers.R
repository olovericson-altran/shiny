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

engagement_dashboard_ui <- function(){
  tabPanel(
    "Engagement Dashboard",
    
    fluidRow(
      
      column(
        2,
        wellPanel(
          h4("Impressions"),
          h5(textOutput("impressions"))
        )
      ),
      
      column(
        2,
        wellPanel(
          h4("Page views"),
          h5(textOutput("page_views"))
        )
      ),
      
      column(
        2,
        wellPanel(
          h4("Position views"),
          h5(textOutput("position_views"))
        )
      ),
      
      column(
        2,
        wellPanel(
          h4("Application clicks"),
          h5(textOutput("app_clicks"))
        )
      ),
      
      column(
        4,
        dateRangeInput("date", "Date", "2017-02-22", "2018-02-22")
      )
    ),
    
    fluidRow(
      column(8, wellPanel(h4("Visits by Country"),leafletOutput("map",  height = 690))),
      
      column(
        4,
        wellPanel(
          h4("Activity by School"),
          DT::dataTableOutput("school_activity")
        ),
        wellPanel(
          h4("Activity by Application"),
          DT::dataTableOutput("application_activity")
        )
      )
    )
  )
}