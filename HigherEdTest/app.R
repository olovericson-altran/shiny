#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(lubridate)
library(shiny)
library(dplyr)
library(dbplyr)
library(stringr)
library(DBI)

source("helpers.R")

con <- pool::dbPool(odbc::odbc(),
                    Driver = "ODBC Driver 13 for SQL Server",
                    Server = "higheredanalytics.database.windows.net",
                    Database = "analytics",
                    UID = "AnalyticsDWHAdmin",
                    PWD = "SJhg8ws72db=!+So")

countries <- readr::read_rds("country_centroids")
schools <- tbl(con, "content_archive_st_het_schools") %>% select(SchoolId = instance_id, name = f_title) %>% collect()


ui <- fluidPage(
  titlePanel("HigherEd dashboard", windowTitle = "HigherEd"),
  
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
    column(8, wellPanel(h4("Visits by Country"),leafletOutput("map",  height = 600))),
    
    column(
      4,
      wellPanel(
        h4("Activity by School"),
        DT::dataTableOutput("school_activity")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  fact_visits <- reactive({
    date_filtered_query(
      "FactVisits",
      input$date,
      "SUM(CASE WHEN APPLICATIONID!=0 THEN Visits ELSE 0 END) position_views,
       SUM(Visits) page_views"
    )
  })
  
  fact_events <- reactive({
    date_filtered_query(
      "FactEvents",
      input$date,
      "SUM(CASE WHEN EventName='ApplicationClick' THEN Events ELSE 0 END) app_clicks,
       SUM(Events) impressions"
    )
  })
  
  output$impressions <- renderText({
    fact_events()$impressions
   })
  
  output$page_views <- renderText({
    fact_visits()$page_views
  })
  
  output$position_views <- renderText({
    fact_visits()$position_views
  })
  
  output$app_clicks <- renderText({
    fact_events()$app_clicks
  })
  
  output$map <- renderLeaflet({
    country_events <- date_filtered_query(
      "FactEvents",
      input$date,
      "[SchoolCountryName] Country,
       sum([Events]) Events",
      group_by = "SchoolCountryName"
    )
    
    country_events <- country_events %>% 
      inner_join(countries, by = c("Country" = "SHORT_NAME")) %>% 
      mutate(Events = 5000000 * sqrt(Events)/sum(sqrt(Events)))
    
    leaflet(country_events) %>% 
      setView(lng = 2, lat = 30, zoom = 2) %>% 
      addTiles() %>% 
      addCircles(lat = ~LAT, lng = ~LONG, weight = 1, radius = ~ Events, label = ~FULL_NAME)
  })
  
  output$school_activity <- DT::renderDataTable({
    
    school_events <- date_filtered_query(
      "FactEvents",
      input$date,
      "[SchoolId],
       sum([Events]) Events",
      where = "EventName = 'ApplicationClick'",
      group_by = "SchoolId"
    )
    
    school_activity <- school_events %>% 
      inner_join(schools, by = "SchoolId") %>% 
      select(Events, `School Name` = name) %>% 
      arrange(-Events)
    
    DT::datatable(school_activity, rownames = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

