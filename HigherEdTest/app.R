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
applications <- tbl(con, "content_archive_st_het_adverts") %>% select(ApplicationId = instance_id, name = f_title) %>% collect()


ui <- fluidPage(
  tabsetPanel(
    engagement_dashboard_ui()
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
    fact_events()$impressions %>% prettyNum(scientific=FALSE, big.mark=",")
   })
  
  output$page_views <- renderText({
    fact_visits()$page_views %>% prettyNum(scientific=FALSE, big.mark=",")
  })
  
  output$position_views <- renderText({
    fact_visits()$position_views %>% prettyNum(scientific=FALSE, big.mark=",")
  })
  
  output$app_clicks <- renderText({
    fact_events()$app_clicks %>% prettyNum(scientific=FALSE, big.mark=",")
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
    
    # a custom table with both header and footer
    sketch = htmltools::withTags(table(
      DT::tableHeader(school_activity)
    ))
    
    DT::datatable(school_activity,  container = sketch, options = list(pageLength = 5, dom = 'tip'), rownames = FALSE, selection = "none")
  })
  
  output$application_activity <- DT::renderDataTable({
    application_events <- date_filtered_query(
      "FactEvents",
      input$date,
      "ApplicationId,
      sum([Events]) Events",
      where = "EventName = 'ApplicationClick'",
      group_by = "ApplicationId"
    )
    
    application_activity <- application_events %>% 
      inner_join(applications, by = "ApplicationId") %>% 
      select(Events, `Application Name` = name) %>% 
      arrange(-Events)
    
    # a custom table with both header and footer
    sketch = htmltools::withTags(table(
      DT::tableHeader(application_activity)
    ))
    
    DT::datatable(application_activity,  container = sketch, options = list(pageLength = 5, dom = 'tip'), rownames = FALSE, selection = "none")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

