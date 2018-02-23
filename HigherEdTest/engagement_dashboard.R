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

engagement_dashboard_server <- function(input, output, countries, schools, applications) {
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