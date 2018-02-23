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
library(lubridate)
library(highcharter)
library(xts)
library(leaflet)

source("engagement_dashboard.R")
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
  tags$style(HTML("
    .h5, h5 {
      font-size: 24px;
      font-weight: bold;
    }

    body {
      background-color: #9c143b;
    }

    .well {
      border: 1px solid #555;
      border-radius: 7px;
    }

    .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
      color: #fcf8e3;
      background-color: #333;
      font-weight: bold;
    }

    .nav-tabs>li>a {
      background-color: #f5f5f5;
      font-weight: bold;
      color: #a94442;
    }

    .irs-bar {
      background: #9c143b;
      border-top: 1px solid #9c143b;
      border-bottom: 1px solid #9c143b;
    }

    .irs-from, .irs-to, .irs-single {
      background: #9c143b;
    }
    ")),
  
  titlePanel(img(src="http://www.highered.global/wp-content/uploads/assets/highered-white.svg", height=60), windowTitle = "HigherEd"),
  
  tabsetPanel(
    engagement_dashboard_ui(),
    
    tabPanel(
      "When the students are engaging",
      wellPanel(
        h4("Unique Student Visits and Application Click by Date"),
        highchartOutput("unique_student_clicks", height = 850)
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  engagement_dashboard_server(con, input, output, countries, schools, applications)
  
  output$unique_student_clicks <- renderHighchart({
    unique_visits <- dbGetQuery(
      con, 
      "SELECT      
      datekey date_key,
      sum(UniqueStudents) unique_student_visits     
      FROM [dbo].[FactStudents]
      GROUP BY datekey"
    )
    
    application_click <- dbGetQuery(
      con, 
      "SELECT      
      datekey date_key,
      sum(Events) app_click     
      FROM [dbo].[FactEvents]
      WHERE EventName = 'ApplicationClick'
      GROUP BY datekey"
    )
    
    uc_student_df <- application_click %>% 
      inner_join(unique_visits, by = "date_key") %>% 
      mutate(date = ymd(date_key))
    
    highchart(type = "stock") %>% 
      hc_add_series(xts(x = uc_student_df$unique_student_visits, order.by = uc_student_df$date), type = "column", name="Unique visitors", color = "#9c143b") %>% 
      hc_add_series(xts(x = uc_student_df$app_click, order.by = uc_student_df$date), name="Application clicks")
      
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

