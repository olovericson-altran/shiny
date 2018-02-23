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
  tabsetPanel(
    engagement_dashboard_ui(),
    
    tabPanel(
      "When the students are engaging",
      wellPanel(
        h4("Unique Student Visits and Application Click by Date"),
        plotOutput("unique_student_clicks")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  engagement_dashboard_server(input, output, countries, schools, applications)
  
  output$unique_student_clicks <- renderPlot({
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
      WHERE EventName = 'PositionImpression'
      GROUP BY datekey"
    )
    
    scaling_coeff = mean(application_click$app_click)/mean(unique_visits$unique_student_visits)
    
    application_click %>% 
      inner_join(unique_visits, by = "date_key") %>% 
      mutate(date = ymd(date_key)) %>% 
      ggplot(mapping = aes(x = date)) + 
      geom_bar(mapping = aes(y = unique_student_visits * scaling_coeff), stat="identity", na.rm = TRUE, color = "lightblue", fill = "lightblue") +
      geom_line(mapping = aes(y = app_click), size = 1) +
      scale_y_continuous(sec.axis = sec_axis(~./scaling_coeff, name = "Unique student visits"), name = "Application clicks")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

