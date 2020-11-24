library(dplyr)
library(shiny)
library(shinyTime)
library(leaflet) 
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(RColorBrewer)
library(rsconnect)

unique_stations = read.csv("location_info.csv")

time.options = sapply(c(names(WEEKDAY_demand)[2:(length(names(WEEKDAY_demand))-1)]), substring, 2, simplify = TRUE, USE.NAMES = FALSE)
time.options = c("Current Time", sapply(time.options, function(y) gsub("\\.", ":",y), simplify = TRUE, USE.NAMES = FALSE))

ui <- shinyUI(dashboardPage(
  skin="red",
  
  dashboardHeader(
    # Try to add image in title
    #title = c("Bicycle Availability in Washington D.C", img(icon("bicycle"))),
    title = "Bicycle Availability in Washington D.C",
    titleWidth = 400
  ),
  
  dashboardSidebar(disable=TRUE),
  
  dashboardBody(
    
    fluidRow(
      
      #location
      column(3, 
             selectInput("location", "Select the Station Closest to You:", sort(unique_stations$name), selected = sort(unique_stations$name)[1], multiple=FALSE)
      ), 
      
      #user chooses day of week 
      column(2, 
             radioButtons(inputId="dayofweek", label="Day of Week:", 
                          choices=c("Weekday", "Saturday", "Sunday"))
      ),
      
      #Input for real time or future data
      #Load button-used to trigger response
      column(3, 
             selectInput("time", "Time:", time.options, selected = "Current Time", multiple=FALSE),
             actionButton("load", "GO", style='color: #FF5349; font-size:150%')),
      #Output for percent availability - increase font and center, change color by % availability
      column(4, 
             box(width="200", height="200")
      ),
      
      hr(),
      
      #2 tabs for different outputs - map and cluster plot
      tabBox(
        width = 12,
        height = 600,
        tabPanel("Availability Map"
        ),
        tabPanel("All Time Availabilities"
        )
      )
    )
  )
)
)

server <- function(input, output, session){
  #scenario 1 - before loading
  
}

shinyApp(ui, server)