library(dplyr)
library(shiny)
library(shinyTime)
library(leaflet) 
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(RColorBrewer)
library(rsconnect)
library(dplyr)

source("real_time_data.R")
unique_stations = read.csv("location_info.csv")
load("model_output.rdata")
load("kmeans_models.rdata")

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
             box(textOutput("avgnum_bikes"),
                 width="200", height="200")
      ),
      
      hr(),
      
      #2 tabs for different outputs - map and cluster plot
      tabBox(
        width = 12,
        height = 600,
        tabPanel("Availability Map",
                 leafletOutput("map", height=500) %>% withSpinner(color="red")
        ),
        tabPanel("All Time Availabilities",
                 imageOutput("clusterplot", height=500) %>% withSpinner(color="red")
        )
      )
    )
  )
)
)

server <- function(input, output, session){
  #before loading - display map of all clusters and average of current availabilities
  dayofweek = weekdays(Sys.time())
  unique_stations = merge(merge_df, unique_stations[,c("station_id", "name")])
  cluster.df = data.frame("cluster" = 1:5, "cluster_color" = brewer.pal(5, "Dark2"))
  if(dayofweek == "Saturday"){
    is_SAT = 1
    is_SUN = 0
    km.clusters = data.frame("station_id" = 1:length(km.obj.sat$cluster), "cluster" =  data.frame(km.obj.sat$cluster)$km.obj.sat.cluster)
    merged = merge(unique_stations, km.clusters)
    merged = merge(merged, cluster.df)
  } else if (dayofweek == "Sunday"){
    is_SAT = 0
    is_SUN = 1
    km.clusters = data.frame("station_id" = 1:length(km.obj.sun$cluster), "cluster" =  data.frame(km.obj.sun$cluster)$km.obj.sun.cluster)
    merged = merge(unique_stations, km.clusters)
    merged = merge(merged, cluster.df)
  } else{
    is_SAT = 0
    is_SUN = 0
    km.clusters = data.frame("station_id" = 1:length(km.obj.weekdays$cluster), "cluster" =  data.frame(km.obj.weekdays$cluster)$km.obj.weekdays.cluster)
    merged = merge(unique_stations, km.clusters)
    merged = merge(merged, cluster.df)
  }
  
  output$map <- renderLeaflet({
    leaflet(data = merged) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addCircleMarkers(lng = ~lon , 
                       lat=~lat, 
                       popup=paste0(merged$name, ": ", as.character(merged$num_bikes_available), " bikes available"), 
                       radius = 4,
                       opacity = .7,
                       color = ~cluster_color
      )
  })
  
  output$avgnum_bikes = renderText({
    paste0("There are about ",as.character(trunc(mean(merged$num_bikes_available))), " bikes available at each station.")
  }) 
  
  output$clusterplot <- renderImage({
    if(is_SAT == 0 & is_SUN == 0){
      return(list(
        src = "www/Weekday_kmeans_plot.png",
        contentType = "image/png"
      ))
    }
    if(is_SAT == 1){
      return(list(
        src = "www/Saturday_kmeans_plot.png",
        contentType = "image/png"
      ))
    }
    if(is_SUN == 1){
      return(list(
        src = "www/Sunday_kmeans_plot.png",
        contentType = "image/png"
      ))
    }
  })
  
}

shinyApp(ui, server)