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
library(plotly)
library(xgboost)
library(lubridate)
library(shinyalert)
library(gbfs)

source("real_time_data.R")
source("k_means_clustering.R")
unique_stations = read.csv("location_info.csv")
unique_stations_2 = unique_stations[,names(unique_stations) != "X"]
load("model_output.RData")
load("kmeans_models.RData")
source("nearby.R")

gen_km_plot <- function(centers, by_n){
  df = data.frame(time = character(), prob = double(), cluster = character())
  k = ncol(centers)-1
  cluster_names = colnames(centers)
  for (i in 1:k){
    temp = data.frame(time = 1:(288/by_n), prob = centers[,i], cluster = cluster_names[i])
    df = rbind(df, temp)
  }
  ggplot(df, aes(x = time,y = prob, color = cluster, group = cluster)) +
    geom_line() +
    geom_point(size = 1)+
    scale_x_continuous(breaks = seq(1,(288/by_n), 12/by_n), labels = c(0:23)) +
    labs(x = "Hour",
         y = "Probability",
         title = "Probability of Getting a Bike per Hour by Station Group") +
    scale_color_manual(name="Station Group", labels = c("1", "2", "3", "4", "5"), values = brewer.pal(5, "Dark2")) +
    theme(text = element_text(size = 20),
          plot.title = element_text(hjust = 0.5), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
}

tryObserve <- function(x) {
  x <- substitute(x)
  env <- parent.frame()
  observe({
    tryCatch(
      eval(x, env),
      error = function(e) {
        showNotification(paste("Error: ", e$message), type = "error")
      }
    )
  })
}

time.options = c("Current Time", names(WEEKDAY_demand)[1:(length(names(WEEKDAY_demand))-1)])

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
      #useShinyalert(),
      column(3, 
             selectInput("location", "Select the Station Closest to You:", sort(unique_stations$name), selected = sort(unique_stations$name)[1], multiple=FALSE)
      ), 
      
      #user chooses day of week 
      column(2, 
             radioButtons(inputId="dayofweek", label="Day of Week:", 
                          choices=c("Weekday", "Saturday", "Sunday"))
      ),
      
      #useShinyalert(),
      #Input for real time or future data
      #Load button-used to trigger response
      column(3, 
             textInput("time", "Time(24 Hour Time in format 'XX:XX'):", value = "Current Time"),
             actionButton("load", "GO", style='color: #FF5349; font-size:150%')),
      #Output for percent availability - increase font and center, change color by % availability
      column(4, 
             box(textOutput("avgnum_bikes"), style="font-size:24px", width="200", height="200")
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
                 plotOutput("clusterplot", height=500) %>% withSpinner(color="red")
        )
      )
    )
  )
)
)

server <- function(input, output, session){
  
  ##before loading - display map of all clusters and average of current availabilities
  dayofweek = weekdays(Sys.time())
  unique_stations = merge(merge_df, unique_stations[,c("station_id", "name")])
  cluster.df = data.frame("cluster" = 1:5, "cluster_color" = brewer.pal(5, "Dark2"))
  if(dayofweek == "Saturday"){
    is_SAT = 1
    is_SUN = 0
    km.clusters = data.frame("station_id" = 1:length(km.obj.sat$cluster), "cluster" =  data.frame(km.obj.sat$cluster)$km.obj.sat.cluster)
    merged = merge(unique_stations, km.clusters)
    merged = merge(merged, cluster.df)
    output$clusterplot <- renderPl({
      gen_km_plot(gen_k_means_center(km.obj.sat, 5),12)
    })
  } else if (dayofweek == "Sunday"){
    is_SAT = 0
    is_SUN = 1
    km.clusters = data.frame("station_id" = 1:length(km.obj.sun$cluster), "cluster" =  data.frame(km.obj.sun$cluster)$km.obj.sun.cluster)
    merged = merge(unique_stations, km.clusters)
    merged = merge(merged, cluster.df)
    output$clusterplot <- renderPlot({
      gen_km_plot(gen_k_means_center(km.obj.sun, 5),12)
    })
  } else{
    is_SAT = 0
    is_SUN = 0
    km.clusters = data.frame("station_id" = 1:length(km.obj.weekdays$cluster), "cluster" =  data.frame(km.obj.weekdays$cluster)$km.obj.weekdays.cluster)
    merged = merge(unique_stations, km.clusters)
    merged = merge(merged, cluster.df)
    output$clusterplot <- renderPlot({
      gen_km_plot(gen_k_means_center(km.obj.weekdays, 5),12)
    })
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
      ) %>% 
      addLegend("bottomright", colors = brewer.pal(5, "Dark2"), labels=1:5,
                title = "Station Group",
                opacity = 1)
  })
  
  output$avgnum_bikes = renderText({
    paste0("There are approximately ",as.character(trunc(mean(merged$num_bikes_available))), " bikes available at each station.")
  })
  'output$Color = renderText({
    if(merged$num_bikes_available>3){
      renderText({ paste("hello input is","<font color=\"#006400\"><b>") })
    } else if (merged$num_bikes_available>0){
      renderText({ paste("hello input is","<font color=\"#FFD700\"><b>") })
    } else {
      renderText({ paste("hello input is","<font color=\"#8B0000\"><b>") })
    }
   })'
  
  ##click "GO"
  observeEvent(input$load, {
    unique_stations = merge(merge_df, unique_stations[,c("station_id", "name")])
    
    if(dayofweek != "Saturday" & dayofweek != "Sunday"){
      dayofweek="Weekday"
    }
    
    #observeEvent(input$load & (input$time != "Current Time")&(is.na(as.numeric(substr(input$time, 1, 2)))|is.na(as.numeric(substr(input$time, 4, 5)))), {
    #  if(input$load & (input$time != "Current Time")&(is.na(as.numeric(substr(input$time, 1, 2)))|is.na(as.numeric(substr(input$time, 4, 5))))){
    #    error=shinyalert("Oops!", "Please reload page and provide a valid time.", type = "error")
    #  }
    #    
    #})
    
    tryObserve(input$load & (input$time != "Current Time")&(is.na(as.numeric(substr(input$time, 1, 2)))|is.na(as.numeric(substr(input$time, 4, 5)))))
    
    if(input$time != "Current Time"){
      hours = as.numeric(substr(input$time, 1, 2))
      minutes =  as.numeric(substr(input$time, 4, 5))
      full.time = 60*hours + minutes
      full.time.sys = 60*hour(Sys.time()) + minute(Sys.time())
      time.diff = full.time - full.time.sys
    } else{
      time.diff=0
    }
    
    if(is.na(hours)|is.na(minutes)){
      output$avgnum_bikes = renderText({
        "Please provide a valid time."
      })
      return("Please reload page and provide a valid time.")
    }
    
    ##CURRENT TIME
    if((dayofweek == input$dayofweek) & ((input$time == "Current Time")|(time.diff>=0 & time.diff<=10))){
      selected.station = unique_stations[unique_stations$name == input$location,]
      stations.max.top5 = merge(find_nearest_5(as.numeric(unique_stations[unique_stations$name == input$location, "station_id"]), distance.mat), merge_df)
      stations.max.top6 = rbind(selected.station, stations.max.top5)
      
      if(input$dayofweek == "Saturday"){
          is_SAT = 1
          is_SUN = 0
          km.clusters = data.frame("station_id" = 1:length(km.obj.sat$cluster), "cluster" =  data.frame(km.obj.sat$cluster)$km.obj.sat.cluster)
          merged = merge(stations.max.top6, km.clusters)
          merged = merge(merged, cluster.df)
          output$clusterplot <- renderPlot({
            gen_km_plot(gen_k_means_center(km.obj.sat, 5),12)
          })
        } else if (input$dayofweek == "Sunday"){
          is_SAT = 0
          is_SUN = 1
          km.clusters = data.frame("station_id" = 1:length(km.obj.sun$cluster), "cluster" =  data.frame(km.obj.sun$cluster)$km.obj.sun.cluster)
          merged = merge(stations.max.top6, km.clusters)
          merged = merge(merged, cluster.df)
          output$clusterplot <- renderPlot({
            gen_km_plot(gen_k_means_center(km.obj.sun, 5),12)
          })
        } else{
          is_SAT = 0
          is_SUN = 0
          km.clusters = data.frame("station_id" = 1:length(km.obj.weekdays$cluster), "cluster" =  data.frame(km.obj.weekdays$cluster)$km.obj.weekdays.cluster)
          merged = merge(stations.max.top6, km.clusters)
          merged = merge(merged, cluster.df)
          output$clusterplot <- renderPlot({
            gen_km_plot(gen_k_means_center(km.obj.weekdays, 5),12)
          })
        }
        
        output$map <- renderLeaflet({
          leaflet(data = merged) %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addCircleMarkers(lng = ~lon , 
                             lat=~lat, 
                             popup=paste0(merged$name, ": ", as.character(merged$num_bikes_available), " bikes available"), 
                             radius = 20,
                             opacity = 1,
                             color = ~cluster_color
            ) %>% 
            addLegend("bottomright", colors = brewer.pal(5, "Dark2"), labels=1:5,
                      title = "Station Group",
                      opacity = 1)
        })
      
        output$avgnum_bikes = renderText({
          if(length(merged$cluster)>1){
            paste0("There are ", as.character(selected.station$num_bikes_available), " bikes available at ", selected.station$name, " station")
          } else{
            paste0("There are ", as.character(selected.station$num_bikes_available), " bikes available at ", selected.station$name, " station. There are no other bike stations nearby.")
            
          }
        })
        
        "output$Color = renderText({
          if(selected.station$num_bikes_available>3){
            as.character('fontsize: 200x; color:green')
          } else if (selected.station$num_bikes_available>0){
            as.character('fontsize: 200x; color:gold')
          } else {
            as.character('fontsize: 200x; color:red')
          }
        })"
    } else {
      #predict probability
      'observeEvent(input$load & !(station_info[station_info$name == input$location, "station_id"] %in% input_data[,1]), {
        if(input$load & !(station_info[station_info$name == input$location, "station_id"] %in% input_data[,1])){
          error=shinyalert("Oops!", "This station is not supported by our model. Please reload page and select another station.", type = "error")
        }
      })'
      
      if(!(station_info[station_info$name == input$location, "station_id"] %in% input_data[,1])){
        output$avgnum_bikes = renderText({
          "This station is not supported by our model. Please select another station."
        })
        return("This station is not supported by our model. Please select another station.")
      }
      
      selected.station = unique_stations_2[unique_stations_2$name == input$location,]
      stations.max.top5 = find_nearest_5(as.numeric(unique_stations[unique_stations$name == input$location, "station_id"]), distance.mat)
      stations.max.top6 = rbind(selected.station, stations.max.top5)
      if(input$time == "Current Time"){
        hours = hour(Sys.time())
        minutes= minute(Sys.time())
      }
      
      if(input$dayofweek == "Saturday"){
        is_SAT = 1
        is_SUN = 0
        km.clusters = data.frame("station_id" = 1:length(km.obj.sat$cluster), "cluster" =  data.frame(km.obj.sat$cluster)$km.obj.sat.cluster)
        merged = merge(stations.max.top6, km.clusters)
        merged = merge(merged, cluster.df)
        output$clusterplot <- renderPlot({
          gen_km_plot(gen_k_means_center(km.obj.sat, 5),12)
        })
      } else if (input$dayofweek == "Sunday"){
        is_SAT = 0
        is_SUN = 1
        km.clusters = data.frame("station_id" = 1:length(km.obj.sun$cluster), "cluster" =  data.frame(km.obj.sun$cluster)$km.obj.sun.cluster)
        merged = merge(stations.max.top6, km.clusters)
        merged = merge(merged, cluster.df)
        output$clusterplot <- renderPlot({
          gen_km_plot(gen_k_means_center(km.obj.sun, 5),12)
        })
      } else{
        is_SAT = 0
        is_SUN = 0
        km.clusters = data.frame("station_id" = 1:length(km.obj.weekdays$cluster), "cluster" =  data.frame(km.obj.weekdays$cluster)$km.obj.weekdays.cluster)
        merged = merge(stations.max.top6, km.clusters)
        merged = merge(merged, cluster.df)
        output$clusterplot <- renderPlot({
          gen_km_plot(gen_k_means_center(km.obj.weekdays, 5),12)
        })
      }
      
      merged = merged[merged$station_id %in% input_data[,1],]
      
      predicted = predict(model, as.matrix(data.frame("lat"=merged$lat, "lng"=merged$lon, "hour"=rep(hours, length(merged$cluster)),  "min"=rep(minutes, length(merged$cluster)), "is_SAT"=rep(is_SAT, length(merged$cluster)), "is_SUN"=rep(is_SUN, length(merged$cluster)))))
      merged = cbind(merged, predicted)
      
      output$map <- renderLeaflet({
        leaflet(data = merged) %>%
          addTiles() %>%  # Add default OpenStreetMap map tiles
          addCircleMarkers(lng = ~lon , 
                           lat=~lat, 
                           popup=paste0(merged$name, ": ", as.character(trunc(merged$predicted*100)), "% chance of finding an available bike"), 
                           radius = 20,
                           opacity = 1,
                           color = ~cluster_color
          ) %>% 
          addLegend("bottomright", colors = brewer.pal(5, "Dark2"), labels=1:5,
                    title = "Station Group",
                    opacity = 1)
      })
      
      output$avgnum_bikes = renderText({
          if(length(merged$cluster)>1){
            paste0("There is a ", as.character(trunc(merged[merged$station_id == selected.station$station_id,]$predicted*100)), "% chance of finding an available bike at ", selected.station$name, " station")
          } else{
            paste0("There is a ", as.character(trunc(merged[merged$station_id == selected.station$station_id,]$predicted*100)), "% chance of finding an available bike at ", selected.station$name, " station. There are no other bike stations nearby.")
          }
        })
      
      
      "output$Color = renderText({
        if(trunc(merged[merged$station_id == selected.station$station_id,]$predicted*100)>80){
          as.character('fontsize: 200x; color:green')
          } else if ((trunc(merged[merged$station_id == selected.station$station_id,]$predicted*100)>50)){
            as.character('fontsize: 200x; color:gold')
          } else {
            as.character('fontsize: 200x; color:red')
        }
      })"
    }
  })
}

shinyApp(ui, server)