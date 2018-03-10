
library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)
library(htmltools)
library(XML)
library(RCurl)
library(RJSONIO)
library(leaflet)
library(tidyverse)

# API_key <- "AIzaSyAPevncZzP2DndypLMvk66xZVsVpcN94c8"
# 
# Get_lat_long <- function(Address,API_key) {
#   URL <- paste("https://maps.googleapis.com/maps/api/geocode/xml?address=",gsub(" ","+",Address),"&key=",API_key,sep="")
#   webpage <- getURL(URL)
#   XML_code <- xmlParse(webpage)
#   XML_data <- xmlToList(XML_code)
#   Result <- tryCatch(data.frame(
#     Address = Address,
#     lat = XML_data$result$geometry$location$lat,
#     lng = XML_data$result$geometry$location$lng
#   ),error = function(e) NA)
#   Result$Address <- tryCatch(as.character(Result$Address),error = function(e) NA)
#   Result$lat <- tryCatch(as.numeric(as.character(Result$lat)),error = function(e) NA)
#   Result$lng <- tryCatch(as.numeric(as.character(Result$lng)),error = function(e) NA)
#   return(tryCatch(Result,error = function(e) NA))
# }
# 
# Convert_to_cart <- function(latitude,longitude,center_lat,radius=3959){
#   x = radius * longitude * cos(center_lat)
#   y = radius * latitude
#   return(cbind(x,y))
# }
# 
# Circle_coordinates <- function(center_x,center_y,radius,theta = seq(0,2*pi,length = 1000)){
#   x = center_x + (radius * cos(theta))
#   y = center_y + (radius * sin(theta))
#   return(cbind(x,y))
# }
# 
# Convert_to_geo <- function(x_coord,y_coord,radius=3959,center_lat){
#   lng = x_coord/(radius*cos(center_lat))
#   lat = y_coord/radius
#   return(cbind(lng,lat))
# }
# 
# Inside_circle <- function(x_coord,y_coord,radius,center_x,center_y){
#   return(((x_coord-center_x)^2+(y_coord-center_y)^2) < radius^2)
# }
# 
# Find_close_SNFs <- function(Home_address,desired_radius,US_center = c(lat = 41.69264,lng = -98.789062),Lat_lng){
#   desired_radius = desired_radius * 51.23298
#   Home <- Get_lat_long(Address = Home_address, API_key = API_key)
#   center <- Convert_to_cart(
#     latitude = Home$lat,
#     longitude = Home$lng,
#     center_lat = US_center["lat"]
#   )
#   SNF_points <- Convert_to_cart(
#     latitude = Lat_lng$lat,
#     longitude = Lat_lng$long,
#     center_lat = US_center["lat"]
#   )
#   SNF_points <- data.frame(SNF_points)
#   Indicator_close_by <- Inside_circle(
#     x_coord = SNF_points[,"x"],
#     y_coord = SNF_points[,"y"],
#     radius = desired_radius,
#     center_x = center[1],
#     center_y = center[2]
#   )
#   Lat_lng <- data.frame(
#     cbind(
#       Lat_lng,
#       SNF_points,
#       Indicator_close_by
#     )
#   )
#   Lat_lng <- Lat_lng[!is.na(Lat_lng$Indicator_close_by) & Lat_lng$Indicator_close_by == TRUE,]
#   Lat_lng$loc <- "SNF"
#   Lat_lng <- Lat_lng %>% select(Provider_ID, lat, long, loc)
#   Lat_lng <- data.frame(
#     rbind(
#       Lat_lng,
#       c(Provider_ID = "NA",lat = Home$lat,long = Home$lng,loc = "Home")
#     )
#   )
#   Lat_lng$lat <- as.numeric(Lat_lng$lat)
#   Lat_lng$long <- as.numeric(Lat_lng$long)
#   return(Lat_lng)
# }
# }

# City list 
City_list <- as.list(unique(SNF_2015_TX$City))
names(City_list) <- unique(SNF_2015_TX$City)

# Define body of dashboard
ui <- dashboardBody(
fluidRow(
  column(width = 9,
         box(width = NULL, solidHeader = TRUE,
             leafletOutput("SNF_Provider_Map", width = "100%", height = 1000)
         ),
      
absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, bottom = 690, right = 20,
                width = 300, 
                selectInput("city", "City:",City_list),
                          selectInput("condition", "Condition:",c("Asthma","Diabetes","Hyperlipidemia"))
                              #,textInput(inputId = "addressInput",label = "Home address",value = "", placeholder = "Home address?"),
                                  #textInput(inputId = "distanceInput",label = "Travel distance",value = "",placeholder = "How far will you go?"),
              # actionButton("SNFAction", "Find SNF.", 
              #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
              
                ))))
   
# Define server logic 
server <- function(input, output) {
  
  Lat_lng = data.frame(SNF_2015_TX[,c("Provider_ID","lat","long")])
  names(Lat_lng) = c("Provider_ID","long","lat")
  
  close_SNFs <- eventReactive(input$SNFAction, {
    Data_plot <- Find_close_SNFs(
      Home_address = input$addressInput,
      desired_radius = as.numeric(input$distanceInput),
      US_center = c(lat = 41.69264,lng = -98.789062),
      Lat_lng = Lat_lng
   )})

    plot_data <- reactive({
      newData <- SNF_2015_TX
      newData <- newData[(newData$City %in% input$city),]
      
    })
    
     output$SNF_Provider_Map <- renderLeaflet({
       plot_data <- plot_data()
       #plot_data <- close_SNFs()
       leaflet(plot_data) %>% ##change to filtered() 
         addTiles() %>%
           addCircleMarkers(lng = plot_data$lat,lat = plot_data$long, 
                                        
                                          popup = paste("<b>","Facility:","</b>", "<i>",plot_data$Facility_Name,"</i>", "<br>",
                                                        "<b>","Daily Cost:","</b>", "$",as.character(round(plot_data$Cost_Per_Day,2)), "<br>",
                                                        "<b>","Avg Length Of Stay:","</b>", as.character(plot_data$ALOS_Days), "<br>",
                                                        "<b>", "TotalStays", input$condition,":","</b>", if (input$condition == "Asthma") {ceiling(plot_data$Total_Stays_Asthma)} 
                                                        else if (input$condition == "Diabetes") {ceiling(plot_data$Total_Stays_Diabetes)} else {ceiling(plot_data$Total_Stays_Hyperlipidemia)}))
                  })
      
  
    output$PacProviderTable <- renderDataTable(SNF_2015_TX)
}
# Run the application 
shinyApp(ui = ui, server = server)



# filtered <- reactive ({
#   dataset %>%
#     filter(column1 == input$year, ##use id for input
#            column2 == input$radius, ##use id for input
#            column3 == input$condition)
#   
# })


##Use filtered()

##maybe use isolate to fix dropdown feature 