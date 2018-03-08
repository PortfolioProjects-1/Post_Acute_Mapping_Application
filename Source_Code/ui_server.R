

library(shinydashboard)
library(leaflet)
library(data.table)
library(DT, lib.loc = "C:/Program Files/R/R-3.4.3/library")
library(htmltools)

# API key
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
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = 1000,
                selectInput("city", "City:",City_list),
                       selectInput("radius", "Radius:",c("5","10","15","20")),
                          selectInput("condition", "Condition:",c("Asthma","Diabetes","Hyperlipidemia")
                                      

                
                )))))
   
# Define server logic 
server <- function(input, output) {

    plot_data <- reactive({
      newData <- SNF_2015_TX
      newData <- newData[(newData$City %in% input$city),]
      
    })
    
     output$SNF_Provider_Map <- renderLeaflet({
       plot_data <- plot_data()
       leaflet(plot_data) %>% ##change to filtered() 
         addTiles() %>%
           addCircleMarkers(lng = plot_data$lat,lat = plot_data$long, 
                                        
                                          popup = paste("<b>","Facility:","</b>", "<i>",plot_data$Facility_Name,"</i>", "<br>",
                                                        "<b>","Daily Cost:","</b>", "$",as.character(round(plot_data$Cost_Per_Day,2)), "<br>",
                                                        "<b>","Avg Length Of Stay:","</b>", as.character(plot_data$ALOS_Days), "<br>",
                                                        "<b>", "TotalStays", input$condition,":","</b>", if (input$condition == "Asthma") {ceiling(plot_data$Total_Stays_Asthma)} 
                                                        else if (input$condition == "Diabetes") {ceiling(plot_data$Total_Stays_Diabetes)} else {ceiling(plot_data$Total_Stays_Hyperlipidemia)}))
                  })
      
  
    #output$PacProviderTable <- renderTable({DT::datatable(data = SNF_2015_TX,
                                                                #options = list(pageLength = 10),
                                                                #rownames = FALSE)})
    
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