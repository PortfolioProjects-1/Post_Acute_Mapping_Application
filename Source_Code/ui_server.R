#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(shinydashboard)
library(leaflet)
library(data.table)
library(DT, lib.loc = "C:/Program Files/R/R-3.4.3/library")
library(htmltools)
# Define body of dashboard
ui <- dashboardBody(
fluidRow(
  column(width = 9,
         box(width = NULL, solidHeader = TRUE,
             leafletOutput("SNF_Provider_Map", width = "100%", height = 1000)
         ),
      
  
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                selectInput("year", "Year:", c("2013", "2014", "2015")),
                       selectInput("radius", "Radius:",c("5","10","15","20")),
                          selectInput("condition", "Condition:",c("Asthma","Diabetes","Hyperlipidemia")
                
                )))))
         # column(width = 3,
         #  box(width = NULL, status = "warning",
         #    selectInput("year", "Year:", c("2013", "2014", "2015")),
         #          selectInput("radius", "Radius:",c("5","10","15","20")),
         #            selectInput("condition", "Condition:",c("Asthma","Diabetes","Hyperlipidemia"))))))


# Define server logic 
server <- function(input, output) {
content<- "A"
    # output$SNF_Provider_Map <- renderLeaflet({
    # leaflet(SNF_2015_TX) %>% addPopups(SNF_2015_TX$lat, SNF_2015_TX$long, content,
    #                                    options = popupOptions(closeButton = FALSE)) %>%
    #                                      addTiles() %>% addMarkers(lat = ~long, lng = ~lat)})
    
     output$SNF_Provider_Map <- renderLeaflet({
       leaflet(SNF_2015_TX) %>% ##change to filtered() 
         addTiles() %>%
           addCircleMarkers(lng = SNF_2015_TX$lat,lat = SNF_2015_TX$long, 
                                        
                                          popup = paste("<b>","Facility:","</b>", "<i>",SNF_2015_TX$Facility_Name,"</i>", "<br>",
                                                        "<b>","Daily Cost:","</b>", as.character(round(SNF_2015_TX$Cost_Per_Day,2)), "<br>",
                                                        "<b>","Avg Length Of Stay:","</b>", as.character(SNF_2015_TX$ALOS_Days)))
                                                        #paste("Facility:", SNF_2015_TX$Facility_Name,"<br>",
                                          #   "Daily Cost:",as.character(round(SNF_2015_TX$Cost_Per_Day,2),"<br>",
                                          #                              "Daily Cost:",as.character(round(SNF_2015_TX$Cost_Per_Day,2))))
                                         
        })
      
  
    #output$PacProviderTable <- renderTable({DT::datatable(data = SNF_2015_TX,
                                                                #options = list(pageLength = 10),
                                                                #rownames = FALSE)})
    
    output$PacProviderTable <- renderDataTable(SNF_2015_TX)
}
# Run the application 
shinyApp(ui = ui, server = server)
#`library(DT, lib.loc = "C:/Users/kenne/Documents/R/win-library/3.4")` I think will do it.



filtered <- reactive ({
  dataset %>%
    filter(column1 == input$year, ##use id for input
           column2 == input$radius, ##use id for input
           column3 == input$condition)
  
})


##Use filtered()

##maybe use isolate to fix dropdown feature 