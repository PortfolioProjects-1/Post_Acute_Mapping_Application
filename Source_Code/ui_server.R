#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinydashboard)
library(leaflet)
library(data.table)
library(DT)
# Define body of dashboard
ui <- dashboardBody(
fluidRow(
  column(width = 9,
         box(width = NULL, solidHeader = TRUE,
             leafletOutput("SNF_Provider_Map", height = 500)
         ),
         
         box(width = NULL,
             #uiOutput("PacProviderTable"),
            DT::dataTableOutput("PacProviderTable")
         )),
         column(width = 3,
          box(width = NULL, status = "warning",
            selectInput("year", "Year:", c("2013", "2014", "2015")),
                  selectInput("radius", "Radius:",c("5","10","15","20")),
                    selectInput("condition", "Condition:",c("Asthma","Diabetes","Hyperlipidemia"))))))


# Define server logic 
server <- function(input, output) {
content<- "A"
    output$SNF_Provider_Map <- renderLeaflet({
    leaflet(SNF_2015_TX) %>% addPopups(SNF_2015_TX$lat, SNF_2015_TX$long, content,
                                       options = popupOptions(closeButton = FALSE)) %>%
                                         addTiles() %>% addMarkers(lat = ~long, lng = ~lat)})
    
     output$SNF_Provider_Map <- renderLeaflet({
       leaflet(SNF_2015_TX) %>% 
         addTiles() %>%
           addCircleMarkers(lng = SNF_2015_TX$lat,lat = SNF_2015_TX$long, 
                                        
                                          popup = ~Cost_Per_Day
                                         ) 
        })
      
  
    #output$PacProviderTable <- renderTable({DT::datatable(data = SNF_2015_TX,
                                                                #options = list(pageLength = 10),
                                                                #rownames = FALSE)})
    
    output$PacProviderTable <- renderDataTable(SNF_2015_TX)
}
# Run the application 
shinyApp(ui = ui, server = server)

