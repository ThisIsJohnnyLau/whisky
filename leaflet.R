library(leaflet)
library(CodeClanData)
library(shiny)

ui <- fluidPage(
    
    selectInput("region", "Which Region?", unique(tidy_whisky$region)),
    
    leafletOutput("map")
)

server <- function(input, output) {
    
    output$map <- renderLeaflet({
        
        
        tidy_whisky %>% 
            filter(region == input$region) %>% 
            leaflet() %>% 
            addTiles() %>% 
            addCircleMarkers(lat = ~longitude, lng = ~latitude, popup = ~distillery)
        
        
    })
    
}

shinyApp(ui = ui, server = server)




