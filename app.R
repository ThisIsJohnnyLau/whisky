

ui <- fluidPage(
    titlePanel("Whisky Distilleries of Scotland"),
    
    sidebarLayout(
        
        
        sidebarPanel(
            sliderInput("smoky",
                        "Smoky",
                        min = 0,
                        max = 4,
                        value = 1.5),
            
            sliderInput("body",
                        "Body",
                        min = 0,
                        max = 4,
                        value = 1.5),
            
            # sliderInput("medicinal",
            #              "Medicinal",
            #              min = 0,
            #             max = 4,
            #             value = 1.5),
            
            sliderInput("sweetness",
                        "Sweetness",
                        min = 1,
                        max = 4,
                        value = 1.5),
            
            # sliderInput("tobacco",
            #             "Tobacco",
            #             min = 0,
            #             max = 1,
            #             value = 0.5),
            
            sliderInput("malty",
                        "Malty",
                        min = 0,
                        max = 4,
                        value = 1.5),
            
            sliderInput("honey",
                        "Honey",
                        min = 0,
                        max = 4,
                        value = 1.5),
            
            sliderInput("spicy",
                        "Spicy",
                        min = 0,
                        max = 4,
                        value = 1.5),
            
            # sliderInput("winey",
            #             "Winey",
            #             min = 0,
            #             max = 4,
            #             value = 1.5),
            
            # sliderInput("nutty",
            #             "Nutty",
            #             min = 0,
            #             max = 4,
            #             value = 1.5),
            
            sliderInput("fruity",
                        "Fruity",
                        min = 0,
                        max = 3,
                        value = 1.5),
            
            sliderInput("floral",
                        "Floral",
                        min = 0,
                        max = 4,
                        value = 1.5)

        ),
     
        
        mainPanel(
            
            # selectInput("region", "Which Region?", unique(tidy_whisky$region)),
            
            leafletOutput("map"),
            
            plotOutput("whisky_plot")
        
        )
    )
)
#NEED TO CHANGE SLIDERS TO NONE LOW MEDIUM HIGH
server <- function(input, output) {
    output$whisky_plot <- renderPlot({
        
        tidy_whisky %>%
            filter(spicy > input$spicy - 1.5 & spicy < input$spicy + 1.5) %>%
            filter(smoky > input$smoky - 1.5 & smoky < input$smoky + 1.5) %>%
            filter(body > input$body - 1.5 & body < input$body + 1.5) %>%
            # filter(medicinal > input$medicinal - 1.5 & sweetness < input$medicinal + 1.5) %>%
            # filter(tobacco > input$tobacco - 1.5 & tobacco < input$tobacco + 1.5) %>%
            filter(honey > input$honey - 1.5 & honey < input$honey + 1.5) %>%
            filter(sweetness > input$sweetness - 1.5 & sweetness < input$sweetness + 1.5) %>%
            # filter(winey > input$winey - 1.5 & winey < input$winey + 1.5) %>%
            # filter(nutty > input$nutty - 1.5 & nutty < input$nutty + 1.5) %>%
            filter(fruity > input$fruity - 1.5 & fruity < input$fruity + 1.5) %>%
            filter(malty > input$malty - 1.5 & malty < input$malty + 1.5) %>%
            filter(floral > input$floral - 1.5 & floral < input$floral + 1.5) %>%
            pivot_longer(smoky:floral, names_to = "flavour", values_to = "strength") %>% 
            group_by(distillery, flavour) %>% 
            ggplot() +
            aes(x = flavour, y = strength) +
            geom_col() +
            facet_wrap(~distillery)

        
        
        
    })
    
    output$map <- renderLeaflet({
        
            tidy_whisky %>%
            filter(spicy > input$spicy - 1.5 & spicy < input$spicy + 1.5) %>%
            filter(smoky > input$smoky - 1.5 & smoky < input$smoky + 1.5) %>%
            filter(body > input$body - 1.5 & body < input$body + 1.5) %>%
            # filter(medicinal > input$medicinal - 1.5 & sweetness < input$medicinal + 1.5) %>%
            # filter(tobacco > input$tobacco - 1.5 & tobacco < input$tobacco + 1.5) %>%
            filter(honey > input$honey - 1.5 & honey < input$honey + 1.5) %>%
            filter(sweetness > input$sweetness - 1.5 & sweetness < input$sweetness + 1.5) %>%
            # filter(winey > input$winey - 1.5 & winey < input$winey + 1.5) %>%
            # filter(nutty > input$nutty - 1.5 & nutty < input$nutty + 1.5) %>%
            filter(fruity > input$fruity - 1.5 & fruity < input$fruity + 1.5) %>%
            filter(floral > input$floral - 1.5 & floral < input$floral + 1.5) %>%
            leaflet() %>% 
            addTiles() %>% 
            addCircleMarkers(lat = ~longitude, lng = ~latitude, popup = ~distillery)
        
        
    })
}

shinyApp(ui, server)