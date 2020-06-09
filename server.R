

#NEED TO CHANGE SLIDERS TO NONE LOW MEDIUM HIGH
server <- function(input, output) {
    output$whisky_plot <- renderPlot({
        
        tidy_whisky %>%
            filter(smoky == input$smoky) %>%
            filter(body == input$body) %>%
            filter(sweetness == input$sweetness) %>%
            # filter(ifelse(input$spicy == "Yes", spicy > 0, spicy == 0)) %>%
            #            filter(ifelse(input$medicinal == "Yes", medicinal > 0, medicinal == 0)) %>%
            #                       filter(ifelse(input$honey == "Yes", honey > 0, honey == 0)) %>%
            #                                  filter(ifelse(input$nutty == "Yes", nutty > 0, nutty == 0)) %>%
            #                                             filter(ifelse(input$floral == "Yes", floral > 0, floral == 0)) %>%
            # filter(tobacco == input$tobacco) %>%
            # filter(malty == input$malty) %>%
            # filter(winey == input$winey ) %>%
            # filter(fruity == input$fruity) %>%
            pivot_longer(smoky:floral, names_to = "flavour", values_to = "strength") %>% 
            group_by(distillery, flavour) %>% 
            ggplot() +
            aes(x = flavour, y = strength, fill = flavour) +
            geom_col(colour = "white") +
            facet_wrap( ~ distillery) +
            theme(strip.background = element_rect(fill = "brown")) +
            theme(strip.text = element_text(colour = "white",
                                            size = 15))
        
        
        
        
    })
    
    
    output$map <- renderLeaflet({
        
        
        tidy_whisky %>%
            filter(smoky == input$smoky) %>%
            filter(body == input$body) %>%
            filter(sweetness == input$sweetness) %>%
            # filter(ifelse(input$spicy == TRUE, filter(spicy > 0), filter(spicy == 0))) %>%
            # filter(ifelse(input$medicinal == "Yes", medicinal > 0, medicinal == 0)) %>%
            # filter(ifelse(input$honey == "Yes", honey > 0, honey == 0)) %>%
            # filter(ifelse(input$nutty == "Yes", nutty > 0, nutty == 0)) %>%
            # filter(ifelse(input$floral == "Yes", floral > 0, floral == 0)) %>%
            # filter(tobacco == input$tobacco) %>%
            # filter(malty == input$malty) %>%
            # filter(winey == input$winey ) %>%
            # filter(fruity == input$fruity) %>%
            leaflet() %>% 
            addTiles() %>% 
            addCircleMarkers(lat = ~longitude, lng = ~latitude, popup = ~distillery, color = ~pal(region)) %>%
            addLegend("topright", pal = pal, values = ~region,                                                                              title = "Whisky Region", opacity = 1
            )
        
        
    })
}