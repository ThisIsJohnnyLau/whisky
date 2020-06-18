

#NEED TO CHANGE SLIDERS TO NONE LOW MEDIUM HIGH
server <- function(input, output){
    output$whisky_plot <- renderPlot({
        
        tidy_whisky %>%
            filter(smoky == input$smoky) %>%
            filter(body == input$body) %>%
            filter(sweetness == input$sweetness) %>%
            filter(if(input$floral == TRUE) {floral == 0} else {floral == floral}) %>%
            filter(if(input$honey == TRUE) {honey == 0} else {honey == honey}) %>%
            filter(if(input$nutty == TRUE) {nutty == 0} else {nutty == nutty}) %>%
            filter(if(input$medicinal == TRUE) {medicinal == 0} else {medicinal == medicinal}) %>%
            filter(if(input$spicy == TRUE) {spicy == 0} else {spicy == spicy}) %>%
            # filter(tobacco == input$tobacco) %>%
            # filter(malty == input$malty) %>%
            # filter(winey == input$winey ) %>%
            # filter(fruity == input$fruity) %>%
            pivot_longer(smoky:floral, names_to = "flavour", values_to = "strength") %>%
            # mutate(flavour = reorder() %>% 
            group_by(distillery, flavour) %>% 
            ggplot() +
            aes(x = factor(flavour, levels = c("smoky", "medicinal", "tobacco", "spicy", "winey", "body", "sweetness", "malty", "nutty", "honey", "fruity", "floral")), y = strength, fill = flavour) +
            geom_col(colour = "white") +
            facet_wrap( ~ distillery) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.33, hjust = 1),
                  legend.position = "none")  +
            theme(strip.background = element_rect(fill = "brown")) +
            theme(strip.text = element_text(colour = "white",
                                            size = 15))
        
    })
    
    
    output$map <- renderLeaflet({
        
        tidy_whisky %>%
            filter(smoky == input$smoky) %>%
            filter(body == input$body) %>%
            filter(sweetness == input$sweetness) %>%
            filter(if(input$floral == TRUE) {floral == 0} else {floral == floral}) %>%
            filter(if(input$honey == TRUE) {honey == 0} else {honey == honey}) %>%
            filter(if(input$nutty == TRUE) {nutty == 0} else {nutty == nutty}) %>%
            filter(if(input$medicinal == TRUE) {medicinal == 0} else {medicinal == medicinal}) %>%
            filter(if(input$spicy == TRUE) {spicy == 0} else {spicy == spicy}) %>%
            # filter(tobacco == input$tobacco) %>%
            # filter(malty == input$malty) %>%
            # filter(winey == input$winey ) %>%
            # filter(fruity == input$fruity) %>%
            leaflet() %>% 
            addTiles() %>% 
            addCircleMarkers(lat = ~longitude, lng = ~latitude, popup = ~distillery, color = ~pal(region), opacity = 0.7) %>%
            addLegend("topright", pal = pal, values = ~region,                                                                              title = "Whisky Region", opacity = 1)
    })
}

