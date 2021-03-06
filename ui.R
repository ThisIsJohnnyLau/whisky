

ui <- fluidPage(titlePanel("Guide to YOUR WHISKIES in Scotland"),sidebarLayout(
        
        sidebarPanel(
            
            titlePanel("Flavour preference"),
            
            sliderInput("smoky",
                        "Smoky",
                        min = 1,
                        max = 3,
                        value = 2,
                        ticks = FALSE),
            
            sliderInput("body",
                        "Body",
                        min = 1,
                        max = 3,
                        value = 2,
                        ticks = FALSE),
            
            # sliderInput("medicinal",
            #             "Medicinal",
            #             min = 0,
            #             max = 3,
            #             value = 2),
            
            sliderInput("sweetness",
                        "Sweetness",
                        min = 1,
                        max = 3,
                        value = 2,
                        ticks = FALSE),
            
            # sliderInput("tobacco",
            #             "Tobacco",
            #             min = 0,
            #             max = 1,
            #             value = 0),
            
                        # sliderInput("malty",
                        #             "Malty",
                        #             min = 0,
                        #             max = 3,
                        #             value = 2),
                        
       
                
                        
                        # sliderInput("spicy",
                        #             "Spicy",
                        #             min = 0,
                        #             max = 3,
                        #             value = 2),
                        
                        # sliderInput("winey",
                        #             "Winey",
                        #             min = 0,
                        #             max = 3,
                        #             value = 2),
                        
                        # sliderInput("nutty",
                        #             "Nutty",
                        #             min = 0,
                        #             max = 3,
                        #             value = 2),
                        
                        # sliderInput("fruity",
                        #             "Fruity",
                        #             min = 0,
                        #             max = 3,
                        #             value = 2),
                        
            
            helpText("Anything you're not keen on?"),
            
            checkboxInput("floral", "floral", value = FALSE),
            checkboxInput("honey", "honey", value = FALSE),
            checkboxInput("nutty", "nutty", value = FALSE),
            checkboxInput("medicinal", "medicinal", value = FALSE),
            # checkboxInput("floral", "floral", value = FALSE),
            # checkboxInput("floral", "floral", value = FALSE),
            checkboxInput("spicy", "spicy", value = FALSE),
        
            #             radioButtons("floral",
            #                          "Are floral notes okay?",
            #                          choices = c("Yes", "No")),
            # radioButtons("honey",
            #              "Do you like honey notes?",
            #              choices = c("Yes", "No")),
            #              
            # radioButtons("nutty",
            #              "Do you like nutty notes?",
            #              choices = c("Yes", "No")),
            # 
            #              radioButtons("medicine",
            #                           "Do you mind medicinal flavours?",
            #                           choices = c("Yes", "No")),
            # 
            #     radioButtons("spicy",
            #                                        "Do you like a bit of spiciness in your whisky?",
            #                                        choices = c("Yes", "No")
            # )
            ),
        
        mainPanel(
            
            # selectInput("region", "Which Region?", unique(tidy_whisky$region)),
            
            leafletOutput("map"),
            
            plotOutput("whisky_plot")
        )
        
        
    )
)

