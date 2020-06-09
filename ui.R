

ui <- fluidPage(titlePanel("Guide to YOUR WHISKIES in Scotland"),sidebarLayout(
        
        sidebarPanel(
            
            titlePanel("Flavour preference"),
            
            sliderInput("smoky",
                        "Smoky",
                        min = 0,
                        max = 4,
                        value = 2),
            
            sliderInput("body",
                        "Body",
                        min = 0,
                        max = 3,
                        value = 2),
            
            # sliderInput("medicinal",
            #             "Medicinal",
            #             min = 0,
            #             max = 3,
            #             value = 2),
            
            sliderInput("sweetness",
                        "Sweetness",
                        min = 0,
                        max = 3,
                        value = 2),
            
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
                        
                        radioButtons("floral",
                                     "Are floral notes okay?",
                                     choices = c("Yes", "No")),
            radioButtons("honey",
                         "Do you like honey notes?",
                         choices = c("Yes", "No")),
                         
            radioButtons("nutty",
                         "Do you like nutty notes?",
                         choices = c("Yes", "No")),
            
                         radioButtons("medicine",
                                      "Do you mind medicinal flavours?",
                                      choices = c("Yes", "No")),
            
                radioButtons("spicy",
                                                   "Do you like a bit of spiciness in your whisky?",
                                                   choices = c("Yes", "No")
            )
            ),
        
        mainPanel(
            
            # selectInput("region", "Which Region?", unique(tidy_whisky$region)),
            
            leafletOutput("map"),
            
            plotOutput("whisky_plot")
        )
        
        
    )
)

