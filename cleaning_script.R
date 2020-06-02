tidy_whisky <- whisky %>%
    mutate(Region = as.factor(Region)) %>% 
    pivot_longer(Body:Floral, names_to = "flavour", values_to = "strength") %>%
    pivot_longer(Latitude:Longitude, names_to = "location_unit", values_to = "location") %>% 
    pivot_longer(c(Distillery, Owner, Postcode), names_to = "headline", values_to = "info") %>%
    pivot_longer(YearFound:Capacity, names_to = "metric", values_to = "figure") %>% 
    relocate(c(Region, headline:figure))


flavours <- tidy_whisky %>%
    distinct(flavour)
