library(tidyverse)
library(rvest)
library(magrittr)
library(httr)

url <- "https://whiskybase.com/whiskies/distilleries"

page <- read_html(url)

distillery.table <- 
    page %>% 
    html_table() %>% 
    extract2(1)

# write.csv(distillery.table, file = "Distillery.csv")

Distillery.Name <- 
    page %>% 
    html_nodes("tbody") %>% 
    html_nodes("td.clickable") %>% 
    html_text()

Distillery.Link <- 
    page %>% 
    html_nodes("tbody") %>% 
    html_nodes("td.clickable") %>% 
    html_nodes("a") %>% 
    html_attr("href")

Distilley.Link <- data.frame(Distillery.Name, Distillery.Link)
# write.csv(Distilley.Link, file = "Distillery_Link.csv")


Distillery_Info_Scraping <- 
    function (url) {
        
        page <- 
            read_html(url)
        
        
        ## Coampany Name
        Company.Name <-
            page %>% 
            html_nodes("div#company-name") %>% 
            html_text()
        
        ## Getting About URL
        link.list <- 
            page %>% 
            html_nodes("ul.menu") %>% 
            html_nodes("li.submenu-item") %>% 
            html_nodes("a") %>% 
            html_attr("href")
        
        about.url <- link.list[11]
        
        
        page.about <- 
            read_html(about.url)
        
        Detail <- 
            page.about %>% 
            html_nodes("div#company-content") %>% 
            html_nodes("div.panel") %>% 
            html_nodes("div.company-details") %>% 
            html_text()
        
        Detail.company <- 
            Detail[1]
        
        Detail.whisky <- 
            Detail[2]
        
        data.frame(Company.Name, about.url, Detail.company, Detail.whisky)}


Distillery.List1 <- apply(data.frame(Distillery.Link[1:200]), 1, Distillery_Info_Scraping)
Distillery.List2 <- apply(data.frame(Distillery.Link[200:300]), 1, Distillery_Info_Scraping)
Distillery.List3 <- apply(data.frame(Distillery.Link[301:500]), 1, Distillery_Info_Scraping)
Distillery.List4 <- apply(data.frame(Distillery.Link[501:800]), 1, Distillery_Info_Scraping)
Distillery.List5 <- apply(data.frame(Distillery.Link[801:1152]), 1, Distillery_Info_Scraping)

# Mergin Dataframes

Distillery.df1 <- do.call(rbind, Distillery.List1)
Distillery.df2 <- do.call(rbind, Distillery.List2)
Distillery.df3 <- do.call(rbind, Distillery.List3)
Distillery.df4 <- do.call(rbind, Distillery.List4)
Distillery.df5 <- do.call(rbind, Distillery.List5)

main.Distillery.df <- 
    rbind(Distillery.df1, Distillery.df2, Distillery.df3, 
          Distillery.df4, Distillery.df5)
main.Distillery.df <- main.Distillery.df[-200,]

# write.csv(main.Distillery.df, file = "Main_Dis.csv")

main.Distillery.df <- read.csv("Main_Dis.csv")

# Cleaning Dataset

main.Distillery.df$Company.Name <- as.character(main.Distillery.df$Company.Name)
main.Distillery.df$Detail.company <- as.character(main.Distillery.df$Detail.company)
main.Distillery.df$Detail.whisky <- as.character(main.Distillery.df$Detail.whisky)

main.Distillery.df$Company.Name <- 
    main.Distillery.df$Company.Name %>% 
    str_remove("\n\t\t\t\t\t\t\t\t\t\t\t")
main.Distillery.df$Company.Name <- 
    main.Distillery.df$Company.Name %>% 
    str_remove("\n\t\t\t\t")





main.Distillery.df$Detail.company <- 
    main.Distillery.df$Detail.company %>% 
    str_remove("\n\t\t\t\t\t\n\t\t\t\t\t\t\n\t\t\t\t\t\t")
main.Distillery.df$Detail.company <- 
    main.Distillery.df$Detail.company %>% 
    str_replace(pattern = "\n\t\t\t\t\t\t", replacement = ":")
main.Distillery.df$Detail.company <- 
    main.Distillery.df$Detail.company %>% 
    str_replace(pattern = "\n\t\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\t\n\t\t\t\t\t\t", replacement = "/")
main.Distillery.df$Detail.company <- 
    main.Distillery.df$Detail.company %>% 
    str_replace(pattern = "\n\t\t\t\t\t\t", replacement = ":")
main.Distillery.df$Detail.company <- 
    main.Distillery.df$Detail.company %>% 
    str_replace(pattern = "\n\t\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\t\n\t\t\t\t\t\t", replacement = "/")
main.Distillery.df$Detail.company <- 
    main.Distillery.df$Detail.company %>% 
    str_replace(pattern = "\n\t\t\t\t\t\t", replacement = ":")
main.Distillery.df$Detail.company <- 
    main.Distillery.df$Detail.company %>% 
    str_replace_all(pattern = "\n\t\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\t\n\t\t\t\t\t\t", replacement = "/")
main.Distillery.df$Detail.company <- 
    main.Distillery.df$Detail.company %>% 
    str_replace_all(pattern = "\n\t\t\t\t\t\t", replacement = ":")
main.Distillery.df$Detail.company <- 
    main.Distillery.df$Detail.company %>% 
    str_remove(pattern = "\n\t\t\t\t\t")


main.Distillery.df$Detail.whisky <- 
    main.Distillery.df$Detail.whisky %>% 
    str_remove("\n\t\t\t\t\t\n\t\t\t\t\t\t\n\t\t\t\t\t\t")
main.Distillery.df$Detail.whisky <- 
    main.Distillery.df$Detail.whisky %>% 
    str_replace(pattern = "\n\t\t\t\t\t\t", replacement = ":")
main.Distillery.df$Detail.whisky <- 
    main.Distillery.df$Detail.whisky %>% 
    str_replace_all(pattern = "\n\t\t\t\t\t\n\t\t\t\t\t\n\t\t\t\t\t\t\n\t\t\t\t\t\t", replacement = "/")
main.Distillery.df$Detail.whisky <- 
    main.Distillery.df$Detail.whisky %>% 
    str_replace_all(pattern = "\n\t\t\t\t\t\t", replacement = ":")
main.Distillery.df$Detail.whisky <- 
    main.Distillery.df$Detail.whisky %>% 
    str_remove_all("\n\t\t\t\n\t\t\t\t ")
main.Distillery.df$Detail.whisky <- 
    main.Distillery.df$Detail.whisky %>% 
    str_remove_all("\n\t\t\t\n\t\t\n\t\n\t\t\t\t\t")
main.Distillery.df$Detail.whisky <- 
    main.Distillery.df$Detail.whisky %>% 
    str_replace_all(pattern = "\n\t\t\t\n\t\t\n\t\t\t", replacement = " & ")


main.Distillery.df <- 
    main.Distillery.df %>% 
    select(Company.Name, Detail.company, Detail.whisky, about.url)

main.Distillery.df$Detail.company <- 
    main.Distillery.df$Detail.company %>% 
    str_replace(pattern = "/", replacement = "|")
main.Distillery.df <- 
    main.Distillery.df %>% 
    separate(Detail.company, into = c("Country", "Status"),
             sep = "\\|")

main.Distillery.df$Status <- 
    main.Distillery.df$Status %>% 
    str_replace(pattern = "/", replacement = "|")
main.Distillery.df <- 
    main.Distillery.df %>% 
    separate(Status, into = c("Status", "Other"),
             sep = "\\|")

# write.csv(main.Distillery.df, file = "Main_Distillery.csv")

main.Distillery.df$Country <- 
    main.Distillery.df$Country %>% 
    str_remove("Country:")
main.Distillery.df$Status <- 
    main.Distillery.df$Status %>% 
    str_remove("Status:")

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(Website = sub('.*Web', '', main.Distillery.df$Other))
main.Distillery.df$Other <- sub("Website.*", "", main.Distillery.df$Other)
main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(Capacity = sub('.*Capacity', '', main.Distillery.df$Other))
main.Distillery.df$Other <- sub("Capacity.*", "", main.Distillery.df$Other)

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(WashStill = sub('.*Wash', '', main.Distillery.df$Other))
main.Distillery.df$Other <- sub("Wash.*", "", main.Distillery.df$Other)
main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(SpiritSill = sub('.*Spir', '', main.Distillery.df$Other))
main.Distillery.df$Other <- sub("Spirit stills.*", "", main.Distillery.df$Other)

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(Owner = sub('.*Owner', '', main.Distillery.df$Other))
main.Distillery.df$Other <- sub("Owner.*", "", main.Distillery.df$Other)

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(Closed = sub('.*Clo', '', main.Distillery.df$Other))
main.Distillery.df$Other <- sub("Closed.*", "", main.Distillery.df$Other)
names(main.Distillery.df)[4] <- "Founded"

# write.csv(main.Distillery.df, file = "Main_Dis_1st_Clean.csv")

main.Distillery.df$Founded <-
    main.Distillery.df$Founded %>% 
    str_remove("Founded") %>% 
    str_remove("/")
main.Distillery.df$Founded <-
    main.Distillery.df$Founded %>% 
    str_remove(":")
main.Distillery.df$Founded <- 
    main.Distillery.df$Founded %>% 
    str_replace_all(pattern = "\\.", replacement = "-")

main.Distillery.df <- 
    main.Distillery.df %>% 
    select(Company.Name:Founded, Closed, Website:Owner , Detail.whisky, about.url)
main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(TF = str_detect(main.Distillery.df$Closed, pattern = "Founded"))

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(ClosedFixed = case_when(TF == TRUE ~ "NA",
                                   TF == FALSE ~ Closed))
main.Distillery.df <- 
    main.Distillery.df %>% 
    select(-Closed)
main.Distillery.df <- 
    main.Distillery.df %>% 
    select(Company.Name:Founded, ClosedFixed, Website:Owner , Detail.whisky, about.url)
main.Distillery.df$ClosedFixed <- 
    main.Distillery.df$ClosedFixed %>% 
    str_remove("sed:") %>% 
    str_remove("/")

main.Distillery.df$ClosedFixed <- 
    main.Distillery.df$ClosedFixed %>% 
    str_replace_all(pattern = "\\.", replacement = "-")

main.Distillery.df$Website <-
    main.Distillery.df$Website %>% 
    str_remove("site:")

main.Distillery.df$Owner <- 
    main.Distillery.df$Owner %>% 
    str_remove(":") %>% 
    str_remove("/")

names(main.Distillery.df)[5] <- "Closed"
names(main.Distillery.df)[12] <- "URL"

# write.csv(main.Distillery.df, file = "Second_MainDis_df.csv")

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(TF = str_detect(main.Distillery.df$Capacity, "per year:"))
main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(CapacityFixed = case_when(TF == "TRUE" ~ Capacity,
                                     TF == "FALSE" ~ "NA"))


main.Distillery.df$CapacityFixed <- 
    main.Distillery.df$CapacityFixed %>% 
    str_remove(" per year:") %>% 
    str_remove("/")

main.Distillery.df <- 
    main.Distillery.df %>% 
    select(-TF, - Capacity)

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(TF = str_detect(main.Distillery.df$SpiritSill, "it stills:"))
main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(SpiritSillFixed = case_when(TF == "TRUE" ~ SpiritSill,
                                       TF == "FALSE" ~ "NA"))
main.Distillery.df$SpiritSillFixed <- 
    main.Distillery.df$SpiritSillFixed %>% 
    str_remove("it stills:") %>% 
    str_remove("/")

main.Distillery.df <- 
    main.Distillery.df %>% 
    select(-TF, -SpiritSill)

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(TF = str_detect(main.Distillery.df$Website, "Founded"))
main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(WebsiteFixed = case_when(TF == "FALSE" ~ Website,
                                    TF == "TRUE" ~ "NA"))
main.Distillery.df <- 
    main.Distillery.df %>% 
    select(-TF, -Website)
# write.csv(main.Distillery.df, file = "HalfCleaned_dis_df.csv")

names(main.Distillery.df)[1] <- "Company"

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(TF = str_detect(WashStill, "stills:"))
main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(WashStillFixed = case_when(TF == "TRUE" ~ WashStill,
                                      TF == "FALSE" ~ "NA"))
main.Distillery.df <- 
    main.Distillery.df %>% 
    select(-TF, -WashStill)
main.Distillery.df <-
    main.Distillery.df %>% 
    select(Company:Owner, URL:WashStillFixed, Detail.whisky)


main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(Spcialist = sub('.*Specia', '', main.Distillery.df$Detail.whisky))
main.Distillery.df$Detail.whisky <- sub("Specialists:.*", "", main.Distillery.df$Detail.whisky)

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(Votes = sub('.*Vot', '', main.Distillery.df$Detail.whisky))
main.Distillery.df$Detail.whisky <- sub("/Votes:.*", "", main.Distillery.df$Detail.whisky)

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(Views = sub('.*Vie', '', main.Distillery.df$Detail.whisky))
main.Distillery.df$Detail.whisky <- sub("/Views:.*", "", main.Distillery.df$Detail.whisky)

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(Wishlist = sub('.*Wis', '', main.Distillery.df$Detail.whisky))
main.Distillery.df$Detail.whisky <- sub("/Wishlist:.*", "", main.Distillery.df$Detail.whisky)

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(Collection = sub('.*Col', '', main.Distillery.df$Detail.whisky))
main.Distillery.df$Detail.whisky <- sub("/Collection:.*", "", main.Distillery.df$Detail.whisky)

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(Rating = sub('.*Rati', '', main.Distillery.df$Detail.whisky))
main.Distillery.df$Detail.whisky <- sub("/Rating:.*", "", main.Distillery.df$Detail.whisky)

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(Ranking = sub('.*WB', '', main.Distillery.df$Detail.whisky))
main.Distillery.df$Detail.whisky <- sub("\\|WB.*", "", main.Distillery.df$Detail.whisky)

names(main.Distillery.df)[12] <- "Whisky"

main.Distillery.df$Ranking <- sub("\\|.*", "", main.Distillery.df$Ranking)
main.Distillery.df$Views <- sub("/Vote:.*", "", main.Distillery.df$Views)
main.Distillery.df$Views <- 
    main.Distillery.df$Views %>% 
    str_remove("ws:") %>% 
    str_remove("\n\t\t\t\t\t") %>% 
    str_remove("/")

main.Distillery.df$Ranking <- 
    main.Distillery.df$Ranking %>% 
    str_remove(" Ranking:")

# write.csv(main.Distillery.df, file = "Almost_Distillery.csv")

main.Distillery.df <- read.csv("Almost_Distillery.csv")

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(TF = str_detect(main.Distillery.df$Spcialist, "lists:"))


main.Distillery.df$Spcialist <- as.character(main.Distillery.df$Spcialist)

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(Specialist = case_when(TF == "TRUE" ~ Spcialist,
                                  TF == "FALSE" ~ "NA"))

main.Distillery.df <- 
    main.Distillery.df %>% 
    select(-TF, -Spcialist)

main.Distillery.df$Specialist <- main.Distillery.df$Specialist %>% 
    str_remove("lists:")

main.Distillery.df$Votes <- as.character(main.Distillery.df$Votes)

main.Distillery.df <- main.Distillery.df %>% 
    mutate(TF = str_detect(main.Distillery.df$Votes, "Ranking:"))

main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(VotesFixed = case_when(TF == "FALSE" ~ Votes,
                                  TF == "TRUE" ~ "NA"))

main.Distillery.df <- 
    main.Distillery.df %>% 
    select(-TF, -Votes)

# write.csv(main.Distillery.df, file = "Distillery_NEAR.csv")

main.Distillery.df$VotesFixed <- 
    main.Distillery.df$VotesFixed %>% 
    str_remove("/") %>% 
    str_remove("\n\t\t\t\t\t") %>% 
    str_remove("es:") %>% 
    str_remove("e:")

main.Distillery.df <- main.Distillery.df %>% 
    mutate(TF = str_detect(main.Distillery.df$Wishlist, "Ranking:"))

main.Distillery.df$Wishlist <- as.character(main.Distillery.df$Wishlist)
main.Distillery.df <- main.Distillery.df %>% 
    mutate(WishlistFixed = case_when(TF == "TRUE" ~ "NA",
                                     TF == "FALSE" ~ Wishlist))
main.Distillery.df <- main.Distillery.df %>% 
    select(-TF)

main.Distillery.df$WishlistFixed <- main.Distillery.df$WishlistFixed %>% 
    str_remove("hlist:")

main.Distillery.df$Rating <- as.character(main.Distillery.df$Rating)
main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(TF = str_detect(main.Distillery.df$Rating, "Ranking:"))
main.Distillery.df <- 
    main.Distillery.df %>% 
    mutate(RatingFixed = case_when(TF == "TRUE" ~ "NA",
                                   TF == "FALSE" ~ Rating))

main.Distillery.df <- main.Distillery.df %>% 
    select(-TF)
main.Distillery.df$RatingFixed <- main.Distillery.df$RatingFixed %>% 
    str_remove("ng:")

main.Distillery.df$WashStillFixed <- main.Distillery.df$WashStillFixed %>% 
    str_remove(" stills:") %>% 
    str_remove("/")

main.Distillery.df <- main.Distillery.df %>% 
    mutate(TF = str_detect(main.Distillery.df$Wishlist, "hlist:"))

main.Distillery.df <- main.Distillery.df %>% 
    select(-TF, -Wishlist)

main.Distillery.df <- main.Distillery.df %>% 
    mutate(TF = str_detect(main.Distillery.df$Whisky, "Whiskies:"))
main.Distillery.df$Whisky <- as.character(main.Distillery.df$Whisky)
main.Distillery.df <- main.Distillery.df %>% 
    mutate(WhiskyFixed = case_when(TF == "TRUE" ~ Whisky,
                                   TF == "FALSE" ~ "NA"))

main.Distillery.df <- main.Distillery.df %>% 
    select(-TF, -Whisky)
main.Distillery.df$WhiskyFixed <- main.Distillery.df$WhiskyFixed %>% 
    str_remove("Whiskies:")

main.Distillery.df <- main.Distillery.df %>% 
    mutate(TF = str_detect(main.Distillery.df$Rating, "Ranking:"))
main.Distillery.df <- main.Distillery.df %>% 
    mutate(RatingFixed = case_when(TF == "FALSE" ~ Rating,
                                   TF == "TRUE" ~ "NA"))
main.Distillery.df <- main.Distillery.df %>% 
    select(-TF, -Rating)
main.Distillery.df$RatingFixed <- main.Distillery.df$RatingFixed %>% str_remove("ng:")

main.Distillery.df <- main.Distillery.df %>% 
    mutate(TF = str_detect(main.Distillery.df$Collection, "lection:"))
main.Distillery.df$Collection <- as.character(main.Distillery.df$Collection)
main.Distillery.df <- main.Distillery.df %>% 
    mutate(CollectionFixed = case_when(TF == "TRUE" ~ Collection,
                                       TF == "FALSE" ~ "NA"))

main.Distillery.df <- main.Distillery.df %>% 
    select(-TF, -Collection)
main.Distillery.df$CollectionFixed <- main.Distillery.df$CollectionFixed %>% 
    str_remove("lection:")
names(main.Distillery.df) <- c("X", "Company", "Country", "Status", "Founded", "Closed", "Owner", "URL", "Capacity", 
                               "SpiritSill", "Website", "WashStill", "Views", "Ranking", "Specialist", "Votes", 
                               "Wishlist", "Rating", "Whisky", "Collection")

main.Distillery.df <- main.Distillery.df %>% 
    select(-X)

main.Distillery.df <- main.Distillery.df %>% 
    select(Company:Owner, Capacity:SpiritSill, WashStill:Ranking, Votes:Collection, Specialist, URL, Website)
# write.csv(main.Distillery.df, file = "Main.csv")


### Get Location

about.url <- main.Distillery.df$URL


Address_Scraping<- function (url) {
    
    page <- read_html(as.character((url)))
    
    Company.Name <- page %>% 
        html_nodes("div#company-name") %>%
        html_nodes("h1") %>% 
        html_text()
    
    Address <- page %>% 
        html_nodes("div.panel") %>% 
        html_nodes("div.company-address") %>% 
        html_text()
    
    data.frame(Company.Name, Address)
}

Address <- apply(data.frame(about.url), 1, Address_Scraping)
Address.df <- do.call(rbind, Address)


Address.df$Address <- as.character(Address.df$Address)

Address.df$Address <- Address.df$Address %>% 
    str_remove("\n\t\t\t\t\t\t") %>% 
    str_remove("\n\t\t\t\t\t")
Address.df$Company.Name


library(ggmap)
Address.df$Address[1:3]
Location_Scraping <- function (address) {
    adrress <- as.character(address)
    
    Location <- geocode(adrress)
    data.frame(adrress, data.frame(Location))
}

Location <- Location_Scraping(Address.df$Address)

Location.df <- Location


Location.df1 <- Location.df[!is.na(Location.df$lon),]
Location.df.na <- Location.df[is.na(Location.df$lon),]

Location2 <- Location_Scraping(Location.df.na$adrress)

Location2.df <- Location2

Location2.df <- Location2.df[!is.na(Location2.df$lon),]
Location.df.na2 <- Location2[is.na(Location2$lon),]


Location3 <- Location_Scraping(Location.df.na2$adrress)


Location.na3 <- Location3[is.na(Location3$lon),]
Location.df3 <- Location3[!is.na(Location3$lon),]

Location4 <- Location_Scraping(Location.na3$adrress)


GeoLocation.df <- rbind(Location.df1, Location2.df, Location.df3, Location4)
# write.csv(GeoLocation.df, file = "Location.csv")

Adrress.Geolocation <- merge(Address.df, GeoLocation.df, by.x = "Address", by.y = "adrress")

main.Distillery.df$Company <- as.character(main.Distillery.df$Company)
main.Distillery.df$Country <- as.character(main.Distillery.df$Country)
main.Distillery.df$Company <- main.Distillery.df$Company %>% 
    str_remove(pattern = main.Distillery.df$Country)


Distillery.Location <- merge(main.Distillery.df, Adrress.Geolocation, by.x = "Company", by.y = "Company.Name")
# write.csv(Distillery.Location, file = "Location_Distillery.csv")

Distillery.Location <- Distillery.Location %>% 
    mutate(TF = str_detect(Distillery.Location$WashStill, pattern = "Spirit"))

Distillery.Location <- Distillery.Location %>% 
    mutate(WashStillFixed = case_when(TF == "TRUE" ~ "NA",
                                      TF == "FALSE" ~ WashStill))
Distillery.Location <- Distillery.Location %>% 
    select(-TF, -WashStill)

Distillery.Location$Owner %>% na.omit()
Distillery.Location <- Distillery.Location %>% 
    mutate(TF = str_detect(Distillery.Location$Owner, "Founded|Closed"))
Distillery.Location$Owner <- as.character(Distillery.Location$Owner)
Distillery.Location <- Distillery.Location %>% 
    mutate(OwnerFixed = case_when(TF == "FALSE" ~ Owner,
                                  TF == "TRUE" ~ "NA"))
Distillery.Location <- Distillery.Location %>% 
    select(-TF, -Owner)

# write.csv(Distillery.Location, file = "Distillery_Final.csv")


# Whisky Brands All Around the World

url <- "https://www.whiskybase.com/whiskies/brands"
