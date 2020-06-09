library(tidyverse)
library(janitor)
library(readr)
library(here)

# Using CodeClan whisky data
library(CodeClanData)

whisky %>%
    mutate(Region = as.factor(Region)) %>%
    select(-RowID) %>%
    clean_names() %>%
    # pivot_longer(body:floral, names_to = "flavour", values_to = "strength") %>%
    relocate(smoky, .before = body) %>% 
    relocate(medicinal, .after = smoky) %>% 
    write_csv(here("clean_data/tidy_whisky.csv"))

tidy_whisky <-
    read_csv("clean_data/tidy_whisky.csv")
