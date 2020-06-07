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
    write_csv(here("clean_data/tidy_whisky.csv"))

flavours <- tidy_whisky %>%
    select(body:floral) %>% 
    names()