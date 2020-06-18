
library(shiny)
library(leaflet)
library(janitor)
library(plotly)
library(ggrepel)
library(tidyverse)
library(RColorBrewer)

tidy_whisky <-
    read_csv("clean_data/tidy_whisky.csv")

pal <- colorFactor(
    palette = 'Dark2',
    domain = tidy_whisky$region
)