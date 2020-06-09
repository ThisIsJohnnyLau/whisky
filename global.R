
library(shiny)
library(leaflet)
library(CodeClanData)
library(janitor)
library(modelr)
library(GGally)
library(fastDummies)
library(plotly)
library(ggrepel)
library(tidyverse)
library(fmsb)


library(RColorBrewer)


pal <- colorFactor(
    palette = 'Dark2',
    domain = tidy_whisky$region
)