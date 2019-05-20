
library(DT)
library(fmsb)
library(ggrepel)
library(leaflet)
library(RPostgreSQL)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(viridis)

## connect to postgresql server
con <- dbConnect(drv = dbDriver('PostgreSQL'), 
                 dbname = 'sql_fifa',
                 host = 's19db.apan5310.com', port = 50203,
                 user = 'postgres', password = 'rjxklxet')

f <- read.csv('data/fifa.csv')
c <- read.csv('data/countries.csv')

countries <- f %>% select(Nationality) %>% distinct() %>% arrange(Nationality)

leagues <- dbGetQuery(con, "SELECT DISTINCT league_name FROM standings ORDER BY 1")

posTypes <- c("Forward", "Goalkeeper", "Midfielder", "Defender")

world_cup <- read.csv('data/world_cup.csv')

onStop(function() 
  {
    dbDisconnect(con)
    cat('Thank you!')
  }
)