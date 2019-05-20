library(ggplot2)
library(fmsb)
library(RPostgreSQL)
library(tidyverse)
library(fmsb)

# connect to postgresql server
con <- dbConnect(drv = dbDriver('PostgreSQL'), 
                 dbname = 'sql_fifa',
                 host = 's19db.apan5310.com', port = 50203,
                 user = 'postgres', password = 'rjxklxet')

#############################################
# Best player tab 
# how to change the lower bound of y aix
#############################################

# render plot showing home run leaders
output$plotA <- renderPlot(
  # sets height of plot to 1/2 of width; otherwise height stays at 480px
  {sq <- paste0(
    "select * from player_rating as pr 
    natural join (select player_id, player_name from player_info)as a
    natural join (select player_id, preferred_position from player_position) as b
    order by potential DESC
    limit 20;")
  a <- dbGetQuery(con, sq)
  a %>% 
    ggplot(aes(x = player_name, y = potential)) +
    geom_bar(stat = 'identity') +
    coord_flip()
  }
  )

#############################################
# skill 
# we want to compare any two selected players' physical skills, game intelligence and technique
# this tab should include 6 plots, and can allow users to select players by their names
#############################################

# physical skills
output$plotB <- renderPlot(
  {sq <- paste0(
    "select * from 
    (select player_id, strength, acceleration, balance, agility, reactions from physical_skill) as pr
    left join
    (select player_id, player_name from player_info) as pi
    using(player_id);")
  b <- dbGetQuery(con, sq)
  data=rbind(rep(100,5) , rep(0,5) , b[1,2:6])
  radarchart( data , axistype=1 , 
              pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
              cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
              vlcex=0.8,
              pty = 32,
              title = 'physical skills'
  )
  }
  )

# game intelligence
output$plotC <- renderPlot(
  {sq2 <- paste0(
    "select * from (select * from game_intelligence) as pr
    left join
    (select player_id, player_name from player_info) as pi
    using(player_id);")
  c <- dbGetQuery(con, sq2)
  data=rbind(rep(100,3) , rep(0,3) , c[1,2:4])
  radarchart( data , axistype=1 , 
              pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
              cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
              vlcex=0.8,
              pty = 32,
              title = 'game intelligence'
  )
  }
  )

# technique
output$plotD <- renderPlot(
  {sq2 <- paste0(
    "select * from
    (select player_id, ball_control, dribbling, (long_passing+short_passing)/2 as passing, volleys, crossing, finishing from technique) as a
    left join
    (select player_id, player_name from player_info) as pi
    using(player_id);")
  d <- dbGetQuery(con, sq2)
  data=rbind(rep(100,5) , rep(0,5) , d[1,2:7])
  radarchart( data , axistype=1 , 
              pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
              cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8,
              vlcex=0.8,
              pty = 32,
              title = 'technique'
  )
  }
)


#############################################
# the count of clubs in each country
# need map plot
#############################################


output$plotE <- renderPlot(
  {sq2 <- paste0(
    "select count(*), nationality from
    (select player_id, player_name, country_id from player_info) as a 
    left join
    (select country_id, nationality from country) as b
    using(country_id)
    group by nationality
    order by count(*) DESC;")
  e <- dbGetQuery(con, sq2)
  }
)


#############################################
# the club ranking in each league
# show the clubs' rankings of selected league
#############################################

output$plotF <- renderPlot(
  {sq2 <- paste0(
    "select * from (select league_name, club_id, ranking from standings ) as a 
    left join 
    (select club_id, club_name from club) as b
    using(club_id)
    group by club_id,club_name, ranking,league_name
    order by league_name, ranking ASC;")
  f <- dbGetQuery(con, sq2)
  league_clubs <- function(league) {
    data = f[f$league_name == league,]
    ggplot(data,aes(x = club_name, y = ranking)) +
      geom_bar(stat = 'identity') +
      coord_flip()}
  }
  )

#############################################
# historical country ranking in previous years world cups
# We want to show the top three countries of selected year, use side bar to choose year
#############################################

sq2 <- paste0(
    "select year, first, second, nationality as third
    from(
    select year, first_place, second_place, third_place, first, nationality as second 
    from
    (select year, first_place, second_place, third_place, nationality as first
    from tournament_name, country
    where tournament_name.first_place = country.country_id) as a, country
    where a.second_place = country.country_id) as b, country
    where b.third_place = country.country_id")
  g <- dbGetQuery(con, sq2)


#############################################
# player ranking in each country
# how to change the lower bound of y aix
#############################################

output$plotH <- renderPlot(
  {sq2 <- paste0(
    "select player_name, nationality,overall_rating from 
    ((select player_id, overall_rating from player_rating)as a
    left join 
    (select player_id, player_name, country_id from player_info) as b
    using(player_id))c
    left join
    (select country_id, nationality from country) d
    using(country_id)
    group by country_id, nationality, player_id,player_name, overall_rating
    order by nationality, overall_rating DESC;")
  h <- dbGetQuery(con, sq2)
  country_players <- function(country_name) {
    data = h[h$nationality== country_name,]
    ggplot(data,aes(x = player_name, y = overall_rating)) +
      geom_bar(stat = 'identity') +
      coord_flip()}
  }
  )











