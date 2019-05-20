
function(input, output, session) {

  srcP <- reactive(
    f %>% filter(Name == input$plyr) %>% select(Photo)
  )
  
  srcP3a <- reactive(
    f %>% filter(Name == input$plyr3a) %>% select(Photo)
  )
  srcP3b <- reactive(
    f %>% filter(Name == input$plyr3b) %>% select(Photo)
  )
  
  srcF <- reactive(
    f %>% filter(Name == input$plyr) %>% select(Flag)
  )
  
  inf <- reactive(
    f %>% 
      filter(Name == input$plyr) %>% 
      select('o' = Overall, 
             'p' = Potential) %>% 
      mutate('h' = ifelse(is.null(f$Height), '',
                          paste(f$Height, '', sep = '')),
             'w' = ifelse(is.null(f$Weight), '',
                          paste(substr(f$Weight,1,3), 
                                substr(f$Weight,4,6), sep = ' '))
      )
  )
  
  plyrs1 <- reactive(
    {
      mydata <- f %>% 
        filter(Nationality == input$cntry) %>% 
        select(Name)
      as.character(unique(mydata$Name))
    }
  )
  
  plyrs3a <- reactive(
    {
      mydata <- f %>% 
        filter(Nationality == input$cntry3a) %>% 
        select(Name)
      as.character(unique(mydata$Name))
    }
  )
  
  plyrs3b <- reactive(
    {
      mydata <- f %>% 
        filter(Nationality == input$cntry3b) %>% 
        select(Name)
      as.character(unique(mydata$Name))
    }
  )
  
  posNs <- reactive(
    {
      cG <- c('GK')
      cD <- c('CB','LB','LCB','LWB','RB','RCB','RWB')
      cM <- c('CAM','CDM','CM','LAM','LCM','LDM','LM','RAM','RCM','RDM','RM')
      cF <- c('CF','LF','LS','LW','RF','RS','RW','ST')
      myData <- f %>% 
        select(Position) %>% 
        filter(Position %in% ifelse(input$posT == 'Goalkeeper', cG ,
                             ifelse(input$posT == 'Defender', cD ,
                             ifelse(input$posT == 'Midfielder', cM ,
                             ifelse(input$posT == 'Forward', cF, c('')))))) %>% 
        unique() %>% 
        arrange(Position)
      as.character(myData$Position)
    }
  )
  
  observe(
    {
      updateSelectInput(session, 'plyr', choices = plyrs1())
      updateSelectInput(session, 'plyr3a', choices = plyrs3a())
      updateSelectInput(session, 'plyr3b', choices = plyrs3b())
      updateSelectInput(session, 'clu1', choices = clubs1())
      #updateSelectInput(session, 'clu2', choices = clubs2())
      #updateSelectInput(session, 'posN', choices = posNs())
    }
  )
  
  output$worldMap <- renderLeaflet(
    {
      cc <- c %>% filter(country == input$cntry)
      leaflet(cc, options = leafletOptions(zoomControl = FALSE)) %>%
        addTiles() %>% 
        addMarkers(lng = ~lng, 
                   lat = ~lat,
                   popup = paste0('<b><font size="3" color="blue">', 
                                  cc$country, '</font></b>')) %>% 
        setView(lng = cc$lng, lat = cc$lat, zoom = 2)
    
    }
  )
  
  
  output$plyrPic <- renderText(
    paste('<img src = "', 
          srcP()$Photo[1], 
          '", height = 100%, width = 100%>', 
          sep = ''))
  
  output$plyr1Pic <- renderText(
    paste('<img src = "', 
          srcP3a()$Photo[1], 
          '", height = 30%, width = 30%>', 
          sep = ''))
  
  output$plyr2Pic <- renderText(
    paste('<img src = "', 
          srcP3b()$Photo[1], 
          '", height = 30%, width = 30%>', 
          sep = ''))
  
  output$flagPic <- renderText(
    paste('<img src = "', 
          srcF()$Flag[1], 
          '", height = 25%, width = 25%>', 
          sep = ''))
  
  output$plyrInfo <-  renderText(
    paste0('<h1>', input$plyr, ' (',
          input$cntry, ')</h1>',
          '<p style = "font-size: 24px;">Height: ', inf()$h[1], 
          '"&nbsp;&#8212;&nbsp;',
          'Weight: ', inf()$w[1], '</p><hr>',
          '<style>td{padding:5px}</style>',
          '<h2><table><col width=100><col width=50>',
          '<tr><td>Overall:</td><td>', inf()$o[1], '</td></tr>',
          '<tr><td>Potential:</td><td>', inf()$p[1], '</td></tr>',
          '</table></h2>'))

  output$posTitle <- renderText(
    paste0('<h2>Top Ten ', input$posT, 's by POTENTIAL & OVERALL Ratings</h2>')
  )
  
  output$plot10a <- renderPlot(
    {
      cG <- c('GK')
      cD <- c('CB','LB','LCB','LWB','RB','RCB','RWB')
      cM <- c('CAM','CDM','CM','LAM','LCM','LDM','LM','RAM','RCM','RDM','RM')
      cF <- c('CF','LF','LS','LW','RF','RS','RW','ST')
      ifelse(input$posT == 'Forward',    po <- cF, 
      ifelse(input$posT == 'Goalkeeper', po <- cG,
      ifelse(input$posT == 'Midfielder', po <- cM,
                                         po <- cD)))
      f %>% 
        filter(Position %in% po) %>% 
        select(Name, Potential) %>% 
        arrange(desc(Potential)) %>% 
        slice (1:10) %>% 
        ggplot(aes(x = reorder(Name, Potential), y = Potential, fill = Name)) +
          geom_bar(stat = 'identity') +
          coord_flip(ylim = c(75, 95)) +
          geom_text(aes(label = Potential), hjust = -0.5, size = 8) +
          scale_fill_viridis(discrete = TRUE) +
          labs(x = 'PLAYER', y = 'POTENTIAL') +
          theme_minimal(base_size = 24) +
          theme(legend.position = 'none')
    }
  )
  
  output$plot10b <- renderPlot(
    {
      cG <- c('GK')
      cD <- c('CB','LB','LCB','LWB','RB','RCB','RWB')
      cM <- c('CAM','CDM','CM','LAM','LCM','LDM','LM','RAM','RCM','RDM','RM')
      cF <- c('CF','LF','LS','LW','RF','RS','RW','ST')
      ifelse(input$posT == 'Forward',    po <- cF, 
      ifelse(input$posT == 'Goalkeeper', po <- cG,
      ifelse(input$posT == 'Midfielder', po <- cM,
                                         po <- cD)))
      f %>% 
        filter(Position %in% po) %>% 
        select(Name, Overall) %>% 
        arrange(desc(Overall)) %>% 
        slice (1:10) %>% 
        ggplot(aes(x = reorder(Name, Overall), y = Overall, fill = Name)) +
          geom_bar(stat = 'identity') +
          coord_flip(ylim = c(75, 95)) +
          geom_text(aes(label = Overall), hjust = -0.5, size = 8) +
          scale_fill_viridis(discrete = TRUE) +
          labs(x = 'PLAYER', y = 'OVERALL') +
          theme_minimal(base_size = 24) +
          theme(legend.position = 'none')
    }
  )
  
  output$plotSkl1a <- renderPlot(
    {
      if (!is.null(input$plyr3a)) {
        # physical_skills
        physical_skills <- f %>% 
          filter(Name == input$plyr3a & Nationality == input$cntry3a) %>% 
          select(ID,Name,Strength,Acceleration,Balance,Agility,Reactions)
        
        # the radarchart for the 1st player  
        data0=rbind(rep(100,5) , rep(0,5) , physical_skills[,3:7])
        radarchart( data0 , axistype=1 , 
                pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
                cglcol='grey', cglty=1, axislabcol='grey', cglwd=0.8,
                vlcex=0.8,
                pty = 32,
                title = '')
      }
    }
  )
  output$plotSkl2a <- renderPlot(
    {
      if (!is.null(input$plyr3b)) {
        # physical_skills
        physical_skills <- f %>% 
          filter(Name == input$plyr3b & Nationality == input$cntry3b) %>% 
          select(ID,Name,Strength,Acceleration,Balance,Agility,Reactions)
      
        # the radarchart for the 1st player  
        data0=rbind(rep(100,5) , rep(0,5) , physical_skills[,3:7])
        radarchart( data0 , axistype=1 , 
          pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
          cglcol='grey', cglty=1, axislabcol='grey', cglwd=0.8,
          vlcex=0.8,
          pty = 32,
          title = '')
      }
    }
  )
  
  output$plotSkl1b <- renderPlot(
    {
      if (!is.null(input$plyr3a)) {
        # game_intelligence
        game_intelligence <- f %>% 
          filter(Name == input$plyr3a & Nationality == input$cntry3a) %>% 
          select(ID,Name,Composure,Vision,Aggression)

        # the radarchart for the 1st player 
        data0=rbind(rep(100,3) , rep(0,3) , game_intelligence[,3:5])
        radarchart( data0 , axistype=1 , 
          pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
          cglcol='grey', cglty=1, axislabcol='grey', cglwd=0.8,
          vlcex=0.8,
          pty = 32,
          title = '')
      }
    }
  )
  
  output$plotSkl2b <- renderPlot(
    {
      if (!is.null(input$plyr3b)) {
        # game_intelligence
        game_intelligence <- f %>% 
          filter(Name == input$plyr3b & Nationality == input$cntry3b) %>% 
          select(ID,Name,Composure,Vision,Aggression)

        # the radarchart for the 1st player 
        data0=rbind(rep(100,3) , rep(0,3) , game_intelligence[,3:5])
        radarchart( data0 , axistype=1 , 
          pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
          cglcol='grey', cglty=1, axislabcol='grey', cglwd=0.8,
          vlcex=0.8,
          pty = 32,
          title = '')
      }
    }
  )
  
  output$plotSkl1c <- renderPlot(
    {
      if (!is.null(input$plyr3a)) {
        technique <- f %>% 
          filter(Name == input$plyr3a & Nationality == input$cntry3a) %>%
          select(ID,Name,BallControl,Dribbling,ShortPassing,LongPassing,
                 Volleys,Crossing,Finishing) %>% 
          mutate('Passing' = (ShortPassing+LongPassing)/2) %>% 
          select(ID,Name,BallControl,Dribbling,Passing,Volleys,Crossing,Finishing)

          # the radarchart for the 1st player 
      data0=rbind(rep(100,5) , rep(0,5) , technique[,3:8])
      radarchart( data0 , axistype=1 , 
                    pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
                    cglcol='grey', cglty=1, axislabcol='grey', cglwd=0.8,
                    vlcex=0.8,
                    pty = 32,
                    title = '')
      }
    }
  )
        
  output$plotSkl2c <- renderPlot(
    {
      if (!is.null(input$plyr3b)) {
        technique <- f %>% 
          filter(Name == input$plyr3b & Nationality == input$cntry3b) %>%
          select(ID,Name,BallControl,Dribbling,ShortPassing,LongPassing,
                 Volleys,Crossing,Finishing) %>% 
          mutate('Passing' = (ShortPassing+LongPassing)/2) %>% 
          select(ID,Name,BallControl,Dribbling,Passing,Volleys,Crossing,Finishing)

          # the radarchart for the 1st player 
      data0=rbind(rep(100,5) , rep(0,5) , technique[,3:8])
      radarchart( data0 , axistype=1 , 
                    pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=2 , 
                    cglcol='grey', cglty=1, axislabcol='grey', cglwd=0.8,
                    vlcex=0.8,
                    pty = 32,
                    title = '')
      }
    }
  )
  
  output$wcResult <- renderText(
    if (input$wcYr != 'Start') {
      iUrl1 <- 'http://www.nuoum.com/img/wc1.png'
      iUrl2 <- 'http://www.nuoum.com/img/wc2.png'
      iUrl3 <- 'http://www.nuoum.com/img/wc3.png'
      paste0('<h1>', input$wcYr, '</h2>',
           '<style>td{padding:20px}</style>',
           '<h3><table>',
           '<tr><td><img src = "', iUrl1, '" height = "50%" width = "50%"></img></td><td>', wcT$x[1,3], '</td></tr>',
           '<tr><td><img src = "', iUrl2, '" height = "50%" width = "50%"></img></td><td>', wcT$x[2,3], '</td></tr>',
           '<tr><td><img src = "', iUrl3, '" height = "50%" width = "50%"></img></td><td>', wcT$x[3,3], '</td></tr>',
           '</table></h3>')
    }
  )
  
  wcT <- reactiveValues(x = NULL)
  
  observeEvent(input$wcYr,
    {
      if (input$wcYr != 'Start') {
        wcT$x <- world_cup %>% 
          gather('place', 'country', -year) %>% 
          inner_join(c, by = 'country') %>% 
          filter(year == as.numeric(input$wcYr))
        wcIcon1 <- makeIcon(
          iconUrl = 'www/wc1.png',
          iconWidth = 40, 
          iconHeight = 80,
          iconAnchorX = 20, 
          iconAnchorY = 40
        )
        wcIcon2 <- makeIcon(
          iconUrl = 'www/wc2.png',
          iconWidth = 40, 
          iconHeight = 80,
          iconAnchorX = 20, 
          iconAnchorY = 40
        )
        wcIcon3 <- makeIcon(
          iconUrl = 'www/wc3.png',
          iconWidth = 40, 
          iconHeight = 80,
          iconAnchorX = 20, 
          iconAnchorY = 40
        )
        leafletProxy('wcMap', data = wcT$x[1,]) %>% 
          removeMarker('a') %>%
          addMarkers(
            layerId = 'a',
            lng = ~lng, 
            lat = ~lat,
            icon = wcIcon1,
            popup = paste0(
              '<b><font size="3" color="blue">', wcT$x[1,3], 
              ': 1st Place!</font></b>'
            )
          )
        leafletProxy('wcMap', data = wcT$x[2,]) %>% 
          removeMarker('b') %>%
          addMarkers(
            layerId = 'b',
            lng = ~lng, 
            lat = ~lat,
            icon = wcIcon2,
            popup = paste0(
              '<b><font size="3" color="green">', wcT$x[2,3], 
              ': 2nd Place!</font></b>'
            )
          )
        leafletProxy('wcMap', data = wcT$x[3,]) %>% 
          removeMarker('c') %>%
          addMarkers(
            layerId = 'c',
            lng = ~lng, 
            lat = ~lat,
            icon = wcIcon3,
            popup = paste0(
              '<b><font size="3" color="red">', wcT$x[3,3], 
              ': 3rd Place!</font></b>'
            )
          )
      }
    }  
  )
  
  output$wcMap <- renderLeaflet(
    {
      leaflet() %>%
        addTiles() %>% 
        setView(lng = 0, lat = 18, zoom = 3)
    }
  )
  
  output$exMap <- renderLeaflet(
    {
      z <- f %>% 
        select('country' = Nationality, Age, Potential, Overall)
      if (input$mapOpt == 1) {
        z <- z %>% group_by(country) %>% dplyr::summarize('y' = n())
        zc <- 'blue'
      } else if (input$mapOpt == 2) {
        z <- z %>% group_by(country) %>% dplyr::summarize('y' = -mean(Age))
        zc <- 'red'
      } else if (input$mapOpt == 3) {
        z <- z %>% group_by(country) %>% dplyr::summarize('y' = mean(Potential))
        zc <- 'green'
      } else {
        z <- z %>% group_by(country) %>% dplyr::summarize('y' = mean(Overall))
        zc <- 'black'
      }
      z <- z %>% 
        mutate('rad' = pnorm(y, mean(y), sd(y)) * 25) %>% 
        inner_join(c, by = 'country') 
      leaflet(z) %>%
        addTiles() %>% 
        addCircleMarkers(
          lng = ~lng,
          lat = ~lat,
          radius = ~rad, 
          color = zc,
          stroke = FALSE, 
          fillOpacity = 0.5,
          popup = paste0(
            '<b><font size="3" color="red">', 
            z$country, '</font></b>')) %>% 
        setView(lng = 0, lat = 10, zoom = 3)
    }
  )
  
  clubs1 <- reactive(
    {
      sq <- paste0(
        "SELECT club_name FROM club NATURAL JOIN standings ",
        "WHERE league_name = '", input$lea1, "'",
        "ORDER BY 1"
      )
      mydata <- dbGetQuery(con, sq)
      mydata$club_name
    }
  )
  
  output$clubPlyrs <- renderDataTable(
    {
      sq <- paste0(
        "SELECT player_name, age, height, weight, nationality, ",
        "preferred_position AS position, overall_rating AS overall, potential, value ",
        "FROM (player_info NATURAL JOIN age\n",
        "                  NATURAL JOIN country\n",
        "                  NATURAL JOIN player_value\n",
        "                  NATURAL JOIN player_position\n",
        "                  NATURAL JOIN player_rating\n",
        "                  NATURAL JOIN player_club) AS a\n",
        "     LEFT JOIN club AS b USING (club_id)\n",
        "WHERE club_name = '", input$clu1, "' ",
        "ORDER BY value DESC"
      ) 
      dbGetQuery(con, sq) %>% 
        datatable()
    }
  )
  
  output$club1 <- renderPlot(
    {
      sq <- paste0(
        "SELECT player_name, age, height, weight, ",
        "preferred_position AS position, value ",
        "FROM (player_info NATURAL JOIN age\n",
        "                  NATURAL JOIN player_value\n",
        "                  NATURAL JOIN player_position\n",
        "                  NATURAL JOIN player_club) AS a\n",
        "     LEFT JOIN club AS b USING (club_id)\n",
        "WHERE club_name = '", input$clu1, "'"
      ) 
      xVar <- ifelse(input$clubOpt1 == 'Age',    'age',
              ifelse(input$clubOpt1 == 'Height', 'height',
                                                 'weight'))
      dbGetQuery(con, sq) %>% 
        ggplot(aes(x = get(xVar),
                   y = value, 
                   color = position)) +
        geom_point(size = 5) +
        labs(x = input$clubOpt1, y = 'Player Value') +
        geom_text_repel(aes(label = player_name), hjust = -0.3, size = 6) +
        theme_minimal(base_size = 20) +
        theme(legend.position = 'right')
    }
  )
  
  # clubs2 <- reactive(
  #   {
  #     sq <- paste0(
  #       "SELECT club_name FROM club NATURAL JOIN standings ",
  #       "WHERE league_name = '", input$lea2, "'",
  #       "ORDER BY 1"
  #     )
  #     mydata <- dbGetQuery(con, sq)
  #     mydata$club_name
  #   }
  # )
  
  output$plotLea2a <- renderPlot(
    {
      sq <- paste0(
        "SELECT club_name, player_name, age, ",
        "overall_rating AS overall, value\n",
        "FROM (player_info NATURAL JOIN age\n",
        "                  NATURAL JOIN player_value\n",
        "                  NATURAL JOIN player_rating\n",
        "                  NATURAL JOIN player_club) AS a\n",
        "     LEFT JOIN club AS b USING (club_id)\n",
        "     JOIN standings AS c USING (club_id)\n",
        "WHERE league_name = '", input$lea2, "'"
      )  
      dbGetQuery(con, sq) %>% 
        group_by(club_name) %>% 
        summarize('avg_age' = mean(age), 
                  'avg_overall' = mean(overall), 
                  'avg_value' = mean(value)) %>% 
        select(club_name, avg_age, avg_overall, avg_value) %>% 
        ggplot(aes(x = reorder(club_name, avg_age),
                   y = avg_age, 
                   fill = club_name)) +
        geom_bar(stat = 'identity') +
        labs(x = 'Club', y = 'Average Age') +
        theme_minimal(base_size = 20) +
        theme(legend.position = 'none')
    }
  )
  
  output$plotLea2b <- renderPlot(
    {
      sq <- paste0(
        "SELECT club_name, player_name, age, ",
        "overall_rating AS overall, value\n",
        "FROM (player_info NATURAL JOIN age\n",
        "                  NATURAL JOIN player_value\n",
        "                  NATURAL JOIN player_rating\n",
        "                  NATURAL JOIN player_club) AS a\n",
        "     LEFT JOIN club AS b USING (club_id)\n",
        "     JOIN standings AS c USING (club_id)\n",
        "WHERE league_name = '", input$lea2, "'"
      )  
      dbGetQuery(con, sq) %>% 
        group_by(club_name) %>% 
        summarize('avg_age' = mean(age), 
                  'avg_overall' = mean(overall), 
                  'avg_value' = mean(value)) %>% 
        select(club_name, avg_age, avg_overall, avg_value) %>% 
        ggplot(aes(x = reorder(club_name, avg_overall),
                   y = avg_overall, 
                   fill = club_name)) +
        geom_bar(stat = 'identity') +
        labs(x = 'Club', y = 'Average Overall Rating') +
        theme_minimal(base_size = 20) +
        theme(legend.position = 'none')
    }
  )
  
  output$plotLea2c <- renderPlot(
    {
      sq <- paste0(
        "SELECT club_name, player_name, age, ",
        "overall_rating AS overall, value\n",
        "FROM (player_info NATURAL JOIN age\n",
        "                  NATURAL JOIN player_value\n",
        "                  NATURAL JOIN player_rating\n",
        "                  NATURAL JOIN player_club) AS a\n",
        "     LEFT JOIN club AS b USING (club_id)\n",
        "     JOIN standings AS c USING (club_id)\n",
        "WHERE league_name = '", input$lea2, "'"
      )  
      dbGetQuery(con, sq) %>% 
        group_by(club_name) %>% 
        summarize('avg_age' = mean(age), 
                  'avg_overall' = mean(overall), 
                  'avg_value' = mean(value)) %>% 
        select(club_name, avg_age, avg_overall, avg_value) %>% 
        ggplot(aes(x = reorder(club_name, avg_value),
                   y = avg_value, 
                   fill = club_name)) +
        geom_bar(stat = 'identity') +
        labs(x = 'Club', y = 'Average Player Value') +
        theme_minimal(base_size = 20) +
        theme(legend.position = 'none')
    }
  )
  
  #############################
  # admin tab
  #############################
  
  # fill SQL box with sample code when 1 button clicked
  observeEvent(input$sam1,
    {
      updateTextAreaInput(session, 'sql', 
        value = paste0(
          "-- Argentina history in World Cup\n",
          "SELECT *\n",
          "FROM (SELECT year, first, second, nationality AS third\n",
          "      FROM (SELECT year, first_place, second_place, third_place,\n",
          "                   first, nationality AS second\n", 
    	    "            FROM (SELECT year, first_place, second_place,\n",
          "                         third_place, nationality AS first\n",
    			"                  FROM tournament_name, country\n",
    			"                  WHERE tournament_name.first_place = country.country_id) AS a, country\n",
   		    "            WHERE a.second_place = country.country_id) AS b, country\n",
          "      WHERE b.third_place = country.country_id) as c\n",
          "WHERE c.first = 'Argentina' or c.second = 'Argentina' or c.third = 'Argentina';"
        )
      )
    }
  )
  
  # fill SQL box with sample code when 2 button clicked
  observeEvent(input$sam2,
    {
      updateTextAreaInput(session, 'sql', 
        value = paste0(
          "-- Juventus players\n",
          "SELECT player_name, club_name, height, weight, age, nationality,\n",
          "       jersey_number, preferred_position, overall_rating, potential,\n",
          "       value, joined_date, contract_valid_until, release_clause, wage\n",
          "FROM (player_info NATURAL JOIN age\n",
          "                  NATURAL JOIN country\n",
          "                  NATURAL JOIN player_value\n",
          "                  NATURAL JOIN player_position\n",
          "                  NATURAL JOIN player_rating\n",
          "                  NATURAL JOIN contract\n",
          "                  NATURAL JOIN player_club) AS a\n",
	        "     LEFT JOIN club AS b USING (club_id)\n",
          "WHERE club_name = 'Juventus';"
        )
      )
    }
  )
  
  # fill SQL box with sample code when 3 button clicked
  observeEvent(input$sam3,
    {
      updateTextAreaInput(session, 'sql', 
        value = paste0(
          "-- Brazil history in World Cup\n",
          "SELECT *\n",
          "FROM (SELECT year, first, second, nationality AS third\n",
          "      FROM (SELECT year, first_place, second_place, third_place,\n",
          "                   first, nationality AS second\n", 
    	    "            FROM (SELECT year, first_place, second_place,\n",
          "                         third_place, nationality AS first\n",
    			"                  FROM tournament_name, country\n",
    			"                  WHERE tournament_name.first_place = country.country_id) AS a, country\n",
   		    "            WHERE a.second_place = country.country_id) AS b, country\n",
          "      WHERE b.third_place = country.country_id) as c\n",
          "WHERE c.first = 'Brazil' or c.second = 'Brazil' or c.third = 'Brazil';"
        )
      )
    }
  )
  
  # reactive variable to hold result of query run
  sq <- reactiveValues(l = NULL)
  
  # run query from SQL textbox when RUN button clicked
  observeEvent(input$run,
    { # run query only if SQL starts with letter s or S or --
      if (substr(input$sql,1,1) == 's' |
          substr(input$sql,1,1) == 'S' |
          substr(input$sql,1,2) == '--') {
        sq$l <- dbGetQuery(con, input$sql)
      } else {
        sq$l <- ''
      }
    }
  )
  
  # render table from SQL run
  output$tbl <- renderDataTable(datatable(sq$l))
  
}
