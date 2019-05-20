
dashboardPage(

  header = dashboardHeader(
    title = 'FIFA'
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem('Home', tabName = 'home', icon = icon('home')),
      menuItem('Player', tabName = 'players', icon = icon('user'),
        menuItem('Profile', tabName = 'profile', icon = icon('futbol')),
        menuItem('Position', tabName = 'position', icon = icon('futbol')),
        menuItem('Skills', tabName = 'skills', icon = icon('futbol'))
      ),
      menuItem('Country', tabName = 'country', icon = icon('map'),
        menuItem('World Cup', tabName = 'worldcup', icon = icon('futbol')),
        menuItem('Map Explorer', tabName = 'mapexp', icon = icon('futbol'))
      ),
      menuItem('League', tabName = 'league', icon = icon('users'),
        menuItem('Club Players', tabName = 'club1', icon = icon('futbol')),
        menuItem('Club Analytics', tabName = 'club2', icon = icon('futbol'))
      ),
      menuItem('The Pitch', tabName = 'admin', icon = icon('gears'))
    )
  ),
  body = dashboardBody(
    
    tabItems(
      
      tabItem(
        tabName = 'home',
        wellPanel(
          align = 'center',
          style = 'background-color: #e6faff;',
          # HTML('<h1 style = "font-size: 60px; color: blue"><b>FIFA Analytics</b></h1>'),
          img(src = 'fifa19.jpg', 
              align = 'center', 
              width = '100%',
              deleteFile = FALSE)
        )
      ),
      
      tabItem(
        tabName = 'profile',
        h2('Player Profiles'),
        wellPanel(
          fluidRow(
            column(6, selectInput('cntry',
                                  label = 'Country',
                                  choices = countries,
                                  selected = 'China')),
            column(6, selectInput('plyr',
                                  label = 'Player',
                                  choices = ''))
          )
        ),
        wellPanel(
          fluidRow(
            column(3, wellPanel(align = 'center', htmlOutput('plyrPic'))),
            column(3, 
                   htmlOutput('flagPic'),
                   htmlOutput('plyrInfo')),
            column(6, leafletOutput('worldMap', height = '600'))
          )
        )
      ),
      
      tabItem(
        tabName = 'position',
        h2('Position Analysis'),
        wellPanel(
          align = 'center',
          radioGroupButtons(
            inputId = 'posT',
            label = 'Position Type',
            choices = posTypes,
            status = 'primary',
            size = 'lg'
          )
        ),
        # column(2, 
        #   selectInput(
        #     'posN',
        #     label = 'Position',
        #     choices = c(''),
        #     selected = 0
        #   )
        # )
        wellPanel(
          align = 'center',
          htmlOutput('posTitle'),
          fluidRow(
            column(6, wellPanel(plotOutput('plot10a', height = '600'))),
            column(6, wellPanel(plotOutput('plot10b', height = '600')))
          )
        )
      ),
      
      tabItem(
        tabName = 'skills',
        wellPanel(
          align = 'center',
          fluidRow(
            column(3, 
              wellPanel(
                style = 'background-color: #c6e2ff', 
                h2('Skills Analysis')
              )
            ),
            column(3, wellPanel(h2('Physical Skill'))),
            column(3, wellPanel(h2('Game Intelligence'))),
            column(3, wellPanel(h2('Technique')))
          )
        ),
        wellPanel(
          fluidRow(
            column(3, 
              selectInput('cntry3a',
                          label = 'Country',
                          choices = countries,
                          selected = 'Argentina'),
              selectInput('plyr3a',
                          label = 'Player 1',
                          choices = ''),
              hr(),
              wellPanel(
                align = 'center',
                htmlOutput('plyr1Pic'))
            ),
            column(3, plotOutput('plotSkl1a')),
            column(3, plotOutput('plotSkl1b')),
            column(3, plotOutput('plotSkl1c'))
          )
        ),
        wellPanel(
          fluidRow(
            column(3, 
              selectInput('cntry3b',
                          label = 'Country',
                          choices = countries,
                          selected = 'Portugal'),
              selectInput('plyr3b',
                          label = 'Player 2',
                          choices = ''),
              hr(),
              wellPanel(
                align = 'center',
                htmlOutput('plyr2Pic'))
            ),
            column(3, plotOutput('plotSkl2a')),
            column(3, plotOutput('plotSkl2b')),
            column(3, plotOutput('plotSkl2c'))
          )
        )
      ),
      
      tabItem(
        tabName = 'worldcup',
        h2('World Cup Performances'),
        h3(
          sliderTextInput(
            inputId = 'wcYr',
            label = 'World Cup Year',
            choices = c('Start', '1930', '1934', '1938', '1950', '1954', '1958',
                                 '1962', '1966', '1970', '1974', '1978', '1982',
                                 '1986', '1990', '1994', '1998', '2002', '2006',
                                 '2010', '2014', '2018'),
            selected = 'Start',
            animate = animationOptions(interval = 2000),
            width = '100%'
          )
        ),
        fluidRow(
          column(3,
            wellPanel(
              htmlOutput('wcResult')
            )
          ),
          column(9,
            wellPanel(
              leafletOutput('wcMap', height = '750')
            )
          )
        )
      ),
      
      tabItem(
        tabName = 'mapexp',
        h2('Map Explorer'),
        h3(
          radioGroupButtons(
            inputId = 'mapOpt',
            label = 'View map by:',
            choices = c('Number of Players' = 1,
                        'Average Player Age' = 2,
                        'Average Potential Rating' = 3,
                        'Average Overall Rating' = 4
                        ),
            status = 'primary',
            size = 'lg'
          )
        ),
        fluidRow(
          column(9,
            wellPanel(
              leafletOutput('exMap', height = '750')
            )
          ),
          column(3,
            wellPanel(
              htmlOutput('exTbl')
            )
          )
        )
      ),
      
      tabItem(
        tabName = 'club1',
        h2('Club Players'),
        wellPanel(
          fluidRow(
            column(6,
              selectInput(
                inputId = 'lea1',
                label = 'League',
                choices = leagues$league_name,
                selected = 'Premier League'
              )    
            ),
            column(6,
              selectInput(
                inputId = 'clu1',
                label = 'Club',
                choices = '',
                selected = 0
              )    
            )
          )
        ),
        wellPanel(
          fluidRow(
            column(6,
              wellPanel(
                dataTableOutput('clubPlyrs')
              )
            ),
            column(6,
              wellPanel(
                align = 'center',
                plotOutput('club1', height = '500'),
                fluidRow(
                  column(6,
                    align = 'right',
                    h2('Compare Player Value to:')
                  ),
                  column(6,
                    align = 'left',
                    radioGroupButtons(
                      inputId = 'clubOpt1',
                      label = '',
                      choices = c('Age', 'Height', 'Weight'),
                      status = 'primary'
                    ) 
                  )
                )
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = 'club2',
        h2('Club Analytics'),
        wellPanel(
          fluidRow(
            column(6,
              selectInput(
                inputId = 'lea2',
                label = 'League',
                choices = leagues$league_name,
                selected = 'Premier League'
              )    
            )
            # column(6,
            #   selectInput(
            #     inputId = 'clu2',
            #     label = 'Club',
            #     choices = '',
            #     selected = 0
            #   )    
            # )
          )
        ),
        wellPanel(
          fluidRow(
            column(4,
              wellPanel(
                plotOutput('plotLea2a', height = '500')
              )
            ),
            column(4,
              wellPanel(
                plotOutput('plotLea2b', height = '500')
              )
            ),
            column(4,
              wellPanel(
                plotOutput('plotLea2c', height = '500')
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = 'clubplyr',
        h2('Club Players'),
        wellPanel(
          fluidRow(
            column(6, selectInput('lea4',
                                  label = 'League',
                                  choices = '')),
            column(6, selectInput('club4',
                                  label = 'Club',
                                  choices = ''))
          )
        ),
        wellPanel(
          fluidRow(
            column(4,
              wellPanel(
                plotOutput('plotLea4a')
              )
            ),
            column(4,
              wellPanel(
                plotOutput('plotLea4b')
              )
            ),
            column(4,
              wellPanel(
                plotOutput('plotLea4c')
              )
            )
          )
        )
      ),
      
      ##########################################
      # Admin tab
      ##########################################
    
      tabItem(
        tabName = 'admin',
        h2('The SQL Pitch'),
        
        dropdownButton(
          h3('KICK'),
          passwordInput(
            inputId = 'pwd', 
            label = 'Admin Password:'
          ),
          label = '',
          circle = TRUE, 
          status = 'primary',
          icon = icon('gear'), 
          width = '300px',
          tooltip = tooltipOptions(title = 'Click to Login')
        ),
        
        tags$head(
          tags$style(
            HTML('textArea {font-family: Courier;
                 font-size: 36px;
                 font-weight: bold;}'
            )
          )
        ),
        conditionalPanel(
          'input.pwd == 333', 
          wellPanel(
            fluidRow(
              column(12,
                align = 'center',
                actionBttn(
                  inputId = 'sam1',
                  label = 'Messi', 
                  style = 'minimal',
                  color = 'primary',
                  icon = icon('circle')
                ),
                HTML('&nbsp;|&nbsp;'),
                actionBttn(
                  inputId = 'sam2',
                  label = 'Ronaldo', 
                  style = 'minimal',
                  color = 'primary',
                  icon = icon('circle')
                ),
                HTML('&nbsp;|&nbsp;'),
                actionBttn(
                  inputId = 'sam3',
                  label = 'Neymar', 
                  style = 'minimal',
                  color = 'primary',
                  icon = icon('circle')
                ),
                HTML('&nbsp;&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;&nbsp;'),
                actionBttn(
                  inputId = 'run',
                  label = 'RUN', 
                  style = 'minimal',
                  color = 'success',
                  icon = icon('play', lib = 'glyphicon')
                )
              )
            )
          ),
          wellPanel(
            textAreaInput(
              inputId = 'sql', 
              label = 'SQL:', 
              value = '',
              width = '100%', 
              height = '300px',
              resize = 'vertical'
            )
          ),
          wellPanel(
            dataTableOutput('tbl')
          )
        )
      )
    )
  )
  
  
)
    