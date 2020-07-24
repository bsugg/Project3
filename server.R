library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(leaflet)
library(ragtop)
library(knitr)

# Call data generated from save function within the "CollegeFootballAPI.R" file
load("collegeFootball.Rdata")

function(input, output, session) {

###################### REACTIVE ELEMENTS #####################################
  
  #####
  ##### TEAM ATTRIBUTES
  #####
  
  newTeams <- reactive({
    # Filter on user selected team - generates a single record with team attributes
    teams <- teams %>% filter(school == input$team)
  })
  
  #####
  ##### SEASON SLIDER VALUES
  #####
  
  seasonFinder <- reactive({
    # Filter on user selected team and parse "games" data set for home and away
    gameSeasonsHome <- gameSeasons %>% filter(home_team == input$team)
    gameSeasonsAway <- gameSeasons %>% filter(away_team == input$team)
    # Merge the home and away sets into ONE games data set
    gamesSeasons <- bind_rows(gameSeasonsHome,gameSeasonsAway)
  })
  
  
  #####
  ##### GAMES
  #####
  
  newGames <- reactive({
    # Filter on user selected team and parse "games" data set for home and away
    gamesHome <- games %>% filter(home_team == input$team) %>%
                           filter(season >= input$sliderSeason[1]) %>%
                           filter(season <= input$sliderSeason[2]) %>%
      mutate(home=1) %>%
      mutate(away=0) %>%
      mutate(teamConference=as.character(ifelse(!is.na(home_conference),home_conference,"Other"))) %>%
      mutate(teamPoints=home_points) %>%
      mutate(won=ifelse(home_points>away_points,1,0)) %>%
      mutate(loss=ifelse(home_points<away_points,1,0)) %>%
      mutate(tie=ifelse(home_points==away_points,1,0)) %>%
      mutate(opponent=away_team) %>%
      mutate(oppConference=as.character(ifelse(!is.na(away_conference),away_conference,"Other"))) %>%
      mutate(oppPoints=away_points)
    gamesAway <- games %>% filter(away_team == input$team) %>%
                           filter(season >= input$sliderSeason[1]) %>%
                           filter(season <= input$sliderSeason[2]) %>%
      mutate(home=0) %>%
      mutate(away=1) %>%
      mutate(teamConference=as.character(ifelse(!is.na(away_conference),away_conference,"Other"))) %>%
      mutate(teamPoints=away_points) %>%
      mutate(won=ifelse(away_points>home_points,1,0)) %>%
      mutate(loss=ifelse(away_points<home_points,1,0)) %>%
      mutate(tie=ifelse(home_points==away_points,1,0)) %>%
      mutate(opponent=home_team) %>%
      mutate(oppConference=as.character(ifelse(!is.na(home_conference),home_conference,"Other"))) %>%
      mutate(oppPoints=home_points)
    # Merge the home and away sets into ONE games data set
    games <- bind_rows(gamesHome,gamesAway)
    # Add a few more attributes and sort by date
    games <- games %>% mutate(outcome=as.factor(ifelse(won==1,"Won",ifelse(loss==1,"Loss","Tie")))) %>%
                       mutate(team=input$team) %>%
                       mutate(score=paste0(teamPoints,"-",oppPoints)) %>%
                       mutate(margin=teamPoints-oppPoints) %>%
                       mutate(location=ifelse(neutral_site,"Neutral",ifelse(home==1,"Home","Away")))
    games <- arrange(games,kickoffDate)
    # Join with venues data set to enrich with location details
    gamesVenuesJoin <- left_join(games,venues,by="venueId")
    gamesVenuesJoin <- gamesVenuesJoin %>% mutate(venueCityState=ifelse(!is.na(venueState),paste0(venueCity,", ",venueState),paste0(venueCity)))
  })
  
  #####
  ##### GAME STATS
  #####
  
  gameStatsNew <- reactive({
    gameStats <- gameStats %>% filter(school == input$team)
  })

  
###################### OUTPUT ################################################

  #####
  ##### UI ACTIONS
  #####
  
  ###
  ### SIDEBAR
  ###
  
  # Team logo image
  output$teamLogo<- renderUI({
    tags$img(src=newTeams()$logos[1], width=210,style="display: block; margin-left: auto; margin-right: auto;")
  })
  
  # Season slider, set min and max properties
  observe({
    getSeasons <- seasonFinder()
    minSeason <- min(getSeasons$season)
    maxSeason <- max(getSeasons$season)
    updateSliderInput(session,"sliderSeason",min=minSeason,max=maxSeason,value = c(minSeason, maxSeason))
  })
  
  #####
  ##### DATA EXPLORATION
  #####
  
  ###
  ### TEAM SUMMARY
  ###
  
  # TEXT
  
  # Set team name and mascot title
  output$teamTitle <- renderText({
    getTeams <- newTeams()
    teamName <- paste(getTeams$school,getTeams$mascot, sep = " ")
  })
  
  # Team introduction
  output$teamText <- renderText({
    getGamesPro <- newGames()
    getTeams <- newTeams()
    paste("The average body weight for order", input$team, "is", nrow(getGamesPro), sep = " ")
  })
  
  # MAP
  
  output$mymap <- renderLeaflet({
    getGamesPro <- newGames()
    points <- cbind(getGamesPro$venueLong,getGamesPro$venueLat)
    
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircles(data = points,
                 color = ifelse(getGamesPro$outcome=="Won","#228B22","#FF0000"),
                 weight = 8,
                 popup = paste(tags$strong("Season: "),getGamesPro$season," - ",getGamesPro$seasonType,"- ",ifelse(getGamesPro$conference_game,"Conference","NonConference"),br(),
                               tags$strong("Date: "),format(getGamesPro$kickoffTime,'%A, %B %d, %Y'),br(),
                               tags$strong("Opponent: "),getGamesPro$opponent,br(),
                               tags$strong("Outcome: "),getGamesPro$outcome," ",getGamesPro$score,br(),
                               tags$strong("Location: "),getGamesPro$location,br(),
                               tags$strong("Venue: "),getGamesPro$venueName," in ",getGamesPro$venueCityState
                               )
                )
  })
  
  # TABLE
  
  output$teamTable <- DT::renderDataTable({
    getGamesPro <- newGames()
    getTeams <- newTeams()
    customPrintName <- paste0(getTeams$abbreviation," Team Summary")
    customFileName <- paste0(getTeams$abbreviation,"teamSummary")
    getGamesPro <- getGamesPro %>% select(season,kickoffDate,seasonType,team,teamConference,opponent,oppConference,outcome,score,margin,location,venueName,venueCityState,venueCountry)
    DT::datatable(getGamesPro,extensions = 'Buttons',
                  options = list(orderClasses = TRUE, pageLength = 5,dom = 'Blfrtip',
                                 lengthMenu = list(c(5,10,25,50,100,-1),c('5','10','25','50','100','All')),
                                 buttons = c(list(list(extend = 'copy', title= "")),
                                             list(list(extend = 'print', title= customPrintName)),
                                             list(list(extend = 'csv', filename= customFileName)),
                                             list(list(extend = 'excel',filename= customFileName,title= "")),
                                             list(list(extend = 'pdf', filename= customFileName,title= customPrintName,orientation='landscape',pageSize= 'LEGAL'))
                                             )
                  )
    )
  })
  
  ###
  ### GAMES
  ###
  
  output$tableGames <- DT::renderDataTable({
    DT::datatable(newGames(),options = list(orderClasses = TRUE,pageLength = 5))
  })
  
  
}
