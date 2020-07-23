library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

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
  ##### GAMES
  #####
  
  newGames <- reactive({
    # Filter on user selected team and parse "games" data set for home and away
    gamesHome <- games %>% filter(home_team == input$team) %>%
      mutate(home=1) %>%
      mutate(away=0) %>%
      mutate(teamConference=ifelse(is.na(home_conference),"Other",home_conference)) %>%
      mutate(teamPoints=home_points) %>%
      mutate(won=ifelse(home_points>away_points,1,0)) %>%
      mutate(loss=ifelse(home_points<away_points,1,0)) %>%
      mutate(tie=ifelse(home_points==away_points,1,0)) %>%
      mutate(opponent=away_team) %>%
      mutate(oppConference=ifelse(is.na(away_conference),"Other",away_conference)) %>%
      mutate(oppPoints=away_points)
    gamesAway <- games %>% filter(away_team == input$team) %>%
      mutate(home=0) %>%
      mutate(away=1) %>%
      mutate(teamConference=ifelse(is.na(away_conference),"Other",away_conference)) %>%
      mutate(teamPoints=away_points) %>%
      mutate(won=ifelse(away_points>home_points,1,0)) %>%
      mutate(loss=ifelse(away_points<home_points,1,0)) %>%
      mutate(tie=ifelse(home_points==away_points,1,0)) %>%
      mutate(opponent=home_team) %>%
      mutate(oppConference=ifelse(is.na(home_conference),"Other",home_conference)) %>%
      mutate(oppPoints=home_points)
    # Merge the home and away sets into ONE games data set
    games <- bind_rows(gamesHome,gamesAway)
    # Add a few more attributes and sort by date
    games <- games %>% mutate(outcome=ifelse(won==1,"Won",ifelse(loss==1,"Loss","Tie"))) %>%
                       mutate(team=input$team) %>%
                       mutate(score=paste0(teamPoints,"-",oppPoints)) %>%
                       mutate(location=ifelse(neutral_site,"Neutral",ifelse(home==1,"Home","Away")))
    games <- arrange(games,kickoffDate)
    # Join with venues data set to enrich with location details
    gamesVenues <- left_join(games,venuesTrun,by="venue_id")
  })
  
  #####
  ##### GAME STATS
  #####
  
  gameStatsNew <- reactive({
    gameStats <- gameStats %>% filter(school == input$team)
  })

  
###################### OUTPUT ################################################

  #####
  ##### DATA EXPLORATION
  #####
  
  ###
  ### TEAM SUMMARY
  ###
  
  # TEXT
  
  output$logoURL <- renderUI({
    #getTeams <- newTeams
    url <- paste("http://a.espncdn.com/i/teamlogos/ncaa/500/153.png")
  })
  
  output$teamTitle <- renderText({
    getTeams <- newTeams()
    teamName <- paste(getTeams$school,getTeams$mascot, sep = " ")
  })
  output$teamText <- renderText({
    getGamesPro <- newGames()
    paste("The average body weight for order", input$team, "is", nrow(getGamesPro), sep = " ")
  })
  
  # MAP
  
  
  
  # TABLE
  
  output$teamTable <- DT::renderDataTable({
    getGamesPro <- newGames()
    getGamesPro <- getGamesPro %>% select(season,kickoffDate,season_type,team,teamConference,opponent,oppConference,outcome,score,location,venue,city,state,country_code)
    DT::datatable(getGamesPro,options = list(orderClasses = TRUE,pageLength = 5))
  })
  
  ###
  ### GAMES
  ###
  
  output$tableGames <- DT::renderDataTable({
    DT::datatable(gamesNew(),options = list(orderClasses = TRUE,pageLength = 5))
  })
  
  
}
