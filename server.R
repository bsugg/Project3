library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

load("collegeFootball.Rdata")

function(input, output, session) {
  
  teamsNew <- reactive({
    teams <- teams %>% filter(school == input$team)
  })
  
  gamesNew <- reactive({
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
    games <- bind_rows(gamesHome,gamesAway)
    games <- games %>% mutate(outcome=ifelse(won==1,"Won",ifelse(loss==1,"Loss","Tie"))) %>%
                       mutate(team=input$team) %>%
                       mutate(score=paste0(teamPoints,"-",oppPoints)) %>%
                       mutate(location=ifelse(neutral_site,"Neutral",ifelse(home==1,"Home","Away")))
    #games <- select(games,c(1:7,team))
                       #mutate(teamConference=teamsNew$conference)
    games <- arrange(games,kickoffDate)
  })
  
  gameStatsNew <- reactive({
    gameStats <- gameStats %>% filter(school == input$team)
  })

  
###################### OUTPUT ################################################

  #output$logo <- renderImage({img(src=teamsNew$logos)})

  output$logo <- renderText({
    paste(teamsNew$logos)
  })
  
  #output$logoAlt <- renderText({
  #  logoSrc <- teamsNew$logos
  #})
  

  
  output$tableGames <- DT::renderDataTable({
    DT::datatable(gamesNew(),options = list(orderClasses = TRUE,pageLength = 5))
  })
}
