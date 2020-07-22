library(shinydashboard)
library(dplyr)
library(ggplot2)

load("collegeFootball.Rdata")

function(input, output, session) {
  
  teamsNew <- reactive({
    teams <- teams %>% filter(school == input$team)
  })
  
  gamesNew <- reactive({
    games <- bind_rows(filter(games,home_team == input$team),
                       filter(games,away_team == input$team)
                      )
  })
  
  teamsR <- reactive({teams})
  venuesR <- reactive({venues})
  
  output$tableTeams <- renderTable({
    teamsR()
  })

}
