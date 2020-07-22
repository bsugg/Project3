library(shinydashboard)
library(dplyr)
library(ggplot2)

load("collegeFootball.Rdata")


function(input, output, session) {
  
  teamsR <- reactive({teams})
  venuesR <- reactive({venues})
  
  output$tableTeams <- renderTable({
    teamsR()
  })
  
  
  output$tableVenues <- renderTable({
    venuesR()
  })

  

}
