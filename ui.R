library(shinydashboard)
library(ggplot2)

# Shinydashboard information here https://rstudio.github.io/shinydashboard/index.html
# Icons sourced from https://fontawesome.com/icons?d=gallery&m=free

load("collegeFootball.Rdata")

dashboardPage(
  
  #
  ##
  ### HEADER
  ##
  #
  
  dashboardHeader(title = "NCAA College Football",titleWidth = 250),
  
  #
  ##
  ### SIDEBAR
  ##
  #
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Information", tabName = "info", icon = icon("info")),
      menuItem("Data Exploration", tabName = "explore", icon = icon("bar-chart-o"),
                menuSubItem("Teams", tabName = "teams"),
                menuSubItem("Games", tabName = "games"),
                menuSubItem("Venues", tabName = "venues")
               ),
      menuItem("Unsupervised Learning", tabName = "unsupervised", icon = icon("chalkboard-teacher")),
      menuItem("Modeling", tabName = "model", icon = icon("code-branch"),
                menuSubItem("Model 1", tabName = "model1"),
                menuSubItem("Model 2", tabName = "model2")
               ),
      menuItem("Data", tabName = "data", icon = icon("th")),
      menuItem("Source API", icon = icon("file-code-o"),href = "https://collegefootballdata.com/"),
      selectizeInput("team", "Team",
                  selected = "North Carolina", choices = levels(as.factor(teams$school))
      )
    )
  ),
  
  #
  ##
  ### BODY
  ##
  #
  
  dashboardBody(
    tabItems(
      
      # Information
      tabItem(tabName = "info",
              fluidRow(
                box(title = "ST558 - Project 3", status = "primary",
                  "Brian Sugg", br(),
                  "July 27, 2020", br(),
                  br(),
                  "Describes the data and abilities of the app.", br(),
                  br(),
                  "Utilize HEADERS and FORMAT TEXT."
                )
              )
      ),
      
      # Data Exploration - Teams
      tabItem(tabName = "teams",
              fluidRow(
                column(width=9,
                  box(title = "Team Details", status = "primary", width=9,
                      "Some text here and here and here..."
                  )
                )
              )
      ),
      
      # Data Exploration - Games
      tabItem(tabName = "games",
               fluidRow(
                 column(width=9,
                        box(tableOutput("tableGames"))
                       )
               )
             )
    )
  )
)
