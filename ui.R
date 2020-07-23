library(shinydashboard)
library(ggplot2)
library(DT)

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
                menuSubItem("Team Summary", tabName = "teamSum"),
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
      #uiOutput("logo"),
      img(src = textOutput("logo"),width=210,style="display: block; margin-left: auto; margin-right: auto;"),
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
                  "Other dynamic text here."
                )
              )
      ),
      
      # Data Exploration - Teams
      tabItem(tabName = "teamSum",
              fluidRow(
                column(width=6,
                  box(status = "primary", width=9,
                      tags$h3(textOutput("teamTitle")),
                      textOutput("teamText"),
                      tags$img(src = uiOutput("logoURL"), width = "100px", height = "100px")
                  ),
                )
              ),
              fluidRow(
                column(width = 12,
                       box(div(style = 'overflow-x: scroll', DT::dataTableOutput("teamTable")),width = 12)
                )
              )
      ),
      
      # Data Exploration - Games
      tabItem(tabName = "games",
               fluidRow(
                 column(width = 12,
                        box(div(style = 'overflow-x: scroll', DT::dataTableOutput("tableGames")),width = 12)
                 )
               )
             )
    )
  )
)
