library(shinydashboard)
library(ggplot2)
library(DT)
library(leaflet)
library(RColorBrewer)

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
      uiOutput("teamLogo", click = "Team Logo"),
      selectizeInput("team", "Team",
                  selected = "North Carolina", choices = levels(as.factor(teams$school))
      ),
      sliderInput("sliderSeason", "Season",min=2012, max=2019,value=c(2012,2019),step=1,sep=""
                  ),
      radioButtons("radioGameSelect","Game Type Selection",choiceNames=list("Auto","Manual"),choiceValues=list("auto","manual")
                   ),
      conditionalPanel(condition = "input.radioGameSelect == 'manual'",style = "color:red;",
                       selectizeInput("selectSeasonType", "Season Type",
                                      selected = "All", choices = c("All","Regular","Postseason")
                       ),
                       selectizeInput("selectSchedule", "Schedule",
                                      selected = "All", choices = c("All","Conference","NonConference")
                       )
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
                column(width=12,
                  box(title = "ST558 - Project 3", status = "primary",width=NULL,
                    "Brian Sugg", br(),
                    "July 27, 2020", br(),
                    br(),
                    "Describes the data and abilities of the app.", br(),
                    br(),
                    "Other dynamic text here."
                  )
                )
              )
      ),
      
      # Data Exploration - Team Summary
      tabItem(tabName = "teamSum",
              fluidRow(
                column(width=3,
                       box(status = "primary",width=NULL,
                           uiOutput("teamLogoPro", click = "Team Logo")
                       ),
                ),
                column(width=3,
                  box(title="Team Summary",status = "primary",width=NULL,height=224,
                      tags$h2(textOutput("teamTitle1")),
                      tags$h3(textOutput("teamTitle2")),
                      textOutput("teamText")
                      ),
                ),
                column(width=6,
                  valueBoxOutput("seaBox"),
                  valueBoxOutput("gameBox"),
                  valueBoxOutput("postBox"),
                  valueBoxOutput("winBox"),
                  valueBoxOutput("lossBox"),
                  valueBoxOutput("winPctBox")
                  )
                ),
              fluidRow(
                column(width=6,
                  box(status = "primary",tabsetPanel(type = "tabs",
                                tabPanel("All", plotOutput("allWinPlot")),
                                tabPanel("Conference", plotOutput("confWinPlot")),
                                tabPanel("NonConference", plotOutput("nonConfWinPlot")),
                                tabPanel("Postseason", plotOutput("postWinPlot"))
                    ),width=NULL,height = 475
                  )
                ),
                column(width=6,
                       box(title="Geographic Coverage by Outcome",status = "primary",
                           leafletOutput("mymap"),width=NULL,height = 475
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(status = "primary",div(style = 'overflow-x: scroll', DT::dataTableOutput("teamTable")),width = NULL
                       )
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
