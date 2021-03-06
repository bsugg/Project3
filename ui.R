library(shinydashboard)
library(ggplot2)
library(DT)
library(leaflet)
library(RColorBrewer)
library(shinyjs)
library(V8)
library(mathjaxr)
library(wordcloud2)
library(plotly)

# Shinydashboard information here https://rstudio.github.io/shinydashboard/index.html
# Icons sourced from https://fontawesome.com/icons?d=gallery&m=free

# Call data generated from save function within the "CollegeFootballAPI.R" file
load("collegeFootball.RData")

# Utilize shinyjs package for javascript code to collapse boxes on command
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

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
               menuSubItem("Game Summary", tabName = "gameSum")
      ),
      menuItem("Unsupervised Learning", tabName = "unsupervised", icon = icon("chalkboard-teacher")),
      menuItem("Modeling", tabName = "model", icon = icon("code-branch"),
               menuSubItem("Generalized Linear Model", tabName = "modelGLM"),
               menuSubItem("Ensemble Model", tabName = "modelRF")
      ),
      #menuItem("Data", tabName = "data", icon = icon("th")),
      menuItem("Source API", icon = icon("file-code-o"),
               menuSubItem("Main Page", href = "https://collegefootballdata.com/"),
               menuSubItem("Glossary",href = "https://collegefootballdata.com/Glossary")
      ),
      uiOutput("teamLogo"),
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
    
    # Enable javascript for shinyjs to control collapse of boxes
    useShinyjs(),
    extendShinyjs(text = jscode),
    
    tabItems(
      
      #####
      ##### INFORMATION
      #####
      tabItem(tabName = "info",
              fluidRow(
                column(width=6,
                       box(title = "Information", footer = "Data Last Refreshed: 27-July-2020", status = "primary",width=NULL,
                           tabsetPanel(type = "tabs",
                                       tabPanel("Overview", uiOutput("infoOver")),
                                       tabPanel("Capabilities", uiOutput("infoCap"))
                                       #tabPanel("Fun Things", uiOutput("infoFun")) # For future addition
                           )
                       )
                ), # END COLUMN
                column(width=6,
                       box(title = "Word Cloud of Teams by Wins",footer="Dynamic with Season and Game Type Selection", status = "primary",width=NULL,
                           wordcloud2Output("infoCloudTeams")
                       ),
                ), # END COLUMN
              ) # END FLUID ROW
      ),
      
      #####
      ##### DATA EXPLORATION
      #####
      
      # TEAM SUMMARY
      tabItem(tabName = "teamSum",
              fluidRow(
                column(width=3,
                       # Logo
                       box(status = "primary",width=NULL,
                           uiOutput("teamLogoPro")
                       ),
                ), # end column, continue row
                column(width=3,
                       # Team name and mascot name
                       box(title="",status = "primary",width=NULL,height=224,
                           tags$h2(textOutput("teamTitle1")),
                           tags$h3(textOutput("teamTitle2")),
                           textOutput("teamText")
                       ),
                ), # end column, continue row
                column(width=6,
                       # Top right corner metric boxes
                       valueBoxOutput("seaBox"),
                       valueBoxOutput("gameBox"),
                       valueBoxOutput("postBox"),
                       valueBoxOutput("winBox"),
                       valueBoxOutput("lossBox"),
                       valueBoxOutput("winPctBox")
                ) # end column
              ), # end fluidRow
              fluidRow(
                column(width=6,
                       # Charts and plots
                       box(status = "primary",tabsetPanel(type = "tabs",
                                                          tabPanel("All", plotlyOutput("allWinPlot")),
                                                          tabPanel("Home", plotlyOutput("homeWinPlot")),
                                                          tabPanel("Away", plotlyOutput("awayWinPlot")),
                                                          tabPanel("Neutral", plotlyOutput("neutralWinPlot"))
                       ),width=NULL,height = 475
                       )
                ), # end column
                column(width=6,
                       # Map of wins and losses
                       box(title="Geographic Coverage by Outcome - Clickable",status = "primary",
                           leafletOutput("gamesMap"),width=NULL,height = 475
                       )
                ) # end column
              ), # end fluidRow
              fluidRow(
                column(width = 12,
                       # Data table for export
                       box(title="Team Summary Data Set",status = "primary",
                           div(style = 'overflow-x: scroll', DT::dataTableOutput("teamTable")),width = NULL
                       )
                )
              ) # end row
      ), # END OF TEAM SUMMARY
      
      # GAMES
      tabItem(tabName = "gameSum",
              fluidRow(
                column(width = 5,
                       box(title="Game Summary",status = "primary",width = NULL,
                           "Basic summary statistics and visualizations from the games data set are provided."),
                       box(title="Numeric Summaries",status = "primary",width = NULL,
                           tabsetPanel(type = "tabs",
                                       tabPanel("Summary", uiOutput("gsSumStats")),
                                       tabPanel("Location", uiOutput("gsLocFreq")),
                                       tabPanel("Conference", uiOutput("gsOppCFreq")),
                                       tabPanel("Game Day", uiOutput("gsDayFreq")),
                                       tabPanel("State", uiOutput("gsStateFreq"))
                           ))
                ), # end column
                column(width = 7,
                       box(title="Graphical Summaries",status = "primary",width = NULL,
                           tabsetPanel(type = "tabs",
                                       tabPanel("Team Box Plot", plotlyOutput("gsBoxPointsTeam")),
                                       tabPanel("Team Hist", sliderInput("histSlideTeam", "Number of Bins",min=10, max=30,value=20),
                                                plotlyOutput("gsHistPointsTeam")),
                                       tabPanel("Team Scatter Plot", plotlyOutput("gsPlotScoreOTteam")),
                                       tabPanel("Opp Box Plot", plotlyOutput("gsBoxPointsOpp")),
                                       tabPanel("Opp Hist", sliderInput("histSlideOpp", "Number of Bins",min=10, max=30,value=20),
                                                plotlyOutput("gsHistPointsOpp")),
                                       tabPanel("Opp Scatter Plot", plotlyOutput("gsPlotScoreOTopp"))
                           ))
                ) # end column
              ), # end fluidRow
              fluidRow(
                column(width = 12,
                       box(title="Game Summary Data Set",status = "primary",
                           div(style = 'overflow-x: scroll', DT::dataTableOutput("tableGames")),width = NULL)
                ) # end column
              ) # end fluidRow
      ), # END OF GAMES
      
      #####
      ##### UNSUPERVISED
      #####
      
      tabItem(tabName = "unsupervised",
              fluidRow(
                column(width = 2,
                       box(title="Unsupervised Learning",status = "primary",width = NULL,
                           "Unsupervised learning is exploratory in nature, with no clear objective  or goal
                           in mind. Interpretation of these plots is subjective and should encourage additional exploration."),
                       box(title="Principal Component Analysis",status = "primary",width = NULL,
                           uiOutput("unsuperPcaIntro"))
                ), # end column
                column(width = 2,
                       box(title="Plot Variables",status = "primary",width = NULL,
                           uiOutput("unsuperSelect"),
                           checkboxInput("unsuperTeamScore", "Team Points Scored", TRUE),
                           checkboxInput("unsuperTeamTalent", "Team Talent Level", TRUE),
                           checkboxInput("unsuperOppTalent", "Opponent Talent Level", TRUE),
                           checkboxInput("unsuperLoc", "Game Location", TRUE),
                           checkboxInput("unsuperExcite", "Game Excitement Level", TRUE),
                           checkboxInput("unsuperVenue", "Venue Details", TRUE),
                           checkboxInput("unsuperCrowd", "Crowd Size", TRUE),
                           uiOutput("unsuperSelectNotes"))
                ), # end column
                column(width = 8,
                       box(title="Plots",status = "primary",width = NULL,
                           tabsetPanel(type = "tabs",
                                       tabPanel("PCA", plotlyOutput("unsuperPCA"))
                                       ))
                ) # end column
              ), # end fluidRow
              fluidRow(
                column(width = 12,
                       box(title="Unsupervised Data Set",status = "primary",
                           div(style = 'overflow-x: scroll', DT::dataTableOutput("tableUnsuper")),width = NULL)
                ) # end column
              ) # end fluidRow
      ), # END OF UNSUPERVISED
      
      #####
      ##### MODELING
      #####
      
      # MODEL - GLM
      tabItem(tabName = "modelGLM",
              fluidRow(
                column(width = 3,
                       box(title="Generalized Linear Model",status="primary",width = NULL,height=293,
                           uiOutput("glmIntro"),withMathJax("$$P = \\frac{1}{1 + e^{-(b_0+b_1x)}}$$"))
                ), # end column
                column(width = 6,
                       box(title="Team",status="primary",width = 4,height=170,
                           uiOutput("teamLogoProGLM")
                       ),
                       box(id="glmBoxLoc",title="Location",status="primary",width = 4,height=170,align = "center",collapsible = TRUE,collapsed = TRUE,
                           uiOutput("locForGLM")
                       ),
                       box(id="glmBoxOpp",title="Opponent",status="primary",width = 4,height=170,align = "center",collapsible = TRUE,collapsed = TRUE,
                           uiOutput("oppLogoProGLM")
                       ),
                       valueBoxOutput("glmPredictBox"),
                       valueBoxOutput("glmAccBox"),
                       valueBoxOutput("glmNumVar")
                ), # end column
                column(width = 3,
                       box(title="Model Training Process",status="primary",width = NULL,height=293,
                           uiOutput("glmTrainProcess"))
                ), # end column
              ), # end fluidRow
              fluidRow(
                column(width = 4,
                       box(id="glmStep1",title="Step 1: Create Prediction Model",status="primary",width = NULL,collapsible = TRUE,
                           "Select the predictors to use in the model fit:",
                           checkboxInput("glmTeamScore", "Team Points Scored", TRUE),
                           checkboxInput("glmTeamTalent", "Team Talent Level", TRUE),
                           checkboxInput("glmOppTalent", "Opponent Talent Level", TRUE),
                           checkboxInput("glmLoc", "Game Location", TRUE),
                           checkboxInput("glmExcite", "Game Excitement Level", TRUE),
                           checkboxInput("glmVenue", "Venue Details", TRUE),
                           checkboxInput("glmCrowd", "Crowd Size", TRUE),
                           "Create a new prediction model:",br(),
                           actionButton("glmCreate", "Create Model"),
                           uiOutput("glmSelectNotes")
                       )
                ), # end column
                column(width = 4,
                       box(id="glmStep2",title="Step 2: Adjust Model Predictors",status="primary",width = NULL,collapsible = TRUE,collapsed=TRUE,
                           "Default values based on historical averages and limits:",
                           conditionalPanel(condition = "input.glmTeamScore == 1",
                                            sliderInput("glmSlideScore", "Team Points Scored",min=0, max=80,value=40)),
                           conditionalPanel(condition = "input.glmTeamTalent == 1",
                                            sliderInput("glmSlideTTalent", "Team Talent Level",min=0, max=1000,value=500)),
                           conditionalPanel(condition = "input.glmOppTalent == 1",
                                            selectizeInput("glmSelectOpp", "Opponent",
                                                           selected = "NC State", choices = levels(as.factor(teams$school))),
                                            sliderInput("glmSlideOTalent", "Opponent Talent Level",min=0, max=1000,value=500)),
                           conditionalPanel(condition = "input.glmLoc == 1",
                                            selectizeInput("glmSelectLoc", "Game Location", selected = "Home", choices = c("Home","Away","Neutral"))),
                           conditionalPanel(condition = "input.glmExcite == 1",
                                            sliderInput("glmSlideExcite", "Game Excitement Level",min=0, max=10,value=5)),
                           conditionalPanel(condition = "input.glmVenue == 1",
                                            selectizeInput("glmSelectVenue", "Venue",
                                                           selected = "Mercedes-Benz Stadium - Atlanta, GA", choices = levels(as.factor(venues$venueUniqueName)))),
                           conditionalPanel(condition = "input.glmCrowd == 1",
                                            sliderInput("glmSlideCrowd", "Crowd Size",min=0, max=100000,value=50000))
                       )
                       
                ), # end column
                column(width = 4,
                       box(id="glmStep3",title="Step 3: Generate Prediction",status="primary",width=NULL,collapsible = TRUE,collapsed=TRUE,
                           "Make a prediction from the selected predictors and the last generated model:",br(),
                           actionButton("glmPredict", "Predict")),
                       box(id="glmReset",title="Reset",status="primary",width=NULL,collapsible = TRUE,collapsed=TRUE,
                           "Return to Step 1 and create a new model:",br(),
                           actionButton("glmReset", "Reset"))
                ) # end column
              ), # end fluidRow
              fluidRow(
                column(width = 12,
                       box(title="Custom Data Set for Model Fit",status="primary",
                           div(style = 'overflow-x: scroll', DT::dataTableOutput("tableGlmModelData")),width = NULL)
                ) # end column
              ), # end fluidRow
              fluidRow(
                column(width = 12,
                       box(title="User Data Set for Prediction",status="primary",
                           div(style = 'overflow-x: scroll', DT::dataTableOutput("tableGlmUserData")),width = NULL)
                ) # end column
              ) # end fluidRow
      ), # END OF TAB Model GM
      
      # MODEL - RF
      tabItem(tabName = "modelRF",
              fluidRow(
                column(width = 3,
                       box(title="Ensemble Model",status="primary",width = NULL,height=293,
                           uiOutput("rfIntro"))
                ), # end column
                column(width = 6,
                       box(title="Team",status="primary",width = 4,height=170,
                           uiOutput("teamLogoProRF")
                       ),
                       box(id="rfBoxLoc",title="Location",status="primary",width = 4,height=170,align = "center",collapsible = TRUE,collapsed = TRUE,
                           uiOutput("locForRF")
                       ),
                       box(id="rfBoxOpp",title="Opponent",status="primary",width = 4,height=170,align = "center",collapsible = TRUE,collapsed = TRUE,
                           uiOutput("oppLogoProRF")
                       ),
                       valueBoxOutput("rfPredictBox"),
                       valueBoxOutput("rfAccBox"),
                       valueBoxOutput("rfNumVar")
                ), # end column
                column(width = 3,
                       box(title="Model Training Process",status="primary",width = NULL,height=293,
                           uiOutput("rfTrainProcess"))
                ), # end column
              ), # end fluidRow
              fluidRow(
                column(width = 4,
                       box(id="rfStep1",title="Step 1: Create Prediction Model",status="primary",width = NULL,collapsible = TRUE,
                           "Select the predictors to use in the model fit:",
                           checkboxInput("rfTeamScore", "Team Points Scored", TRUE),
                           checkboxInput("rfTeamTalent", "Team Talent Level", TRUE),
                           checkboxInput("rfOppTalent", "Opponent Talent Level", TRUE),
                           checkboxInput("rfLoc", "Game Location", TRUE),
                           checkboxInput("rfExcite", "Game Excitement Level", TRUE),
                           checkboxInput("rfVenue", "Venue Details", TRUE),
                           checkboxInput("rfCrowd", "Crowd Size", TRUE),
                           "Create a new prediction model:",br(),
                           actionButton("rfCreate", "Create Model"),
                           uiOutput("rfSelectNotes")
                       )
                ), # end column
                column(width = 4,
                       box(id="rfStep2",title="Step 2: Adjust Model Predictors",status="primary",width = NULL,collapsible = TRUE,collapsed=TRUE,
                           "Default values based on historical averages and limits:",
                           conditionalPanel(condition = "input.rfTeamScore == 1",
                                            sliderInput("rfSlideScore", "Team Points Scored",min=0, max=80,value=40)),
                           conditionalPanel(condition = "input.rfTeamTalent == 1",
                                            sliderInput("rfSlideTTalent", "Team Talent Level",min=0, max=1000,value=500)),
                           conditionalPanel(condition = "input.rfOppTalent == 1",
                                            selectizeInput("rfSelectOpp", "Opponent",
                                                           selected = "NC State", choices = levels(as.factor(teams$school))),
                                            sliderInput("rfSlideOTalent", "Opponent Talent Level",min=0, max=1000,value=500)),
                           conditionalPanel(condition = "input.rfLoc == 1",
                                            selectizeInput("rfSelectLoc", "Game Location", selected = "Home", choices = c("Home","Away","Neutral"))),
                           conditionalPanel(condition = "input.rfExcite == 1",
                                            sliderInput("rfSlideExcite", "Game Excitement Level",min=0, max=10,value=5)),
                           conditionalPanel(condition = "input.rfVenue == 1",
                                            selectizeInput("rfSelectVenue", "Venue",
                                                           selected = "Mercedes-Benz Stadium - Atlanta, GA", choices = levels(as.factor(venues$venueUniqueName)))),
                           conditionalPanel(condition = "input.rfCrowd == 1",
                                            sliderInput("rfSlideCrowd", "Crowd Size",min=0, max=100000,value=50000))
                       )
                       
                ), # end column
                column(width = 4,
                       box(id="rfStep3",title="Step 3: Generate Prediction",status="primary",width=NULL,collapsible = TRUE,collapsed=TRUE,
                           "Make a prediction from the selected predictors and the last generated model:",br(),
                           actionButton("rfPredict", "Predict")),
                       box(id="rfReset",title="Reset",status="primary",width=NULL,collapsible = TRUE,collapsed=TRUE,
                           "Return to Step 1 and create a new model:",br(),
                           actionButton("rfReset", "Reset"))
                ) # end column
              ), # end fluidRow
              fluidRow(
                column(width = 12,
                       box(title="Custom Data Set for Model Fit",status="primary",
                           div(style = 'overflow-x: scroll', DT::dataTableOutput("tableRfModelData")),width = NULL)
                ) # end column
              ), # end fluidRow
              fluidRow(
                column(width = 12,
                       box(title="User Data Set for Prediction",status="primary",
                           div(style = 'overflow-x: scroll', DT::dataTableOutput("tableRfUserData")),width = NULL)
                ) # end column
              ) # end fluidRow
      ) # END OF TAB Model RF
      
    ) # END OF tabItems(
    
  ) # END OF dashboardBody(
  
) # END OF dashboardPage(
