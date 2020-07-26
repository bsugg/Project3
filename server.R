library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(leaflet)
library(ragtop)
library(knitr)
library(class)
library(caret)
library(e1071)
library(randomForest)
library(gbm)
library(shinyjs)
library(V8)

# Call data generated from save function within the "CollegeFootballAPI.R" file
load("collegeFootball.Rdata")

# Utilize shinyjs package for javascript code to collapse boxes on command
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

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
      mutate(teamPointsScored=home_points) %>%
      mutate(won=as.integer(ifelse(home_points>away_points,1,0))) %>%
      mutate(loss=as.integer(ifelse(home_points<away_points,1,0))) %>%
      mutate(tie=as.integer(ifelse(home_points==away_points,1,0))) %>%
      mutate(opponent=away_team) %>%
      mutate(oppConference=as.character(ifelse(!is.na(away_conference),away_conference,"Other"))) %>%
      mutate(oppPointsScored=away_points)
    gamesAway <- games %>% filter(away_team == input$team) %>%
                           filter(season >= input$sliderSeason[1]) %>%
                           filter(season <= input$sliderSeason[2]) %>%
      mutate(home=0) %>%
      mutate(away=1) %>%
      mutate(teamConference=as.character(ifelse(!is.na(away_conference),away_conference,"Other"))) %>%
      mutate(teamPointsScored=away_points) %>%
      mutate(won=as.integer(ifelse(away_points>home_points,1,0))) %>%
      mutate(loss=as.integer(ifelse(away_points<home_points,1,0))) %>%
      mutate(tie=as.integer(ifelse(home_points==away_points,1,0))) %>%
      mutate(opponent=home_team) %>%
      mutate(oppConference=as.character(ifelse(!is.na(home_conference),home_conference,"Other"))) %>%
      mutate(oppPointsScored=home_points)
    # Merge the home and away sets into ONE games data set
    games <- bind_rows(gamesHome,gamesAway)
    # Add a few more attributes and sort by date
    games <- games %>% mutate(outcome=as.factor(ifelse(won==1,"Won",ifelse(loss==1,"Loss","Tie")))) %>%
                       mutate(team=input$team) %>%
                       mutate(score=paste0(teamPointsScored,"-",oppPointsScored)) %>%
                       mutate(margin=teamPointsScored-oppPointsScored) %>%
                       mutate(location=ifelse(neutral_site,"Neutral",ifelse(home==1,"Home","Away"))) %>%
                       mutate(schedule=ifelse(teamConference=="Other" & oppConference=="Other",NA,
                                             ifelse(teamConference==oppConference,"Conference","NonConference"))
                       ) %>%
                       rename("excitementIndex"=excitement_index)
    games <- arrange(games,kickoffDate)
    # Join with talent data sets
    games <- left_join(games,talentTeam,by=c("season","team"))
    games <- left_join(games,talentOpp,by=c("season","opponent"))
    # Join with venues data set to enrich with location details
    gamesVenuesJoin <- left_join(games,venues,by="venueId")
    gamesVenuesJoin <- gamesVenuesJoin %>% mutate(venueCityState=ifelse(!is.na(venueState),paste0(venueCity,", ",venueState),paste0(venueCity)))
    
    # Filter data based on manual selections of season type and schedule from user
    if (input$selectSeasonType!="All") {
      
      gamesVenuesJoin <- filter(gamesVenuesJoin,seasonType == input$selectSeasonType)
    } else {return(gamesVenuesJoin)}
    if (input$selectSchedule!="All") {
      gamesVenuesJoin <- filter(gamesVenuesJoin,schedule == input$selectSchedule)
    } else {return(gamesVenuesJoin)}
  })
  
  #####
  ##### MODELING
  #####
  
  ########## GLM ##########
  
  # TALENT
  
  glmModelTeamTalent <- reactive({
    # Filter on user selected team
    gamesHome <- talentTeam %>% filter(team == input$team) %>%
      filter(season >= input$sliderSeason[1]) %>%
      filter(season <= input$sliderSeason[2])
  })
  
  glmModelOppTalent <- reactive({
    # Filter on user selected opponent for modeling
    gamesHome <- talentTeam %>% filter(team == input$glmSelectOpp) %>%
      filter(season >= input$sliderSeason[1]) %>%
      filter(season <= input$sliderSeason[2])
  })
  
  glmOppTeam <- reactive({
    # Filter on user selected opponent for logo
    teamsOppLogo <- teams %>% filter(school == input$glmSelectOpp)
  })
  
  # VENUE
  
  glmModelVenue <- reactive({
    # Filter on user selected venue
    newVenue <- venues %>% filter(venueUniqueName == input$glmSelectVenue)
  })
  
  # MODEL DATA SET
  
  glmModelData <- reactive({
      getGames <- newGames()
      getGames <- getGames %>% select(won,teamPointsScored,teamTalent,oppTalent,location,excitementIndex,
                                      venueElevation,venueGrass,venueDome,venueLat,venueLong,attendance)
      # Transform
      getGames <- getGames %>% mutate("locHome"=as.integer(ifelse(location=="Home",1,0))) %>%
        mutate("locAway"=as.integer(ifelse(location=="Away",1,0))) %>%
        mutate("locNeutral"=as.integer(ifelse(location=="Neutral",1,0))) %>%
        select(-location)
      getGames$won <- as.factor(getGames$won)
      # Dynamic variable selection based on user input
      if(input$glmTeamScore) {getGames} else{getGames <- select(getGames,-teamPointsScored)}
      if(input$glmTeamTalent) {getGames} else{getGames <- select(getGames,-teamTalent)}
      if(input$glmOppTalent) {getGames} else{getGames <- select(getGames,-oppTalent)}
      if(input$glmLoc) {getGames} else{getGames <- getGames %>% select(-starts_with("loc"))}
      if(input$glmExcite) {getGames} else{getGames <- select(getGames,-excitementIndex)}
      if(input$glmVenue) {getGames} else{getGames <- getGames %>% select(-starts_with("venue"))}
      if(input$glmCrowd) {getGames} else{getGames <- select(getGames,-attendance)}
      # Remove records with NA values
      getGames <- as.data.frame(na.omit(getGames))
      print(str(getGames))
      ## Data slicing for model fit later on
      # Set seed for reproducible results
      set.seed(1)
      # Create the training and test sets
      train <- sample(1:nrow(getGames),size=nrow(getGames)*0.7)
      test <- dplyr::setdiff(1:nrow(getGames),train)
      gamesTrain <- getGames[train, ]
      gamesTrain <- gamesTrain %>% mutate("modelFitSet"="Train")
      gamesTest <- getGames[test, ]
      gamesTest <- gamesTest %>% mutate("modelFitSet"="Test")
      # Merge back together, with new modelFitSet column
      getGames <- bind_rows(gamesTrain,gamesTest)
    })
  
  # CREATE USER PREDICTORS DATA SET
  
  glmUserData <- reactive({
  userVenue <- glmModelVenue()
  glmUserData <- data.frame("won"=factor("0",levels = c("0", "1")),"teamPointsScored"=as.numeric(0),
                            "teamTalent"=as.numeric(0),"oppTalent"=as.numeric(0),"excitementIndex"=as.numeric(0),
                            "venueElevation"=as.numeric(0),"venueGrass"=as.logical(FALSE),"venueDome"=as.logical(FALSE),
                            "venueLat"=as.numeric(0),"venueLong"=as.numeric(0),"attendance"=as.integer(0),
                            "locHome"=as.integer(0),"locAway"=as.integer(0),"locNeutral"=as.integer(0))
  # UPDATE with USER PREDICOTRS
  if(input$glmTeamScore) {glmUserData$teamPointsScored <- input$glmSlideScore} else{glmUserData <- select(glmUserData,-teamPointsScored)}
  if(input$glmTeamTalent) {glmUserData$teamTalent <- as.numeric(input$glmSlideTTalent)} else{glmUserData <- select(glmUserData,-teamTalent)}
  if(input$glmOppTalent) {glmUserData$oppTalent <- as.numeric(input$glmSlideOTalent)} else{glmUserData <- select(glmUserData,-oppTalent)}
  if(input$glmLoc) {
    if(input$glmSelectLoc=="Home") {glmUserData$locHome <- as.integer(1)
    } else { if(input$glmSelectLoc=="Away") {glmUserData$locAway <- as.integer(1)
    } else {glmUserData$locNeutral <- as.integer(1)}}
  } else{glmUserData <- glmUserData %>% select(-starts_with("loc"))}
  if(input$glmExcite) {glmUserData$excitementIndex <- as.numeric(input$glmSlideExcite)} else{glmUserData <- select(glmUserData,-excitementIndex)}
  if(input$glmVenue) {
    glmUserData$venueElevation <- userVenue$venueElevation
    glmUserData$venueGrass <- userVenue$venueGrass
    glmUserData$venueDome <- userVenue$venueDome
    glmUserData$venueLat <- userVenue$venueLat
    glmUserData$venueLong <- userVenue$venueLong
  } else{glmUserData <- glmUserData %>% select(-starts_with("venue"))}
  if(input$glmCrowd) {glmUserData$attendance <- input$glmSlideCrowd} else{glmUserData <- select(glmUserData,-attendance)}
  return(glmUserData)
  })
  
  # MODEL FITTING AND USER PREDICTOR CREATION
  
  glmReValue <- reactiveValues()

  # GLM Create Button Actions
  
  glmCreateModel <- observeEvent(input$glmCreate,{
    gamesTrain <- glmModelData()
    userVenue <- glmModelVenue()
    glmUserData <- glmUserData()
    gamesTrain <- gamesTrain %>% filter(modelFitSet=="Train") %>%
      select(-modelFitSet)
    gamesTest <- glmModelData()
    gamesTest <- gamesTest %>% filter(modelFitSet=="Test") %>%
      select(-modelFitSet)
    # Add the test and train data sets to the reactive value
    glmReValue$train <- gamesTrain
    glmReValue$test <- gamesTest
    # 1. Use trainControl() function to control computations and set number of desired folds for cross validation
    trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 3)
    # 2. Set a seed for reproducible result
    set.seed(3333)
    # 3. Use train() function to determine a generalized linear regression model of best fit
    logReg_fit <- train(won ~ ., data = gamesTrain, method = "glm", family="binomial", trControl=trctrl, preProcess = c("center", "scale"), tuneLength = 10)
    # 4. Test newly created model against the test set and generate confusion matric for performance metrics
    testPredGLM <- predict(logReg_fit, newdata = gamesTest)
    conMatrixGLM <- confusionMatrix(testPredGLM,gamesTest$won)
    userPredGLM <- predict(logReg_fit, newdata = glmUserData)
    # Add the model and confusion matrix test accuracy figure to the reactive value
    glmReValue$model <- logReg_fit
    glmReValue$accuracy <- conMatrixGLM[[3]][[1]]
    print("Model created and values stored.")
    js$collapse("glmStep1")
    js$collapse("glmStep2")
    js$collapse("glmStep3")
  })

  # GLM Predict Button Actions
  
  glmMakePrediction <- observeEvent(input$glmPredict,{
    userPredGLM <- predict(glmReValue$model, newdata = glmUserData())
    print(userPredGLM)
  })
  
  # GLM Reset Button Actions
  
  glmMakePrediction <- observeEvent(input$glmReset,{
    js$collapse("glmStep1")
    js$collapse("glmStep2")
    js$collapse("glmStep3")
    # Reset all inputs from Step 1
    reset("glmTeamScore")
    reset("glmTeamTalent")
    reset("glmOppTalent")
    reset("glmLoc")
    reset("glmExcite")
    reset("glmVenue")
    reset("glmCrowd")
    reset("glmSelectOpp")
    reset("glmSelectLoc")
    reset("glmSelectVenue")
    # TEAM SCORE
    
    # Team score selection, set average based on historical 
    observe({
      getGamesPro <- newGames()
      avgPointValue <- as.integer(mean(getGamesPro$teamPointsScored))
      updateSliderInput(session,"glmSlideScore",value = avgPointValue)
    })
    
    # TALENT
    
    # Team talent slider, set average value from previous seasons
    observe({
      getTeamTalent <- glmModelTeamTalent()
      avgTeamTalent <- as.integer(mean(getTeamTalent$teamTalent))
      updateSliderInput(session,"glmSlideTTalent",value = avgTeamTalent)
    })
    # Opponent talent slider, set average value from previous seasons
    observe({
      getOppTalent <- glmModelOppTalent()
      avgOppTalent <- as.integer(mean(getOppTalent$teamTalent))
      updateSliderInput(session,"glmSlideOTalent",value = avgOppTalent)
    })
    
    # EXCITEMENT INDEX
    
    # Excitement index selection, set average based on historical 
    observe({
      getGamesPro <- newGames()
      exciteValue <- as.integer(mean(getGamesPro$excitementIndex))
      updateSliderInput(session,"glmSlideExcite",value = exciteValue)
    })
    
    # VENUE
    
    # Venue selection, set max of crowd slider to venue capacity
    observe({
      getVenue <- glmModelVenue()
      if (input$glmVenue == 1) {
        capacity <- as.integer(getVenue$venueCapacity)
        updateSliderInput(session,"glmSlideCrowd",value = capacity, max = capacity)
      } else {
        updateSliderInput(session,"glmSlideCrowd",value = 50000, max = 100000)
      }
    })
    
    
  })
  
  ########## RF ##########
  
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
    tags$img(src=newTeams()$logos[1], width=150,style="display: block; margin-left: auto; margin-right: auto;")
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
  
  # Team logo image
  output$teamLogoPro <- renderUI({
    tags$img(src=newTeams()$logos[1], width=200,style="display: block; margin-left: auto; margin-right: auto;")
  })
  
  # Set team name and mascot title
  output$teamTitle1 <- renderText({
    getTeams <- newTeams()
    teamName <- paste(getTeams$school, sep = " ")
  })
  output$teamTitle2 <- renderText({
    getTeams <- newTeams()
    teamName <- paste(getTeams$mascot, sep = " ")
  })
  
  # Team introduction
  output$teamText <- renderText({
    getGames <- newGames()
    getTeams <- newTeams()
    if (sum(getGames$location == "Home")>0) {
      homeStatus <- paste0("")
    } else{
      homeStatus <- paste0("")
    }
    paste(homeStatus, sep = " ")
  })
  
  output$seaBox <- renderValueBox({
    getGames <- newGames()
    valueBox(
      length(unique(as.factor(getGames$season))), "Seasons", icon = icon("calendar-alt"),
      color="light-blue"
    )
  })
  output$gameBox <- renderValueBox({
    getGames <- newGames()
    valueBox(
      nrow(getGames), "Total Games", icon = icon("football-ball"),
      color="light-blue"
    )
  })
  output$postBox <- renderValueBox({
    getGames <- newGames()
    getGamesPost <- filter(getGames,seasonType=="Postseason")
    valueBox(
      sum(getGamesPost$outcome == "Won"), "Postseason Wins", icon = icon("trophy"),
      color="yellow"
    )
  })
  output$winBox <- renderValueBox({
    getGames <- newGames()
    valueBox(
      sum(getGames$outcome == "Won"), "Wins", icon = icon("check-circle"),
      color="green"
    )
  })
  output$lossBox <- renderValueBox({
    getGames <- newGames()
    valueBox(
      sum(getGames$outcome == "Loss"), "Losses", icon = icon("times-circle"),
      color="red"
    )
  })
  output$winPctBox <- renderValueBox({
    getGames <- newGames()
    valueBox(
      paste0(round(100*(sum(getGames$outcome == "Won")/nrow(getGames)),1)," %"), "Win Percentage", icon = icon("splotch"),
      color="light-blue"
    )
  })
  
  # Create plot
  output$allWinPlot <- renderPlot({
    getGames <- newGames()
    # Conservation color option
    # conColor <- ifelse(input$conservation,newData$conservation,"color:black;")
    # Scatter plot creation
    clustSkate <- ggplot(data=getGames,aes(x=as.factor(season)))
    clustSkate + geom_bar(position="dodge",aes(fill=outcome)) +
      labs(x="Season",y="Games",title="Game Outcome by Season") +
      scale_fill_discrete(name="") +
      theme_bw()
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
                 # Original green color before matching ggplot was #228B22
                 color = ifelse(getGamesPro$outcome=="Won","#00BFC4","#FF0000"),
                 weight = 10,
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
  
  ###
  ### MODELING
  ###
  
  ########## GLM ##########
  
  # Team logo image
  output$teamLogoProGLM <- renderUI({
    tags$img(src=newTeams()$logos[1], width=100,style="display: block; margin-left: auto; margin-right: auto;")
  })
  
  # Location status
  output$locForGLM <- renderUI({
    if (input$glmLoc) {
        tags$div(
          HTML(paste(tags$h3(input$glmSelectLoc),tags$h4(tags$i("vs")))
          )
        )
    } else{
      tags$div(
        HTML(paste(tags$h3("Anywhere"),tags$h4("vs"))
        )
      )
    }
  })
  
  # Opponent logo image
  output$oppLogoProGLM <- renderUI({
    if (input$glmOppTalent) {
      tags$img(src=glmOppTeam()$logos[1], width=100,style="display: block; margin-left: auto; margin-right: auto;")
    } else{tags$h3("Anyone")}
  })
  
  # TEAM SCORE
  
  # Team score selection, set average based on historical 
  observe({
    getGamesPro <- newGames()
    avgPointValue <- as.integer(mean(getGamesPro$teamPointsScored))
    updateSliderInput(session,"glmSlideScore",value = avgPointValue)
  })
  
  # TALENT
  
  # Team talent slider, set average value from previous seasons
  observe({
    getTeamTalent <- glmModelTeamTalent()
    avgTeamTalent <- as.integer(mean(getTeamTalent$teamTalent))
    updateSliderInput(session,"glmSlideTTalent",value = avgTeamTalent)
  })
  # Opponent talent slider, set average value from previous seasons
  observe({
    getOppTalent <- glmModelOppTalent()
    avgOppTalent <- as.integer(mean(getOppTalent$teamTalent))
    updateSliderInput(session,"glmSlideOTalent",value = avgOppTalent)
  })
  
  # EXCITEMENT INDEX
  
  # Excitement index selection, set average based on historical 
  observe({
    getGamesPro <- newGames()
    exciteValue <- as.integer(mean(getGamesPro$excitementIndex))
    updateSliderInput(session,"glmSlideExcite",value = exciteValue)
  })
  
  # VENUE
  
  # Venue selection, set max of crowd slider to venue capacity
  observe({
    getVenue <- glmModelVenue()
    if (input$glmVenue == 1) {
      capacity <- as.integer(getVenue$venueCapacity)
      updateSliderInput(session,"glmSlideCrowd",value = capacity, max = capacity)
    } else {
      updateSliderInput(session,"glmSlideCrowd",value = 50000, max = 100000)
    }
  })
  
  # THE MODEL
  
  output$tableGlmModelData <- DT::renderDataTable({
    getGamesPro <- newGames()
    getTeams <- newTeams()
    customPrintName <- paste0(getTeams$abbreviation," Model Fit Data Set")
    customFileName <- paste0(getTeams$abbreviation,"modelFitDataSet")
    DT::datatable(glmModelData(),extensions = 'Buttons',
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
  
  output$tableGlmUserData <- DT::renderDataTable({
    getGamesPro <- newGames()
    getTeams <- newTeams()
    newGlmUserData <- glmUserData()
    newGlmUserData <- newGlmUserData %>% select(-won)
    customPrintName <- paste0(getTeams$abbreviation," User Predictors Data Set")
    customFileName <- paste0(getTeams$abbreviation,"userPredictorsDataSet")
    DT::datatable(newGlmUserData,extensions = 'Buttons',
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
  
  ########## RF ##########
  
}
