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
library(mathjaxr)
library(plotly)
library(wordcloud2)
library(tm)
library(data.table)
library(autoplotly)

# Call data generated from save function within the "CollegeFootballAPI.R" file
load("collegeFootball.RData")

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
      mutate(schedule=ifelse(conference_game,"Conference","NonConference")) %>%
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
    } else {gamesVenuesJoin}
    if (input$selectSchedule!="All") {
      gamesVenuesJoin <- filter(gamesVenuesJoin,schedule == input$selectSchedule)
    } else {gamesVenuesJoin}
  })
  
  # Create a joined set with 2 records per game played ... 1 as team and 1 as opponent
  allGamesDouble <- reactive({
    # Filter on user selected team and parse "games" data set for home and away
    allHome <- games %>%
      filter(season >= input$sliderSeason[1]) %>%
      filter(season <= input$sliderSeason[2]) %>%
      mutate(home=1) %>%
      mutate(away=0) %>%
      mutate(teamConference=as.character(ifelse(!is.na(home_conference),home_conference,"Other"))) %>%
      mutate(teamPointsScored=home_points) %>%
      mutate(won=as.integer(ifelse(home_points>away_points,1,0))) %>%
      mutate(loss=as.integer(ifelse(home_points<away_points,1,0))) %>%
      mutate(tie=as.integer(ifelse(home_points==away_points,1,0))) %>%
      mutate(team=home_team) %>%
      mutate(opponent=away_team) %>%
      mutate(oppConference=as.character(ifelse(!is.na(away_conference),away_conference,"Other"))) %>%
      mutate(oppPointsScored=away_points)
    allAway <- games %>% 
      filter(season >= input$sliderSeason[1]) %>%
      filter(season <= input$sliderSeason[2]) %>%
      mutate(home=0) %>%
      mutate(away=1) %>%
      mutate(teamConference=as.character(ifelse(!is.na(away_conference),away_conference,"Other"))) %>%
      mutate(teamPointsScored=away_points) %>%
      mutate(won=as.integer(ifelse(away_points>home_points,1,0))) %>%
      mutate(loss=as.integer(ifelse(away_points<home_points,1,0))) %>%
      mutate(tie=as.integer(ifelse(home_points==away_points,1,0))) %>%
      mutate(team=away_team) %>%
      mutate(opponent=home_team) %>%
      mutate(oppConference=as.character(ifelse(!is.na(home_conference),home_conference,"Other"))) %>%
      mutate(oppPointsScored=home_points)
    # Merge the home and away sets into ONE games data set
    games <- bind_rows(allHome,allAway)
    # Add a few more attributes and sort by date
    games <- games %>% mutate(outcome=as.factor(ifelse(won==1,"Won",ifelse(loss==1,"Loss","Tie")))) %>%
      mutate(score=paste0(teamPointsScored,"-",oppPointsScored)) %>%
      mutate(margin=teamPointsScored-oppPointsScored) %>%
      mutate(location=ifelse(neutral_site,"Neutral",ifelse(home==1,"Home","Away"))) %>%
      mutate(schedule=ifelse(conference_game,"Conference","NonConference")) %>%
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
    } else {gamesVenuesJoin}
    if (input$selectSchedule!="All") {
      gamesVenuesJoin <- filter(gamesVenuesJoin,schedule == input$selectSchedule)
    } else {gamesVenuesJoin}
  })
  
  #####
  ##### GAME STATS
  #####
  
  gameStatsNew <- reactive({
    gameStats <- gameStats %>% filter(school == input$team)
  })
  
  #####
  ##### UNSUPERVISED
  #####
  
  unsuperGames <- reactive({
    getGames <- newGames()
    getGames <- getGames %>% select(won,teamPointsScored,teamTalent,oppTalent,location,excitementIndex,
                                    venueElevation,venueGrass,venueDome,attendance)
    # Transform
    getGames <- getGames %>% mutate("locHome"=as.integer(ifelse(location=="Home",1,0))) %>%
      mutate("locAway"=as.integer(ifelse(location=="Away",1,0))) %>%
      mutate("locNeutral"=as.integer(ifelse(location=="Neutral",1,0))) %>%
      select(-location) %>%
      rename("outcome"=won)
      getGames$outcome <- factor(ifelse(getGames$outcome==1,"Won","Loss"))
    # Dynamic variable selection based on user input
    if(input$unsuperTeamScore) {getGames} else{getGames <- select(getGames,-teamPointsScored)}
    if(input$unsuperTeamTalent) {getGames} else{getGames <- select(getGames,-teamTalent)}
    if(input$unsuperOppTalent) {getGames} else{getGames <- select(getGames,-oppTalent)}
    if(input$unsuperLoc) {getGames} else{getGames <- getGames %>% select(-starts_with("loc"))}
    if(input$unsuperExcite) {getGames} else{getGames <- select(getGames,-excitementIndex)}
    if(input$unsuperVenue) {getGames} else{getGames <- getGames %>% select(-starts_with("venue"))}
    if(input$unsuperCrowd) {getGames} else{getGames <- select(getGames,-attendance)}
    # Remove records with NA values
    getGames <- as.data.frame(na.omit(getGames))
  })
  
  #####
  ##### MODELING
  #####
  
  #########################
  ########## GLM ##########
  #########################
  
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
    # Launch progress bar
    withProgress(message = 'Fitting model...', value = 0,{ 
      gamesTrain <- glmModelData()
      userVenue <- glmModelVenue()
      glmUserData <- glmUserData()
      incProgress(1/3, detail = "Splitting data...")
      gamesTrain <- gamesTrain %>% filter(modelFitSet=="Train") %>%
        select(-modelFitSet)
      gamesTest <- glmModelData()
      gamesTest <- gamesTest %>% filter(modelFitSet=="Test") %>%
        select(-modelFitSet)
      # Add the test and train data sets to the reactive value
      glmReValue$train <- gamesTrain
      glmReValue$test <- gamesTest
      # 1. Use trainControl() function to control computations and set number of desired folds for cross validation
      trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
      # 2. Set a seed for reproducible result
      set.seed(3333)
      incProgress(2/3, detail = "Training model for best results, please be patient...")
      # 3. Use train() function to determine a generalized linear regression model of best fit
      logReg_fit <- train(won ~ ., data = gamesTrain, method = "glm", family="binomial", trControl=trctrl, preProcess = c("center", "scale"), tuneLength = 10)
      incProgress(3/3, detail = "Testing model...")
      # 4. Test newly created model against the test set and generate confusion matric for performance metrics
      testPredGLM <- predict(logReg_fit, newdata = gamesTest)
      conMatrixGLM <- confusionMatrix(testPredGLM,gamesTest$won)
      userPredGLM <- predict(logReg_fit, newdata = glmUserData)
      # Add the model and confusion matrix test accuracy figure to the reactive value
      glmReValue$model <- logReg_fit
      glmReValue$finalModel <- logReg_fit$finalModel
      glmReValue$numVar <- as.character(length(logReg_fit[[22]]))
      glmReValue$accuracy <- conMatrixGLM[[3]][[1]]
      # Finish progress bar
    })
    
    output$glmAccBox <- renderValueBox({
      acc <- round(glmReValue$accuracy*100,1)
      if (glmReValue$accuracy<.5) {
        valueBox(
          acc, "Accuracy", icon = icon("percent"),
          color="red")
      } else{ if (glmReValue$accuracy<.75) {
        valueBox(
          acc, "Accuracy", icon = icon("percent"),
          color="yellow")
      } else{
        valueBox(
          acc, "Accuracy", icon = icon("percent"),
          color="green")}
      }
    })
    
    output$glmNumVar <- renderValueBox({
      valueBox(
        glmReValue$numVar, "Predictors", icon = icon("hashtag"),
        color="light-blue")
    })
    
    js$collapse("glmStep1")
    js$collapse("glmStep2")
    js$collapse("glmStep3")
    js$collapse("glmReset")
    js$collapse("glmBoxLoc")
    js$collapse("glmBoxOpp")
  })
  
  # GLM Predict Button Actions
  
  glmMakePrediction <- observeEvent(input$glmPredict,{
    userPredGLM <- predict(glmReValue$model, newdata = glmUserData())
    glmReValue$userPredict <- userPredGLM
    
    # Value box values
    output$glmPredictBox <- renderValueBox({
      outcome <- ifelse(glmReValue$userPredict==1,"Win","Lose")
      if (outcome=="Win") {
        valueBox(
          outcome, "Prediction", icon = icon("check-circle"),
          color="green")
      } else{
        valueBox(
          outcome, "Prediction", icon = icon("times-circle"),
          color="red")}
    })
  })
  
  # GLM Reset Button Actions
  
  glmMakeReset <- observeEvent(input$glmReset,{
    js$collapse("glmStep1")
    js$collapse("glmStep2")
    js$collapse("glmStep3")
    js$collapse("glmReset")
    js$collapse("glmBoxLoc")
    js$collapse("glmBoxOpp")
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
    # Reset value boxes
    output$glmPredictBox <- renderValueBox({
      valueBox(
        "","",color="light-blue"
      )
    })
    output$glmAccBox <- renderValueBox({
      valueBox(
        "","",color="light-blue"
      )
    })
    output$glmNumVar <- renderValueBox({
      valueBox(
        "","",color="light-blue"
      )
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
  })
  
  #########################
  ########## RF ###########
  #########################
  
  # TALENT
  
  rfModelTeamTalent <- reactive({
    # Filter on user selected team
    gamesHome <- talentTeam %>% filter(team == input$team) %>%
      filter(season >= input$sliderSeason[1]) %>%
      filter(season <= input$sliderSeason[2])
  })
  
  rfModelOppTalent <- reactive({
    # Filter on user selected opponent for modeling
    gamesHome <- talentTeam %>% filter(team == input$rfSelectOpp) %>%
      filter(season >= input$sliderSeason[1]) %>%
      filter(season <= input$sliderSeason[2])
  })
  
  rfOppTeam <- reactive({
    # Filter on user selected opponent for logo
    teamsOppLogo <- teams %>% filter(school == input$rfSelectOpp)
  })
  
  # VENUE
  
  rfModelVenue <- reactive({
    # Filter on user selected venue
    newVenue <- venues %>% filter(venueUniqueName == input$rfSelectVenue)
  })
  
  # MODEL DATA SET
  
  rfModelData <- reactive({
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
    if(input$rfTeamScore) {getGames} else{getGames <- select(getGames,-teamPointsScored)}
    if(input$rfTeamTalent) {getGames} else{getGames <- select(getGames,-teamTalent)}
    if(input$rfOppTalent) {getGames} else{getGames <- select(getGames,-oppTalent)}
    if(input$rfLoc) {getGames} else{getGames <- getGames %>% select(-starts_with("loc"))}
    if(input$rfExcite) {getGames} else{getGames <- select(getGames,-excitementIndex)}
    if(input$rfVenue) {getGames} else{getGames <- getGames %>% select(-starts_with("venue"))}
    if(input$rfCrowd) {getGames} else{getGames <- select(getGames,-attendance)}
    # Remove records with NA values
    getGames <- as.data.frame(na.omit(getGames))
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
  
  rfUserData <- reactive({
    userVenue <- rfModelVenue()
    rfUserData <- data.frame("won"=factor("0",levels = c("0", "1")),"teamPointsScored"=as.numeric(0),
                             "teamTalent"=as.numeric(0),"oppTalent"=as.numeric(0),"excitementIndex"=as.numeric(0),
                             "venueElevation"=as.numeric(0),"venueGrass"=as.logical(FALSE),"venueDome"=as.logical(FALSE),
                             "venueLat"=as.numeric(0),"venueLong"=as.numeric(0),"attendance"=as.integer(0),
                             "locHome"=as.integer(0),"locAway"=as.integer(0),"locNeutral"=as.integer(0))
    # UPDATE with USER PREDICOTRS
    if(input$rfTeamScore) {rfUserData$teamPointsScored <- input$rfSlideScore} else{rfUserData <- select(rfUserData,-teamPointsScored)}
    if(input$rfTeamTalent) {rfUserData$teamTalent <- as.numeric(input$rfSlideTTalent)} else{rfUserData <- select(rfUserData,-teamTalent)}
    if(input$rfOppTalent) {rfUserData$oppTalent <- as.numeric(input$rfSlideOTalent)} else{rfUserData <- select(rfUserData,-oppTalent)}
    if(input$rfLoc) {
      if(input$rfSelectLoc=="Home") {rfUserData$locHome <- as.integer(1)
      } else { if(input$rfSelectLoc=="Away") {rfUserData$locAway <- as.integer(1)
      } else {rfUserData$locNeutral <- as.integer(1)}}
    } else{rfUserData <- rfUserData %>% select(-starts_with("loc"))}
    if(input$rfExcite) {rfUserData$excitementIndex <- as.numeric(input$rfSlideExcite)} else{rfUserData <- select(rfUserData,-excitementIndex)}
    if(input$rfVenue) {
      rfUserData$venueElevation <- userVenue$venueElevation
      rfUserData$venueGrass <- userVenue$venueGrass
      rfUserData$venueDome <- userVenue$venueDome
      rfUserData$venueLat <- userVenue$venueLat
      rfUserData$venueLong <- userVenue$venueLong
    } else{rfUserData <- rfUserData %>% select(-starts_with("venue"))}
    if(input$rfCrowd) {rfUserData$attendance <- input$rfSlideCrowd} else{rfUserData <- select(rfUserData,-attendance)}
    return(rfUserData)
  })
  
  # MODEL FITTING AND USER PREDICTOR CREATION
  
  rfReValue <- reactiveValues()
  
  # RF Create Button Actions
  
  rfCreateModel <- observeEvent(input$rfCreate,{
    # Launch progress bar
    withProgress(message = 'Fitting model...', value = 0,{ 
      gamesTrain <- rfModelData()
      userVenue <- rfModelVenue()
      rfUserData <- rfUserData()
      incProgress(1/3, detail = "Splitting data...")
      gamesTrain <- gamesTrain %>% filter(modelFitSet=="Train") %>%
        select(-modelFitSet)
      gamesTest <- rfModelData()
      gamesTest <- gamesTest %>% filter(modelFitSet=="Test") %>%
        select(-modelFitSet)
      # Add the test and train data sets to the reactive value
      rfReValue$train <- gamesTrain
      rfReValue$test <- gamesTest
      # 1. Use trainControl() function to control computations and set number of desired folds for cross validation
      trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 8)
      # 2. Set a seed for reproducible result
      set.seed(3333)
      incProgress(2/3, detail = "Training model for best results. Growing trees takes time, please be patient...")
      # 3. Use train() function to determine a generalized linear regression model of best fit
      randFor_fit <- train(won ~ ., data = gamesTrain, method = "rf", trControl=trctrl, preProcess = c("center", "scale"), tuneLength = 10)
      incProgress(3/3, detail = "Testing model...")
      # 4. Test newly created model against the test set and generate confusion matric for performance metrics
      testPredRF <- predict(randFor_fit, newdata = gamesTest)
      conMatrixRF <- confusionMatrix(testPredRF,gamesTest$won)
      userPredRF <- predict(randFor_fit, newdata = rfUserData)
      # Add the model and confusion matrix test accuracy figure to the reactive value
      rfReValue$model <- randFor_fit
      rfReValue$finalModel <- randFor_fit$finalModel
      rfReValue$numVar <- as.character(length(randFor_fit[[22]]))
      rfReValue$accuracy <- conMatrixRF[[3]][[1]]
      # Finish progress bar
    })
    
    output$rfAccBox <- renderValueBox({
      acc <- round(rfReValue$accuracy*100,1)
      if (rfReValue$accuracy<.5) {
        valueBox(
          acc, "Accuracy", icon = icon("percent"),
          color="red")
      } else{ if (rfReValue$accuracy<.75) {
        valueBox(
          acc, "Accuracy", icon = icon("percent"),
          color="yellow")
      } else{
        valueBox(
          acc, "Accuracy", icon = icon("percent"),
          color="green")}
      }
    })
    
    output$rfNumVar <- renderValueBox({
      valueBox(
        rfReValue$numVar, "Predictors", icon = icon("hashtag"),
        color="light-blue")
    })
    
    js$collapse("rfStep1")
    js$collapse("rfStep2")
    js$collapse("rfStep3")
    js$collapse("rfReset")
    js$collapse("rfBoxLoc")
    js$collapse("rfBoxOpp")
  })
  
  # RF Predict Button Actions
  
  rfMakePrediction <- observeEvent(input$rfPredict,{
    userPredRF <- predict(rfReValue$model, newdata = rfUserData())
    rfReValue$userPredict <- userPredRF
    
    # Value box values
    output$rfPredictBox <- renderValueBox({
      outcome <- ifelse(rfReValue$userPredict==1,"Win","Lose")
      if (outcome=="Win") {
        valueBox(
          outcome, "Prediction", icon = icon("check-circle"),
          color="green")
      } else{
        valueBox(
          outcome, "Prediction", icon = icon("times-circle"),
          color="red")}
    })
  })
  
  # RF Reset Button Actions
  
  rfMakeReset <- observeEvent(input$rfReset,{
    js$collapse("rfStep1")
    js$collapse("rfStep2")
    js$collapse("rfStep3")
    js$collapse("rfReset")
    js$collapse("rfBoxLoc")
    js$collapse("rfBoxOpp")
    # Reset all inputs from Step 1
    reset("rfTeamScore")
    reset("rfTeamTalent")
    reset("rfOppTalent")
    reset("rfLoc")
    reset("rfExcite")
    reset("rfVenue")
    reset("rfCrowd")
    reset("rfSelectOpp")
    reset("rfSelectLoc")
    reset("rfSelectVenue")
    # Reset value boxes
    output$rfPredictBox <- renderValueBox({
      valueBox(
        "","",color="light-blue"
      )
    })
    output$rfAccBox <- renderValueBox({
      valueBox(
        "","",color="light-blue"
      )
    })
    output$rfNumVar <- renderValueBox({
      valueBox(
        "","",color="light-blue"
      )
    })
    # TEAM SCORE
    
    # Team score selection, set average based on historical 
    observe({
      getGamesPro <- newGames()
      avgPointValue <- as.integer(mean(getGamesPro$teamPointsScored))
      updateSliderInput(session,"rfSlideScore",value = avgPointValue)
    })
    
    # TALENT
    
    # Team talent slider, set average value from previous seasons
    observe({
      getTeamTalent <- rfModelTeamTalent()
      avgTeamTalent <- as.integer(mean(getTeamTalent$teamTalent))
      updateSliderInput(session,"rfSlideTTalent",value = avgTeamTalent)
    })
    # Opponent talent slider, set average value from previous seasons
    observe({
      getOppTalent <- rfModelOppTalent()
      avgOppTalent <- as.integer(mean(getOppTalent$teamTalent))
      updateSliderInput(session,"rfSlideOTalent",value = avgOppTalent)
    })
    
    # EXCITEMENT INDEX
    
    # Excitement index selection, set average based on historical 
    observe({
      getGamesPro <- newGames()
      exciteValue <- as.integer(mean(getGamesPro$excitementIndex))
      updateSliderInput(session,"rfSlideExcite",value = exciteValue)
    })
    
    # VENUE
    
    # Venue selection, set max of crowd slider to venue capacity
    observe({
      getVenue <- rfModelVenue()
      if (input$rfVenue == 1) {
        capacity <- as.integer(getVenue$venueCapacity)
        updateSliderInput(session,"rfSlideCrowd",value = capacity, max = capacity)
      } else {
        updateSliderInput(session,"rfSlideCrowd",value = 50000, max = 100000)
      }
    })
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
  
  # Season slider, set min and max properties based on available data
  observe({
    getSeasons <- seasonFinder()
    minSeason <- min(getSeasons$season)
    maxSeason <- max(getSeasons$season)
    updateSliderInput(session,"sliderSeason",min=minSeason,max=maxSeason,value = c(minSeason, maxSeason))
  })
  
  # Game type selections, reset defaults when "auto"
  observe({
    if (input$radioGameSelect == 'auto') {
      reset("selectSeasonType")
      reset("selectSchedule")
    } else{}
  })
  
  #####
  ##### INFORMATION
  #####
  
  # WORD CLOUDS
  
  # Word cloud of wins by teams
  output$infoCloudTeams <- renderWordcloud2({
    withProgress(message = 'Generating word cloud...', value = 0,{
      getGamesDouble <- allGamesDouble()
      getGamesDouble <- getGamesDouble %>% filter(won==1) %>% select(team,won,outcome)
      freqTab <- as.data.frame(table(getGamesDouble$outcome,getGamesDouble$team))
      freqTab <- tibble::rownames_to_column(freqTab) %>% filter(Var1=="Won") %>% select(Var2,Freq) %>% rename("word"=Var2,"freq"=Freq) %>% dplyr::arrange(desc(freq))
      incProgress(1/1, detail = "Generating word cloud...")
      wordcloud2(freqTab,size=.2,color="random-dark")
    }) # end progress bar
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
  
  # Value box render in top right hand corner
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
  
  # Create plots
  output$allWinPlot <- renderPlotly({
    getGames <- newGames()
    plotSumAll <- ggplot(data=getGames,aes(x=as.factor(season))) +
      geom_bar(position="dodge",aes(fill=outcome)) +
      labs(x="Season",y="Games",title="Game Outcome by Season") +
      scale_fill_discrete(name="") +
      theme_bw()
    y <- ggplotly(plotSumAll)
    y
  })
  # Create plots
  output$homeWinPlot <- renderPlotly({
    getGames <- newGames()
    getGames <- filter(getGames,location=="Home")
    plotSumAll <- ggplot(data=getGames,aes(x=as.factor(season))) +
      geom_bar(position="dodge",aes(fill=outcome)) +
      labs(x="Season",y="Games",title="Game Outcome by Season") +
      scale_fill_discrete(name="") +
      theme_bw()
    y <- ggplotly(plotSumAll)
    y
  })
  # Create plots
  output$awayWinPlot <- renderPlotly({
    getGames <- newGames()
    getGames <- filter(getGames,location=="Away")
    plotSumAll <- ggplot(data=getGames,aes(x=as.factor(season))) +
      geom_bar(position="dodge",aes(fill=outcome)) +
      labs(x="Season",y="Games",title="Game Outcome by Season") +
      scale_fill_discrete(name="") +
      theme_bw()
    y <- ggplotly(plotSumAll)
    y
  })
  # Create plots
  output$neutralWinPlot <- renderPlotly({
    getGames <- newGames()
    getGames <- filter(getGames,location=="Neutral")
    plotSumAll <- ggplot(data=getGames,aes(x=as.factor(season))) +
      geom_bar(position="dodge",aes(fill=outcome)) +
      labs(x="Season",y="Games",title="Game Outcome by Season") +
      scale_fill_discrete(name="") +
      theme_bw()
    y <- ggplotly(plotSumAll)
    y
  })
  
  # MAP
  
  output$gamesMap <- renderLeaflet({
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
  ### GAME SUMMARY
  ###
  
  # Box plot
  output$gsBoxPoints <- renderPlotly({
    getGames <- newGames()
    boxPoints <- ggplot(data=getGames) +
      geom_jitter(aes(x=location,y=teamPointsScored,color=location)) +
      geom_boxplot(aes(x=location,y=teamPointsScored)) +
      labs(x="Location",y="Team Points Scored",title="Boxplot for Team Points Scored by Location") +
      theme(legend.position = "none")
    y <- ggplotly(boxPoints)
    y
  })
  
  
  
  # Histogram
  output$gsHistPoints <- renderPlotly({
    getGames <- newGames()
    histPoints <- ggplot(data=getGames,aes(x=teamPointsScored,y=..density..)) +
      geom_density(adjust=0.4,size=3,color="red") +
      geom_histogram(bins=30) + labs(x="Team Points Scored",y="Game Count",title="Histogram of Team Points Scored")
    y <- ggplotly(histPoints)
    y
  })
  
  
  output$tableGames <- DT::renderDataTable({
    getGamesPro <- newGames()
    getTeams <- newTeams()
    customPrintName <- paste0(getTeams$abbreviation," Game Summary")
    customFileName <- paste0(getTeams$abbreviation,"gameSummary")
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
  
  #####
  ##### UNSUPERVISED
  #####
  
  # Text
  output$unsuperPcaIntro <- renderUI({
    tags$div(HTML(paste("Identifies linear combinations of selected variables that account for as 
                        much variability as possible. Variables are scaled and centered to neutralize different 
                        units of measure. This PCA biplot contains",tags$b(nrow(unsuperGames())),"observations and",
                        tags$b(ncol(unsuperGames())-1)," variables.")
    )
    )
  })
  output$unsuperSelect <- renderUI({
    tags$div(HTML(paste("Select at least 2:")
    )
    )
  })
  output$unsuperSelectNotes <- renderUI({
    tags$div(HTML(paste(tags$br(),tags$i("Game Location"),"includes 3 binary variables: Home, Away, and Neutral.",tags$br(),tags$br(),
                        tags$i("Venue Details"),"includes 3 variables: elevation, grass (binary), and dome (binary).")
    )
    )
  })
  

  # Create plots
  output$unsuperPCA <- renderPlotly({
    unsuper <- unsuperGames()
    unsuper <- unsuper %>% rename("Outcome"=outcome)
    endCol <- ncol(unsuper)
    pca <- autoplotly(prcomp(unsuper[2:endCol]), data = unsuper,center=TRUE,scale=TRUE,colour = "Outcome", frame = TRUE) +
      ggplot2::ggtitle("Principal Components Analysis") +
      ggplot2::labs(y = "Second Principal Component", x = "First Principal Component")
    # Add center point
    pca %>% plotly::layout(annotations = list(
      text = "Center",
      font = list(
        family = "Courier New, monospace",
        size = 14,
        color = "black"),
      x = 0,
      y = 0,
      showarrow = TRUE))
  })
  
  # Data table
  output$tableUnsuper <- DT::renderDataTable({
    getUnsuperGames <- unsuperGames()
    getTeams <- newTeams()
    customPrintName <- paste0(getTeams$abbreviation," Unsupervised Data Set")
    customFileName <- paste0(getTeams$abbreviation,"unsupervisedDataSet")
    DT::datatable(getUnsuperGames,extensions = 'Buttons',
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
  
  #####
  ##### MODELING
  #####
  
  #########################
  ########## GLM ##########
  #########################
  
  # Team logo image
  output$teamLogoProGLM <- renderUI({
    tags$img(src=newTeams()$logos[1], width=90,style="display: block; margin-left: auto; margin-right: auto;")
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
      tags$img(src=glmOppTeam()$logos[1], width=90,style="display: block; margin-left: auto; margin-right: auto;")
    } else{tags$h3("Anyone")}
  })
  
  # Model introduction
  output$glmIntro <- renderUI({
    tags$div(HTML(paste(tags$strong("Purpose:"),"Predicting the outcome of a college football game, with a binary response of either 1",tags$i("- Win"),
                        ", or 0",tags$i("- Lose"),".",tags$br(),tags$br(),
                        tags$strong("Model Type:"),"Logistic Regression",tags$br(),
                        tags$strong("Model Description:"),"Often the primary method used for predicting a non-continuous response variable with a binary classification. 
                        Applies a binomial regression with a logistic function.")
    )
    )
  })
  
  # Model training process text
  output$glmTrainProcess <- renderUI({
    getModelData <- glmModelData()
    nTotal <- nrow(newGames())
    nFit <- nrow(glmModelData())
    nTrain <- sum(getModelData$modelFitSet == "Train")
    nTest <- sum(getModelData$modelFitSet == "Test")
    
    tags$div(HTML(paste("The custom filtered data set is split into a",tags$i("model training"),"set (70%) and a",tags$i("model testing"),"set (30%). The",
                        tags$i("model training"),"set undergoes k-fold cross validation for the model fit. The fitted model is selected automatically by the",
                        tags$code("caret"),"package based on resulting accuracy. Final accuracy is determined by applying the model to the",
                        tags$i("testing set"),".",tags$br(),
                        tags$b(nTotal),"total records:",tags$b(nTotal-nFit),"records omitted containing",tags$code("NA"),",",tags$b(nTrain),
                        "records in the",tags$i("training set,"),"and",tags$b(nTest),"records in the",tags$i("testing set"),".")
    )
    )
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
  
  #########################
  ########## RF ###########
  #########################
  
  # Team logo image
  output$teamLogoProRF <- renderUI({
    tags$img(src=newTeams()$logos[1], width=90,style="display: block; margin-left: auto; margin-right: auto;")
  })
  
  # Location status
  output$locForRF <- renderUI({
    if (input$rfLoc) {
      tags$div(
        HTML(paste(tags$h3(input$rfSelectLoc),tags$h4(tags$i("vs")))
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
  output$oppLogoProRF <- renderUI({
    if (input$rfOppTalent) {
      tags$img(src=rfOppTeam()$logos[1], width=90,style="display: block; margin-left: auto; margin-right: auto;")
    } else{tags$h3("Anyone")}
  })
  
  # Model introduction
  output$rfIntro <- renderUI({
    tags$div(HTML(paste(tags$strong("Purpose:"),"Predicting the outcome of a college football game, with a binary response of either 1",tags$i("- Win"),
                        ", or 0",tags$i("- Lose"),".",tags$br(),tags$br(),
                        tags$strong("Model Type:"),"Random Forests",tags$br(),
                        tags$strong("Model Description:"),"Builds decision trees on bootstrapped training samples, then takes a random sample of predictors 
                        to be used as split candidates. This helps prevent any strong predictors from consistently occuring in each tree.")
    )
    )
  })
  
  # Model training process text
  output$rfTrainProcess <- renderUI({
    getModelData <- rfModelData()
    nTotal <- nrow(newGames())
    nFit <- nrow(rfModelData())
    nTrain <- sum(getModelData$modelFitSet == "Train")
    nTest <- sum(getModelData$modelFitSet == "Test")
    
    tags$div(HTML(paste("The custom filtered data set is split into a",tags$i("model training"),"set (70%) and a",tags$i("model testing"),"set (30%). The",
                        tags$i("model training"),"set undergoes k-fold cross validation for the model fit. The fitted model is selected automatically by the",
                        tags$code("caret"),"package based on resulting accuracy. Final accuracy is determined by applying the model to the",
                        tags$i("testing set"),".",tags$br(),
                        tags$b(nTotal),"total records:",tags$b(nTotal-nFit),"records omitted containing",tags$code("NA"),",",tags$b(nTrain),
                        "records in the",tags$i("training set,"),"and",tags$b(nTest),"records in the",tags$i("testing set"),".")
    )
    )
  })
  
  # TEAM SCORE
  
  # Team score selection, set average based on historical 
  observe({
    getGamesPro <- newGames()
    avgPointValue <- as.integer(mean(getGamesPro$teamPointsScored))
    updateSliderInput(session,"rfSlideScore",value = avgPointValue)
  })
  
  # TALENT
  
  # Team talent slider, set average value from previous seasons
  observe({
    getTeamTalent <- rfModelTeamTalent()
    avgTeamTalent <- as.integer(mean(getTeamTalent$teamTalent))
    updateSliderInput(session,"rfSlideTTalent",value = avgTeamTalent)
  })
  # Opponent talent slider, set average value from previous seasons
  observe({
    getOppTalent <- rfModelOppTalent()
    avgOppTalent <- as.integer(mean(getOppTalent$teamTalent))
    updateSliderInput(session,"rfSlideOTalent",value = avgOppTalent)
  })
  
  # EXCITEMENT INDEX
  
  # Excitement index selection, set average based on historical 
  observe({
    getGamesPro <- newGames()
    exciteValue <- as.integer(mean(getGamesPro$excitementIndex))
    updateSliderInput(session,"rfSlideExcite",value = exciteValue)
  })
  
  # VENUE
  
  # Venue selection, set max of crowd slider to venue capacity
  observe({
    getVenue <- rfModelVenue()
    if (input$rfVenue == 1) {
      capacity <- as.integer(getVenue$venueCapacity)
      updateSliderInput(session,"rfSlideCrowd",value = capacity, max = capacity)
    } else {
      updateSliderInput(session,"rfSlideCrowd",value = 50000, max = 100000)
    }
  })
  
  # THE MODEL
  
  output$tableRfModelData <- DT::renderDataTable({
    getGamesPro <- newGames()
    getTeams <- newTeams()
    customPrintName <- paste0(getTeams$abbreviation," Model Fit Data Set")
    customFileName <- paste0(getTeams$abbreviation,"modelFitDataSet")
    DT::datatable(rfModelData(),extensions = 'Buttons',
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
  
  output$tableRfUserData <- DT::renderDataTable({
    getGamesPro <- newGames()
    getTeams <- newTeams()
    newRfUserData <- rfUserData()
    newRfUserData <- newRfUserData %>% select(-won)
    customPrintName <- paste0(getTeams$abbreviation," User Predictors Data Set")
    customFileName <- paste0(getTeams$abbreviation,"userPredictorsDataSet")
    DT::datatable(newRfUserData,extensions = 'Buttons',
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
}

# Text
#output$unsuperSelect <- renderUI({
#  tags$div(HTML(paste()
#  )
#  )
#})

