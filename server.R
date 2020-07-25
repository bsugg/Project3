library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(leaflet)
library(ragtop)
library(knitr)

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
  
  ########## GLM #########
  
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
  
  # VENUE
  
  glmModelVenue <- reactive({
    # Filter on user selected venue
    newVenue <- venues %>% filter(venueUniqueName == input$glmSelectVenue)
  })
  
  # MODEL DATA SET
  
  glmModelData <- eventReactive(input$genGLM, {
    getGames <- newGames()
    getGames <- getGames %>% select(won,teamPointsScored,teamTalent,oppTalent,location,excitementIndex,
                                          venueElevation,venueGrass,venueDome,venueLat,venueLong,attendance)
    # Transform
    getGames <- getGames %>% mutate("locHome"=as.integer(ifelse(location=="Home",1,0))) %>%
      mutate("locAway"=as.integer(ifelse(location=="Away",1,0))) %>%
      mutate("locNeutral"=as.integer(ifelse(location=="Neutral",1,0))) %>%
      select(-location)
    getGames$venueGrass <- as.integer(getGames$venueGrass)
    getGames$venueDome <- as.integer(getGames$venueDome)
    # Dynamic variable selection based on user input
    if(input$glmTeamScore) {getGames} else{getGames <- select(getGames,-teamPointsScored)}
    if(input$glmTeamTalent) {getGames} else{getGames <- select(getGames,-teamTalent)}
    if(input$glmOppTalent) {getGames} else{getGames <- select(getGames,-oppTalent)}
    if(input$glmLoc) {getGames} else{getGames <- getGames %>% select(-starts_with("loc"))}
    if(input$glmExcite) {getGames} else{getGames <- select(getGames,-excitementIndex)}
    if(input$glmVenue) {getGames} else{getGames <- getGames %>% select(-starts_with("venue"))}
    if(input$glmCrowd) {getGames} else{getGames <- select(getGames,-attendance)}
    # Remove records with NA values
    getGames <- na.omit(getGames)
    print(length(getGames$omit))
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
    return(getGames)
  })
  
  ########## RF #########
  
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
  
  ########## GLM #########
  
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
  
  #output$tableGlmModelData <- DT::renderDataTable({
  #  DT::datatable(glmModelData(),options = list(orderClasses = TRUE,pageLength = 5))
  #})
  
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
  
  ########## RANDOM #########
  
}
