# Load necessary packages for API query and cleaning
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(httr)
library(jsonlite)
library(lubridate)
library(anytime)

# Create base URL to call from from https://api.collegefootballdata.com/
baseUrl <- "https://api.collegefootballdata.com/"

#
## Extract information from "games" endpoint starting with 2012 season when excitement_index first available
#

for (i in 2012:2019){
  if (i==2012) {
    year <- i
    # Generate regular season games list
    fullUrlGames <- paste0(baseUrl,"games?year=",year,"&seasonType=regular")
    getGames <- GET(fullUrlGames)
    getGamesText <- content(getGames, "text")
    games <- as_tibble(fromJSON(getGamesText, flatten = TRUE))
    # Generate postseason games list and append
    fullUrlGames <- paste0(baseUrl,"games?year=",year,"&seasonType=postseason")
    getGames <- GET(fullUrlGames)
    getGamesText <- content(getGames, "text")
    games <-  bind_rows(games,as_tibble(fromJSON(getGamesText, flatten = TRUE)))
  } else{
    year <- i
    # Generate regular season games list
    fullUrlGames <- paste0(baseUrl,"games?year=",year,"&seasonType=regular")
    getGames <- GET(fullUrlGames)
    getGamesText <- content(getGames, "text")
    games <- bind_rows(games,as_tibble(fromJSON(getGamesText, flatten = TRUE)))
    # Generate postseason games list and append
    fullUrlGames <- paste0(baseUrl,"games?year=",year,"&seasonType=postseason")
    getGames <- GET(fullUrlGames)
    getGamesText <- content(getGames, "text")
    games <-  bind_rows(games,as_tibble(fromJSON(getGamesText, flatten = TRUE)))
  }
}  

# Derive unique list of active teams from games data set for filtering other end points later on
homeTeams <- unique(games$home_id)
names(homeTeams) <- "id"
awayTeams <- unique(games$away_id)
names(awayTeams) <- "id"
activeTeams <- unique(c(homeTeams,awayTeams))

## Transform and Clean
games <- games %>% rename("gameId"=id,"seasonType"=season_type) %>%
                   select(-home_line_scores,-away_line_scores)
games$seasonType <- factor(ifelse(games$seasonType=="regular","Regular",ifelse(games$seasonType=="postseason","Postseason","Other")))
games <- games %>% mutate(kickoffDate=as.Date(anydate(start_date))) %>% 
                   mutate(kickoffDay=weekdays(kickoffDate)) %>%
                   mutate(kickoffTime=anytime(start_date))
games <- games %>% select(-start_date,-start_time_tbd)
games$home_post_win_prob <- round(as.numeric(as.character(games$home_post_win_prob)),4)
games$away_post_win_prob <- round(as.numeric(as.character(games$away_post_win_prob)),4)
games$excitement_index <- round(as.numeric(as.character(games$excitement_index)),4)
games <- games %>% select(1:3,21:23,4:20)

# Prepare for future joining with venues
games <- games %>% select(-venue) %>% rename("venueId"=venue_id)
games <- games %>% select(1:9,12:22,10:11)

# Derive unique list of active venues from games data set for filtering other end points later on
activeVenues <- unique(games$venueId)
names(activeVenues) <- "venueId"

#
## Extract information from "games/teams" endpoint for similar season range as "games"
#

for (i in 2012:2019){
  if (i==2012) {
    year <- i
    for (j in 1:16){
      if (j==1) {
        week <- j
        # Generate regular season games list
        fullUrlGames <- paste0(baseUrl,"games/teams?year=",year,"&week=",week,"&seasonType=regular")
        getGames <- GET(fullUrlGames)
        getGamesText <- content(getGames, "text")
        gameStatRaw <- as.list(fromJSON(getGamesText, flatten = TRUE))
        # Generate postseason games list and append
        fullUrlGames <- paste0(baseUrl,"games/teams?year=",year,"&week=",week,"&seasonType=postseason")
        getGames <- GET(fullUrlGames)
        getGamesText <- content(getGames, "text")
        gameStatRaw <-  bind_rows(gameStatRaw,as.list(fromJSON(getGamesText, flatten = TRUE)))
      } else{
          week <- j
          # Generate regular season games list
          fullUrlGames <- paste0(baseUrl,"games/teams?year=",year,"&week=",week,"&seasonType=regular")
          getGames <- GET(fullUrlGames)
          getGamesText <- content(getGames, "text")
          gameStatRaw <- bind_rows(gameStatRaw,as.list(fromJSON(getGamesText, flatten = TRUE)))
          # Generate postseason games list and append
          fullUrlGames <- paste0(baseUrl,"games/teams?year=",year,"&week=",week,"&seasonType=postseason")
          getGames <- GET(fullUrlGames)
          getGamesText <- content(getGames, "text")
          gameStatRaw <-  bind_rows(gameStatRaw,as.list(fromJSON(getGamesText, flatten = TRUE)))
        }
    }  
  } else{
      year <- i
      for (j in 1:16){
        week <- j
        # Generate regular season games list
        fullUrlGames <- paste0(baseUrl,"games/teams?year=",year,"&week=",week,"&seasonType=regular")
        getGames <- GET(fullUrlGames)
        getGamesText <- content(getGames, "text")
        gameStatRaw <- bind_rows(gameStatRaw,as.list(fromJSON(getGamesText, flatten = TRUE)))
        # Generate postseason games list and append
        fullUrlGames <- paste0(baseUrl,"games/teams?year=",year,"&week=",week,"&seasonType=postseason")
        getGames <- GET(fullUrlGames)
        getGamesText <- content(getGames, "text")
        gameStatRaw <-  bind_rows(gameStatRaw,as.list(fromJSON(getGamesText, flatten = TRUE)))
      }
    }
}

## Need to transform some of the nested lists that are returned inside of gameStat
## transformGameStat
gameIdLength <- length(gameStatRaw[[1]])
#still need to automate the length of i by determining count of gameID in returned list
for (k in 1:gameIdLength) {
  if (k==1) {
    tID <- rep(gameStatRaw[[1]][[k]])
    tSUM <- cbind(tID,gameStatRaw[[2]][[k]])
    tT1 <- as_tibble((tSUM[6])[[1]][[1]] %>% spread(category,stat))
    tT2 <- as_tibble((tSUM[6])[[1]][[2]] %>% spread(category,stat))
    tTT <- bind_rows(tT1,tT2)
    tSUM <- select(tSUM,-6)
    gameStats <- bind_cols(tSUM,tTT)
  }
  else{
    tID <- rep(gameStatRaw[[1]][[k]])
    tSUM <- cbind(tID,gameStatRaw[[2]][[k]])
    tT1 <- as_tibble((tSUM[6])[[1]][[1]] %>% spread(category,stat))
    tT2 <- as_tibble((tSUM[6])[[1]][[2]] %>% spread(category,stat))
    tTT <- bind_rows(tT1,tT2)
    tSUM <- select(tSUM,-6)
    gameStatsApp <- bind_cols(tSUM,tTT)
    gameStats <- bind_rows(gameStats,gameStatsApp)
  }
}

# Transform and Clean
gameStats <- gameStats %>% mutate(passCompletions=as.integer(stringr::word(completionAttempts,1,sep="-"))) %>%
                           mutate(passAttempts=as.integer(stringr::word(completionAttempts,2,sep="-"))) %>%
                           mutate(passCompletionPct=round(passCompletions/passAttempts,3)) %>%
                           rename("gameId"=tID) %>%
                           mutate(penaltyCount=as.integer(stringr::word(totalPenaltiesYards,1,sep="-"))) %>%
                           mutate(penaltyYards=as.integer(stringr::word(totalPenaltiesYards,2,sep="-"))) %>% 
                           mutate(fourthDownConverts=as.integer(stringr::word(fourthDownEff,1,sep="-"))) %>%
                           mutate(fourthDownAttempts=as.integer(stringr::word(fourthDownEff,2,sep="-"))) %>%
                           mutate(fourthDownEffPct=ifelse(fourthDownAttempts==0,NA,round(fourthDownConverts/fourthDownAttempts,3))) %>%
                           mutate(thirdDownConverts=as.integer(stringr::word(thirdDownEff,1,sep="-"))) %>%
                           mutate(thirdDownAttempts=as.integer(stringr::word(thirdDownEff,2,sep="-"))) %>%
                           mutate(thirdDownEffPct=ifelse(thirdDownAttempts==0,NA,round(thirdDownConverts/thirdDownAttempts,3)))

gameStats$firstDowns <- as.integer(as.character(gameStats$firstDowns))
for (c in 9:20) {
  gameStats[,c] <- as.integer(as.character(gameStats[,c]))
}
for (c in 22:27) {
  gameStats[,c] <- as.integer(as.character(gameStats[,c]))
}
for (c in 30:31) {
  gameStats[,c] <- as.integer(as.character(gameStats[,c]))
}
for (c in 32:33) {
  gameStats[,c] <- as.numeric(as.character(gameStats[,c]))
}
for (c in 34:40) {
  gameStats[,c] <- as.integer(as.character(gameStats[,c]))
}

# Find missing games from gameStats compared to games
 missingGames <- anti_join(games,gameStats,by="gameId")

#
## Extract information from "teams" endpoint for list of all teams and properties
#

## Extract information for all teams.
fullUrlConf <- paste0(baseUrl,"teams")
## Generate team list by conference
getConf <- GET(fullUrlConf)
getConfText <- content(getConf, "text")
getConfJson <- as.list(fromJSON(getConfText, flatten = TRUE))
# Extract first nested logo (2 are returned in same character string) and replace NULL values
getLogos <- as_tibble(getConfJson[12])
getLogos <- map_df(transpose(getLogos), ~map_chr(.,~ifelse(is.null(.),NA,.)))
# Bind back together team properties with the now unique logo web addresses
teams <- as_tibble(bind_cols(getConfJson[1:11],getLogos))
## Reduce teams list to active teams that are included in the games data set
teams <- subset(teams, id %in% activeTeams)
## Turn conference into a factor and replace NA values to make selection user friendly
teams$conference <- replace_na(teams$conference,"Other")

#
## Extract information from "venues" endpoint to build list of stadiums and their properties
#

# buildVenueURL
fullUrlVenues <- paste0(baseUrl,"venues")

# Generate venue list
getVenues <- GET(fullUrlVenues)
getVenuesText <- content(getVenues, "text")
venues <- as_tibble(fromJSON(getVenuesText, flatten = TRUE))

# Clean and transform variables
venues$elevation <- round(as.numeric(as.character(venues$elevation)),1)

# Rename variables
venues <- venues %>% rename("venueId"=id,"venueName"=name,"venueCapacity"=capacity,"venueGrass"=grass,"venueCity"=city,"venueState"=state,
                            "venueZip"=zip,"venueCountry"=country_code,"venueElevation"=elevation,
                            "venueConstructed"=year_constructed,"venueDome"=dome,"venueTimezone"=timezone,
                            "venueLat"=location.x,"venueLong"=location.y)
## Reduce venues list to active venues that are included in the games data set
venues <- subset(venues, venueId %in% activeVenues)
# Make a joined data set between games and venues for validation
gamesVenues <- left_join(games,venues,by="venueId")

#
## Extract information from "talent" endpoint to build list of stadiums and their properties
#

# buildTalentURL
fullUrlTalent <- paste0(baseUrl,"talent")

# Generate talent list
getTalent <- GET(fullUrlTalent)
getTalentText <- content(getTalent, "text")
talent <- as_tibble(fromJSON(getTalentText, flatten = TRUE))
talent <- talent %>% rename("season"=year)
# Duplicate and create data sets for joining to tables where 2 team columns exist
talentTeam <- talent
talentTeam <- talentTeam %>% rename("team"=school,"teamTalent"=talent)
talentOpp <- talent
talentOpp <- talentOpp %>% rename("opponent"=school,"oppTalent"=talent)

#
## Extract information from "pregameWP" endpoint to build list of pregame win probabilities
#

# buildPreWpURL
fullUrlPreWp <- paste0(baseUrl,"metrics/wp/pregame")

# Generate pregameWP list
getPreWp <- GET(fullUrlPreWp)
getPreWpText <- content(getPreWp, "text")
pregameWP <- as_tibble(fromJSON(getPreWpText, flatten = TRUE))

#
## Save desired objects in RData file for calling/usage elsewhere
#
gameSeasons <- games
save(teams,venues,games,gameStats,gamesVenues,gameSeasons,talentOpp,talentTeam,file="collegeFootball.RData")

#
## END END END END END END END END END END END END
#

## Alternate code to pulling teams list by conference, with both logo types
## Extract information from "conference" endpoint to build list of teams by conference

# buildConfURL
#conf <- "sec"
#fullUrlConf <- paste0(baseUrl,"teams?conference=",conf)

# Generate team list by conference
#getConf <- GET(fullUrlConf)
#getConfText <- content(getConf, "text")
#getConfJson <- fromJSON(getConfText, flatten = TRUE)
#  getLogos <- getConfJson[,12]
#    logo <- as_tibble(getLogos %>% map_chr(1))
#      names(logo) <- "logo"
#    logoDark <- as_tibble(getLogos %>% map_chr(2))
#      names(logoDark) <- "logoDark"
#teams <- as_tibble(bind_cols(getConfJson[1:11],logo,logoDark))

