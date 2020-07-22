# Load necessary packages for API query and cleaning
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(httr)
library(jsonlite)

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

# Derive unique list of active venues from games data set for filtering other end points later on
activeVenues <- unique(games$venue_id)
names(activeVenues) <- "id"

#
## Extract information from "games/teams" endpoint for similar season range as "games"
#

for (i in 2012:2019){
  if (i==2012) {
    year <- i
    for (j in 1:16){
      week <- j
      # Generate regular season games list
      fullUrlGames <- paste0(baseUrl,"games/teams?year=",year,"&week=",week,"&seasonType=regular")
      getGames <- GET(fullUrlGames)
      getGamesText <- content(getGames, "text")
      gameStats <- as.list(fromJSON(getGamesText, flatten = TRUE))
      # Generate postseason games list and append
      fullUrlGames <- paste0(baseUrl,"games/teams?year=",year,"&week=",week,"&seasonType=postseason")
      getGames <- GET(fullUrlGames)
      getGamesText <- content(getGames, "text")
      gameStats <-  bind_rows(gameStats,as.list(fromJSON(getGamesText, flatten = TRUE)))
    }  
  } else{
      year <- i
      for (j in 1:16){
        week <- j
        # Generate regular season games list
        fullUrlGames <- paste0(baseUrl,"games/teams?year=",year,"&week=",week,"&seasonType=regular")
        getGames <- GET(fullUrlGames)
        getGamesText <- content(getGames, "text")
        gameStats <- bind_rows(gameStats,as.list(fromJSON(getGamesText, flatten = TRUE)))
        # Generate postseason games list and append
        fullUrlGames <- paste0(baseUrl,"games/teams?year=",year,"&week=",week,"&seasonType=postseason")
        getGames <- GET(fullUrlGames)
        getGamesText <- content(getGames, "text")
        gameStats <-  bind_rows(gameStats,as.list(fromJSON(getGamesText, flatten = TRUE)))
      }
    }
}

## Need to transform some of the nested lists that are returned inside of gameStats
## transformGameStats
gameIdLength <- length(gameStats[[1]])
#still need to automate the length of i by determining count of gameID in returned list
for (i in 1:gameIdLength) {
  if (i==1) {
    tID <- rep(gameStats[[1]][[i]])
    tSUM <- cbind(tID,gameStats[[2]][[i]])
    tT1 <- as_tibble((tSUM[6])[[1]][[1]] %>% spread(category,stat))
    tT2 <- as_tibble((tSUM[6])[[1]][[2]] %>% spread(category,stat))
    tTT <- bind_rows(tT1,tT2)
    tSUM <- select(tSUM,-6)
    teamStats <- bind_cols(tSUM,tTT)
  }
  else{
    tID <- rep(gameStats[[1]][[i]])
    tSUM <- cbind(tID,gameStats[[2]][[i]])
    tT1 <- as_tibble((tSUM[6])[[1]][[1]] %>% spread(category,stat))
    tT2 <- as_tibble((tSUM[6])[[1]][[2]] %>% spread(category,stat))
    tTT <- bind_rows(tT1,tT2)
    tSUM <- select(tSUM,-6)
    teamStatsApp <- bind_cols(tSUM,tTT)
    teamStats <- bind_rows(teamStats,teamStatsApp)
  }
}

# Transform and Clean
teamStats <- teamStats %>% mutate(passCompletions=as.integer(stringr::word(completionAttempts,1,sep="-"))) %>%
                           mutate(passAttempts=as.integer(stringr::word(completionAttempts,2,sep="-"))) %>%
                           mutate(passCompletionPct=round(passCompletions/passAttempts,3))

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

#
## Extract information from "venues" endpoint to build list of stadiums and their properties
#

# buildVenueURL
fullUrlVenues <- paste0(baseUrl,"venues")

# Generate venue list
getVenues <- GET(fullUrlVenues)
getVenuesText <- content(getVenues, "text")
venues <- as_tibble(fromJSON(getVenuesText, flatten = TRUE))
## Reduce venues list to active venues that are included in the games data set
venues <- subset(venues, id %in% activeVenues)

#
## Save desired objects in RData file for calling/usage elsewhere
#

save(teams,venues,games,teamStats,file="collegeFootball.RData")

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

