getPlayerData <- function(playerNum=180) {
    
    library(jsonlite)
    
    playerURL = paste("http://fantasy.premierleague.com/web/api/elements/", 
                      playerNum, '/', sep="")
    jsonData <- fromJSON(playerURL)
    
    jsonData
}

getPlayerHistory <- function(playerNum=180) {
    
    library(jsonlite)
    
    playerURL = paste("http://fantasy.premierleague.com/web/api/elements/", 
                      playerNum, '/', sep="")
    jsonData <- fromJSON(playerURL)
    
    jsonData$fixture_history
}

buildPlayerList <- function(playerRange = seq(1, 15)) {
    
    library(jsonlite)
    
    baseURL <- "http://fantasy.premierleague.com/web/api/elements/"
    playerID <- vector(mode="integer", length=length(playerRange))
    lastName <- vector(mode="character", length=length(playerRange))
    firstName <- vector(mode="character", length=length(playerRange))
    position <- vector(mode="character", length=length(playerRange))
    team <- vector(mode="character", length=length(playerRange))
    totalPoints <- vector(mode="integer", length=length(playerRange))
    cost <- vector(mode="integer", length=length(playerRange))
    minutes <- vector(mode="integer", length=length(playerRange))
    
    i <- 0
    for (playerNum in playerRange) {
        playerURL = paste(baseURL, playerNum, '/', sep="")
        #print(playerURL)
        jsonData <- fromJSON(playerURL)
        i <- i+1
        
        with(jsonData, {
            playerID[i] <<- playerNum
            lastName[i] <<- second_name
            firstName[i] <<- first_name
            position[i] <<- type_name
            team[i] <<- team_name
            totalPoints[i] <<- total_points
            cost[i] <<- now_cost
            minutes[i] <<- minutes
        })
        
    }
    playerList <- cbind(playerID, lastName, firstName, position, team,
                        totalPoints, cost, minutes)
    names(playerList) <- c("playerID", "lastName", "firstName", "position", "team",
                           "totalPoints", "cost", "minutes")
    playerList
}
