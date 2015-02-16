download.player <- function(playerNum=180) {
    
    library(jsonlite)
    
    playerURL = paste("http://fantasy.premierleague.com/web/api/elements/", 
                      playerNum, '/', sep="")
    jsonData <- fromJSON(playerURL)
    
    jsonData
}

download.history <- function(playerNum=180) {
    
    library(jsonlite)
    
    if (!file.exists("./data raw")) { dir.create("./data raw") }
    
    playerURL = paste("http://fantasy.premierleague.com/web/api/elements/", 
                      playerNum, '/', sep="")
    jsonData <- fromJSON(playerURL)
    
    history.all <- jsonData$fixture_history$all
    history.all <- as.data.frame(history.all, stringsAsFactors=FALSE)
    names(history.all) <- c("date", "week", "result", "minutes", "goals", "assists", 
                            "clean", "conceded", "own", "pen save", "pen miss", 
                            "yellows", "reds", "saves", "bonus", "ppi", "bps",
                            "transfers", "value", "points")
    for (i in c(2, 4:20)) {
        history.all[,i] <- as.integer(history.all[,i])
    }
    opponent <- substr(history.all$result, 1, 3)
    hora <- substr(history.all$result, 5, 5)
    home <- (hora=="H")
    
    us <- vector(mode="integer", length=length(home))
    them <- vector(mode="integer", length=length(home))
    for (i in seq(1:length(home))) {
        if (home[i]) {
            us[i] <- as.integer(substr(history.all$result[i], 7, 8))
            them[i] <- as.integer(substr(history.all$result[i], 10, 11))
        } else {
            them[i] <- as.integer(substr(history.all$result[i], 7, 8))
            us[i] <- as.integer(substr(history.all$result[i], 10, 11))
        }
    }
    
    history.new <- cbind(history.all[,1:3], opponent, home, us, them, history.all[,4:20])
    
    csvName <- paste("./data raw/", playerNum, ".csv", sep="")
    print(paste("writing... ", csvName))
    write.csv(history.new, file=csvName)
    history.new
}

download.playerDB <- function(playerRange = seq(1, 15)) {
    
    library(jsonlite)
    
    baseURL <- "http://fantasy.premierleague.com/web/api/elements/"
    playerID <- vector(mode="integer", length=length(playerRange))
    photo <- vector(mode="character", length=length(playerRange))
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
            photo[i] <<- photo
            position[i] <<- type_name
            team[i] <<- team_name
            totalPoints[i] <<- total_points
            cost[i] <<- now_cost
            minutes[i] <<- minutes
        })
        
        history.all <- jsonData$fixture_history$all
        history.all <- as.data.frame(history.all, stringsAsFactors=FALSE)
        names(history.all) <- c("date", "week", "result", "minutes", "goals", "assists", 
                                "clean", "conceded", "own", "pen save", "pen miss", 
                                "yellows", "reds", "saves", "bonus", "ppi", "bps",
                                "transfers", "value", "points")
        for (j in c(2, 4:20)) {
            history.all[,j] <- as.integer(history.all[,j])
        }
        opponent <- substr(history.all$result, 1, 3)
        hora <- substr(history.all$result, 5, 5)
        home <- (hora=="H")
        
        us <- vector(mode="integer", length=length(home))
        them <- vector(mode="integer", length=length(home))
        for (k in seq(1:length(home))) {
            if (home[k]) {
                us[k] <- as.integer(substr(history.all$result[k], 7, 8))
                them[k] <- as.integer(substr(history.all$result[k], 10, 11))
            } else {
                them[k] <- as.integer(substr(history.all$result[k], 7, 8))
                us[k] <- as.integer(substr(history.all$result[k], 10, 11))
            }
        }
        
        history.new <- cbind(history.all[,1:3], opponent, home, us, them, history.all[,4:20])
        
        csvName <- paste("./data raw/", playerNum, ".csv", sep="")
        print(paste("writing... ", csvName))
        write.csv(history.new, file=csvName)
        
        #print(i)
        Sys.sleep(5)
    }
    
    playerList <- cbind(playerID, lastName, firstName, photo, position, team,
                        totalPoints, cost, minutes)
    names(playerList) <- c("playerID", "lastName", "firstName", "photo", "position", 
                           "team", "totalPoints", "cost", "minutes")
    
    playerList
}
