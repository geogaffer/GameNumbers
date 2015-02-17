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
    
    if (!file.exists("./data raw")) { dir.create("./data raw") }
    if (!file.exists("./photos")) { dir.create("./photos")}
    
    baseURL <- "http://fantasy.premierleague.com/web/api/elements/"
    photoURL <- "http://cdn.ismfg.net/static/plfpl/img/shirts/photos/"
    playerID <- vector(mode="integer", length=length(playerRange))
    photo <- vector(mode="character", length=length(playerRange))
    lastName <- vector(mode="character", length=length(playerRange))
    firstName <- vector(mode="character", length=length(playerRange))
    position <- vector(mode="character", length=length(playerRange))
    team <- vector(mode="character", length=length(playerRange))
    totalPoints <- vector(mode="integer", length=length(playerRange))
    cost <- vector(mode="integer", length=length(playerRange))
    minutes <- vector(mode="integer", length=length(playerRange))
    selectbyteam <- vector(mode="character", length=length(playerRange))
    status <- vector(mode="character", length=length(playerRange))
    cost_chg_start <- vector(mode="integer", length=length(playerRange))
    cost_chg_event <- vector(mode="integer", length=length(playerRange))
    cost_chg_start_fall <- vector(mode="integer", length=length(playerRange))
    cost_chg_event_fall <- vector(mode="integer", length=length(playerRange))
    dreamteam <- vector(mode="integer", length=length(playerRange))
    trans_out <- vector(mode="integer", length=length(playerRange))
    trans_in <- vector(mode="integer", length=length(playerRange))
    trans_in_event <- vector(mode="integer", length=length(playerRange))
    trans_out_event <- vector(mode="integer", length=length(playerRange))
    play_this <- vector(mode="integer", length=length(playerRange))
    play_next <- vector(mode="integer", length=length(playerRange))
    
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
            selectbyteam[i] <<- selected_by_percent
            status[i] <<- status
            cost_chg_start[i] <<- cost_change_start
            cost_chg_event[i] <<- cost_change_event
            cost_chg_start_fall[i] <<- cost_change_start_fall
            cost_chg_event_fall[i] <<- cost_change_event_fall
            dreamteam[i] <<- dreamteam_count
            trans_out[i] <<- transfers_out
            trans_in[i] <<- transfers_in
            trans_out_event[i] <<- transfers_out_event
            trans_in_event[i] <<- transfers_in_event
            if (length(chance_of_playing_this_round)>0) {
                play_this[i] <<- chance_of_playing_this_round
            }
            if (length(chance_of_playing_next_round)>0) {
                play_next[i] <<- chance_of_playing_next_round
            }
        })
        
        localPlayerPhoto <- paste("./photos/", photo[i], sep="")
        if (!file.exists(localPlayerPhoto)) {
            urlPlayerPhoto <- paste(photoURL, photo[i], sep="")
            download.file(url=urlPlayerPhoto, destfile=localPlayerPhoto, 
                          method="curl", quiet=TRUE)
        }
        
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
                        totalPoints, cost, minutes, status, cost_chg_start,
                        cost_chg_event, cost_chg_start_fall, cost_chg_event_fall,
                        dreamteam, trans_out, trans_in, trans_out_event, 
                        trans_in_event, play_this, play_next)
    names(playerList) <- c("playerID", "lastName", "firstName", "photo", "position", 
                           "team", "totalPoints", "cost", "minutes", "status", 
                           "costChgStart", "costChgEvent", "costStartFall", "costEventFall",
                           "dreamteam", "transOut", "transIn", "transEventOut",
                           "transEventIn", "playThis", "playNext")
    
    playerList
}
