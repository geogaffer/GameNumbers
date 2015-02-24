plotCSsPerWeek <- function(goalsFile) {
    
    goalsEach <- read.csv(file=goalsFile)
    
    goalsPerWeek <- vector(mode="integer", length=length(goalsEach[,1]))
    cleanSheets <- vector(mode="integer", length=length(goalsEach[,1]))
    countCS <- vector(mode="integer", length=length(goalsEach[,1]))
    
    for (i in 1:length(goalsEach[,1])) {
        goalsPerWeek[i] <- sum(goalsEach[i,])
        countCS[i] <- sum(goalsEach[i,] == cleanSheets)
    }
    #plot(goalsPerWeek, type="b")
    plot(countCS, type="b", col="red")
}

plotGoalsPerWeek <- function(goalsFile) {
    
    goalsEach <- read.csv(file=goalsFile)
    
    goalsPerWeek <- vector(mode="integer", length=length(goalsEach[,1]))
    cleanSheets <- vector(mode="integer", length=length(goalsEach[,1]))
    countCS <- vector(mode="integer", length=length(goalsEach[,1]))
    
    for (i in 1:length(goalsEach[,1])) {
        goalsPerWeek[i] <- sum(goalsEach[i,])
        countCS[i] <- sum(goalsEach[i,] == cleanSheets)
    }
    plot(goalsPerWeek, type="b", col="blue")
}

goalsByWeek <- function(team, player) {
    
    for (j in 1:length(team)) {
        playerTable <- read.csv(paste("./data raw/", player[j], ".csv", sep=""))
        #print(playerTable$us, playerTable$them)
        
        forWeek <- vector(mode="integer", length=length(playerTable$us))
        againstWeek <- vector(mode="integer", length=length(playerTable$us))
        forTotal <- vector(mode="integer", length=length(playerTable$us))
        againstTotal <- vector(mode="integer", length=length(playerTable$us))
        
        for (i in 1:length(playerTable$us)) {
            #print(paste(i, "; ", playerTable$us[i]))
            forWeek[i] <- playerTable$us[i]
            againstWeek[i] <- playerTable$them[i]
        }
        
        forTotal[1] <- forWeek[1]
        againstTotal[1] <- againstWeek[1]
        
        for (i in 2:length(playerTable$us)) {
            forTotal[i] <- forTotal[i-1] + forWeek[i]
            againstTotal[i] <- againstTotal[i-1] + againstWeek[i]
        }
        
        if (j==1) {
            fws <- as.data.frame(forWeek)
            fts <- as.data.frame(forTotal)
            aws <- as.data.frame(againstWeek)
            ats <- as.data.frame(againstTotal)
        } else {
            fws <- cbind(fws, forWeek)
            fts <- cbind(fts, forTotal)
            aws <- cbind(aws, againstWeek)
            ats <- cbind(ats, againstTotal)
        }
    }
    
    names(fws) <- team
    names(fts) <- team
    names(aws) <- team
    names(ats) <- team
    
    write.csv(fws, "./Weekly For.csv")
    write.csv(fts, "./Total For.csv")
    write.csv(aws, "./Weekly Against.csv")
    write.csv(ats, "./Total Against.csv")
    
    
    #print(ptsWeek)
    #print(ptsTotal)
}

pointsByWeek <- function(team, player) {
    
    for (j in 1:length(team)) {
        playerTable <- read.csv(paste("./data raw/", player[j], ".csv", sep=""))
        #print(playerTable$us, playerTable$them)
        
        ptsWeek <- vector(mode="integer", length=length(playerTable$us))
        ptsTotal <- vector(mode="integer", length=length(playerTable$us))
        
        for (i in 1:length(playerTable$us)) {
            #print(paste(i, "; ", playerTable$us[i]))
            if(playerTable$us[i]>playerTable$them[i]) {ptsWeek[i] <- 3} else 
                if (playerTable$us[i]==playerTable$them[i]) {ptsWeek[i] <- 1} else 
                {ptsWeek[i] <- 0}
        }
        
        ptsTotal[1] <- ptsWeek[1]
        
        for (i in 2:length(playerTable$us)) {
            ptsTotal[i] <- ptsTotal[i-1] + ptsWeek[i]
        }
        
        if (j==1) {
            weeks <- as.data.frame(ptsWeek)
            totals <- as.data.frame(ptsTotal)
        } else {
            weeks <- cbind(weeks, ptsWeek)
            totals <-cbind(totals, ptsTotal)
        }
    }
    
    names(weeks) <- team
    names(totals) <- team
    
    write.csv(weeks, "./Weekly Points.csv")
    write.csv(totals, "./Total Points.csv")

    #print(ptsWeek)
    #print(ptsTotal)
    
}

PLFFpointsbyminutes <- function() {
    
    require(ggplot2)
    
    if (!exists("playerList")) {
        playerList <- read.csv("playerList.csv")
    }
    
    p <- ggplot(data=playerList, aes(y=totalPoints, x=minutes, colour=position))
    #p <- p + facet_grid(position ~ .)
    p <- p + geom_point()
    #p <- p + geom_smooth()
    p
}

bySeason <- function() {
    
    require(ggplot2)
    
    # assumes: htable <- read.csv("Historical Tables.csv")
    
    htable2 <- subset(htable, Tier==1, select=c(FASeason, pctPts, Team))
    
    p <- ggplot(data=htable2, aes(x=FASeason, y=pctPts))
    p <- p + geom_jitter(alpha=I(1/2))
    p <- p + geom_vline(xintercept=27.5) + geom_vline(xintercept=47.5)
    p
    
    # vline at 27.5 for WWI
    # vline at 47.5 for WWII
}

homeAdvantageScored <- function() {
    
    htable2 <- subset(htable, Tier==1, select=c(FASeason, FH, FA, Team, Tier.Rank))
    
    p <- ggplot(data=htable2, aes(x=FASeason, y=FH/FA, colour=Tier.Rank))
    p <- p + geom_jitter(alpha=I(1/2))
    p <- p + geom_vline(xintercept=27.5) + geom_vline(xintercept=47.5)
    p
    
}

homeAdvantageConceeded <- function() {
    
    htable2 <- subset(htable, Tier==1, select=c(FASeason, AH, AA, Team))
    
    p <- ggplot(data=htable2, aes(x=FASeason, y=AA/AH))
    p <- p + geom_jitter(alpha=I(1/2))
    p <- p + geom_vline(xintercept=27.5, aes(colour="red")) + geom_vline(xintercept=47.5, colour="red")
    p
    
}

homeAdvantageRank <- function() {
    
    htable2 <- subset(htable, Tier==1, select=c(FASeason, AH, AA, Tier.Rank))
    
    p <- ggplot(data=htable2, aes(x=Tier.Rank, y=AA/AH))
    p <- p + geom_jitter(alpha=I(1/2))
    p
}