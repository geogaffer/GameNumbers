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