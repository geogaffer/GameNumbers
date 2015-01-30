bySeason <- function() {
    
    require(ggplot2)
    
    # assumes: htable <- read.csv("Historical Tables.csv")
    
    htable2 <- subset(htable, Tier==1, select=c(FASeason, pctPts, Team))
    
    p <- ggplot(data=htable2, aes(x=FASeason, y=pctPts, colour=Team))
    p <- p + geom_jitter(alpha=I(1/2))
    p
}

