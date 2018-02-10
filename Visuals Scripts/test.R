outcome = "Rev"

subset <- names(correlRanks)[grepl(outcome, names(correlRanks))]
if (outcome == "Rev" || outcome == "NOI") {
  subset <- subset[!grepl(".EqPart", subset)]
  subset <- subset[!grepl(".Lawyer", subset)]
}
subset <- c("Year", "AggMnA", "GDP", subset)



thisDF <- correlRanks[,subset]
names(thisDF) <- gsub(paste(".",outcome,sep=""), "", names(thisDF))


#fac <- max(thisDF) * 1.6 / max(correlRanks$AggMnA)
#thisDF$GDP <- thisDF$GDP/max(thisDF$GDP)

width <- 3
thisDF2 <- thisDF[1:(nrow(thisDF)-width+1),]

modGDP <- function(x) {
  # as.numeric(running(thisDF$GDP,thisDF$Rank1,fun=cor, width=width))
  #return ((thisDF2$GDP/max(thisDF2$GDP))-(x/max(x)))
  return ((thisDF2$GDP-x)/max(thisDF2$GDP-x))
}

modAggMnA <- function(x) {
  # as.numeric(running(thisDF$AggMnA,thisDF$Rank1,fun=cor, width=width))
  #return ((thisDF2$AggMnA/max(thisDF2$AggMnA))-(x/max(x)))
  return ((thisDF2$AggMnA-x)/max(thisDF2$AggMnA-x))
}

#thisDF$Rank1_mod <- (thisDF$GDP/max(thisDF$GDP))-(thisDF$Rank1/max(thisDF$Rank1))
#thisDF$Rank2_mod <- (thisDF$GDP/max(thisDF$GDP))-(thisDF$Rank2/max(thisDF$Rank2))
#thisDF2$Rank1_mod <- modGDP(thisDF2$Rank1)

thisDF2[,grepl("Rank", names(thisDF2))] <- sapply(thisDF2[,grepl("Rank", names(thisDF2))], modAggMnA )
#thisDF2[,grepl("Rank", names(thisDF2))] <- sapply(thisDF2[,grepl("Rank", names(thisDF2))], modGDP )



thisDF2 <- thisDF2[,!(names(thisDF2) %in% c("AggMnA", "GDP"))]

# create the plotting data frame
plotDF <- melt(thisDF2, id="Year")
# add the linetypes
#plotDF$lt <- rep("dotdash", nrow(plotDF))


p <- ggplot(data=plotDF, aes(x=Year, y=value, color=variable)) + geom_line() + 
  labs(title = paste("Ranked Firms by", outcomeText[i]), x="Year", y=outcomeText[i]) +
  scale_color_discrete(name="Variable") + theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(~./fac, name = "Agg M&A, GDP (in millions)"))
p








# replace all the rank columns with their correlations
#thisDF[,grepl("Rank", names(thisDF))]
#grepl("Rank", names(thisDF))
thisDF[,grepl("Rank", names(thisDF))] <- sapply(thisDF[,grepl("Rank", names(thisDF))], function(x) (x-thisDF$GDP)/max(thisDF$GDP) )


fac <- max(thisDF) * 1.6 / max(correlRanks$AggMnA)

#fac <- 3000
thisDF$GDP <- thisDF$GDP*fac
#thisDF$AggMnA <- thisDF$AggMnA*fac

# create the plotting data frame
plotDF <- melt(thisDF, id="Year")
# add the linetypes
plotDF$lt <- rep("dotdash", nrow(plotDF))
# change AggMnA to different line style
plotDF$lt[plotDF$variable == "AggMnA"] <- "solid"
plotDF$lt[plotDF$variable == "GDP"] <- "solid"

p <- ggplot(data=plotDF, aes(x=Year, y=value, color=variable, linetype=lt)) + geom_line() + 
  labs(title = paste("Ranked Firms by", outcomeText[i]), x="Year", y=outcomeText[i]) +
  scale_linetype_discrete(guide=FALSE) + scale_color_discrete(name="Variable") + theme_bw() +
  scale_y_continuous(sec.axis = sec_axis(~./fac, name = "Agg M&A, GDP (in millions)"))
p