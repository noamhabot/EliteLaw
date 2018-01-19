# This code allows us to do some first data visualizations



# Clear environment
rm(list = ls())



# Load sources and libraries
library(GGally)
library(ggplot2)
library(reshape2)
library(dplyr)
library(lattice)



# Load dataframe
setwd("~/Google Drive/Stanford Law Project")
load('Data/EliteLawDf.RData')
load('Data/GDP.RData')


# Function to perform multiple plots with regressions
ggpairs_with_linreg <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="red", ...)
  p
}



# Create new variables (per lawyer ones)
df = df %>% mutate(NOI.Lawyer = NOI/Lawyers, Rev.Lawyer = GrossRev/Lawyers)


# Summary statistics
summary = summary(df %>% select(Lawyers, Leverage, GrossRev, NOI, IPO, Equity, MnA, Rev.Lawyer, NOI.Lawyer, NOI.eqPart))



# Scatter plots
# Fancy one
toPlot = df[, c(4, 5, 6, 8, 9, 10, 16, 19, 18, 17)]
graph = ggpairs(toPlot, lower = list(continuous = ggpairs_with_linreg), columnLabels=c("Lawyers", "Rev", "NOI", "IPO", "D&E", "M&A", "Leverage", "Rev/Law", "NOI/Law", "NOI/EqPart"))
#graph

# Standard one
#plot(df[,c(4, 5, 6, 7, 9, 10, 11, 17, 18)])



# VCov
corrDf = df %>% select(Lawyers, Leverage, GrossRev, NOI, IPO,
                       Equity, MnA, Rev.Lawyer, NOI.Lawyer, NOI.eqPart) %>% as.data.frame()
names(corrDf) = c("Lawyers", "Leverage", "Rev", "NOI", "IPO", "Equity", "MnA", "Rev/Law", "NOI/Law", "NOI/EqPart")
corrMat = cor(corrDf)
levelplot(corrMat, pretty=TRUE, names.attr = c("Lawyers", "Leverage", "Rev", "NOI", "IPO", "Equity", "MnA", "Rev/Law", "NOI/Law", "NOI/EqPart"))



# Correlations
correlRanks = as.data.frame(unique(sort(df$Year)))

# Top 1
# GrossRev
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(1, GrossRev) %>% filter(GrossRev == min(GrossRev)) %>% summarise(AvgRev = mean(GrossRev)) %>% arrange(Year) %>% select(AvgRev))
# Rev/Lawyer
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(1, Rev.Lawyer) %>% filter(Rev.Lawyer == min(Rev.Lawyer)) %>% summarise(AvgRev.Lawyer = mean(Rev.Lawyer)) %>% arrange(Year) %>% select(AvgRev.Lawyer))
# NOI/Lawyer
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(1, NOI.Lawyer) %>% filter(NOI.Lawyer == min(NOI.Lawyer)) %>% summarise(AvgNOI.Lawyer = mean(NOI.Lawyer)) %>% arrange(Year) %>% select(AvgNOI.Lawyer))
# NOI/EqPart
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(1, NOI.eqPart) %>% filter(NOI.eqPart == min(NOI.eqPart)) %>% summarise(AvgNOI.EqPart = mean(NOI.eqPart)) %>% arrange(Year) %>% select(AvgNOI.EqPart))

# Top 2
# GrossRev
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(2, GrossRev) %>% filter(GrossRev == min(GrossRev)) %>% summarise(AvgRev = mean(GrossRev)) %>% arrange(Year) %>% select(AvgRev))
# Rev/Lawyer
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(2, Rev.Lawyer) %>% filter(Rev.Lawyer == min(Rev.Lawyer)) %>% summarise(AvgRev.Lawyer = mean(Rev.Lawyer)) %>% arrange(Year) %>% select(AvgRev.Lawyer))
# NOI/Lawyer
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(2, NOI.Lawyer) %>% filter(NOI.Lawyer == min(NOI.Lawyer)) %>% summarise(AvgNOI.Lawyer = mean(NOI.Lawyer)) %>% arrange(Year) %>% select(AvgNOI.Lawyer))
# NOI/EqPart
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(2, NOI.eqPart) %>% filter(NOI.eqPart == min(NOI.eqPart)) %>% summarise(AvgNOI.EqPart = mean(NOI.eqPart)) %>% arrange(Year) %>% select(AvgNOI.EqPart))

# Top 10
# GrossRev
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(10, GrossRev) %>% filter(GrossRev == min(GrossRev)) %>% summarise(AvgRev = mean(GrossRev)) %>% arrange(Year) %>% select(AvgRev))
# Rev/Lawyer
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(10, Rev.Lawyer) %>% filter(Rev.Lawyer == min(Rev.Lawyer)) %>% summarise(AvgRev.Lawyer = mean(Rev.Lawyer)) %>% arrange(Year) %>% select(AvgRev.Lawyer))
# NOI/Lawyer
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(10, NOI.Lawyer) %>% filter(NOI.Lawyer == min(NOI.Lawyer)) %>% summarise(AvgNOI.Lawyer = mean(NOI.Lawyer)) %>% arrange(Year) %>% select(AvgNOI.Lawyer))
# NOI/EqPart
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(10, NOI.eqPart) %>% filter(NOI.eqPart == min(NOI.eqPart)) %>% summarise(AvgNOI.EqPart = mean(NOI.eqPart)) %>% arrange(Year) %>% select(AvgNOI.EqPart))

# Top 50
# GrossRev
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(50, GrossRev) %>% filter(GrossRev == min(GrossRev)) %>% summarise(AvgRev = mean(GrossRev)) %>% arrange(Year) %>% select(AvgRev))
# Rev/Lawyer
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(50, Rev.Lawyer) %>% filter(Rev.Lawyer == min(Rev.Lawyer)) %>% summarise(AvgRev.Lawyer = mean(Rev.Lawyer)) %>% arrange(Year) %>% select(AvgRev.Lawyer))
# NOI/Lawyer
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(50, NOI.Lawyer) %>% filter(NOI.Lawyer == min(NOI.Lawyer)) %>% summarise(AvgNOI.Lawyer = mean(NOI.Lawyer)) %>% arrange(Year) %>% select(AvgNOI.Lawyer))
# NOI/EqPart
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(50, NOI.eqPart) %>% filter(NOI.eqPart == min(NOI.eqPart)) %>% summarise(AvgNOI.EqPart = mean(NOI.eqPart)) %>% arrange(Year) %>% select(AvgNOI.EqPart))

# Top 100
# GrossRev
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(100, GrossRev) %>% filter(GrossRev == min(GrossRev)) %>% summarise(AvgRev = mean(GrossRev)) %>% arrange(Year) %>% select(AvgRev))
# Rev/Lawyer
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(100, Rev.Lawyer) %>% filter(Rev.Lawyer == min(Rev.Lawyer)) %>% summarise(AvgRev.Lawyer = mean(Rev.Lawyer)) %>% arrange(Year) %>% select(AvgRev.Lawyer))
# NOI/Lawyer
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(100, NOI.Lawyer) %>% filter(NOI.Lawyer == min(NOI.Lawyer)) %>% summarise(AvgNOI.Lawyer = mean(NOI.Lawyer)) %>% arrange(Year) %>% select(AvgNOI.Lawyer))
# NOI/EqPart
correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(100, NOI.eqPart) %>% filter(NOI.eqPart == min(NOI.eqPart)) %>% summarise(AvgNOI.EqPart = mean(NOI.eqPart)) %>% arrange(Year) %>% select(AvgNOI.EqPart))

# Aggregated M&A
correlRanks = cbind(correlRanks, df %>% select(Year, AggMnA) %>% unique() %>% arrange(Year) %>% select(AggMnA))

# GDP
correlRanks$GDP = gdp

names(correlRanks) = c("Year", 
                       "Rank1.Rev", "Rank1.Rev.Lawyer", "Rank1.NOI.Lawyer", "Rank1.NOI.EqPart", 
                       "Rank2.Rev", "Rank2.Rev.Lawyer", "Rank2.NOI.Lawyer", "Rank2.NOI.EqPart", 
                       "Rank10.Rev", "Rank10.Rev.Lawyer", "Rank10.NOI.Lawyer", "Rank10.NOI.EqPart",
                       "Rank50.Rev", "Rank50.Rev.Lawyer", "Rank50.NOI.Lawyer", "Rank50.NOI.EqPart",
                       "Rank100.Rev", "Rank100.Rev.Lawyer", "Rank100.NOI.Lawyer", "Rank100.NOI.EqPart",
                       "AggMnA", "GDP")

# Only after 1998 there was more than 100 firms!
lessThan100 = df %>% group_by(Year) %>% summarise(lessThan100 = n()<=100) %>% filter (lessThan100 == TRUE) %>% select(Year) %>% as.list()
greaterThan100 = df %>% group_by(Year) %>% summarise(lessThan100 = n()>100) %>% filter (lessThan100 == TRUE) %>% select(Year) %>% as.list()
correlRanks[which(correlRanks$Year %in% lessThan100$Year), c("Rank100.Rev", "Rank100.Rev.Lawyer", "Rank100.NOI.Lawyer", "Rank100.NOI.EqPart")] = NA

# Plots
# -------------------------------------------------------------------------------------------------------------
# Aggregated M&A
# -------------------------------------------------------------------------------------------------------------

# First part
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(correlRanks$Year, correlRanks$Rank1.NOI.EqPart, type='l', col='brown2', lwd=2, ylab="NOI / Equity Partner", xlab="Year", 
     main="Ranked Firms & Agg M&A", ylim = c(min(correlRanks$Rank50.NOI.EqPart, na.rm=TRUE), max(correlRanks$Rank1.NOI.EqPart, na.rm=TRUE))) # first plot

#Lines
lines(correlRanks$Year, correlRanks$Rank2.NOI.EqPart, col='magenta', lwd=1)
lines(correlRanks$Year, correlRanks$Rank10.NOI.EqPart, col='darkgreen', lwd=1)
lines(correlRanks$Year, correlRanks$Rank50.NOI.EqPart, col='darkgoldenrod4', lwd=1)
lines(correlRanks$Year, correlRanks$Rank100.NOI.EqPart, col='blue', lwd=1)
legend(1984, 6.6e+06, c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100", "Agg M&A"),# places a legend at the appropriate place
       lwd=c(2, 2, 2, 2, 2, 4), lty=c(1, 1, 1, 1, 1, 6), 
       col=c("brown2", "magenta", "darkgreen", "darkgoldenrod4", "blue", "black"), cex=1) # gives the legend lines 

# Second  part (agg M&A)
par(new = TRUE)
plot(correlRanks$Year, correlRanks$AggMnA, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd=4, lty=6, col="black")
axis(side=4, at = pretty(range(correlRanks$AggMnA, na.rm = TRUE)))
mtext("Agg M&A", side=4, line=3)



# -------------------------------------------------------------------------------------------------------------
# GDP
# -------------------------------------------------------------------------------------------------------------

# First part
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(correlRanks$Year, correlRanks$Rank1.NOI.EqPart, type='l', col='brown2', lwd=2, ylab="NOI / Equity Partner", xlab="Year", 
     main="Ranked Firms & GDP", ylim = c(min(correlRanks$Rank50.NOI.EqPart, na.rm=TRUE), max(correlRanks$Rank1.NOI.EqPart, na.rm=TRUE))) # first plot

#Lines
lines(correlRanks$Year, correlRanks$Rank2.NOI.EqPart, col='magenta', lwd=1)
lines(correlRanks$Year, correlRanks$Rank10.NOI.EqPart, col='darkgreen', lwd=1)
lines(correlRanks$Year, correlRanks$Rank50.NOI.EqPart, col='darkgoldenrod4', lwd=1)
lines(correlRanks$Year, correlRanks$Rank100.NOI.EqPart, col='blue', lwd=1)
legend(1984, 6.6e+06, c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100", "GDP"),# places a legend at the appropriate place
       lwd=c(2, 2, 2, 2, 2, 4), lty=c(1, 1, 1, 1, 1, 6), 
       col=c("brown2", "magenta", "darkgreen", "darkgoldenrod4", "blue", "black"), cex=1) # gives the legend lines 

# Second  part (agg M&A)
par(new = TRUE)
plot(correlRanks$Year, correlRanks$GDP, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd=4, lty=6, col="black")
axis(side=4, at = pretty(range(correlRanks$GDP, na.rm = TRUE)))
mtext("GDP", side=4, line=3)





# Calculating the correlations
correlations.Rev = data.frame(row.names = c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100"))
correlations.Rev = cbind(correlations.Rev, NA, NA)
names(correlations.Rev) = c("Correl_Rev_AggMA", "Correl_Rev_GDP")
dfRank.Rev = correlRanks %>% select(Rank1.Rev, Rank2.Rev, Rank10.Rev, Rank50.Rev, Rank100.Rev, AggMnA, GDP)

correlations.Rev.Lawyer = data.frame(row.names = c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100"))
correlations.Rev.Lawyer = cbind(correlations.Rev.Lawyer, NA, NA)
names(correlations.Rev.Lawyer) = c("Correl_Rev_AggMA", "Correl_Rev_GDP")
dfRank.Rev.Lawyer = correlRanks %>% select(Rank1.Rev.Lawyer, Rank2.Rev.Lawyer, Rank10.Rev.Lawyer, Rank50.Rev.Lawyer, Rank100.Rev.Lawyer, AggMnA, GDP)

correlations.NOI.Lawyer = data.frame(row.names = c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100"))
correlations.NOI.Lawyer = cbind(correlations.NOI.Lawyer, NA, NA)
names(correlations.NOI.Lawyer) = c("Correl_Rev_AggMA", "Correl_Rev_GDP")
dfRank.NOI.Lawyer = correlRanks %>% select(Rank1.NOI.Lawyer, Rank2.NOI.Lawyer, Rank10.NOI.Lawyer, Rank50.NOI.Lawyer, Rank100.NOI.Lawyer, AggMnA, GDP)

correlations.NOI.EqPart = data.frame(row.names = c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100"))
correlations.NOI.EqPart = cbind(correlations.NOI.EqPart, NA, NA)
names(correlations.NOI.EqPart) = c("Correl_Rev_AggMA", "Correl_Rev_GDP")
dfRank.NOI.EqPart = correlRanks %>% select(Rank1.NOI.EqPart, Rank2.NOI.EqPart, Rank10.NOI.EqPart, Rank50.NOI.EqPart, Rank100.NOI.EqPart, AggMnA, GDP)

for(i in i:5){
  correlations.Rev[i,1] = cor(cbind(dfRank.Rev[,i], dfRank.Rev$AggMnA) %>% na.omit())[2, 1]
  correlations.Rev[i,2] = cor(cbind(dfRank.Rev[,i], dfRank.Rev$GDP) %>% na.omit())[2, 1]
  
  correlations.Rev.Lawyer[i,1] = cor(cbind(dfRank.Rev.Lawyer[,i], dfRank.Rev.Lawyer$AggMnA) %>% na.omit())[2, 1]
  correlations.Rev.Lawyer[i,2] = cor(cbind(dfRank.Rev.Lawyer[,i], dfRank.Rev.Lawyer$GDP) %>% na.omit())[2, 1]
  
  correlations.NOI.Lawyer[i,1] = cor(cbind(dfRank.NOI.Lawyer[,i], dfRank.NOI.Lawyer$AggMnA) %>% na.omit())[2, 1]
  correlations.NOI.Lawyer[i,2] = cor(cbind(dfRank.NOI.Lawyer[,i], dfRank.NOI.Lawyer$GDP) %>% na.omit())[2, 1]
  
  correlations.NOI.EqPart[i,1] = cor(cbind(dfRank.NOI.EqPart[,i], dfRank.NOI.EqPart$AggMnA) %>% na.omit())[2, 1]
  correlations.NOI.EqPart[i,2] = cor(cbind(dfRank.NOI.EqPart[,i], dfRank.NOI.EqPart$GDP) %>% na.omit())[2, 1]
}



# Plot correlations
plot(correlations.Rev$Correl_Rev_AggMA, type='b', lwd=3, col='red', ylab = "Correlation", ylim=c(0, 1), main='Correlation with Agg M&A', xaxt='n', xlab="")
lines(correlations.Rev.Lawyer$Correl_Rev_AggMA, lwd=3, col='blue', type='b')
lines(correlations.NOI.Lawyer$Correl_Rev_AggMA, lwd=3, col='darkgreen', type='b')
lines(correlations.NOI.EqPart$Correl_Rev_AggMA, lwd=3, col='darkgoldenrod', type='b')
axis(1, at=seq(1, nrow(correlations.Rev)), labels=row.names(correlations.Rev))
legend(1, 0.4, c("Rev", "Rev/Lawyer", "NOI/Lawyer", "NOI/EqPart"), lwd=c(3, 3, 3, 3), col=c("red", "blue", "darkgreen", "darkgoldenrod"), cex=1) # gives the legend lines 

plot(correlations.Rev$Correl_Rev_GDP, type='b', lwd=3, col='red', ylab = "Correlation", ylim=c(0.8, 1), main='Correlation with GDP', xaxt='n', xlab="")
lines(correlations.Rev.Lawyer$Correl_Rev_GDP, lwd=3, col='blue', type='b')
lines(correlations.NOI.Lawyer$Correl_Rev_GDP, lwd=3, col='darkgreen', type='b')
lines(correlations.NOI.EqPart$Correl_Rev_GDP, lwd=3, col='darkgoldenrod', type='b')
axis(1, at=seq(1, nrow(correlations.Rev)), labels=row.names(correlations.Rev))
legend(1, 0.875, c("Rev", "Rev/Lawyer", "NOI/Lawyer", "NOI/EqPart"), lwd=c(3, 3, 3, 3), col=c("red", "blue", "darkgreen", "darkgoldenrod"), cex=1) # gives the legend lines 

