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
load('Data/EliteLawDf2016.RData')
#load('From Dan/Data/GDP.RData')
gdpYears <- 1984:2016
GDP <- c(4.041, 4.347, 4.590, 4.870, 5.253, 5.658, 5.980, 6.174, 6.539, 6.879, 7.309, 7.664,
               8.1, 8.609, 9.089, 9.661, 10.285, 10.622, 10.978, 11.511, 12.275, 13.094, 13.856,
               14.478, 14.719, 14.419, 14.964, 15.518, 16.155, 16.692, 17.428, 18.121, 18.625)
GDP <- 100*GDP
gdpDF <- data.frame(gdpYears, GDP)

# Function to perform multiple plots with regressions
ggpairs_with_linreg <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method=lm, fill="blue", color="red", ...)
  p
}





# Summary statistics
summary = summary(df %>% select(Lawyers, Leverage, GrossRev, NOI, IPORevenue, EquityRevenue, MnARevenue,
                                Rev.Lawyer, NOI.Lawyer, NOI.eqPart))



# Scatter plots
# Fancy one
toPlot = df[, c(4, 5, 6, 8, 9, 10, 16, 19, 18, 17)]
graph = ggpairs(toPlot, lower = list(continuous = ggpairs_with_linreg), columnLabels=c("Lawyers", "Rev", "NOI", "IPO", "D&E", "M&A", "Leverage", "Rev/Law", "NOI/Law", "NOI/EqPart"))
#graph

# Standard one
#plot(df[,c(4, 5, 6, 7, 9, 10, 11, 17, 18)])



# VCov
corrDf = df %>% select(Lawyers, Leverage, GrossRev, NOI, IPORevenue,
                       EquityRevenue, MnARevenue, Rev.Lawyer, NOI.Lawyer, NOI.eqPart) %>% as.data.frame()
names(corrDf) = c("Lawyers", "Leverage", "Rev", "NOI", "IPO", "Equity", "MnA", "Rev/Law", "NOI/Law", "NOI/EqPart")
corrMat = cor(corrDf)
levelplot(corrMat, pretty=TRUE, names.attr = c("Lawyers", "Leverage", "Rev", "NOI", "IPO", "Equity", "MnA", "Rev/Law", "NOI/Law", "NOI/EqPart"))



# Correlations
correlRanks = as.data.frame(unique(sort(df$Year)))
names(correlRanks)[1] <- "Year"
for (n in c(1,2,10,50,75,100,125,150,175,200)) {
  # Top n
  # GrossRev
  correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(n, GrossRev) %>% filter(GrossRev == min(GrossRev)) %>% summarise(AvgRev = mean(GrossRev)) %>% arrange(Year) %>% select(AvgRev))
  names(correlRanks)[ncol(correlRanks)] <- paste("Rank",n,".Rev", sep="")
  # Rev/Lawyer
  correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(n, GrossRev.Lawyer) %>% filter(GrossRev.Lawyer == min(GrossRev.Lawyer)) %>% summarise(AvgGrossRev.Lawyer = mean(GrossRev.Lawyer)) %>% arrange(Year) %>% select(AvgGrossRev.Lawyer))
  names(correlRanks)[ncol(correlRanks)] <- paste("Rank",n,".Rev.Lawyer", sep="")
  # Rev/eqPart
  correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(n, GrossRev.eqPart) %>% filter(GrossRev.eqPart == min(GrossRev.eqPart)) %>% summarise(AvgGrossRev.eqPart = mean(GrossRev.eqPart)) %>% arrange(Year) %>% select(AvgGrossRev.eqPart))
  names(correlRanks)[ncol(correlRanks)] <- paste("Rank",n,".Rev.EqPart", sep="")
  
  # NOI
  correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(n, NOI) %>% filter(NOI == min(NOI)) %>% summarise(AvgNOI = mean(NOI)) %>% arrange(Year) %>% select(AvgNOI))
  names(correlRanks)[ncol(correlRanks)] <- paste("Rank",n,".NOI", sep="")
  # NOI/Lawyer
  correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(n, NOI.Lawyer) %>% filter(NOI.Lawyer == min(NOI.Lawyer)) %>% summarise(AvgNOI.Lawyer = mean(NOI.Lawyer)) %>% arrange(Year) %>% select(AvgNOI.Lawyer))
  names(correlRanks)[ncol(correlRanks)] <- paste("Rank",n,".NOI.Lawyer", sep="")
  # NOI/EqPart
  correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(n, NOI.eqPart) %>% filter(NOI.eqPart == min(NOI.eqPart)) %>% summarise(AvgNOI.EqPart = mean(NOI.eqPart)) %>% arrange(Year) %>% select(AvgNOI.EqPart))
  names(correlRanks)[ncol(correlRanks)] <- paste("Rank",n,".NOI.EqPart", sep="")
}

# Add Aggregated M&A and GDP
correlRanks = cbind(correlRanks, df %>% select(Year, AggMnA) %>% unique() %>% arrange(Year) %>% select(AggMnA))
correlRanks$GDP <- gdpDF$GDP


# Only after 1998 there was more than 100 firms!
lessThan100 = df %>% group_by(Year) %>% summarise(lessThan100 = n()<=100) %>% filter (lessThan100 == TRUE) %>% select(Year) %>% as.list()
greaterThan100 = df %>% group_by(Year) %>% summarise(lessThan100 = n()>100) %>% filter (lessThan100 == TRUE) %>% select(Year) %>% as.list()
correlRanks[which(correlRanks$Year %in% lessThan100$Year), grepl("Rank100", names(correlRanks))] = NA

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
       lwd=2, lty=c(1, 1, 1, 1, 1, 6), 
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
       lwd=2, lty=c(1, 1, 1, 1, 1, 6), 
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

correlations.Rev.EqPart = data.frame(row.names = c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100"))
correlations.Rev.EqPart = cbind(correlations.Rev.EqPart, NA, NA)
names(correlations.Rev.EqPart) = c("Correl_Rev_AggMA", "Correl_Rev_GDP")
dfRank.Rev.EqPart = correlRanks %>% select(Rank1.Rev.EqPart, Rank2.Rev.EqPart, Rank10.Rev.EqPart, Rank50.Rev.EqPart, Rank100.Rev.EqPart, AggMnA, GDP)


correlations.NOI = data.frame(row.names = c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100"))
correlations.NOI = cbind(correlations.NOI, NA, NA)
names(correlations.NOI) = c("Correl_Rev_AggMA", "Correl_Rev_GDP")
dfRank.NOI = correlRanks %>% select(Rank1.NOI, Rank2.NOI, Rank10.NOI, Rank50.NOI, Rank100.NOI, AggMnA, GDP)

correlations.NOI.Lawyer = data.frame(row.names = c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100"))
correlations.NOI.Lawyer = cbind(correlations.NOI.Lawyer, NA, NA)
names(correlations.NOI.Lawyer) = c("Correl_Rev_AggMA", "Correl_Rev_GDP")
dfRank.NOI.Lawyer = correlRanks %>% select(Rank1.NOI.Lawyer, Rank2.NOI.Lawyer, Rank10.NOI.Lawyer, Rank50.NOI.Lawyer, Rank100.NOI.Lawyer, AggMnA, GDP)

correlations.NOI.EqPart = data.frame(row.names = c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100"))
correlations.NOI.EqPart = cbind(correlations.NOI.EqPart, NA, NA)
names(correlations.NOI.EqPart) = c("Correl_Rev_AggMA", "Correl_Rev_GDP")
dfRank.NOI.EqPart = correlRanks %>% select(Rank1.NOI.EqPart, Rank2.NOI.EqPart, Rank10.NOI.EqPart, Rank50.NOI.EqPart, Rank100.NOI.EqPart, AggMnA, GDP)

for(i in 1:5){
  correlations.Rev[i,1] = cor(cbind(dfRank.Rev[,i], dfRank.Rev$AggMnA) %>% na.omit())[2, 1]
  correlations.Rev[i,2] = cor(cbind(dfRank.Rev[,i], dfRank.Rev$GDP) %>% na.omit())[2, 1]
  
  correlations.Rev.Lawyer[i,1] = cor(cbind(dfRank.Rev.Lawyer[,i], dfRank.Rev.Lawyer$AggMnA) %>% na.omit())[2, 1]
  correlations.Rev.Lawyer[i,2] = cor(cbind(dfRank.Rev.Lawyer[,i], dfRank.Rev.Lawyer$GDP) %>% na.omit())[2, 1]
  
  correlations.Rev.EqPart[i,1] = cor(cbind(dfRank.Rev.EqPart[,i], dfRank.Rev.EqPart$AggMnA) %>% na.omit())[2, 1]
  correlations.Rev.EqPart[i,2] = cor(cbind(dfRank.Rev.EqPart[,i], dfRank.Rev.EqPart$GDP) %>% na.omit())[2, 1]
  
  
  correlations.NOI[i,1] = cor(cbind(dfRank.NOI[,i], dfRank.NOI$AggMnA) %>% na.omit())[2, 1]
  correlations.NOI[i,2] = cor(cbind(dfRank.NOI[,i], dfRank.NOI$GDP) %>% na.omit())[2, 1]
  
  correlations.NOI.Lawyer[i,1] = cor(cbind(dfRank.NOI.Lawyer[,i], dfRank.NOI.Lawyer$AggMnA) %>% na.omit())[2, 1]
  correlations.NOI.Lawyer[i,2] = cor(cbind(dfRank.NOI.Lawyer[,i], dfRank.NOI.Lawyer$GDP) %>% na.omit())[2, 1]
  
  correlations.NOI.EqPart[i,1] = cor(cbind(dfRank.NOI.EqPart[,i], dfRank.NOI.EqPart$AggMnA) %>% na.omit())[2, 1]
  correlations.NOI.EqPart[i,2] = cor(cbind(dfRank.NOI.EqPart[,i], dfRank.NOI.EqPart$GDP) %>% na.omit())[2, 1]
}

colnames(correlations.Rev) <- c("AggMnA", "GDP")
colnames(correlations.Rev.Lawyer) <- c("AggMnA", "GDP")
colnames(correlations.Rev.EqPart) <- c("AggMnA", "GDP")
colnames(correlations.NOI) <- c("AggMnA", "GDP")
colnames(correlations.NOI.Lawyer) <- c("AggMnA", "GDP")
colnames(correlations.NOI.EqPart) <- c("AggMnA", "GDP")

print(xtable(correlations.Rev), file="Generate Latex/IndivTexOutput/MnAGDP-GrossRevenue.tex")
print(xtable(correlations.Rev.Lawyer), file="Generate Latex/IndivTexOutput/MnAGDP-GrossRevenuePLawyer.tex")
print(xtable(correlations.Rev.EqPart), file="Generate Latex/IndivTexOutput/MnAGDP-GrossRevenuePEqPart.tex")
print(xtable(correlations.NOI), file="Generate Latex/IndivTexOutput/MnAGDP-NOI.tex")
print(xtable(correlations.NOI.Lawyer), file="Generate Latex/IndivTexOutput/MnAGDP-NOIPLawyer.tex")
print(xtable(correlations.NOI.EqPart), file="Generate Latex/IndivTexOutput/MnAGDP-NOIPEqPart.tex")




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

