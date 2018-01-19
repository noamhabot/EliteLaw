# Clear environment
rm(list = ls())



# Load libraries
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(multiwayvcov)



# Load dataframe
load('Data/EliteLawDf.RData')



# Get the main variables
commonCovariates = df %>% select(Year, Rank, Lawyers, Lawyers2, MnA,
                                 Equity, IPO, Leverage)
aggMnA = df %>% select(Year, AggMnA) %>% arrange(Year) %>% unique()



# Create dataframes for outputs
numTopBottom = 50

pValuesOverTime = data.frame(Year = sort(unique(df$Year)), GrossRevAll = NA, NOIAll = NA, GrossRevTop = NA, NOITop = NA, GrossRevBottom = NA, NOIBottom = NA)
coeffsOverTime = data.frame(Year = sort(unique(df$Year)), GrossRev = NA, NOI = NA, GrossRevTop = NA, NOITop = NA, GrossRevBottom = NA, NOIBottom = NA)
pValuesOverRank = data.frame(RankPerc = seq(1, 10), GrossRev = NA, NOI = NA)
coeffsOverRank = data.frame(RankPerc = seq(1, 10), GrossRev = NA, NOI = NA)



# Main loop for all models
# For debugging only--------------------
modelOutput = "GrossRev"
crossSection = "Time"
# --------------------------------------



for(modelOutput in c("GrossRev", "NOI")){
  for(crossSection in c("Time", "Rank")){
    
    # Setting the main covariates
    currDf = commonCovariates  
    
    # Setting the output
    if(modelOutput == "GrossRev"){
      currDf$Output = df$GrossRev
    } else if(modelOutput == "NOI") {
      currDf$Output = df$NOI
    }
   
    # Cleaning for missing data
    currDf = currDf %>% na.omit()
    
    # Differente analysis for each cross section
    # Time
    if(crossSection == "Time"){
      
      firstYear = min(currDf$Year)
      for(i in 1:length(unique(currDf$Year))){
        # Pick year
        currYear = firstYear + i - 1
        
        # Set dataframes
        subDf = currDf %>% filter(Year == currYear) %>% select(-Year, -Rank)
        subDfTop =currDf %>% filter(Year == currYear) %>% top_n(numTopBottom, -Rank) %>% select(-Year, -Rank)
        subDfBottom =currDf %>% filter(Year == currYear) %>% top_n(numTopBottom, Rank) %>% select(-Year, -Rank)
        
        
        
        # Run model
        #currSubDf = subDf %>% filter(Year == currYear) %>% select(-Year)
        subModel = lm(Output ~., data = subDf)
        subModelTop = lm(Output ~., data = subDfTop)
        subModelBottom = lm(Output ~., data = subDfBottom)
        
        
        
        # Adjust PValues
        vcovMatrix = vcovHC(subModel, type = "HC0")
        adjustedModel = coeftest(subModel, vcovMatrix)
        adjustedModelTop = coeftest(subModelTop, vcovMatrix)
        adjustedModelBottom = coeftest(subModelBottom, vcovMatrix)
        pos = which(names(adjustedModel[, 1])=='MnA')
        
        pValuesOverTime[i, modelOutput] = adjustedModel[pos, 4]
        pValuesOverTime[i, paste(modelOutput, "Top", sep="")] = adjustedModelTop[pos, 4]
        pValuesOverTime[i, paste(modelOutput, "Bottom", sep="")] = adjustedModelBottom[pos, 4]
        
        coeffsOverTime[i, modelOutput] = adjustedModel[pos, 1]
        coeffsOverTime[i, paste(modelOutput, "Top", sep="")] = adjustedModelTop[pos, 1]
        coeffsOverTime[i, paste(modelOutput, "Bottom", sep="")] = adjustedModelBottom[pos, 1]
      }
    # Rank
    } else if(crossSection == "Rank"){
      
      subDf = currDf %>% group_by(Year) %>% mutate(RankPerc = Rank/max(Rank)) %>% ungroup() %>% select(-Year, -Rank)
      
      for(i in 1:10){
        
        # Run model
        currSubDf = subDf %>% filter(RankPerc >= (i-1)/10) %>% filter(RankPerc < i/10) %>% select(-RankPerc)
        subModel = lm(Output ~., data = currSubDf)
        
        # Adjust PValues
        vcovMatrix = vcovHC(subModel, type = "HC0")
        adjustedModel = coeftest(subModel, vcovMatrix)
        pos = which(names(adjustedModel[, 1])=='MnA')
        
        pValuesOverRank[i, modelOutput] = adjustedModel[pos, 4]
        coeffsOverRank[i, modelOutput] = adjustedModel[pos, 1]
      }
    }
    
  }
}



# ------------------------------------------------------------------------------------------------------------
# Plot coefficients over time
# ------------------------------------------------------------------------------------------------------------
# Plotting First Per Time
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(coeffsOverTime$Year, coeffsOverTime$GrossRev, type='l', col='aquamarine4', lwd=4, ylab="M&A Coefficient", xlab="Year", 
     main="M&A Coefficient over time")#, ylim = c(min(coeffsOverTime, na.rm = TRUE), max(coeffsOverTime, na.rm = TRUE) # first plot

#Lines
lines(coeffsOverTime$Year, coeffsOverTime$NOI, col='blueviolet', lwd=4)
legend(2005, 7000, c("Gross Rev", "NOI", "Agg M&A"),# places a legend at the appropriate place
       lwd=c(4, 4, 2), lty=c(1, 1, 3), 
       col=c("aquamarine4", "blueviolet", "black"), cex=0.9) # gives the legend lines 

# Second  part (GDP)
par(new = TRUE)
plot(aggMnA$Year, aggMnA$AggMnA, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd=3, lty=3, col="black")
axis(side=4, at = pretty(range(aggMnA$AggMnA, na.rm = TRUE)))
mtext("Agg M&A", side=4, line=3)

cor(cbind(aggMnA$AggMnA, coeffsOverTime$GrossRev) %>% na.omit())[2, 1]
cor(cbind(aggMnA$AggMnA, coeffsOverTime$NOI) %>% na.omit())[2, 1]



# ------------------------------------------------------------------------------------------------------------
# Plot coefficients over rank
# ------------------------------------------------------------------------------------------------------------
plot(coeffsOverRank$RankPerc, coeffsOverRank$GrossRev, type='l', col='aquamarine4', lwd=4, ylab="M&A Coefficient", xlab="Rank Percentile", 
     main="M&A Coefficient per Rank Percentiles", xaxt="n")#, ylim = c(min(correlRanks$Bottom1), max(correlRanks$Top1))) # first plot
axis(1, at=coeffsOverRank$RankPerc,labels=coeffsOverRank$RankPerc*10)

#Lines
lines(coeffsOverRank$RankPerc, coeffsOverRank$NOI, col='blueviolet', lwd=4)
legend(2, -250, c("Gross Rev", "NOI"),# places a legend at the appropriate place
       lwd=c(4, 4), lty=c(1, 1), 
       col=c("aquamarine4", "blueviolet", "black"), cex=1) # gives the legend lines 



# ------------------------------------------------------------------------------------------------------------
# Plot PValues over time
# ------------------------------------------------------------------------------------------------------------
plot(pValuesOverTime$Year, log(pValuesOverTime$GrossRev), type='l', col='coral1', lwd=4, ylab="log(P Value)", xlab="Rank Percentile", 
     main="M&A Coefficient's P Value over Time", ylim = c(min(log(pValuesOverTime$GrossRev), log(pValuesOverTime$NOI)), 
                                                          max(log(pValuesOverTime$GrossRev), log(pValuesOverTime$NOI))))

#Lines
lines(pValuesOverTime$Year, log(pValuesOverTime$NOI), col='blue4', lwd=4)
legend(1987, -40, c("Gross Rev", "NOI"),# places a legend at the appropriate place
       lwd=c(4, 4), lty=c(1, 1), 
       col=c("coral1", "blue4"), cex=1) # gives the legend lines 





# ------------------------------------------------------------------------------------------------------------
# Plot PValues over rank
# ------------------------------------------------------------------------------------------------------------
plot(pValuesOverRank$RankPerc, log(pValuesOverRank$GrossRev), type='l', col='coral1', lwd=4, ylab="log(P Value)", xlab="Rank Percentile", 
     main="M&A Coefficient's P Value per Rank Percentile", xaxt="n")#, ylim = c(min(correlRanks$Bottom1), max(correlRanks$Top1))) # first plot
axis(1, at=coeffsOverRank$RankPerc,labels=coeffsOverRank$RankPerc*10)

#Lines
lines(pValuesOverRank$RankPerc, log(pValuesOverRank$NOI), col='blue4', lwd=4)
legend(7, -25, c("Gross Rev", "NOI"),# places a legend at the appropriate place
       lwd=c(4, 4), lty=c(1, 1), 
       col=c("coral1", "blue4", "black"), cex=1) # gives the legend lines 



# ------------------------------------------------------------------------------------------------------------
# Plot coefficients over time splitted by bottom vs top
# ------------------------------------------------------------------------------------------------------------
# Plotting First Per Time
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(coeffsOverTime$Year, coeffsOverTime$GrossRevTop, type='l', col='aquamarine4', lwd=4, ylab="M&A Coefficient", xlab="Year", 
     main="M&A Coefficient over time", ylim = c(
       min(rbind(coeffsOverTime$GrossRevBottom, coeffsOverTime$GrossRevTop), na.rm = TRUE),
       max(rbind(coeffsOverTime$GrossRevBottom, coeffsOverTime$GrossRevTop), na.rm = TRUE))) # first plot

#Lines
lines(coeffsOverTime$Year, coeffsOverTime$GrossRevBottom, col='blueviolet', lwd=4)
legend(1985, 13000, c("Top50", "Bottom50", "Agg M&A"),# places a legend at the appropriate place
       lwd=c(4, 4, 2), lty=c(1, 1, 3), 
       col=c("aquamarine4", "blueviolet", "black"), cex=0.7) # gives the legend lines 

# Second  part (GDP)
par(new = TRUE)
plot(aggMnA$Year, aggMnA$AggMnA, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd=3, lty=3, col="black")
axis(side=4, at = pretty(range(aggMnA$AggMnA, na.rm = TRUE)))
mtext("Agg M&A", side=4, line=3)

cor(cbind(aggMnA$AggMnA, coeffsOverTime$GrossRev) %>% na.omit())[2, 1]
cor(cbind(aggMnA$AggMnA, coeffsOverTime$GrossRevTop) %>% na.omit())[2, 1]
cor(cbind(aggMnA$AggMnA, coeffsOverTime$GrossRevBottom) %>% na.omit())[2, 1]







