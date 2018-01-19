############################################################################################################
# This is a visualization file that plots a density scatter plot of Individual MnA per Year
#
# It also creates a data frame, "yearlyStats", that incorporates various statistics
#     about the individual MnA variable per year, such as:
#     Min MnA, Max MnA, Number of Positive MnA's (> 0),
#     Total data points for that year, and Proportion of firms that had positive MnA's
#
# Decisions:
#   1) the interval ranges
#   2) the range of how to determine the coloring
#           - now, the color gradient ranges from 1 to the 3rd quantile of frequencies (with MnA>0)
#           - which colors to use in the gradient
#           - whether or not to include the MnA=0 data points in the coloring (will skew coloring)
#   3) set a maximum frequency to limit the range of frequencies.
#           - this will allow differences in sizes to be more visible
#
# Created by Noam Habot
############################################################################################################

# Clear environment
rm(list = ls())


# Load libraries
library(dplyr)
library(ggplot2)


# Load dataframe
setwd("~/Google Drive/Stanford Law Project")
load('Data/EliteLawDf.RData')


# Initialization variables
#interval <- c(0, 1000, 10000, 30000, 50000, 70000, 90000, 120000, 200000)

# The "interval" variable determines the ranges to subset the data
# i.e. 0, 20000 will consider all MnA values between 0<MnA<=20000 and 
# provide both the frequency and mean of those in order to plot them.
# This allows for the size and color parameters to be included in the plot.
interval <- c(0, 20000, 40000, 60000, 80000, 100000, 150000, 200000, 300000)
byInterval <- FALSE
numFirmsPerBin <- 10

# Create the data frames
subDF <- df[, c("Year", "MnA")]
freqDF <- data.frame(Year= numeric(0), MnA= numeric(0), Freq=integer(0))
yearlyStats <- data.frame(Year=numeric(0), MinMnA=numeric(0), MinPositiveMnA=numeric(0), 
                          MaxMnA=numeric(0), ZeroMnA=numeric(0), PositiveMnA=numeric(0), Total=numeric(0), 
                          PercentPositive=numeric(0))



for (y in unique(df$Year)) {
  thisDF <- subset(subDF, Year==y)
  
  # update the yearly stats table
  yearlyStats <- rbind(yearlyStats, data.frame("Year"=y, MinMnA=min(thisDF$MnA), MinPositiveMnA=min(subset(thisDF, MnA>0)$MnA), 
                                               MaxMnA=max(thisDF$MnA), ZeroMnA=nrow(subset(thisDF, MnA==0)), PositiveMnA=nrow(subset(thisDF, MnA>0)), 
                                               Total=nrow(thisDF), PercentPositive=nrow(subset(thisDF, MnA>0))/nrow(thisDF)))
  
  # all those that are equal to 0
  freqDF <- rbind(freqDF, data.frame("Year"=y, "MnA"=mean(thisDF$MnA[thisDF$MnA == 0]), "Freq"=length(thisDF$MnA[thisDF$MnA == 0])))
  
  if (byInterval) {
    for (i in 1:(length(interval)-1)) {
      q = thisDF$MnA[thisDF$MnA > interval[i] & thisDF$MnA <= interval[i+1]]
      if (length(q) > 0) {
        freqDF <-rbind(freqDF, data.frame("Year"=y, "MnA"=mean(q), "Freq"=length(q)))
      }
    }
    
    # add all the rest of the points
    numAboveMax = nrow(thisDF[thisDF$MnA>interval[length(interval)],])
    if (numAboveMax > 0) {
      theRest <- cbind(thisDF[thisDF$MnA>interval[length(interval)],], Freq=rep(1, numAboveMax))
      freqDF <- rbind(freqDF, theRest) 
    }
  } else {
    # do this by bins, put 10 in one bin
    for (i in 1: (floor(nrow(thisDF) / numFirmsPerBin )+1) ) {
      q <- na.omit(thisDF$MnA[((i-1)*numFirmsPerBin+1):(i*numFirmsPerBin)])
      freqDF <-rbind(freqDF, data.frame("Year"=y, "MnA"=mean(q), "Freq"=length(q)))
    }
  }

  
}
maxFreq = max(subset(freqDF, MnA>0)$Freq)
freqDF[freqDF$MnA==0 & freqDF$Freq>maxFreq,]$Freq = maxFreq

if (byInterval) {
  ggplot(freqDF, aes(factor(Year), MnA)) + geom_point(aes(size=Freq, color=Freq)) + 
    scale_color_gradient(low="lightblue", high="black", limits=c(1,quantile(subset(freqDF, MnA>0)$Freq, .75))) +
    ggtitle("Density of Individual MnA per Year") + xlab("Year") + ylab("Individual MnA") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
} else {
  # we are doing it by the bins, so all the same size
  ggplot(freqDF, aes(factor(Year), MnA)) + geom_point(aes(color=MnA)) + 
    scale_color_gradient(low="lightblue", high="black") +
    ggtitle("Density of Individual MnA per Year") + xlab("Year") + ylab("Individual MnA") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
View(yearlyStats)

#write.csv(yearlyStats, file = "Data/YearlyStats3.csv")
