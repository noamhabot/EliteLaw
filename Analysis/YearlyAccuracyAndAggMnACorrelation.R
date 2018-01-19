# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(grid)

# clear the current workspace
#rm(list = ls())

# Set the working directory
setwd("~/Google Drive/Stanford Law Project")


# This script accepts the following file generated from MnACutoff.Rmd,
# appends aggregated MnA data to it, and tests and plots their correlations

load('Data/EliteLawDf.RData')
load('Data/YearlyCutoffAccuracies.RData')
yearlyStats <- read.csv('Data/YearlyStats2.csv')
chowCutoff <- read.csv('Data/ChowCutoff.csv')

# get all the indices of unique years
indices <- order(df$Year)[!duplicated(sort(df$Year))]
AggMnADF <- data.frame(df$Year[indices], df$AggMnA[indices])
colnames(AggMnADF) <- c("Year", "AggMnA")



# add a column with the cutoff values to the AggMnADF corresponding to the correct Year
#AggMnADF <- left_join(AggMnADF, (wholeResults %>% select(YearTo, OptimalCutoff)), by = c("Year" = "YearTo"))

AggMnADF <- left_join(AggMnADF, (chowCutoff %>% select(YearTo, OptimalCutoff)), by = c("Year" = "YearTo"))
AggMnADF <- left_join(AggMnADF, (yearlyStats %>% select(Year, ZeroMnA, PositiveMnA, PercentPositive, Total)), by = c("Year"))




# remove the columns with NA's
AggMnADF <- na.omit(AggMnADF)


plotXY <- function(Year, y1, y1title, y2, y2title, maintitle) {
  plotY1 <- ggplot() +
    geom_point(aes(x = Year, y = y1), color="red", alpha = 0.75) +
    geom_line(aes(x = Year, y = y1), size = 0.5, alpha = 0.75) +
    xlab("Year") + ylab(y1title) + ggtitle(maintitle) +
    theme_minimal() +
    theme(axis.title.x = element_blank())
  plotY2 <- ggplot() +
    geom_point(aes(x = Year, y = y2), color="red", alpha = 0.75) +
    geom_line(aes(x = Year, y = y2), size = 0.5, alpha = 0.75) +
    xlab("Year") + ylab(y2title) +
    theme_minimal() +
    theme(axis.title.x = element_blank()) +
    labs(caption = paste("Correlation between the two variables:", 
                         format(cor(y1, y2, use="pairwise.complete.obs"),digits=6)))
  grid.newpage()
  grid.draw(rbind(ggplotGrob(plotY1), ggplotGrob(plotY2), size = "last"))
}


plotXY(AggMnADF$Year, AggMnADF$OptimalCutoff, "Chow Optimal Cutoff Value", 
       AggMnADF$AggMnA, "Aggregated MnA's", "Chow: Optimal Cutoff Values and Aggregated MnA's by year")

plotXY(AggMnADF$Year, AggMnADF$OptimalCutoff, "Chow Optimal Cutoff Value", 
       AggMnADF$PositiveMnA, "Positive MnA's", "Chow: Optimal Cutoff Values and Positive MnA's by year")

plotXY(AggMnADF$Year, AggMnADF$PositiveMnA, "Positive MnA's", 
       AggMnADF$AggMnA, "Aggregated MnA's", "Positive MnA's and Aggregated MnA's by year")

plotXY(AggMnADF$Year, AggMnADF$ZeroMnA, "Zero MnA's", 
       AggMnADF$AggMnA, "Aggregated MnA's", "Zero MnA's and Aggregated MnA's by year")



