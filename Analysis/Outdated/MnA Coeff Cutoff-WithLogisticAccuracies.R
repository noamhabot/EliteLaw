############################################################################################################
# This is an analysis file to determine the cutoff points between top/bottom law firms
#     where the top/bottom is chosen according in a couple of different methods.
#
# Ways that top/bottom law firms are decided:
#   1) (Gross Revenue) / (Number of Lawyers)
#           - with a rolling cutoff from the top tier being the top 5 to the top tier being the top 120
#                 as ranked by the outcome=(Gross Revenue)/(Number of Lawyers)
#   2) Individual MnA
#           - the cutoff being Mna>0 and MnA=0
#
# Decisions:
#   1) the rolling window in number of years - determines how many years to look back from 2015
#           - i.e. if rollingWindow = 5, then data will be considered from all of 2011 to all of 2015
#   2) which variables and plots to create at the end to show the results
#
# Created by Dan Zylberglejd and Noam Habot
############################################################################################################

##### Clear environment
rm(list = ls())



##### Load libraries
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(multiwayvcov)
library(ggplot2)


##### Load dataframe
setwd("~/Google Drive/Stanford Law Project")
load('Data/EliteLawDf.RData')


##### Initialization variables

# Rolling window, in number of years
rollingWindow = 3

# Set the output
#Output = df$GrossRev/df$Lawyers
Output = df$MnA
# Set the main covariates
covariates = df %>% select(FirmName, Year, Lawyers, Lawyers2, MnA, Equity, IPO, Leverage)


# Get factor variables Year and Firm
years = data.frame(Years = as.factor(df$Year - min(df$Year) + 1))
firms = data.frame(FirmID = as.character(df$FirmID))


# Merge into one main dataset
dataset = cbind(Output, covariates)
dataset = dataset %>% na.omit()

# Dataframes for results
#results = data.frame(AdjR2=numeric(0), AIC=numeric(0), BIC=numeric(0), CV=numeric(0), MnATop=numeric(0), MnABottom=numeric(0))
results = data.frame(accuracy=numeric(0), numPredicted=numeric(0), MnATop=numeric(0), MnABottom=numeric(0))



for(cutoff in 5:120){
  # Tracking
  print(paste("Running for cutoff at firm with rank", toString(cutoff)))
  
  # Set the M&A variables according to the cutoff
  currDataset = dataset %>% filter(Year <= 2015) %>% filter(Year >= 2015 - rollingWindow + 1)# %>% select(-Year, -FirmName)
  outputCutoff = sort(currDataset$Output,partial=nrow(currDataset)-1)[nrow(currDataset)-cutoff+1] 
  
  # Create the 2 groups
  #currDataset = currDataset %>% mutate(isTop = Output>=outputCutoff) 
  currDataset = currDataset %>% mutate(isTop = Output>=outputCutoff) 

  # Run model
  model = glm(isTop ~ ., data = currDataset[,-2])
  
  # Extract results
  coeffs = summary(model)$coefficients[, 1]
  predictions = predict(model, newdata=currDataset[,-2], type="response")
  predictions = predictions > 0.5
  accuracy = sum(predictions==currDataset$isTop) / nrow(currDataset)
  #which(predictions)
  
  
  mnaCoeffTop = coeffs["MnATop"]
  mnaCoeffBottom = coeffs["MnABottom"]
  
  # Populate results table
  results[cutoff, "accuracy"] =  accuracy 
  results[cutoff, "numPredicted"] = sum(predictions)
  results[cutoff, "MnATop"] = mnaCoeffTop
  results[cutoff, "MnABottom"] = mnaCoeffBottom
}
results["name"] = 1:120


ggplot(data=results, aes(x=1:120, y=accuracy)) +
  geom_line(color="red")+
  geom_point() + ggtitle("Accuracy vs. Cutoff") + xlab("Cutoff") + ylab("Accuracy") +
  geom_text(data=subset(results, name>=31 & name <= 38), aes(x=31:38, accuracy,label=name))


ggplot(data=results, aes(x=1:120, y=numPredicted)) +
  geom_line(color="blue")+
  geom_point() + ggtitle("Number Predicted to be in Top vs. Cutoff") + xlab("Cutoff") + ylab("Number Predicted to be in Top") +
  geom_text(data=subset(results, name>=31 & name <= 38), aes(x=31:38, numPredicted,label=name))
