# Clear environment
#rm(list = ls())



# Load libraries
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(MuMIn)



# Load dataframe
setwd("~/Google Drive/EliteLaw/Generate Latex/")
load('../Data/EliteLawDf2016.RData')
#load('Data/EliteLawDf.RData')




# -------------------------------------------------------------------------------------------------
# Current Analysis
# -------------------------------------------------------------------------------------------------

saveModelAveraging <- function(df) {
  print("Performing Model Averages")
  
  # Set the possible Configurations
  modelOutputs = c("GrossRev", "GrossRev.eqPart", "NOI", "NOI.eqPart")
  outputDenominators = c("NoRatio", "PerLawyer")
  excludeLawyers = c("WithLawyers2", "WithLawyersLog", "WithoutLawyers")
  dealsAndOrRevenues = c("Both", "Revenue", "Deals")
  
  modelOutput = "NOI.eqPart"
  outputDenominator = "NoRatio"
  
  # Get the main variables
  commonCovariates = df %>% select(Lawyers, Lawyers2, Leverage, FirmName, MnARevenue, EquityRevenue, IPORevenue, MnANumOfDeals, IPOIssues, EquityIssues)
  # hack to change Lawyers^2 to log(lawyers)
  commonCovariates = commonCovariates %>% mutate(LawyersLog = log(Lawyers), MnAIssues=MnANumOfDeals) %>% select(-MnANumOfDeals)
  commonCovariates <- commonCovariates[, c(1:2, 10, 3:9, 11)]
  
  # Create the categorical variables
  # Year
  years = data.frame(Years = as.factor(df$Year - min(df$Year) + 1))
  # Firm
  firms = data.frame(FirmID = as.character(df$FirmID))
  
  
  
  
  
  
  
  # Set configurations for Cross Validation
  set.seed(1)
  nCV = 10
  
  
  
  #  -------------------------------------------------------------------------------------------------
  # Main loop for all models
  #  -------------------------------------------------------------------------------------------------
  
  # For debugging only--------------------
  dealsAndOrRevenue = dealsAndOrRevenues[1]
  modelOutput = modelOutputs[1]
  outputDenominator = outputDenominators[1]
  excludeLawyer = excludeLawyers[1]
  # --------------------------------------
  
  models <- list()
  counter <- 1
  for(modelOutput in modelOutputs) {
    for (outputDenominator in outputDenominators){
      for(dealsAndOrRevenue in dealsAndOrRevenues){
        for(excludeLawyer in excludeLawyers){
              
              if (outputDenominator == "PerLawyer" && (modelOutput == "NOI.eqPart" || modelOutput == "GrossRev.eqPart")) {
                next
              }
              
              # Set current configuration
              resultConfig = paste(counter,modelOutput, outputDenominator, dealsAndOrRevenue, excludeLawyer, sep = '_')
              #print(resultConfig)
              
              
              
              # Setting the main covariates
              currDf = commonCovariates  
              
              # Setting the output numerator
              if(modelOutput == "GrossRev"){
                currDf$Output = df$GrossRev
              } else if(modelOutput == "NOI") {
                currDf$Output = df$NOI
              } else if(modelOutput == "NOI.eqPart"){
                currDf$Output = df$NOI.eqPart
              } else if(modelOutput == "GrossRev.eqPart"){
                currDf$Output = df$GrossRev.eqPart
              }
              
              # Setting the output denominator
              if(outputDenominator == "PerLawyer"){
                currDf$Output = currDf$Output / currDf$Lawyers
              }
              
            
              
              
              # Exclude Lawyers if necessary
              if(excludeLawyer == "WithoutLawyers"){
                currDf = currDf %>% select(-Lawyers, -Lawyers2, -LawyersLog)
              } else if (excludeLawyer == "WithLawyers2"){
                currDf = currDf %>% select(-LawyersLog)
              } else if (excludeLawyer == "WithLawyersLog"){
                currDf = currDf %>% select(-Lawyers2)
              }
              
              
              # Remove deals if selected "Revenue" or vice versa
              if (dealsAndOrRevenue == "Deals") {
                currDf = currDf %>% select(-MnARevenue, -IPORevenue, -EquityRevenue)
              } else if (dealsAndOrRevenue == "Revenue") {
                currDf = currDf %>% select(-MnAIssues, -IPOIssues, -EquityIssues)
              }
              
              
  
              
              
              
              
              # Cleaning for missing data
              currDf = currDf %>% na.omit()
              
              
              
              # Run the linear regression
              currModel = lm(Output ~., data = currDf[ , !(names(currDf) %in% c("FirmName"))])
              models[[counter]] <- currModel
              counter <- counter + 1
              
        }
      }
    }
  }
  
  
  
  
  outcomes <- c("Gross Rev", "Gross Rev/Lawyer", "GrossRev/Eq Partner", "NOI", "NOI/Lawyer", "NOI/Eq Partner")
  i <- 0
  result <- data.frame()
  for (outcome in outcomes) {
    start <- i*9+1
    end <- i*9+9
    avg <- model.avg(models[c(start:end)])
    
    avgCoeffs <- round(avg$coefficients, 3)
    
    newRows <- cbind(rep(outcome, 2),c("full", "subset"),avgCoeffs)
    result <- rbind(result, newRows)
    i <- i + 1
  }
  row.names(result) <- c()
  names(result)[1:2] <- c("Outcome", "Full/Subset")
  
  captionText <- "The entries in this table are coefficients. For each outcome variable, 
                  we have 2 rows - one is a \"full\" model, and one is a \"subsetted\" model.
                  When performing the model averaging, the full one treats variables missing from the model as 0's,
                  whereas the subset model averages coefficients only where that variable appears."
  
  print(xtable(result[,1:7]), file="Generate Latex/IndivTexOutput/ModelAveragingCoefs1.tex")
  print(xtable(result[,8:13], caption=captionText), file="Generate Latex/IndivTexOutput/ModelAveragingCoefs2.tex")

}
