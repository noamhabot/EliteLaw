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
  excludeLawyers = c("WithLawyers", "WithLawyers2", "WithLawyersLog", "WithoutLawyers")
  dealsAndOrRevenues = c("Both", "Revenue", "Deals")
  firmFixedEffects = c("FirmFE", "NoFirmFE", "Lawyers")
  fixedEffects = c("FE3", "FE1", "FEYear", "NoFE")
  
  
  #modelOutput = "NOI.eqPart"
  #outputDenominator = "NoRatio"
  
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
  
  
  
  
  #  -------------------------------------------------------------------------------------------------
  # Main loop for all models
  #  -------------------------------------------------------------------------------------------------
  
  # For debugging only--------------------
  dealsAndOrRevenue = dealsAndOrRevenues[1]
  modelOutput = modelOutputs[1]
  outputDenominator = outputDenominators[1]
  excludeLawyer = excludeLawyers[1]
  firmFixedEffect = firmFixedEffects[1]
  fixedEffect = fixedEffects[1]
  # --------------------------------------
  
  models <- list()
  counter <- 1
  for(modelOutput in modelOutputs) {
    for (outputDenominator in outputDenominators){
      for(dealsAndOrRevenue in dealsAndOrRevenues){
        for(excludeLawyer in excludeLawyers){
          # for(firmFixedEffect in firmFixedEffects){
          #   for(fixedEffect in fixedEffects){
              if (outputDenominator == "PerLawyer" && (modelOutput == "NOI.eqPart" || modelOutput == "GrossRev.eqPart")) {
                next
              }
              
              # Set current configuration
              resultConfig = paste(modelOutput, outputDenominator, dealsAndOrRevenue, excludeLawyer, sep = '_')
              print(paste(counter, resultConfig))
              
              
              
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
              
              # # Set the variables regarding the fixed effects
              # if(fixedEffect == "FE3"){
              #   
              #   # Skip if we are regressing only lawyers
              #   if(firmFixedEffect ==  "Lawyers"){ next }
              #   currDf = cbind(currDf, AggMnA = df$AggMnA, AggEquity = df$AggEquity, AggIPO = df$AggIPO)
              #   
              #   # Check for 0 values and replace them with NA
              #   
              #   if (sum(currDf$AggMnA==0,na.rm=TRUE) > 0) {
              #     currDf[currDf$AggMnA==0,]$AggMnA <- NA
              #   }
              #   if (sum(currDf$AggEquity==0) > 0) {
              #     currDf[currDf$AggEquity==0,]$AggEquity <- NA
              #   }
              #   if (sum(currDf$AggIPO==0) > 0) {
              #     currDf[currDf$AggIPO==0,]$AggIPO <- NA
              #   }
              #   
              # } else if(fixedEffect == "FE1"){
              #   
              #   # Skip if we are regressing only lawyers
              #   if(firmFixedEffect ==  "Lawyers"){ next }
              #   
              #   currDf = cbind(currDf, AggMnA = df$AggMnA)
              #   # Check for 0 values and replace them with NA
              #   if (sum(currDf$AggMnA==0) > 0) {
              #     currDf[currDf$AggMnA==0,]$AggMnA <- NA
              #   }
              # } else if(fixedEffect == "FEYear"){
              #   
              #   # Skip if we are regressing only lawyers
              #   if(firmFixedEffect ==  "Lawyers"){ next }
              #   
              #   currDf = cbind(currDf, years)
              # }
              
            
              
              
              # Exclude Lawyers if necessary
              if(excludeLawyer == "WithoutLawyers"){
                currDf = currDf %>% select(-Lawyers, -Lawyers2, -LawyersLog)
              } else if (excludeLawyer == "WithLawyers2"){
                currDf = currDf %>% select(-LawyersLog, -Lawyers)
              } else if (excludeLawyer == "WithLawyersLog"){
                currDf = currDf %>% select(-Lawyers2, -Lawyers)
              } else if (excludeLawyer == "WithLawyers"){
                currDf = currDf %>% select(-Lawyers2, -LawyersLog)
              }
              
              
              # Remove deals if selected "Revenue" or vice versa
              if (dealsAndOrRevenue == "Deals") {
                currDf = currDf %>% select(-MnARevenue, -IPORevenue, -EquityRevenue)
              } else if (dealsAndOrRevenue == "Revenue") {
                currDf = currDf %>% select(-MnAIssues, -IPOIssues, -EquityIssues)
              }
              
              
              # # Setting the variables regarding the fixed firm effects
              # if(firmFixedEffect == "FirmFE"){
              #   currDf$FirmID = firms$FirmID
              # } else if(firmFixedEffect == "Lawyers"){
              #   if(excludeLawyer == "WithoutLawyers"){ 
              #     next 
              #   } else if (excludeLawyer == "WithLawyers") {
              #     currDf = currDf %>% select(Output, Lawyers)
              #   } else if (excludeLawyer == "WithLawyers2") {
              #     currDf = currDf %>% select(Output, Lawyers2)
              #   } else if (excludeLawyer == "WithLawyersLog") {
              #     currDf = currDf %>% select(Output, LawyersLog)
              #   }
              #   
              # }

              
              # Cleaning for missing data
              currDf = currDf %>% na.omit()
              
              
              
              # Run the linear regression
              currModel = lm(Output ~., data = currDf[ , !(names(currDf) %in% c("FirmName"))])
              models[[counter]] <- currModel
              counter <- counter + 1
          #   }
          # }
        }
      }
    }
  }
  
  
  
  
  outcomes <- c("Gross Rev", "Gross Rev/Lawyer", "GrossRev/Eq Partner", "NOI", "NOI/Lawyer", "NOI/Eq Partner")
  i <- 0
  result <- data.frame()
  for (outcome in outcomes) {
    start <- (i*12)+1
    end <- (i+1)*12
    avg <- model.avg(models[c(start:end)], revised.var = TRUE)
    
    avgCoeffs <- round(avg$coefficients, 3)
    
    ct <- coefTable(avg)
    pvals1 <- format(round(pnorm(-abs(ct[,1]/ct[,2]))*2,3), nsmall = 3)
    pvals1 <- paste("(",pvals1,")",sep="")
    pvals2 <- pvals1
    
    newRows <- cbind(rep(outcome, 2),c("full", "subset"),avgCoeffs)
    # add the pvalues underneath
    newRows <- rbind(newRows[1,], c("", "", pvals1), newRows[2,], c("", "", pvals2))
    result <- rbind(result, newRows)
    i <- i + 1
  }
  row.names(result) <- c()
  names(result)[1:2] <- c("Outcome", "Full/Subset")
  
  result <- result[,c(1:4,ncol(result)-1, ncol(result), 7:ncol(result)-2)]
  
  names(result) <- gsub("LawyersLog", "log(Lawyers)", names(result))
  names(result) <- gsub("Revenue", " Deal Value", names(result))
  names(result) <- gsub("Issues", " Transactions", names(result))
  names(result) <- gsub("\\(Intercept\\)", "Intercept", names(result))
  
  result <- apply(result, 2, as.character)
  
  
  # these are 0.1, 1, 5 because we previously multiplied all of the p-values by 100
  # in the journal, this is the correct format:
  # ** p < 0.01
  # * p < 0.05
  # + p < 0.10
  for (i in 1:(nrow(result)/2)) {
    for (j in 3:ncol(result)) {
      coeffRow <- (2*i)-1
      pvalRow <- (2*i)
      # remove the parenthesis from the numbers
      pval <- as.numeric(gsub("[^0-9.]", "", as.character(result[pvalRow, j])))
      if (is.na(pval)) {
        next;
      }
      if(pval <= 0.01){ # 0.1
        result[coeffRow, j] = paste(result[coeffRow, j], "**", sep="")
      } else if(pval <= 0.05){ # 1
        result[coeffRow, j] = paste(result[coeffRow, j], "*", sep="")
      } else if(pval <= 0.10){ # 5
        print(paste(i,j))
        result[coeffRow, j] = paste(result[coeffRow, j], "$^{+}$", sep="")
      }
    }
  }
  
  
  
  captionText <- "The entries in this table are coefficients. For each outcome variable, 
                  we have 2 rows - one is a \"full\" model, and one is a \"subsetted\" model.
                  When performing the model averaging, the full one treats variables missing from the model as 0's,
                  whereas the subset model averages coefficients only where that variable appears."
  
  print(xtable(result[,1:9]), file="IndivTexOutput/ModelAveragingCoefs1.tex", sanitize.text.function=function(x){x})
  print(xtable(result[,10:13], caption=captionText), file="IndivTexOutput/ModelAveragingCoefs2.tex", sanitize.text.function=function(x){x})

}
