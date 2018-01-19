# Clear environment
rm(list = ls())



# Load libraries
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(multiwayvcov)



# Load dataframe
setwd("~/Google Drive/Stanford Law Project/")
load('Data/EliteLawDf2016.RData')
source('From Dan/CV.R')




# -------------------------------------------------------------------------------------------------
# Current Analysis
# -------------------------------------------------------------------------------------------------

# Set the possible Configurations
modelOutputs = c("GrossRev", "NOI", "NOI.eqPart")
outputDenominators = c("PerLawyer", "NoRatio")
firmFixedEffects = c("FirmFE", "NoFirmFE", "Lawyers")
excludeLawyers = c("WithLawyers", "WithoutLawyers")
fixedEffects = c("FE3", "FE1", "FEYear", "NoFE")



# Get the main variables
commonCovariates = df %>% select(Lawyers, Lawyers2, MnARevenue, Equity, IPO, Leverage)



# Create the categorical variables
# Year
years = data.frame(Years = as.factor(df$Year - min(df$Year) + 1))
# Firm
firms = data.frame(FirmID = as.character(df$FirmID))



# Prepare the table for the results
covariates = c("Lawyers", "Lawyers2", "AggMnA", "AggEquity", "AggIPO",
               "MnA", "Equity", "IPO", "Leverage", "(Intercept)")
modelAcessment = c('Adj_R_2', 'AIC / 10e+2', 'BIC / 10e+2', 'CV / 10e+7')

resultsCoeffs = data.frame(row.names = c(covariates, 'Observations', 'R2', 'Adj R2'))
resultsTValues = data.frame(row.names = covariates)
resultsPValues = data.frame(row.names = covariates)
resultsPerformance = data.frame(row.names = modelAcessment)



# Set configurations for Cross Validation
set.seed(1)
nCV = 10



#  -------------------------------------------------------------------------------------------------
# Main loop for all models
#  -------------------------------------------------------------------------------------------------

# For debugging only--------------------
modelOutput = modelOutputs[length(modelOutputs)]
outputDenominator = outputDenominators[length(outputDenominators)]
firmFixedEffect = firmFixedEffects[length(firmFixedEffects)]
fixedEffect = fixedEffects[length(fixedEffects)]
excludeLawyer = excludeLawyers[length(excludeLawyers)]
# --------------------------------------

# Loop through every possible configuration
for(outputDenominator in outputDenominators){
  for(excludeLawyer in excludeLawyers){
    for(modelOutput in modelOutputs){
      for(firmFixedEffect in firmFixedEffects){
        for(fixedEffect in fixedEffects){
          
          # Set current configuration
          resultConfig = paste(outputDenominator, excludeLawyer, modelOutput, firmFixedEffect, fixedEffect, sep = '_')
          print(resultConfig)
          
          
          
          # Setting the main covariates
          currDf = commonCovariates  
          
          
          
          # Setting the output numerator
          if(modelOutput == "GrossRev"){
            currDf$Output = df$GrossRev
          } else if(modelOutput == "NOI") {
            currDf$Output = df$NOI
          } else if(modelOutput == "NOI.eqPart"){
            currDf$Output = df$NOI.eqPart
          }
          
          # Setting the output denominator
          if(outputDenominator == "PerLawyer"){
            currDf$Output = currDf$Output / currDf$Lawyers
          }
          
          
          
          # Set the variables regarding the fixed effects
          if(fixedEffect == "FE3"){
            
            # Skip if we are regressing only lawyers
            if(firmFixedEffect ==  "Lawyers"){ next }
            
            currDf = cbind(currDf, AggMnA = df$AggMnA, AggIPO = df$AggIPO, AggEquity = df$AggEquity)
          } else if(fixedEffect == "FE1"){
            
            # Skip if we are regressing only lawyers
            if(firmFixedEffect ==  "Lawyers"){ next }
            
            currDf = cbind(currDf, AggMnA = df$AggMnA)
          } else if(fixedEffect == "FEYear"){
            
            # Skip if we are regressing only lawyers
            if(firmFixedEffect ==  "Lawyers"){ next }
            
            currDf = cbind(currDf, years)
          }
          
          
          
          # Exclude Lawyers if necessary
          if(excludeLawyer == "WithoutLawyers"){
            currDf = currDf %>% select(-Lawyers, -Lawyers2)
          }
          
          
          
          # Setting the variables regarding the fixed firm effects
          if(firmFixedEffect == "FirmFE"){
            currDf$FirmID = firms$FirmID
          } else if(firmFixedEffect == "Lawyers"){
            if(excludeLawyer == "WithoutLawyers"){ next }
            currDf = currDf %>% select(Output, Lawyers, Lawyers2)
          }
          
          
          
          # Cleaning for missing data
          currDf = currDf %>% na.omit()
          
          
          
          # Run the linear regression
          currModel = lm(Output ~., data = currDf)
          
          
          
          # Compute the coefficients, TValues and performace metrics
          coeffs = summary(currModel)$coefficients[, 1]
          r2 = round(summary(currModel)$r.squared, 2)
          adjR2 = round(summary(currModel)$adj.r.squared, 2)
          aic = round(AIC(currModel)/100)
          bic = round(BIC(currModel)/100)
          if(outputDenominator == "NoRatio" && modelOutput != "NOI.eqPart"){ denom = 10^13} else{ denom = denom = 10^7}
          cv = round(CrossValidationError(currDf, 20)/denom)
          
          
          
          # Picking the right covariance matrix to adjust for heteroskedasticity
          if(firmFixedEffect == "FirmFE"){
            vcovMatrix = cluster.vcov(currModel, currDf[, "FirmID"])
          } else if(firmFixedEffect == "FEYear"){
            vcovMatrix = cluster.vcov(currModel, currDf[, "Years"])
          } else {
            vcovMatrix = vcovHC(currModel, type = "HC0")
          }
          
          adjustedModel = coeftest(currModel, vcovMatrix)
          adjPValues = adjustedModel[, 4]
          adjTValues = adjustedModel[, 3]
          
          
          
          # Update coefficients/TValues results
          resultsPValues = cbind(resultsPValues, NA)
          names(resultsPValues)[length(resultsPValues)] = resultConfig
          
          resultsTValues = cbind(resultsTValues, NA)
          names(resultsTValues)[length(resultsTValues)] = resultConfig
          
          resultsCoeffs = cbind(resultsCoeffs, NA)
          names(resultsCoeffs)[length(resultsCoeffs)] = resultConfig
          
          # Update only if the covariate was tested in the current configuration
          for(covariate in covariates){
            if(covariate %in% row.names(as.data.frame(adjTValues))){
              #TValues
              resultsTValues[covariate, resultConfig] = round(adjTValues[covariate], digits = 1)
              resultsPValues[covariate, resultConfig] = round(adjPValues[covariate]*100, digits = 1)
              
              #Coefficients
              if(covariate == "Leverage" || covariate == "Lawyers" || covariate == "(Intercept)"){
                if(outputDenominator == "NoRatio"){ 
                  denom = 1000000
                  numDigits = 1
                } else{ 
                  denom = 1000
                  numDigits = 2
                }
                resultsCoeffs[covariate, resultConfig] = round(coeffs[covariate]/denom, digits = numDigits)
                #resultsCoeffs[covariate, resultConfig] = round(coeffs[covariate]/1000, digits = 3)
              } else {
                resultsCoeffs[covariate, resultConfig] = round(coeffs[covariate], digits = 1)
              }
            }
          }
          
          resultsCoeffs['Observations', resultConfig] = nrow(currDf)
          resultsCoeffs['R2', resultConfig] = r2
          resultsCoeffs['Adj R2', resultConfig] = adjR2
          
          
          
          # Update performance tables
          resultsPerformance = cbind(resultsPerformance, c(adjR2, aic, bic, cv))
          names(resultsPerformance)[length(resultsPerformance)] = resultConfig
    
        } 
      }
    }
  }
}



# Save regressions results
save(resultsCoeffs, resultsTValues, resultsPValues, resultsPerformance, file = 'RegressionsResults.RData')



