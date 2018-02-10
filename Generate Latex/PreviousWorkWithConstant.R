# Clear environment
rm(list = ls())



# Load libraries
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(multiwayvcov)
library(car)



# Load dataframe
setwd("~/Google Drive/EliteLaw/")
load('Data/EliteLawDf2016.RData')
#load('Data/EliteLawDf.RData')
source('Generate Latex/CV.R')




# -------------------------------------------------------------------------------------------------
# Current Analysis
# -------------------------------------------------------------------------------------------------

modelOutput = "NOI.eqPart"
outputDenominator = "NoRatio"
firmFixedEffect = "NoFirm"

# Get the main variables
commonCovariates = df %>% select(Lawyers, Lawyers2, Leverage, FirmName, MnARevenue, EquityRevenue, IPORevenue, MnANumOfDeals, EquityIssues, IPOIssues)
# hack to change Lawyers^2 to log(lawyers)
commonCovariates = commonCovariates %>% mutate(LawyersLog = log(Lawyers), MnAIssues=MnANumOfDeals) %>% select(-MnANumOfDeals)
commonCovariates <- commonCovariates[, c(1:2, 10, 3:9, 11)]

# Create the categorical variables
# Year
years = data.frame(Years = as.factor(df$Year - min(df$Year) + 1))
# Firm
firms = data.frame(FirmID = as.character(df$FirmID))



# Prepare the table for the results
#covariates = c("Lawyers", "log(Lawyers)", "AggMnA", "AggEquity", "AggIPO", "MnARevenue", "EquityRevenue", "IPORevenue", "Leverage", "(Intercept)")
covariates = c("(Intercept)", "Lawyers", "Lawyers2", "LawyersLog", "Leverage",  
               "MnARevenue", "EquityRevenue", "IPORevenue", 
               "MnAIssues", "EquityIssues", "IPOIssues")
modelAcessment = c('Adj_R_2', 'AIC / 10e+2', 'BIC / 10e+2', 'CV / 10e+7', 'Params', 'Max VIF')

resultsCoeffs = data.frame(row.names = c(covariates, 'Observations', 'R2', 'Adj R2', 'AIC', 'BIC', 'CV', 'Params', 'MaxVIF'))
resultsTValues = data.frame(row.names = covariates)
resultsPValues = data.frame(row.names = covariates)
resultsPerformance = data.frame(row.names = modelAcessment)



# Set configurations for Cross Validation
set.seed(1)
nCV = 10



#  -------------------------------------------------------------------------------------------------
# Main loop for all models
#  -------------------------------------------------------------------------------------------------

# Set the possible Configurations
modelOutputs = c("GrossRev", "GrossRev.eqPart", "NOI", "NOI.eqPart")
outputDenominators = c("NoRatio", "PerLawyer")
dealsAndOrRevenues = c("Both", "Revenue", "Deals")
excludeLawyers = c("WithLawyers", "WithLawyers2", "WithLawyersLog", "WithoutLawyers")
firmFixedEffects = c("FirmFE", "NoFirmFE", "Lawyers")
fixedEffects = c("FE3", "FE1", "FEYear", "NoFE")



# For debugging only--------------------
modelOutput = modelOutputs[1]
outputDenominator = outputDenominators[1]
dealsAndOrRevenue = dealsAndOrRevenues[2]
excludeLawyer = excludeLawyers[1]
firmFixedEffect = firmFixedEffects[1]
fixedEffect = fixedEffects[3]
counter <- 1
# --------------------------------------

counter <- 1
for(modelOutput in modelOutputs) {
  for (outputDenominator in outputDenominators){
    for(dealsAndOrRevenue in dealsAndOrRevenues){
      for(excludeLawyer in excludeLawyers){
        for(firmFixedEffect in firmFixedEffects){
          for(fixedEffect in fixedEffects){
          
            if (outputDenominator == "PerLawyer" && (modelOutput == "NOI.eqPart" || modelOutput == "GrossRev.eqPart")) {
              next
            }
          
            # Set current configuration
            resultConfig = paste(modelOutput, outputDenominator, dealsAndOrRevenue, excludeLawyer, firmFixedEffect, fixedEffect, sep = '_')
            print(paste(counter,modelOutput, outputDenominator, dealsAndOrRevenue, excludeLawyer, firmFixedEffect, fixedEffect, sep = '_'))
            
            
            
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
            
            
            # Set the variables regarding the fixed effects
            if(fixedEffect == "FE3"){
              
              # Skip if we are regressing only lawyers
              if(firmFixedEffect ==  "Lawyers"){ next }
              currDf = cbind(currDf, AggMnA = df$AggMnA, AggEquity = df$AggEquity, AggIPO = df$AggIPO)
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
            if(excludeLawyer == "WithLawyers"){
              currDf = currDf %>% select(-Lawyers2, -LawyersLog)
            } else if (excludeLawyer == "WithLawyers2"){
              #currDf = currDf %>% select(-LawyersLog)
              currDf = currDf %>% select(-LawyersLog, -Lawyers)
            } else if (excludeLawyer == "WithLawyersLog"){
              #currDf = currDf %>% select(-Lawyers2)
              currDf = currDf %>% select(-Lawyers2, -Lawyers)
            } else if (excludeLawyer == "WithoutLawyers"){
              currDf = currDf %>% select(-Lawyers, -Lawyers2, -LawyersLog)
            }
            
            
            # Remove deals if selected "Revenue" or vice versa
            if (dealsAndOrRevenue == "Deals") {
              currDf = currDf %>% select(-MnARevenue, -IPORevenue, -EquityRevenue)
            } else if (dealsAndOrRevenue == "Revenue") {
              currDf = currDf %>% select(-MnAIssues, -IPOIssues, -EquityIssues)
            }
            
            
            # Setting the variables regarding the fixed firm effects
            if(firmFixedEffect == "FirmFE"){
              currDf$FirmID = firms$FirmID
            } else if(firmFixedEffect == "Lawyers"){
              if(excludeLawyer == "WithoutLawyers"){ 
                next 
              } else if (excludeLawyer == "WithLawyers") {
                currDf = currDf %>% select(Output, Lawyers)
              } else if (excludeLawyer == "WithLawyers2") {
                currDf = currDf %>% select(Output, Lawyers2)
              } else if (excludeLawyer == "WithLawyersLog") {
                currDf = currDf %>% select(Output, LawyersLog)
              }
              
            }
            

            
            
            
            # Cleaning for missing data
            currDf = currDf %>% na.omit()
            
            
  
            # Run the linear regression
            currModel = lm(Output ~., data = currDf[ , !(names(currDf) %in% c("FirmName"))])
            maxVIF <- 0
            try({
              thisVIF <- vif(currModel)
              if (class(thisVIF) == "numeric") {
                maxVIF <- max(thisVIF)
              } else {
                maxVIF <- max(vif(currModel)[,1])
              }
            })


            # Compute the coefficients, TValues and performace metrics
            coeffs = summary(currModel)$coefficients[, 1]
            r2 = round(summary(currModel)$r.squared, 2)
            adjR2 = round(summary(currModel)$adj.r.squared, 2)
            aic = round(AIC(currModel)/100) # k+n*(log((2*pi*deviance(currModel))/(n-k))+1)
            bic = round(BIC(currModel)/100)
            RSS = deviance(currModel)
            numParams = length(coef(currModel))-1
            
            #print(resettest(currModel)$p.value)
            if(outputDenominator == "NoRatio" && modelOutput != "NOI.eqPart"){ denom = 10^13} else{ denom = denom = 10^7}

            cv = round(CrossValidationError(currDf, 20)/denom)


            # Picking the right covariance matrix to adjust for heteroskedasticity
            if(firmFixedEffect == "FirmFE"){
              vcovMatrix = cluster.vcov(currModel, currDf[, "FirmName"])
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
                resultsTValues[covariate, resultConfig] = round(adjTValues[covariate], digits = 2)
                resultsPValues[covariate, resultConfig] = round(adjPValues[covariate], digits = 3)

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
                } else {
                  resultsCoeffs[covariate, resultConfig] = round(coeffs[covariate], digits = 1)
                }
              }
            }


            resultsCoeffs['Observations', resultConfig] = nrow(currDf)
            resultsCoeffs['R2', resultConfig] = r2
            resultsCoeffs['Adj R2', resultConfig] = adjR2
            resultsCoeffs['AIC', resultConfig] = aic
            resultsCoeffs['BIC', resultConfig] = bic
            resultsCoeffs['CV', resultConfig] = cv
            resultsCoeffs['Params', resultConfig] = numParams
            resultsCoeffs['MaxVIF', resultConfig] = maxVIF
            
            
            
            

            
            
            # Update performance tables
            resultsPerformance = cbind(resultsPerformance, c(adjR2, aic, bic, cv, numParams, round(maxVIF,2)))
            names(resultsPerformance)[length(resultsPerformance)] = resultConfig
            
            counter <- counter + 1
          }
        } 
      }
    }
  }
}

row.names(resultsCoeffs) <- gsub("\\(Intercept\\)", "Intercept", row.names(resultsCoeffs))


# Save regressions results
save(resultsCoeffs, resultsTValues, resultsPValues, resultsPerformance, file = 'Generate Latex/RegressionsResults.RData')




