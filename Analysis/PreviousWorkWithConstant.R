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
source('Analysis/CV.R')




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

covariates = c("(Intercept)", "Lawyers", "Lawyers2", "LawyersLog", "Leverage",  
               "MnARevenue", "EquityRevenue", "IPORevenue", 
               "MnAIssues", "EquityIssues", "IPOIssues",
               "AggMnA", "AggEquity", "AggIPO", "GDP")
#covariatesWithoutIntercept = c("Lawyers", "Lawyers2", "LawyersLog", "Leverage",  
#                            "MnARevenue", "EquityRevenue", "IPORevenue", 
#                            "MnAIssues", "EquityIssues", "IPOIssues",
#                            "AggMnA", "AggEquity", "AggIPO", "GDP")

modelAcessment = c('Adj_R_2', 'AIC', 'BIC', 'CV', 'Params', 'Max VIF')

resultsCoeffs = data.frame(row.names = c(covariates, 'Observations', 'R2', 'Adj R2', 'AIC', 'BIC', 'CV', 'Params', 'MaxVIF'))
resultsTValues = data.frame(row.names = covariates)
resultsPValues = data.frame(row.names = covariates)
resultsPerformance = data.frame(row.names = modelAcessment)

yearPValues = data.frame(row.names = sort(unique(df$Year)))
firmPValues = data.frame(row.names = sort(unique(df$FirmID)))



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
fixedEffects = c("FE4", "FE1", "FEYear", "NoFE")



# For debugging only--------------------
modelOutput = modelOutputs[1]
outputDenominator = outputDenominators[1]
dealsAndOrRevenue = dealsAndOrRevenues[3]
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
            print(paste(counter,resultConfig))
            
            
            
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
            if(fixedEffect == "FE4"){
              
              # Skip if we are regressing only lawyers
              if(firmFixedEffect ==  "Lawyers"){ next }
              
              currDf = cbind(currDf, AggMnA = df$AggMnA, AggEquity = df$AggEquity, 
                             AggIPO = df$AggIPO, GDP=df$GDP)
              
              # Check for 0 values and replace them with NA
              
              if (sum(currDf$AggMnA==0,na.rm=TRUE) > 0) {
                currDf[currDf$AggMnA==0,]$AggMnA <- NA
              }
              if (sum(currDf$AggEquity==0) > 0) {
                currDf[currDf$AggEquity==0,]$AggEquity <- NA
              }
              if (sum(currDf$AggIPO==0) > 0) {
                currDf[currDf$AggIPO==0,]$AggIPO <- NA
              }
              
            } else if(fixedEffect == "FE1"){
              # FE1 will correspond to having only GDP
              
              # Skip if we are regressing only lawyers
              if(firmFixedEffect ==  "Lawyers"){ next }
              
              currDf = cbind(currDf, GDP = df$GDP)
              
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
              currDf$FirmID = as.factor(firms$FirmID)
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
            currDf <- currDf %>% na.omit()
            
            
  
            # Run the linear regression without the intercept
            currModel <- lm(Output ~.-1, data = currDf[ , !(names(currDf) %in% c("FirmName"))])
            #covariates <- covariatesWithoutIntercept
            # Unless we do not have any fixed effects, in which we include the intercept
            if (firmFixedEffect == "Lawyers" || (firmFixedEffect=="NoFirmFE" && fixedEffect=="NoFE")) {
              currModel <- lm(Output ~., data = currDf[ , !(names(currDf) %in% c("FirmName"))])
              #covariates <- covariatesWithIntercept
            }
            
            
            # We keep the intercept in the model where we calculate the VIF
            # because VIF depends on the intercept being in the model.
            vifModel <- lm(Output ~., data = currDf[ , !(names(currDf) %in% c("FirmName"))])
            # reference for VIF: https://statisticalhorizons.com/multicollinearity
            maxVIF <- 0
            try({
              thisVIF <- car::vif(vifModel)
              if (class(thisVIF) == "numeric") {
                # remove the firmID and Years collinearity because they don't matter
                maxVIF <- max(thisVIF[!names(thisVIF)%in%c("FirmID", "Years")])
              } else {
                
                # remove the firmID and Years collinearity because they don't matter
                maxVIF <- max(thisVIF[!row.names(thisVIF)%in%c("FirmID", "Years"),1])
                
              }
            })


            # Compute the coefficients, TValues and performace metrics
            coeffs = summary(currModel)$coefficients[, 1]
            r2 = round(summary(currModel)$r.squared, 2)
            adjR2 = round(summary(currModel)$adj.r.squared, 2)
            aic = round(AIC(currModel)) # k+n*(log((2*pi*deviance(currModel))/(n-k))+1)
            bic = round(BIC(currModel))
            RSS = deviance(currModel)
            numParams = length(coef(currModel))-1
            
            #print(resettest(currModel)$p.value)
            if(outputDenominator == "NoRatio" && modelOutput != "NOI.eqPart"){ denom = 10^13} else{ denom = denom = 10^7}

            #cv = round(CrossValidationError(currDf, 10)/denom)
            cv = "NA"


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
              } else {
                resultsTValues[covariate, resultConfig] = NA
                resultsPValues[covariate, resultConfig] = NA
                resultsCoeffs[covariate, resultConfig] = NA
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
            
            #######################################################################
            ###### P-values for year fixed effects and firm fixed effects #########
            if(fixedEffect == "FEYear"){
              # add the p value for each year
              yearpvals <- adjPValues[names(currModel$coefficients)[grepl("Years", names(currModel$coefficients))]]
              names(yearpvals) <- as.numeric(gsub("Years", "", names(yearpvals)))+1983
              #yearPValues <- cbind(yearPValues, c(NA,yearpvals))
              yearPValues <- cbind(yearPValues, yearpvals)
              names(yearPValues)[length(yearPValues)] = resultConfig
            }
            
            if(firmFixedEffect == "FirmFE"){
              # add the p value for each firm
              firmnames <- names(currModel$coefficients)[grepl("FirmID", names(currModel$coefficients))]
              firmpvals <- adjPValues[firmnames]
              
              
              names(firmpvals) <- as.numeric(gsub("FirmID", "", names(firmpvals)))
              
              addToDF <- firmpvals[row.names(firmPValues)]
              
              firmPValues <- cbind(firmPValues, addToDF)
              names(firmPValues)[length(firmPValues)] = resultConfig
            }
            #######################################################################
            

            
            
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
save(resultsCoeffs, resultsTValues, resultsPValues, resultsPerformance, yearPValues, firmPValues, 
      file = 'Data/RegressionsResults.RData')




