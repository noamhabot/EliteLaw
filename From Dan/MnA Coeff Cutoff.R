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
source('CV.R')


# Get factor variables Year and Firm
years = data.frame(Years = as.factor(df$Year - min(df$Year) + 1))
firms = data.frame(FirmID = as.character(df$FirmID))

# Set the main covariates
covariates = df %>% select(FirmName, Year, Lawyers, Lawyers2, MnA, Equity, IPO, Leverage)
#covariates$Year = years$Years
#covariates = cbind(covariates, AggMnA = df$AggMnA, AggIPO = df$AggIPO, AggEquity = df$AggEquity)
#covariates$FirmID = firms$FirmID

# Set the output
Output = df$GrossRev/df$Lawyers

# Merge into the main dataset
dataset = cbind(Output, covariates)
dataset = dataset %>% na.omit()

# Dataframes for results
results = data.frame(AdjR2=numeric(0), AIC=numeric(0), BIC=numeric(0),
                     CV=numeric(0), MnATop=numeric(0), MnABottom=numeric(0))

# Rolling window, in number of years
rollingWindow = 3

for(cutoff in 20:120){
  # Tracking
  print(paste("Running for cutoff at firm with rank", toString(cutoff)))
  
  # Set the M&A variables according to the cutoff
  currDataset = dataset %>% filter(Year <= 2015) %>% filter(Year >= 2015 - rollingWindow + 1)# %>% select(-Year, -FirmName)
  outputCutoff = sort(currDataset$Output,partial=nrow(currDataset)-1)[nrow(currDataset)-cutoff+1] 
  
  # Create the 2 groups
  currDataset = currDataset %>% mutate(isTop = Output>=outputCutoff) %>%
    mutate(LawyersTop = Lawyers*isTop,
           Lawyers2Top = Lawyers2*isTop,
           MnATop = MnA*isTop,
           EquityTop = Equity*isTop,
           IPOTop = IPO*isTop,
           LeverageTop = Leverage*isTop) %>%
    mutate(LawyersBottom = Lawyers*(1-isTop),
           Lawyers2Bottom = Lawyers2*(1-isTop),
           MnABottom = MnA*(1-isTop),
           EquityBottom = Equity*(1-isTop),
           IPOBottom = IPO*(1-isTop),
           LeverageBottom = Leverage*(1-isTop)) %>%
    select(-Lawyers, -Lawyers2, -MnA, -Equity, -IPO, -Leverage, -isTop)

  # Run model
  model = lm(Output ~., data = currDataset)
  
  # Extract results
  coeffs = summary(model)$coefficients[, 1]
  adjR2 = round(100*summary(model)$adj.r.squared, 2)
  aic = round(AIC(model)/10)
  bic = round(BIC(model)/10)
  #cv = round(CrossValidationError(currDataset, 5)/100000000, 2)
  
  mnaCoeffTop = coeffs["MnATop"]
  mnaCoeffBottom = coeffs["MnABottom"]
  
  # Populate results table
  results[cutoff, "AdjR2"] =  adjR2 
  results[cutoff, "AIC"] = aic
  results[cutoff, "BIC"] = bic
  #results[cutoff, "CV"] = cv
  results[cutoff, "MnATop"] = mnaCoeffTop
  results[cutoff, "MnABottom"] = mnaCoeffBottom
}
