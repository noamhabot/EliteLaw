# Clear environment
#rm(list = ls())



# Load libraries
library(dplyr)
library(lm.beta)
library(xtable)
library(corrplot)


# Load dataframe
setwd("~/Google Drive/EliteLaw/Generate Latex/")
load('../Data/EliteLawDf2016.RData')
#load('Data/EliteLawDf.RData')


# computePairwiseDifferencesZtestPVal <- function(coefbyyear, variable, years) {
#   MnARevcomparisons <- c()
#   MnARev <- as.numeric(coefbyyear[variable,])
#   MnARev2016 <- as.numeric(coefbyyear[variable,"2016"])
#   denom <- sd(MnARev)/sqrt(length(MnARev))
#   for (year in years) {
#     if (year == 2016) {
#       MnARevcomparisons <- c(MnARevcomparisons, NA)
#       next;
#     }
#     thisMnARev <- as.numeric(coefbyyear[variable,as.character(year)])
#     zval <- abs((thisMnARev - MnARev2016)/denom)
#     pval <- 1-pnorm(zval)
#     if (pval < 0.001) {
#       pval <- "\\textless 0.001"
#     } else {
#       pval <- prettyNum(pval, digits=3, mode="double", width=3)
#     }
#     
#     MnARevcomparisons <- c(MnARevcomparisons, pval)
#   }
#   return(MnARevcomparisons)
# }

computePairwiseDifferencesPVal <- function(df, years) {
  comparisonsRevenue <- c()
  comparisonsIssues <- c()
  for (year in years) {
    if (year == 2016) {
      comparisonsRevenue <- c(comparisonsRevenue, NA)
      comparisonsIssues <- c(comparisonsIssues, NA)
      next;
    }
    thisDF <- df %>% filter(Year %in% c(year, 2016))
    
    # convert year into a factor
    thisDF$Year <- as.factor(thisDF$Year)
    
    modelRevenue <- lm(GrossRev ~ Lawyers + Leverage + 
                   MnARevenue + EquityRevenue + IPORevenue +
                   MnANumOfDeals + EquityIssues + IPOIssues + MnARevenue*Year, data=thisDF)
    pvalRevenue <- summary(modelRevenue)$coefficients["MnARevenue:Year2016",4]
    if (pvalRevenue < 0.001) {
      pvalRevenue <- "\\textless 0.001"
    } else {
      pvalRevenue <- prettyNum(pvalRevenue, digits=3, mode="double", width=1)
    }
    comparisonsRevenue <- c(comparisonsRevenue, pvalRevenue)
    
    
    
    modelIssues <- lm(GrossRev ~ Lawyers + Leverage + 
                  MnARevenue + EquityRevenue + IPORevenue +
                  MnANumOfDeals + EquityIssues + IPOIssues + MnANumOfDeals*Year, data=thisDF)
    
    pvalIssues <- summary(modelIssues)$coefficients["MnANumOfDeals:Year2016",4]
    if (pvalIssues < 0.001) {
      pvalIssues <- "\\textless 0.001"
    } else {
      pvalIssues <- prettyNum(pvalIssues, digits=3, mode="double", width=1)
    }
    comparisonsIssues <- c(comparisonsIssues, pvalIssues)
  }
  
  
  return(rbind(comparisonsRevenue, comparisonsIssues))
}


# -------------------------------------------------------------------------------------------------
# Current Analysis
# -------------------------------------------------------------------------------------------------

buildAndSaveCoefficientsByYear <- function(df) {
  print("Building Table for Coefficients by Year")
  years <- sort(unique(as.numeric(df$Year)), decreasing=TRUE)
  covariates = c("(Intercept)", "Lawyers", "Leverage",  
                 "MnARevenue", "EquityRevenue", "IPORevenue", 
                 "MnANumOfDeals", "EquityIssues", "IPOIssues")
  coefbyyear <- data.frame(matrix(ncol=length(years), nrow=(length(covariates)+1)))
  names(coefbyyear) <- years
  row.names(coefbyyear) <- c(covariates, "metric\\tablefootnote[3]{Mean of the p-values of the interaction terms ($covariate_i*year$), where year=(2016 or X). Data includes year X and 2016.}")
  
  preciseTable <- data.frame(matrix(ncol=length(years), nrow=length(covariates)))
  names(preciseTable) <- years
  row.names(preciseTable) <- c(covariates)
  
  for (year in years) {
    #thisDF <- df %>% filter(Year == year)
    thisDF <- df %>% filter(Year %in% c(year, 2016))
    
    # defines them exactly
    
    model5 <- lm(GrossRev ~ Lawyers + Leverage + 
                   MnARevenue + EquityRevenue + IPORevenue +
                   MnANumOfDeals + EquityIssues + IPOIssues, data=thisDF)
    
    coefficients <- coef(model5)
    
    coefbyyear[1:length(covariates),as.character(year)] <- prettyNum(coefficients, digits=3, mode="double", width=1)
    preciseTable[1:length(covariates),as.character(year)] <- coefficients
    
    
    
    if (year != 2016) {
      interactionPValues <- c()
      for (variable in covariates[2:length(covariates)]) {
        formula <- as.formula(paste(c("GrossRev ~", paste(covariates[2:length(covariates)], collapse=" + "), paste("+ ", variable, "*factor(Year)", sep="")), collapse=" "))
        model5interaction <- lm(formula, data=thisDF)
        
        whichInteraction <- grepl(":", names(summary(model5interaction)$coefficients[,4]))
        interactionpval <- summary(model5interaction)$coefficients[whichInteraction,4]
        interactionPValues <- c(interactionPValues, interactionpval)
      }
      # model5interaction <- lm(GrossRev ~ Lawyers + Leverage + 
      #                           MnARevenue + EquityRevenue + IPORevenue +
      #                           MnANumOfDeals + EquityIssues + IPOIssues, data=thisDF)
      # 
      # whichInteraction <- grepl(":", names(summary(model5interaction)$coefficients[,4]))
      # interactionPValues <- summary(model5interaction)$coefficients[whichInteraction,4]
      
      coefbyyear[nrow(coefbyyear),as.character(year)] <- prettyNum(mean(interactionPValues), digits=3, mode="double", width=1)
      
      # add the t-statistic column to see if there are significant differences
      #pval <- t.test(as.numeric(preciseTable[2:length(covariates),"2016"]), coefficients[2:length(coefficients)], paired=TRUE)$p.value
      #coefbyyear[nrow(coefbyyear),as.character(year)] <- prettyNum(pval, digits=3, mode="double", width=1)
    }
  }
  
  
  # remove the intercept column
  #coefbyyear <- coefbyyear[-1,]
  
  
  
  
  #corrplot(cor(preciseTable), method = "color")

  
  
  newRows <- computePairwiseDifferencesPVal(df, years)
  coefbyyear <- rbind(coefbyyear, newRows[1,])
  coefbyyear <- rbind(coefbyyear, newRows[2,])
  
  row.names(coefbyyear)[(nrow(coefbyyear)-1):nrow(coefbyyear)] <- c("metric\\tablefootnote[4]{MnA Deal Value coefficient of year X vs. MnA Deal Value coefficient of 2016 (p-value)}",
                                                                    "metric\\tablefootnote[5]{MnA Transactions coefficient of year X vs. MnA Transactions coefficient of 2016 (p-value)}")
  
  row.names(coefbyyear) <- gsub("MnARevenue", " MnA D.V.\\\\tablefootnote[1]{D.V. = Deal Value}", row.names(coefbyyear))
  row.names(coefbyyear) <- gsub("MnAIssues", " MnA T.\\\\tablefootnote[2]{T. = Transactions}", row.names(coefbyyear))
  row.names(coefbyyear) <- gsub("MnANumOfDeals", " MnA T.\\\\tablefootnote[2]{T. = Transactions}", row.names(coefbyyear))
  row.names(coefbyyear) <- gsub("Revenue", " D.V.", row.names(coefbyyear))
  row.names(coefbyyear) <- gsub("Issues", " T.", row.names(coefbyyear))
  
  
  captionText <- "The entries in this table are coefficients for regression \\#5, per year. 
                  We have 9+3 columns - one for each coefficient that is in the model and 3 more described in footnotes.
                  Agg MnA, Agg Equity, Agg IPO, and GDP are excluded since the regressions are for one year and those 
                  variables are fixed for a given year."
  
  print(xtable(t(coefbyyear), digits=3, caption=captionText), 
        file="IndivTexOutput/coefbyyear.tex", 
        NA.string = getOption("xtable.NA.string", "NA"),
        sanitize.text.function=function(x){x})
  print("Done!")
  
  return(coefbyyear)
}

