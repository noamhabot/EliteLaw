# This code opens the several linear regression results produced earlier
# And combine them in a way that can be easily written to LaTeX
rm(list = ls())
library(xtable)
library(dplyr)
library(ggplot2)
library(reshape)
library(GGally)


setwd("~/Google Drive/Stanford Law Project/Generate Latex/")

# Open files
load('RegressionsResults.RData')
load('../Data/EliteLawDf2016.RData')
source('GenerateLatex.R')




###################### Summary Statistics ######################
summaryTables <- GenerateSummaryStatistics(df)
for (i in 1:length(summaryTables)) {
  print(xtable(summaryTables[[i]]), file=paste("IndivTexOutput/summary",i,".tex",sep=""), sanitize.text.function=function(x){x})
}
##################################################################




###################### Correlations ######################
# # get the correlations table
corDF <- GenerateCorrelations(df)
corDF <- round(cor(corDF),3) # the journal uses 3 significant figures

print(xtable(corDF[,1:5], digits=3), file="IndivTexOutput/corrTable1.tex")
print(xtable(corDF[,6:ncol(corDF)], digits=3), file="IndivTexOutput/corrTable2.tex")


# prints the correlation table by rank
corByRank <- GenerateCorrelationsByRank(df)
correlationsByRank <- corByRank[[1]]
print(xtable(correlationsByRank, digits=3), file="IndivTexOutput/MnAGDP.tex")

# now make the 6 graphs with correlations
correlRanks <- corByRank[[2]]
SaveCorrelationsByRankPlots(df, correlRanks)
##################################################################



###################### Regression Tables ######################

# Build the appropriate dataframes by slicing from the correct columns 
#Performance.Rev.Lawyer.With.Lawyers = resultsPerformance[, 1:9] # unused
resultsPerformance <- t(resultsPerformance)


# Set the possible Configurations
modelOutputs = c("GrossRev", "GrossRev.eqPart", "NOI", "NOI.eqPart")
outputDenominators = c("NoRatio", "PerLawyer")
excludeLawyers = c("WithLawyers2", "WithLawyersLog", "WithoutLawyers")
firmFixedEffects = c("FirmFE", "NoFirmFE", "Lawyers")
fixedEffects = c("FE3", "FE1", "FEYear", "NoFE")
dealsAndOrRevenues = c("Both", "Revenue", "Deals")

# we do the subtraction because NOI.eqPart should not have the PerLawyer attached to it.
numTables <- length(dealsAndOrRevenues)*length(modelOutputs)*length(outputDenominators)*length(excludeLawyers) - 
  2*length(excludeLawyers)*length(dealsAndOrRevenues)


dealsAndOrRevenue <- "Deals"
modelOutput <- "GrossRev"
outputDenominator <- "NoRatio"
excludeLawyer <- "WithLawyers2"

tables <- vector("list", numTables)
counter <- 1


for (modelOutput in modelOutputs) {
  # first subset
  thisSubset1 <- names(resultsCoeffs)[grepl(modelOutput, names(resultsCoeffs))]
  
  if (modelOutput == "NOI") {
    # deselect all of the NOI.eqPart
    thisSubset1 <- thisSubset1[!grepl("NOI.eqPart", thisSubset1)]
  } else if (modelOutput == "GrossRev") {
    # deselect all of the GrossRev.eqPart
    thisSubset1 <- thisSubset1[!grepl("GrossRev.eqPart", thisSubset1)]
  }
  
  
    
    
  for (outputDenominator in outputDenominators) {
    if (outputDenominator == "PerLawyer" && (modelOutput == "NOI.eqPart" || modelOutput == "GrossRev.eqPart")) {
      next
    }
    thisSubset2 <- thisSubset1[grepl(outputDenominator, thisSubset1)]
    
    
    
    
    for (dealsAndOrRevenue in dealsAndOrRevenues) {
      thisSubset3 <- thisSubset2[grepl(dealsAndOrRevenue, thisSubset2)]
      
      for (excludeLawyer in excludeLawyers) {
        thisSubset4 <- thisSubset3[grepl(excludeLawyer, thisSubset3)]
        
        
        
        
        # STRICTLY DEBUGGING# STRICTLY DEBUGGING# STRICTLY DEBUGGING
        dfCoeff = resultsCoeffs[, thisSubset4]
        dfTValue = resultsTValues[, thisSubset4]
        dfPValue = resultsPValues[, thisSubset4]
        # STRICTLY DEBUGGING# STRICTLY DEBUGGING# STRICTLY DEBUGGING
        
        
        
        thisTable <- GenerateLatexInference(resultsCoeffs[, thisSubset4], resultsTValues[, thisSubset4], 
                                            resultsPValues[, thisSubset4])
        
        caption <- ""
        if (modelOutput == "GrossRev") {
          caption <- paste(caption, "Gross Revenue", sep="")
        } else if (modelOutput == "NOI") {
          caption <- paste(caption, "NOI", sep="")
        } else if (modelOutput == "NOI.eqPart") {
          caption <- paste(caption, "NOI/EquityPartner", sep="")
        } else if (modelOutput == "GrossRev.eqPart") {
          caption <- paste(caption, "GrossRevenue/EquityPartner", sep="")
        }
        
        if (outputDenominator == "PerLawyer" && modelOutput != "NOI.eqPart" && modelOutput != "GrossRev.eqPart") {
          caption <- paste(caption, "/Lawyer", sep="")
        }
        
        if (dealsAndOrRevenue == "Both") {
          caption <- paste(caption, " $\\sim$ Revenue + Counts", sep="")
        } else if (dealsAndOrRevenue == "Deals") {
          caption <- paste(caption, " $\\sim$ Counts", sep="")
        } else if (dealsAndOrRevenue == "Revenue") {
          caption <- paste(caption, " $\\sim$ Revenue", sep="")
        }
        
        if (excludeLawyer == "WithoutLawyers") {
          caption <- paste(caption, " (without Lawyers)", sep="")
        } else if (excludeLawyer == "WithLawyers2") {
          caption <- paste(caption, " (with Lawyers$^2$)", sep="")
        } else if (excludeLawyer == "WithLawyersLog") {
          caption <- paste(caption, " (with log(Lawyers))", sep="")
        }
        print(paste(counter, modelOutput, outputDenominator, dealsAndOrRevenue, excludeLawyer))
        tables[[counter]] <- xtable(thisTable, caption=caption)
        counter <- counter + 1
      }
    }
    
  }
  
}


  

# save all of the latex tables to files
for (i in 1:length(tables)) {
  istr = as.character(i)
  if (i < 10) {
    istr = paste("0",i,sep="")
  }
  fileName <- paste("IndivTexOutput/regressions-table",istr,".tex",sep="")
  print(tables[[i]], file=fileName, sanitize.text.function=function(x){x})
}










####################################################################################
######################################### UNUSED CORRELATION PLOTS##################
####################################################################################
# 
# # save the correlation heatmap
# heatmap <- ggplot(data = melt(corDF), aes(x=X1, y=X2, fill=value)) + geom_tile() +
#   xlab("") + ylab("") + ggtitle("Correlations HeatMap") +
#   labs(fill='Correlation')
# ggsave(file="IndivTexOutput/corrHeatmap.jpg", heatmap, width=8, height=5, dpi=300)
# 
# # the ggpairs plot
# # Function to perform multiple plots with regressions
# ggpairs_with_linreg <- function(data, mapping, ...){
#   p <- ggplot(data = data, mapping = mapping) +
#     geom_point() +
#     geom_smooth(method=lm, fill="blue", color="red", ...)
#   return(p)
# }
# 
# 
# 
# toPlot <- df[, c("Lawyers", "Leverage", "GrossRev", "NOI", "IPORevenue", "EquityRevenue",
#                  "MnARevenue", "GrossRev.Lawyer", "NOI.Lawyer", "NOI.eqPart")]
# 
# labels <- names(toPlot)
# labels <- gsub("GrossRev", "Rev", labels)
# labels <- gsub(".eqPart", "/EqPart", labels)
# labels <- gsub(".Lawyer", "/Law", labels)
# labels <- gsub("Revenue", "", labels)
# labels <- gsub("MnA", "M&A", labels)
# 
# 
# graph <- ggpairs(toPlot, lower = list(continuous = ggpairs_with_linreg),
#       columnLabels=labels)
# ggsave(file="IndivTexOutput/corrGGpairs.jpg", graph, width=8, height=5, dpi=300)
##################################################################