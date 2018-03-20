# This code opens the several linear regression results produced earlier
# And combine them in a way that can be easily written to LaTeX
rm(list = ls())
library(xtable)
library(dplyr)
library(ggplot2)
library(reshape)
library(GGally)
library(MuMIn)


setwd("~/Google Drive/EliteLaw/Generate Latex/")

# Open files
load('RegressionsResults.RData')
load('../Data/EliteLawDf2016.RData')
source('GenerateLatex.R')
source('../Analysis/ModelAveraging.R')
source('../Analysis/InterpretPValueSummary.R')
source('../Analysis/CoefficientsByYear.R')



################################################################
###################### Summary Statistics ######################
summaryTables <- GenerateSummaryStatistics(df)
alignment <- paste(c("l",rep("r",ncol(summaryTables))), collapse="")
print(xtable(summaryTables, align=alignment), hline.after=c(-1,0, 7, 13, 16, 19, nrow(summaryTables)), 
      file="IndivTexOutput/summary.tex", sanitize.text.function=function(x){x})

aggVariableSummaryTable <- GenerateAggVarSummary(df)
print(xtable(aggVariableSummaryTable), file="IndivTexOutput/summaryaggvars.tex", sanitize.text.function=function(x){x})
################################################################



################################################################
###################### Correlations ############################
# get the correlations table
GenerateAndSaveCorrelations(df)


# prints the correlation table by rank
corByRank <- GenerateCorrelationsByRank(df)
correlationsByRank <- corByRank[[1]]
print(xtable(correlationsByRank, digits=3), file="IndivTexOutput/MnAGDP.tex")

# now make the 6 graphs with correlations
correlRanks <- corByRank[[2]]
#SaveCorrelationsByRankPlots(df, correlRanks)
################################################################



################################################################
###################### Regression Tables #######################




# Build all of the regression tables and save them in an organized fashion.
tables <- GenerateRegressionTables(resultsCoeffs,resultsTValues, resultsPValues)

# save all of the latex tables to files
for (i in 1:length(tables)) {
  istr = as.character(i)
  if (i < 10) {
    istr = paste("0",i,sep="")
  }
  fileName <- paste("IndivTexOutput/regressions-table",istr,".tex",sep="")
  print(tables[[i]], file=fileName, sanitize.text.function=function(x){x})
}



# Build the appropriate dataframes by slicing from the correct columns 
#Performance.Rev.Lawyer.With.Lawyers = resultsPerformance[, 1:9] # unused
# Generate the Performance Table
GeneratePerformanceTable(resultsPerformance)


# Generate and save the summary table for the p-values
table <- GeneratePValueSummaryTable(resultsPValues, yearPValues, firmPValues)
alignment <- paste(c("l",rep("c",ncol(table))), collapse="")
caption <- paste("Percentage of regressions in which each variable is significant at, and in how many the variable appears.\\\\Total number of regressions: ", 
                 max(table[1:(nrow(table)-2),ncol(table)]), ".", sep="")
print(xtable(table,digits=0, align=alignment, caption=caption), 
      file="IndivTexOutput/pvaltable.tex", sanitize.text.function=function(x){x})

# Create the temp table from InterpretPValueSummary
# This table helps identify trends in where the p-values are not significant.
createAndSaveTempTable(df, resultsCoeffs, resultsPValues)
################################################################



################################################################
#################### Regressions by Year #######################
# This function will generate the table that, given the regression of choice,
# will include the coefficients of all variables in the model for a specific year.
# Note that it would not make sense to include any fixed effects in this model.
buildAndSaveCoefficientsByYear(df)
################################################################



################################################################
####################### Model Averaging ########################
# save the model averaging to the tables
saveModelAveraging(df)
################################################################









####################################################################################
############################ UNUSED CORRELATION PLOTS ##############################
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