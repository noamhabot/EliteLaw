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



################################################################
###################### Summary Statistics ######################
summaryTables <- GenerateSummaryStatistics(df)
alignment <- paste(c("l",rep("r",ncol(summaryTables))), collapse="")
print(xtable(summaryTables, align=alignment), file="IndivTexOutput/summary.tex", 
      sanitize.text.function=function(x){x})
################################################################



################################################################
###################### Correlations ############################
# # get the correlations table
corDF <- GenerateCorrelations(df)

print(xtable(corDF, digits=3), file="IndivTexOutput/corrTable.tex")
# print(xtable(corDF[,7:10], digits=3), file="IndivTexOutput/corrTable2.tex")
# print(xtable(corDF[,11:ncol(corDF)], digits=3), file="IndivTexOutput/corrTable3.tex")


# prints the correlation table by rank
corByRank <- GenerateCorrelationsByRank(df)
correlationsByRank <- corByRank[[1]]
print(xtable(correlationsByRank, digits=3), file="IndivTexOutput/MnAGDP.tex")

# now make the 6 graphs with correlations
#correlRanks <- corByRank[[2]]
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
table <- GeneratePValueSummaryTable(resultsPValues)
alignment <- paste(c("l",rep("c",ncol(table))), collapse="")
caption <- paste("Percentage of regressions in which each variable is significant at, and in how many the variable appears.\\\\Total number of regressions: ", max(table[,ncol(table)]), ".", sep="")
print(xtable(table,digits=0, align=alignment, caption=caption), 
      file="IndivTexOutput/pvaltable.tex", sanitize.text.function=function(x){x})
################################################################




################################################################
####################### Model Averaging# #######################
# save the model averaging to the tables
#saveModelAveraging(df)
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