library(xtable)

#setwd("~/Google Drive/EliteLaw/Generate Latex/")

#load('RegressionsResults.RData')
#load('../Data/EliteLawDf2016.RData')

createAndSaveTempTable <- function(df, resultsCoeffs, resultsPValues) {

  modelOutputs = c("GrossRev", "GrossRev.eqPart", "NOI", "NOI.eqPart")
  outputDenominators = c("NoRatio", "PerLawyer")
  outputs <- c("GrossRev", "GrossRev.Lawyer", "GrossRev.eqPart", "NOI", "NOI.Lawyer", "NOI.eqPart")
  dealsAndOrRevenues = c("Both", "Revenue", "Deals")
  excludeLawyers = c("WithLawyers", "WithLawyers2", "WithLawyersLog", "WithoutLawyers")
  firmFixedEffects = c("FirmFE", "NoFirmFE", "Lawyers")
  fixedEffects = c("FE4", "FE1", "FEYear", "NoFE")
  
  columns <- c(outputs, dealsAndOrRevenues, excludeLawyers, firmFixedEffects, fixedEffects)
  analysis <- data.frame(matrix(0, nrow=14, ncol=length(columns)))
  row.names(analysis) <- row.names(resultsCoeffs)[2:15]
  colnames(analysis) <- columns
  
  for (var in row.names(resultsCoeffs)) {
    for (col in which(resultsPValues[var,]>=0.05)) {
      
      strings <- strsplit(names(resultsCoeffs[col]), "_")[[1]]
      if ("PerLawyer" %in% strings) {
        if ("GrossRev" %in% strings) {
          analysis[var,which(columns == "GrossRev.Lawyer")] <- analysis[var,which(columns == "GrossRev.Lawyer")] + 1
        } else {
          analysis[var,which(columns == "NOI.Lawyer")] <- analysis[var,which(columns == "NOI.Lawyer")] + 1
        }
      }
      for (string in strings) {
        if (string %in% c(outputDenominators)) {
          next;
        }
        if (string %in% modelOutputs && "PerLawyer" %in% strings) {
          next;
        }
        analysis[var,which(columns == string)] <- analysis[var,which(columns == string)] + 1
      }
    }
  }
  analysis <- cbind(analysis,rowSums(analysis)/5)
  names(analysis)[length(analysis)] <- "\\textbf{Total}"
  
  row.names(analysis) <- row.names(analysis)
  row.names(analysis) <- gsub("Lawyers2", "Lawyers^2", row.names(analysis))
  row.names(analysis) <- gsub("LawyersLog", "log(Lawyers)", row.names(analysis))
  row.names(analysis) <- gsub("Revenue", " Deal Value", row.names(analysis))
  row.names(analysis) <- gsub("Issues", " Transactions", row.names(analysis))
  row.names(analysis) <- gsub("Agg", "Agg ", row.names(analysis))
  
  caption <- "On the left, we see the variable name. For each of those, we consider each and every one of their regressions
  that have p-values greater than or equal to 0.05. Out of those, we obtain the regression specifications and keep a counter
  for how many of each type of specification there is. The top of the table (columns) signify which specification has how many counts of high p-values."
  
  print(xtable(analysis[,1:11], digits=0), file="IndivTexOutput/pvaltableconclusion1.tex", sanitize.text.function=function(x){x})
  print(xtable(analysis[,12:21], caption=caption, digits=0), file="IndivTexOutput/pvaltableconclusion2.tex", sanitize.text.function=function(x){x})

}