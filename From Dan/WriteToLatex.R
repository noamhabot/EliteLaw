# This code opens the several linear regression results produced earlier
# And combine them in a way that can be easily written to LaTeX
rm(list = ls())
library(xtable)



# Open files
load('RegressionsResults.RData')
source('GenerateLatex.R')



# Build the appropriate dataframes by slicing from the correct columns 

# Ratios (per Lawyer)
  # With Lawyers
  Coeffs.Rev.Lawyer.With.Lawyers = resultsCoeffs[, 1:9]
  TValues.Rev.Lawyer.With.Lawyers = resultsTValues[, 1:9]
  PValues.Rev.Lawyer.With.Lawyers = resultsPValues[, 1:9]
  Performance.Rev.Lawyer.With.Lawyers = resultsPerformance[, 1:9]
  
  Coeffs.NOI.Lawyer.With.Lawyers = resultsCoeffs[, 10:18]
  TValues.NOI.Lawyer.With.Lawyers = resultsTValues[, 10:18]
  PValues.NOI.Lawyer.With.Lawyers = resultsPValues[, 10:18]
  Performance.NOI.Lawyer.With.Lawyers = resultsPerformance[, 10:18]

  # Without Lawyers
  Coeffs.Rev.Lawyer.Without.Lawyers = resultsCoeffs[, 28:35]
  TValues.Rev.Lawyer.Without.Lawyers = resultsTValues[, 28:35]
  PValues.Rev.Lawyer.Without.Lawyers = resultsPValues[, 28:35]
  Performance.Rev.Lawyer.Without.Lawyers = resultsPerformance[, 28:35]
  
  Coeffs.NOI.Lawyer.Without.Lawyers = resultsCoeffs[, 36:43]
  TValues.NOI.Lawyer.Without.Lawyers = resultsTValues[, 36:43]
  PValues.NOI.Lawyer.Without.Lawyers = resultsPValues[, 36:43]
  Performance.NOI.Lawyer.Without.Lawyers = resultsPerformance[, 36:43]

# No ratios (or per eqPart)
  # With lawyers
  Coeffs.Rev.With.Lawyers = resultsCoeffs[, 52:60]
  TValues.Rev.With.Lawyers = resultsTValues[, 52:60]
  PValues.Rev.With.Lawyers = resultsPValues[, 52:60]
  Performance.Rev.With.Lawyers = resultsPerformance[, 52:60]
  
  Coeffs.NOI.With.Lawyers = resultsCoeffs[, 61:69]
  TValues.NOI.With.Lawyers = resultsTValues[, 61:69]
  PValues.NOI.With.Lawyers = resultsPValues[, 61:69]
  Performance.NOI.With.Lawyers = resultsPerformance[, 61:69]
  
  Coeffs.NOI.eqPart.With.Lawyers = resultsCoeffs[, 70:78]
  TValues.NOI.eqPart.With.Lawyers = resultsTValues[, 70:78]
  PValues.NOI.eqPart.With.Lawyers = resultsPValues[, 70:78]
  Performance.NOI.eqPart.With.Lawyers = resultsPerformance[, 70:78]

  # Without lawyers
  #Coeffs.Rev.Without.Lawyers = resultsCoeffs[, 79:86]
  #TValues.Rev.Without.Lawyers = resultsTValues[, 79:86]
  #PValues.Rev.Without.Lawyers = resultsPValues[, 79:86]
  #Performance.Rev.Without.Lawyers = resultsPerformance[, 79:86]
  
  #Coeffs.NOI.Without.Lawyers = resultsCoeffs[, 87:95]
  #TValues.NOI.Without.Lawyers = resultsTValues[, 87:95]
  #PValues.NOI.Without.Lawyers = resultsPValues[, 87:95]
  #Performance.NOI.Without.Lawyers = resultsPerformance[, 87:95]
  
  #Coeffs.NOI.eqPart.With.Lawyers = resultsCoeffs[, 96:102]
  #TValues.NOI.eqPart.With.Lawyers = resultsTValues[, 96:102]
  #PValues.NOI.eqPart.With.Lawyers = resultsPValues[, 96:102]
  #Performance.NOI.eqPart.With.Lawyers = resultsPerformance[, 96:102]



# Create the inference latex dataframes
# Ratios (per Lawyer)
  # With Lawyers
  Latex.Rev.Lawyer.With.Lawyers = GenerateLatexInference(Coeffs.Rev.Lawyer.With.Lawyers, TValues.Rev.Lawyer.With.Lawyers, PValues.Rev.Lawyer.With.Lawyers, FALSE)
  Latex.NOI.Lawyer.With.Lawyers = GenerateLatexInference(Coeffs.NOI.Lawyer.With.Lawyers, TValues.NOI.Lawyer.With.Lawyers, PValues.NOI.Lawyer.With.Lawyers, FALSE)
  
  # Without Lawyers
  Latex.Rev.Lawyer.Without.Lawyers = GenerateLatexInference(Coeffs.Rev.Lawyer.Without.Lawyers, TValues.Rev.Lawyer.Without.Lawyers, PValues.Rev.Lawyer.Without.Lawyers, FALSE)
  Latex.NOI.Lawyer.Without.Lawyers = GenerateLatexInference(Coeffs.NOI.Lawyer.Without.Lawyers, TValues.NOI.Lawyer.Without.Lawyers, PValues.NOI.Lawyer.Without.Lawyers, FALSE)
  
# No Ratios (or per eqPart)
  # With Lawyers
  Latex.Rev.With.Lawyers = GenerateLatexInference(Coeffs.Rev.With.Lawyers, TValues.Rev.With.Lawyers, PValues.Rev.With.Lawyers, TRUE)
  Latex.NOI.With.Lawyers = GenerateLatexInference(Coeffs.NOI.With.Lawyers, TValues.NOI.With.Lawyers, PValues.NOI.With.Lawyers, TRUE)
  Latex.NOI.eqPart.With.Lawyers = GenerateLatexInference(Coeffs.NOI.eqPart.With.Lawyers, TValues.NOI.eqPart.With.Lawyers, PValues.NOI.eqPart.With.Lawyers, TRUE)
  

# Create the performance latex dataframes


