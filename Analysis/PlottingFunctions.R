# make a plot to show the different coefficients per tier Over the years
plotOverYears <- function(wholeResults, numTiers, method, outputText, coefficientText) {
  br1 <- c(NA)
  br2 <- c(NA)
  br3 <- c(NA)
  br4 <- c(NA)
  br5 <- c(NA)
  
  tierNames <- c("tier1Coeff")
  
  
  # First, do some sanity checks and make sure that we have all the coefficients for each tier
  # Then, we get the breakpoints for each tier
  if (all(is.na(wholeResults$tier1Coeff))) {
    print("Error: the 1st tier's coefficients are not valid.")
    return()
  }
  br1 <- wholeResults$YearFrom[breakpoints(wholeResults$tier1Coeff ~ 1)$breakpoints]
  if (all(is.na(br1))) { br1 <- c(NA) }
  
  if (numTiers >= 2) {
    if (all(is.na(wholeResults$tier2Coeff))) {
      print("Error: one of the first two tier's coefficients are not valid.")
      return()
    }
    br2 <- wholeResults$YearFrom[breakpoints(wholeResults$tier2Coeff ~ 1)$breakpoints]
    if (all(is.na(br2))) { br2 <- c(NA) }
    tierNames <- c(tierNames, "tier2Coeff")
  }
  
  if (numTiers >= 3) {
    if (all(is.na(wholeResults$tier3Coeff))) {
      print("Error: the 3rd tier's coefficients are not valid.")
      return()
    }
    
    tryCatch( {br3 <- wholeResults$YearFrom[breakpoints(wholeResults$tier3Coeff ~ 1)$breakpoints]},
              error=function(cond) {br3 <- NA})
    
    if (all(is.na(br3))) { br3 <- c(NA) }
    tierNames <- c(tierNames, "tier3Coeff")
  }
  
  if (numTiers >= 4) {
    if (all(is.na(wholeResults$tier4Coeff))) {
      print("Error: the 4th tier's coefficients are not valid.")
      return()
    }
    
    tryCatch( {br4 <- wholeResults$YearFrom[breakpoints(wholeResults$tier4Coeff ~ 1)$breakpoints]},
              error=function(cond) {br4 <- NA})
    
    if (all(is.na(br4))) { br4 <- c(NA) }
    tierNames <- c(tierNames, "tier4Coeff")
  }
  
  if (numTiers >= 5) {
    if (all(is.na(wholeResults$tier5Coeff))) {
      print("Error: the 5th tier's coefficients are not valid.")
      return()
    }
    
    tryCatch( {br5 <- wholeResults$YearFrom[breakpoints(wholeResults$tier5Coeff ~ 1)$breakpoints]},
                  error=function(cond) {br5 <- NA})
    
    if (all(is.na(br5))) { br5 <- c(NA) }
    tierNames <- c(tierNames, "tier5Coeff")
  }
  
  
  
  
  
  captionText <- paste("Tier 1 (Top) Firms Coeff Breakpoint(s): ", paste(as.character(br1), collapse=","), ".", sep="")
  if (numTiers >= 2) {
    captionText <- paste(captionText, "\n Tier 2 Coeff Breakpoint(s): ", paste(as.character(br2), collapse=","), ".", sep="")
  }
  if (numTiers >= 3) {
    captionText <- paste(captionText, "\n Tier 3 Coeff Breakpoint(s): ", paste(as.character(br3), collapse=","), ".", sep="")
  }
  if (numTiers >= 4) {
    captionText <- paste(captionText, "\n Tier 4 Coeff Breakpoint(s): ", paste(as.character(br4), collapse=","), ".", sep="")
  }
  if (numTiers >= 5) {
    captionText <- paste(captionText, "\n Tier 5 Coeff Breakpoint(s): ", paste(as.character(br5), collapse=","), ".", sep="")
  }
  
  
  titleText <- paste(coefficientText," Coefficients From ", wholeResults$YearFrom[1], " To ", wholeResults$YearFrom[nrow(wholeResults)], 
                     " (", method, "-", outputText , ")", sep="")
  
  
  dfForPlotting <- melt(wholeResults, id=c("YearFrom"), measure.vars=tierNames)
  
  p <- ggplot(data=dfForPlotting, aes(x=YearFrom)) +
    geom_line(aes(y = value, colour = variable, group=variable)) +
    scale_color_manual(labels = paste("Tier", 1:numTiers), values = 1:numTiers) +
    labs(title = titleText, x = "Years", y = paste(coefficientText, "Standardized Coefficient"), color = "Legend",
         caption=captionText) 
  #geom_vline(data=vertLinesDF, aes(xintercept=xint, color=cols), linetype="dashed")
  return(p)
}
