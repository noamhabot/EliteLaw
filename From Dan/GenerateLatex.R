GenerateLatexInference = function(dfCoeff, dfTValue, dfPValue, useK=FALSE){
  
  output = data.frame()
  
  # Adjust the name to account for units
  if(useK == TRUE){
    row.names(dfCoeff)[1] = paste(row.names(dfCoeff)[1], " * 10^6", sep="")
    row.names(dfCoeff)[9] = paste(row.names(dfCoeff)[9], " * 10^6", sep="")
    row.names(dfCoeff)[10] = paste(row.names(dfCoeff)[10], " * 10^6", sep="")
  } else {
    row.names(dfCoeff)[1] = paste(row.names(dfCoeff)[1], " * 10^3", sep="")
    row.names(dfCoeff)[9] = paste(row.names(dfCoeff)[9], " * 10^3", sep="")
    row.names(dfCoeff)[10] = paste(row.names(dfCoeff)[10], " * 10^3", sep="")
  }
  
  
  # Loop through every coefficient
  currRow = 0
  for(i in 1:(nrow(dfCoeff)-3)){
    
    # Incremenet rows with coefficients
    output = rbind(output, dfCoeff[i, ])
    currRow = currRow + 1
   
    
    
    # Add stars
    for(j in 1:ncol(output)){
      if(is.na(dfPValue[i, j])==FALSE){
        if(dfPValue[i, j] <= 0.1){
          output[currRow, j] = paste(output[currRow, j], "**", sep="")
        } else if(dfPValue[i, j] <= 1){
          output[currRow, j] = paste(output[currRow, j], "*", sep="")
        } else if(dfPValue[i, j] <= 5){
          output[currRow, j] = paste(output[currRow, j], "$^{+}$", sep="")
        }
      } else {
        output[currRow, j] = ""
      }
    }
    
    # Incremenet rows with tValues
    output = rbind(output, dfTValue[i, ])
    currRow = currRow + 1
    row.names(output)[currRow] = paste("tStat", row.names(output)[currRow-1], sep=' ')
    
    # Add parenthesis
    for(j in 1:ncol(output)){
      # Add parenthesis
      if(is.na(dfPValue[i, j])==FALSE){
        output[currRow, j] = paste("(", toString(output[currRow, j]), ")", sep="")
      } else {
        output[currRow, j] = ""
      }
    }
  }
  
  # Add observations, r-squared and adjusted r-squared
  output = rbind(output, dfCoeff[(nrow(dfCoeff)-2):nrow(dfCoeff), ])
  
  return(output)
}

