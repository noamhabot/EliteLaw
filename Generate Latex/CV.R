# Function to perform cross validation


CrossValidationError = function(currDf, nCV){
  library(glmnet)
  
  # Initialize vector with 0s and random sequence
  cvErrors = rep(0, nCV)
  randomOrder = sample(nrow(currDf), nrow(currDf))
  
  # Loop for every iteration
  for(cvIter in 1:nCV){
    
    # Set training and validation sets for each iteration
    nStart = (round((cvIter-1)*nrow(currDf)/nCV, 0)+1)
    if(cvIter == nCV){nEnd = nrow(currDf)} else {nEnd = round((cvIter)*nrow(currDf)/nCV, 0)}
    validation = randomOrder[nStart:nEnd]
    train = seq(1, nrow(currDf))[-validation]
    
    # Run Linear Regression on the training set
    linReg = lm(Output ~ ., data=currDf[ , !(names(currDf) %in% c("FirmName"))], subset = train)
    
    # Evaluate on the validation set
    linRegPredictions = predict.lm(linReg, newdata = currDf[validation,])    
    
    # Store the error
    cvErrors[cvIter] = mean((linRegPredictions - currDf$Output[validation])^2, na.rm = TRUE)
  }
  
  # Return the average error
  return(mean(cvErrors))
}

