library(stringr)
library(ggplot2)

GenerateLatexInference = function(dfCoeff, dfTValue, dfPValue){
  
  
  
  # Adjust the name to account for units
  # if(useK == TRUE){
  #   row.names(dfCoeff)[1] = paste(row.names(dfCoeff)[1], " * 10^6", sep="")
  #   row.names(dfCoeff)[9] = paste(row.names(dfCoeff)[9], " * 10^6", sep="")
  #   row.names(dfCoeff)[10] = paste(row.names(dfCoeff)[10], " * 10^6", sep="")
  # } else {
  #   row.names(dfCoeff)[1] = paste(row.names(dfCoeff)[1], " * 10^3", sep="")
  #   row.names(dfCoeff)[9] = paste(row.names(dfCoeff)[9], " * 10^3", sep="")
  #   row.names(dfCoeff)[10] = paste(row.names(dfCoeff)[10], " * 10^3", sep="")
  # }
  
  
  # Loop through every coefficient
  output = data.frame()
  currRow = 0
  for(i in 1:(nrow(dfCoeff)-5)){
    
    # Incremenet rows with coefficients
    output = rbind(output, dfCoeff[i, ])
    currRow = currRow + 1
    
    
    
    powerOf10 <- 0
    output[is.na(output)] <- ""
    if (sum(is.na(output[currRow,])) == 0) {
      if (sum(abs(na.omit(as.numeric(output[currRow,]))) > 9999) > 0) {
        # if the number is great than 9999
        powerOf10 <- min(as.numeric(str_sub(formatC(na.omit(as.numeric(output[currRow, ])), format = "e", digits = 3), start= -2)))
        #rowChangeName <- (currRow+1)/2
        row.names(output)[currRow] = paste(row.names(output)[currRow], " * 10^", powerOf10, sep="")
      }
    }
    # Add stars
    for(j in 1:ncol(output)){
      if(is.na(dfPValue[i, j])==FALSE){
        
        if (powerOf10 > 0) {
          newOutput <- formatC(as.numeric(output[currRow, j]), format = "e", digits = 3)
          power <- as.numeric(str_sub(newOutput, start= -2))
          if (power - powerOf10 > 0) {
            newOutput <- formatC(as.numeric(output[currRow, j]), format = "e", digits = (3+power - powerOf10))
            newOutput <- substr(newOutput,1,nchar(newOutput)-4)
            newOutput <- as.character(as.numeric(newOutput)*10^(power - powerOf10))
          } else {
            newOutput <- substr(newOutput,1,nchar(newOutput)-4)
          }
          
          output[currRow, j] = newOutput
        }
        
        # these are 0.1, 1, 5 because we previously multiplied all of the p-values by 100
        # in the journal, this is the correct format:
        # ** p < 0.01
        # * p < 0.05
        # + p < 0.10
        if(dfPValue[i, j] <= 0.01){ # 0.1
          output[currRow, j] = paste(output[currRow, j], "**", sep="")
        } else if(dfPValue[i, j] <= 0.05){ # 1
          output[currRow, j] = paste(output[currRow, j], "*", sep="")
        } else if(dfPValue[i, j] <= 0.10){ # 5
          output[currRow, j] = paste(output[currRow, j], "$^{+}$", sep="")
        }
      } else {
        output[currRow, j] = ""
      }
    }
    
    # Incremenet rows with tValues
    #output = rbind(output, dfTValue[i, ])
    output = rbind(output, dfPValue[i, ])
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
  output = rbind(output, dfCoeff[(nrow(dfCoeff)-4):nrow(dfCoeff), ])
  
  return(output)
}






GenerateSummaryStatistics <- function(df) {
  answer <- data.frame(matrix(NA, nrow = 6, ncol = 0))
  numeric <- df[, sapply(df, is.numeric)]
  numeric <- numeric[, -grep("Hits", colnames(numeric))] # get rid of Hits variables
  numeric <- numeric[, -grep("FirmID", colnames(numeric))] # get rid of Hits variables
  #numeric <- numeric[, c(1:3, 5, 7, 6, 4, 8:ncol(numeric))]
  names(numeric) <- gsub(".eqPart", "/Eq Partner", names(numeric))
  names(numeric) <- gsub(".Lawyer", "/Lawyer", names(numeric))
  names(numeric) <- gsub("Lawyers2", "Lawyers^2", names(numeric))
  counter <- 1
  for (name in names(numeric)) {
    twofive <- quantile(as.data.frame(numeric[,counter])[,1], 0.25)
    sevenfive <- quantile(as.data.frame(numeric[,counter])[,1], 0.75)
    med <- median(as.data.frame(numeric[,counter])[,1])
    mn <- mean(as.data.frame(numeric[,counter])[,1])
    vec <- c(min(numeric[,counter]),twofive, med, mn, sevenfive, max(numeric[,counter]))
    # for (v in 1:length(vec)) {
    #   if (abs(as.numeric(vec[v])) > 9999) {
    #     vec[v] <- formatC(as.numeric(vec[v]), format = "e", digits = 3)
    #   } else {
    #     vec[v] <- formatC(as.numeric(vec[v]), format = "f", digits = 3)
    #   }
    # }
    # if (name == "Year") {
    #   vec <- as.character(round(as.numeric(vec)))
    # }
    #
    if (name == "Year") {
      vec <- as.character(round(as.numeric(vec)))
    } else if (name == "Leverage") {
      vec <- prettyNum(vec,big.mark=",",scientific=FALSE, trim=TRUE, digits=4)
    } else {
      vec <- prettyNum(vec,big.mark=",",scientific=FALSE, trim=TRUE, digits=0)
    }
    
    
    temp <- data.frame(name=vec)
    names(temp)[1] <- name
    answer <- cbind(answer, temp)
    counter <- counter + 1
  }
  
  names(answer) <- gsub("GrossRev", "Gross Rev", names(answer))
  names(answer) <- gsub(".eqPart", "/Eq Partners", names(answer))
  names(answer) <- gsub("EqPartners", "Eq Partners", names(answer))
  names(answer) <- gsub(".Lawyer", "/Lawyer", names(answer))
  names(answer) <- gsub("MnA", "MnA ", names(answer))
  names(answer) <- gsub("Equity", "Equity ", names(answer))
  names(answer) <- gsub("IPO", "IPO ", names(answer))
  names(answer) <- gsub("Agg", "Agg ", names(answer))
  names(answer) <- gsub("Rank", " Rank", names(answer))
  names(answer) <- gsub("Revenue", "Rev", names(answer))
  names(answer) <- gsub("Issues", "Count", names(answer))
  names(answer) <- gsub("NumOfDeals", "Count", names(answer))
  
  row.names(answer) <- c("Min", "1st Q", "Median", "Mean", "3rd Q", "Max")
  return(list(answer[1:6], answer[7:12], answer[13:19], answer[20:27]))
}



GenerateCorrelations <- function(df) {
  numeric <- df[, sapply(df, is.numeric)]
  #numeric$RevPLawyer <- numeric$GrossRevenue/numeric$Lawyers
  #numeric$NOIPLawyer <- numeric$NOI/numeric$Lawyers
  corDF <- numeric %>% select(Lawyers, Leverage, GrossRev, GrossRev.Lawyer, GrossRev.eqPart,
                              NOI, NOI.Lawyer, NOI.eqPart,
                              MnARevenue, IPORevenue, EquityRevenue)
  print(names(corDF))
  names(corDF) <- gsub("GrossRev", "Gross Rev", names(corDF))
  names(corDF) <- gsub(".eqPart", "/Eq Partner", names(corDF))
  names(corDF) <- gsub(".Lawyer", "/Lawyer", names(corDF))
  names(corDF) <- gsub("MnA", "M&A ", names(corDF))
  names(corDF) <- gsub("Equity", "Equity ", names(corDF))
  names(corDF) <- gsub("IPO", "IPO ", names(corDF))
  
  return (corDF)
}






GenerateCorrelationsByRank <- function(df) {
  corrDf = df %>% select(Lawyers, Leverage, GrossRev, GrossRev.Lawyer, GrossRev.eqPart, 
                         NOI, NOI.Lawyer, NOI.eqPart, MnARevenue, IPORevenue, EquityRevenue) %>% as.data.frame()
  names(corrDf) = c("Lawyers", "Leverage", "Rev", "Rev/Law", "Rev/EqPart", "NOI", "NOI/Law", "NOI/EqPart",
                    "MnA", "IPO", "Equity")
  
  gdpYears <- 1984:2016
  GDP <- c(4.041, 4.347, 4.590, 4.870, 5.253, 5.658, 5.980, 6.174, 6.539, 6.879, 7.309, 7.664,
           8.1, 8.609, 9.089, 9.661, 10.285, 10.622, 10.978, 11.511, 12.275, 13.094, 13.856,
           14.478, 14.719, 14.419, 14.964, 15.518, 16.155, 16.692, 17.428, 18.121, 18.625)
  GDP <- 100*GDP
  gdpDF <- data.frame(gdpYears, GDP)
  
  # Correlations
  correlRanks = as.data.frame(unique(sort(df$Year)))
  names(correlRanks)[1] <- "Year"
  
  # Add Aggregated M&A and GDP
  correlRanks = cbind(correlRanks, df %>% select(Year, AggMnA) %>% unique() %>% arrange(Year) %>% select(AggMnA))
  correlRanks$GDP <- gdpDF$GDP
  
  correlations = data.frame(matrix(nrow=1,ncol=12))
  counter <- 1
  for (n in c(1,2,10,50,75,100,125,150,175,200)) {
    thisRow <- c()
    # Top n
    # GrossRev
    correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(n, GrossRev) %>% filter(GrossRev == min(GrossRev)) %>% summarise(AvgRev = mean(GrossRev)) %>% arrange(Year) %>% select(AvgRev))
    names(correlRanks)[ncol(correlRanks)] <- paste("Rank",n,".Rev", sep="")
    thisRow <- c(thisRow, cor(correlRanks[,ncol(correlRanks)], correlRanks$AggMnA))
    thisRow <- c(thisRow, cor(correlRanks[,ncol(correlRanks)], correlRanks$GDP))
    # Rev/Lawyer
    correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(n, GrossRev.Lawyer) %>% filter(GrossRev.Lawyer == min(GrossRev.Lawyer)) %>% summarise(AvgGrossRev.Lawyer = mean(GrossRev.Lawyer)) %>% arrange(Year) %>% select(AvgGrossRev.Lawyer))
    names(correlRanks)[ncol(correlRanks)] <- paste("Rank",n,".Rev.Lawyer", sep="")
    thisRow <- c(thisRow, cor(correlRanks[,ncol(correlRanks)], correlRanks$AggMnA))
    thisRow <- c(thisRow, cor(correlRanks[,ncol(correlRanks)], correlRanks$GDP))
    # Rev/eqPart
    correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(n, GrossRev.eqPart) %>% filter(GrossRev.eqPart == min(GrossRev.eqPart)) %>% summarise(AvgGrossRev.eqPart = mean(GrossRev.eqPart)) %>% arrange(Year) %>% select(AvgGrossRev.eqPart))
    names(correlRanks)[ncol(correlRanks)] <- paste("Rank",n,".Rev.EqPart", sep="")
    thisRow <- c(thisRow, cor(correlRanks[,ncol(correlRanks)], correlRanks$AggMnA))
    thisRow <- c(thisRow, cor(correlRanks[,ncol(correlRanks)], correlRanks$GDP))
    
    # NOI
    correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(n, NOI) %>% filter(NOI == min(NOI)) %>% summarise(AvgNOI = mean(NOI)) %>% arrange(Year) %>% select(AvgNOI))
    names(correlRanks)[ncol(correlRanks)] <- paste("Rank",n,".NOI", sep="")
    thisRow <- c(thisRow, cor(correlRanks[,ncol(correlRanks)], correlRanks$AggMnA))
    thisRow <- c(thisRow, cor(correlRanks[,ncol(correlRanks)], correlRanks$GDP))
    # NOI/Lawyer
    correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(n, NOI.Lawyer) %>% filter(NOI.Lawyer == min(NOI.Lawyer)) %>% summarise(AvgNOI.Lawyer = mean(NOI.Lawyer)) %>% arrange(Year) %>% select(AvgNOI.Lawyer))
    names(correlRanks)[ncol(correlRanks)] <- paste("Rank",n,".NOI.Lawyer", sep="")
    thisRow <- c(thisRow, cor(correlRanks[,ncol(correlRanks)], correlRanks$AggMnA))
    thisRow <- c(thisRow, cor(correlRanks[,ncol(correlRanks)], correlRanks$GDP))
    # NOI/EqPart
    correlRanks = cbind(correlRanks, df %>% group_by(Year) %>% top_n(n, NOI.eqPart) %>% filter(NOI.eqPart == min(NOI.eqPart)) %>% summarise(AvgNOI.EqPart = mean(NOI.eqPart)) %>% arrange(Year) %>% select(AvgNOI.EqPart))
    names(correlRanks)[ncol(correlRanks)] <- paste("Rank",n,".NOI.EqPart", sep="")
    thisRow <- c(thisRow, cor(correlRanks[,ncol(correlRanks)], correlRanks$AggMnA))
    thisRow <- c(thisRow, cor(correlRanks[,ncol(correlRanks)], correlRanks$GDP))
    
    correlations[counter,] <- thisRow
    row.names(correlations)[counter] <- paste("Rank", n)
    counter <- counter + 1
  }
  cnames <- c()
  for (var in c("GrossRev", "Rev/Law", "Rev/EqPart", "NOI", "NOI/Law", "NOI/EqPart")) {
    for (t in c("AggMnA", "GDP")) {
      cnames <- c(cnames, paste(var, t))
    }
  }
  names(correlations) <- cnames
  
  
  return(list(correlations, correlRanks))
}


SaveCorrelationsByRankPlots <- function(df, correlRanks) {
  
  outcomes <- c("Rev", "Rev.Lawyer", "Rev.EqPart", "NOI", "NOI.Lawyer", "NOI.EqPart")
  
  outcomeText <- gsub(".EqPart", "/Eq Partner", outcomes)
  outcomeText <- gsub(".Lawyer", "/Lawyer", outcomeText)
  outcomeText <- gsub("Rev", "Gross Rev", outcomeText)
  
  
  for (i in 1:length(outcomes)) {
    outcome <- outcomes[i]
    subset <- names(correlRanks)[grepl(outcome, names(correlRanks))]
    if (outcome == "Rev" || outcome == "NOI") {
      subset <- subset[!grepl(".EqPart", subset)]
      subset <- subset[!grepl(".Lawyer", subset)]
    }
    subset <- c("Year", "AggMnA", "GDP", subset)
    
    
    
    thisDF <- correlRanks[,subset]
    names(thisDF) <- gsub(paste(".",outcome,sep=""), "", names(thisDF))
    fac <- max(thisDF) * 1.6 / max(correlRanks$AggMnA)
    
    #fac <- 3000
    thisDF$GDP <- thisDF$GDP*fac
    thisDF$AggMnA <- thisDF$AggMnA*fac
    
    # create the plotting data frame
    plotDF <- melt(thisDF, id="Year")
    # add the linetypes
    plotDF$lt <- rep("dotdash", nrow(plotDF))
    # change AggMnA to different line style
    plotDF$lt[plotDF$variable == "AggMnA"] <- "solid"
    plotDF$lt[plotDF$variable == "GDP"] <- "solid"
    
    p <- ggplot(data=plotDF, aes(x=Year, y=value, color=variable, linetype=lt)) + geom_line() + 
      labs(title = paste("Ranked Firms by", outcomeText[i]), x="Year", y=outcomeText[i]) +
      scale_linetype_discrete(guide=FALSE) + scale_color_discrete(name="Variable") + theme_bw() +
      scale_y_continuous(sec.axis = sec_axis(~./fac, name = "Agg M&A, GDP (in millions)"))
    graphName <- paste("IndivTexOutput/MnAGDP-", i, ".jpg", sep="")
    ggsave(graphName, p)
  }
}


printHorizontally <- function(mainDF) {
  mainDFH <- t(mainDF)
  numPerLine <- 6
  for (i in 1:round(ncol(mainDFH)/numPerLine)) {
    istr = as.character(i)
    if (i < 10) {
      istr = paste("0",i,sep="")
    }
    end <- i*numPerLine
    if (end > ncol(mainDFH)) {
      end <- ncol(mainDFH)
    }
    writeDF <- mainDF[,(((i-1)*numPerLine)+1):end]
    print(xtable(mainDFH[,(((i-1)*numPerLine)+1):end], digits=3), 
          file=paste("IndivTexOutput/performance",istr,".tex",sep=""),
          sanitize.text.function=function(x){x})
  }
}

printVertically <- function(mainDF) {
  numPerLine <- 40
  for (i in 1:ceiling(nrow(mainDF)/numPerLine)) {
    istr = as.character(i)
    if (i < 10) {
      istr = paste("0",i,sep="")
    }
    end <- i*numPerLine
    if (end > nrow(mainDF)) {
      end <- nrow(mainDF)
    }
    writeDF <- mainDF[(((i-1)*numPerLine)+1):end,]
    print(xtable(writeDF, digits=3), 
          file=paste("IndivTexOutput/performance",istr,".tex",sep=""),
          sanitize.text.function=function(x){x})
  }
}



# Generate the Performance Graphs
GeneratePerformanceTable <- function(resultsPerformance) {
  resultsPerformance1 <- t(resultsPerformance)
  
  outcomes <- c("GrossRev_NoRatio", "GrossRev_PerLawyer", "GrossRev.eqPart_NoRatio", 
                "NOI_NoRatio", "NOI_PerLawyer", "NOI.eqPart_NoRatio")
  types <- c("Both", "Revenue", "Deals")
  lawyers <- c("WithLawyers2", "WithLawyersLog", "WithoutLawyers")
  
  mainDF <- data.frame(matrix(ncol=8, nrow=0))
  
  for (i in 1:length(outcomes)) {
    outcome <- outcomes[i]
    subset <- row.names(resultsPerformance1)[grepl(outcome, row.names(resultsPerformance1))]
    
    thisDF <- data.frame()
    
    for (type in types) {
      subset1 <- subset[grepl(type, subset)]
      
      for (lawyer in lawyers) {
        subset2 <- subset1[grepl(lawyer, subset1)]
        
        fixedEffects <- c()
        splitted <- strsplit(subset2, '_')
        for (line in splitted) {
          fixedEffects <- c(fixedEffects, paste(line[5], line[6], sep="\\_"))
        }
        
        mainDF <- rbind(mainDF,cbind(rep(outcome,length(subset2)), rep(type,length(subset2)), 
                                     rep(lawyer,length(subset2)), fixedEffects, resultsPerformance1[subset2,]))
        
      }
    }
  }
  
  row.names(mainDF) <- 1:nrow(mainDF)
  names(mainDF) <- c("Outcome", "Dependent Variables", "Lawyers", "Fixed Effects", "Adj R^2", names(mainDF)[6:8])
  
  mainDF$Outcome <- gsub("_NoRatio", "", mainDF$Outcome)
  mainDF$Outcome <- gsub("GrossRev", "Gross Rev", mainDF$Outcome)
  mainDF$Outcome <- gsub("_PerLawyer", "/Lawyer", mainDF$Outcome)
  mainDF$Outcome <- gsub(".eqPart", "/Eq Partner", mainDF$Outcome)
  
  mainDF$`Dependent Variables` <- gsub("Both", "Revenue + Deals", mainDF$`Dependent Variables`)
  
  mainDF$Lawyers <- gsub("WithLawyers2", "Lawyers^2", mainDF$Lawyers)
  mainDF$Lawyers <- gsub("WithLawyersLog", "log(Lawyers)", mainDF$Lawyers)
  mainDF$Lawyers <- gsub("WithoutLawyers", "No", mainDF$Lawyers)
  
  #printHorizontally(mainDF)
  printVertically(mainDF)
  
  
  
  # plot the scores by order with decreasing R^2
  orderedDF <- t(mainDF)
  orderedDF <- mainDF[order(as.numeric(mainDF[,8]), decreasing=FALSE),]
  colnames(orderedDF) <- c("Adj.R2", "AIC", "BIC", "CV")
  orderedDF <- data.frame(apply(orderedDF, 2, as.numeric))
  orderedDF <- melt(orderedDF, id="Adj.R2")
  ggplot(data=orderedDF, aes(x=Adj.R2, y=value)) + geom_line(data=orderedDF,aes(color=variable, alpha=0.2)) + 
    ggtitle(("AIC, BIC, CV vs. Adj.R^2"))
  
  
  #return(mainDF)
}
#GeneratePerformanceTable(resultsPerformance)







# # Only after 1998 there was more than 100 firms!
# lessThan100 = df %>% group_by(Year) %>% summarise(lessThan100 = n()<=100) %>% filter (lessThan100 == TRUE) %>% select(Year) %>% as.list()
# greaterThan100 = df %>% group_by(Year) %>% summarise(lessThan100 = n()>100) %>% filter (lessThan100 == TRUE) %>% select(Year) %>% as.list()
# correlRanks[which(correlRanks$Year %in% lessThan100$Year), grepl("Rank100", names(correlRanks))] = NA
# 
# View(correlRanks)
# 
# 
# 
# correlations.Rev = data.frame(row.names = c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100"))
# correlations.Rev = cbind(correlations.Rev, NA, NA)
# names(correlations.Rev) = c("Correl_Rev_AggMA", "Correl_Rev_GDP")
# dfRank.Rev = correlRanks %>% select(Rank1.Rev, Rank2.Rev, Rank10.Rev, Rank50.Rev, Rank100.Rev, AggMnA, GDP)
# 
# correlations.Rev.Lawyer = data.frame(row.names = c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100"))
# correlations.Rev.Lawyer = cbind(correlations.Rev.Lawyer, NA, NA)
# names(correlations.Rev.Lawyer) = c("Correl_Rev_AggMA", "Correl_Rev_GDP")
# dfRank.Rev.Lawyer = correlRanks %>% select(Rank1.Rev.Lawyer, Rank2.Rev.Lawyer, Rank10.Rev.Lawyer, Rank50.Rev.Lawyer, Rank100.Rev.Lawyer, AggMnA, GDP)
# 
# correlations.Rev.EqPart = data.frame(row.names = c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100"))
# correlations.Rev.EqPart = cbind(correlations.Rev.EqPart, NA, NA)
# names(correlations.Rev.EqPart) = c("Correl_Rev_AggMA", "Correl_Rev_GDP")
# dfRank.Rev.EqPart = correlRanks %>% select(Rank1.Rev.EqPart, Rank2.Rev.EqPart, Rank10.Rev.EqPart, Rank50.Rev.EqPart, Rank100.Rev.EqPart, AggMnA, GDP)
# 
# 
# correlations.NOI = data.frame(row.names = c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100"))
# correlations.NOI = cbind(correlations.NOI, NA, NA)
# names(correlations.NOI) = c("Correl_Rev_AggMA", "Correl_Rev_GDP")
# dfRank.NOI = correlRanks %>% select(Rank1.NOI, Rank2.NOI, Rank10.NOI, Rank50.NOI, Rank100.NOI, AggMnA, GDP)
# 
# correlations.NOI.Lawyer = data.frame(row.names = c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100"))
# correlations.NOI.Lawyer = cbind(correlations.NOI.Lawyer, NA, NA)
# names(correlations.NOI.Lawyer) = c("Correl_Rev_AggMA", "Correl_Rev_GDP")
# dfRank.NOI.Lawyer = correlRanks %>% select(Rank1.NOI.Lawyer, Rank2.NOI.Lawyer, Rank10.NOI.Lawyer, Rank50.NOI.Lawyer, Rank100.NOI.Lawyer, AggMnA, GDP)
# 
# correlations.NOI.EqPart = data.frame(row.names = c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100"))
# correlations.NOI.EqPart = cbind(correlations.NOI.EqPart, NA, NA)
# names(correlations.NOI.EqPart) = c("Correl_Rev_AggMA", "Correl_Rev_GDP")
# dfRank.NOI.EqPart = correlRanks %>% select(Rank1.NOI.EqPart, Rank2.NOI.EqPart, Rank10.NOI.EqPart, Rank50.NOI.EqPart, Rank100.NOI.EqPart, AggMnA, GDP)
# 
# for(i in 1:5){
#   correlations.Rev[i,1] = cor(cbind(dfRank.Rev[,i], dfRank.Rev$AggMnA) %>% na.omit())[2, 1]
#   correlations.Rev[i,2] = cor(cbind(dfRank.Rev[,i], dfRank.Rev$GDP) %>% na.omit())[2, 1]
#   
#   correlations.Rev.Lawyer[i,1] = cor(cbind(dfRank.Rev.Lawyer[,i], dfRank.Rev.Lawyer$AggMnA) %>% na.omit())[2, 1]
#   correlations.Rev.Lawyer[i,2] = cor(cbind(dfRank.Rev.Lawyer[,i], dfRank.Rev.Lawyer$GDP) %>% na.omit())[2, 1]
#   
#   correlations.Rev.EqPart[i,1] = cor(cbind(dfRank.Rev.EqPart[,i], dfRank.Rev.EqPart$AggMnA) %>% na.omit())[2, 1]
#   correlations.Rev.EqPart[i,2] = cor(cbind(dfRank.Rev.EqPart[,i], dfRank.Rev.EqPart$GDP) %>% na.omit())[2, 1]
#   
#   
#   correlations.NOI[i,1] = cor(cbind(dfRank.NOI[,i], dfRank.NOI$AggMnA) %>% na.omit())[2, 1]
#   correlations.NOI[i,2] = cor(cbind(dfRank.NOI[,i], dfRank.NOI$GDP) %>% na.omit())[2, 1]
#   
#   correlations.NOI.Lawyer[i,1] = cor(cbind(dfRank.NOI.Lawyer[,i], dfRank.NOI.Lawyer$AggMnA) %>% na.omit())[2, 1]
#   correlations.NOI.Lawyer[i,2] = cor(cbind(dfRank.NOI.Lawyer[,i], dfRank.NOI.Lawyer$GDP) %>% na.omit())[2, 1]
#   
#   correlations.NOI.EqPart[i,1] = cor(cbind(dfRank.NOI.EqPart[,i], dfRank.NOI.EqPart$AggMnA) %>% na.omit())[2, 1]
#   correlations.NOI.EqPart[i,2] = cor(cbind(dfRank.NOI.EqPart[,i], dfRank.NOI.EqPart$GDP) %>% na.omit())[2, 1]
# }
# 
# colnames(correlations.Rev) <- c("AggMnA", "GDP")
# colnames(correlations.Rev.Lawyer) <- c("AggMnA", "GDP")
# colnames(correlations.Rev.EqPart) <- c("AggMnA", "GDP")
# colnames(correlations.NOI) <- c("AggMnA", "GDP")
# colnames(correlations.NOI.Lawyer) <- c("AggMnA", "GDP")
# colnames(correlations.NOI.EqPart) <- c("AggMnA", "GDP")
# 
# print(xtable(correlations.Rev), file="Generate Latex/IndivTexOutput/MnAGDP-GrossRevenue.tex")
# print(xtable(correlations.Rev.Lawyer), file="Generate Latex/IndivTexOutput/MnAGDP-GrossRevenuePLawyer.tex")
# print(xtable(correlations.Rev.EqPart), file="Generate Latex/IndivTexOutput/MnAGDP-GrossRevenuePEqPart.tex")
# print(xtable(correlations.NOI), file="Generate Latex/IndivTexOutput/MnAGDP-NOI.tex")
# print(xtable(correlations.NOI.Lawyer), file="Generate Latex/IndivTexOutput/MnAGDP-NOIPLawyer.tex")


# First part
# par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
# plot(correlRanks$Year, correlRanks$Rank1.NOI.EqPart, type='l', col='brown2', lwd=2, ylab="NOI / Equity Partner", xlab="Year", 
#      main="Ranked Firms & GDP", ylim = c(min(correlRanks$Rank50.NOI.EqPart, na.rm=TRUE), max(correlRanks$Rank1.NOI.EqPart, na.rm=TRUE))) # first plot
# 
# #Lines
# lines(correlRanks$Year, correlRanks$Rank2.NOI.EqPart, col='magenta', lwd=1)
# lines(correlRanks$Year, correlRanks$Rank10.NOI.EqPart, col='darkgreen', lwd=1)
# lines(correlRanks$Year, correlRanks$Rank50.NOI.EqPart, col='darkgoldenrod4', lwd=1)
# lines(correlRanks$Year, correlRanks$Rank100.NOI.EqPart, col='blue', lwd=1)
# legend(1984, 6.6e+06, c("Rank 1", "Rank 2", "Rank 10", "Rank 50", "Rank 100", "GDP"),# places a legend at the appropriate place
#        lwd=2, lty=c(1, 1, 1, 1, 1, 6), 
#        col=c("brown2", "magenta", "darkgreen", "darkgoldenrod4", "blue", "black"), cex=1) # gives the legend lines 
# 
# # Second  part (agg M&A)
# par(new = TRUE)
# plot(correlRanks$Year, correlRanks$GDP, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", lwd=4, lty=6, col="black")
# axis(side=4, at = pretty(range(correlRanks$GDP, na.rm = TRUE)))
# mtext("GDP", side=4, line=3)
