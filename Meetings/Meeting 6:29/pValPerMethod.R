# from wholeResults, select the p-values per year

pvals <- wholeResults$AllFirmsMnAPval

pvaldf <- data.frame(pvals)
rownames(pvaldf) <- 1988:2015
colnames(pvaldf) <- c("Rev/Lawyer Linear Model",  "PPEP Linear Model")
pvaldf[,2] <- wholeResults$AllFirmsMnAPval
