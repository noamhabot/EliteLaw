#Clear environment-------------------------------------------------------------------------
rm(list = ls())

# Set working directory
setwd("~/Google Drive/EliteLaw/Merged Data 2016/")

library("openxlsx")
library("dplyr")


#==========================================================================================
#====================================== GET DATA ==========================================
#==========================================================================================

eqDF <- read.csv("EquityBonds-1984-2016.csv")
ipoDF <- read.csv("IPO-1984-2016.csv")
mnaDF <- read.csv("MnA-1984-2016.csv")
amlaw <- read.xlsx("AmLaw200-1985-2017.xlsx", 1)
colnames(amlaw)[1] <- "Year"
amlaw$Year <- amlaw$Year - 1

# hardcode some fixes to firmnames
from <- c("Akerman")
to <- c("Akerman Senterfitt")
changes <- data.frame(as.character(from), as.character(to))
for (i in 1:nrow(changes)) {
  amlaw$Firm.Name <- as.character(amlaw$Firm.Name)
  eqDF$FirmName <- as.character(eqDF$FirmName)
  ipoDF$FirmName <- as.character(ipoDF$FirmName)
  mnaDF$FirmName <- as.character(mnaDF$FirmName)
  
  amlaw$Firm.Name <- replace(amlaw$Firm.Name, amlaw$Firm.Name==changes[i,1], as.character(changes[i,2]))
  eqDF$FirmName <- replace(eqDF$FirmName, eqDF$FirmName==changes[i,1], as.character(changes[i,2]))
  ipoDF$FirmName <- replace(ipoDF$FirmName, ipoDF$FirmName==changes[i,1], as.character(changes[i,2]))
  mnaDF$FirmName <- replace(mnaDF$FirmName, mnaDF$FirmName==changes[i,1], as.character(changes[i,2]))
}



# get rid of asterisks in the rank columns

ridAsterisks <- function(df) {
  idx <- which(grepl('Rank', colnames(eqDF)))
  df[,idx] <- as.character(df[,idx])
  df[,idx] <- gsub("*", "", df[,idx], fixed=TRUE)
  df[,idx] <- as.numeric(df[,idx])
  return(df)
}
eqDF <- ridAsterisks(eqDF)
ipoDF <- ridAsterisks(ipoDF)
mnaDF <- ridAsterisks(mnaDF)

replacePunctuationFirmNames <- function(df, col) {
  short <- tolower(as.character(df[,col]))
  short <- gsub(", ", " ", short, fixed=TRUE)
  short <- gsub(" & ", " ", short, fixed=TRUE)
  short <- gsub(" and ", " ", short, fixed=TRUE)
  short <- gsub(" LLP", "", short, fixed=TRUE)
  short <- gsub("^((\\w+\\W+){1}\\w+).*$","\\1",short)
  df$Short <- as.factor(short)
  return(df)
}

eqDF <- replacePunctuationFirmNames(eqDF, 1)
ipoDF <- replacePunctuationFirmNames(ipoDF, 1)
mnaDF <- replacePunctuationFirmNames(mnaDF, 1)
amlaw <- replacePunctuationFirmNames(amlaw, "Firm.Name")


# deal with duplicates for a single firm for the same year
eqDF <- eqDF %>% group_by_(.dots=c("Short","Year")) %>% mutate(EquityBonds.Revenue=sum(EquityBonds.Revenue),
                                                               EquityBonds.Rank=min(EquityBonds.Rank[EquityBonds.Rank>0]),
                                                               EquityBonds.Market.Share=sum(EquityBonds.Market.Share),
                                                               EquityBonds.Number.of.Issues=sum(EquityBonds.Number.of.Issues),
                                                               EquityHits=max(row_number())) %>%
                                                        filter(row_number() == 1)
ipoDF <- ipoDF %>% group_by_(.dots=c("Short","Year")) %>% mutate(IPO.Revenue=sum(IPO.Revenue), 
                                                                 IPO.Rank=min(IPO.Rank[IPO.Rank>0]),
                                                                 IPO.Market.Share=sum(IPO.Market.Share),
                                                                 IPO.Num.of.Issues=sum(IPO.Num.of.Issues),
                                                                 IPOHits=max(row_number())) %>%
                                                        filter(row_number() == 1)
mnaDF <- mnaDF %>% group_by_(.dots=c("Short","Year")) %>% mutate(MnA.Revenue=sum(MnA.Revenue),
                                                                 MnA.Rank=min(MnA.Rank[MnA.Rank>0]),
                                                                 MnA.Market.Share=sum(MnA.Market.Share),
                                                                 Mna.Num.Of.Deals=sum(Mna.Num.Of.Deals),
                                                                 MnAHits=max(row_number())) %>%
                                                        filter(row_number() == 1)


#Firm-specific data------------------------------------------------------------------------
amlaw <- amlaw %>% 
  mutate(EqPartner.Lawyers = Number.of.Equity.Partners / Number.of.Lawyers) %>%
  select(Year,
         Firm.Name, Short,
         Number.of.Lawyers,
         EqPartner.Lawyers,
         Gross.Revenue,
         Net.Operating.Income, 
         `Am.Law.200.Rank.(FY:Previous.Year)`)
names(amlaw) = c('Year', 'FirmName', 'Short', 'Lawyers', 'EqPartner.Lawyers', 'GrossRev', 'NOI', 'AmLawRank')


#Financial data----------------------------------------------------------------------------
final_ipo_equity_data <- full_join(eqDF, ipoDF, by=c("Year" = "Year", "Short" = "Short")) %>% 
    select(Year, FirmName.x, Short, 
           EquityBonds.Revenue,  EquityBonds.Rank, EquityBonds.Market.Share, EquityBonds.Number.of.Issues,
           IPO.Revenue, IPO.Rank, IPO.Market.Share, IPO.Num.of.Issues, 
           EquityHits, IPOHits)
names(final_ipo_equity_data) = c('Year', 'FirmName', 'Short', 
                                 'EquityRevenue', 'EquityRank', 'EquityMarketShare', 'EquityIssues', 
                                 'IPORevenue', 'IPORank', 'IPOMarketShare', 'IPOIssues', 
                                 'EquityHits', 'IPOHits')






#M&A data ---------------------------------------------------------------------------------
mnaDF <- mnaDF %>% select(Year, FirmName, MnA.Revenue, MnA.Rank, MnA.Market.Share, Mna.Num.Of.Deals, Short, MnAHits)
names(mnaDF) <- c("Year", "FirmName", "MnARevenue", "MnARank", "MnAMarketShare", "MnANumOfDeals", "Short", 'MnAHits')






df <- left_join(amlaw, final_ipo_equity_data %>% select(-FirmName), by=c("Year" = "Year", "Short" = "Short"))
df <- left_join(df, mnaDF %>% select(-FirmName), by=c("Year" = "Year", "Short" = "Short"))
df <- df %>% mutate(Lawyers2 = Lawyers^2) 
df <- df %>% mutate(LawyersLog = log(Lawyers))



############## Adding the Agg variables (MnA, Equity, IPO) ##############

# Add AggMnA to the dataset
agg_dealogic_years = c(1988:2016)
agg_dealogic_values = c(336, 292, 225, 137, 124, 225, 347, 519, 659, 919, 1600, 1750, 1770,
                        757, 448, 524, 875,1300,1560, 1570, 925, 767, 875, 997, 980, 1180,
                        1610, 2360, 1670)
agg_dealogic = as.data.frame(cbind(agg_dealogic_years, agg_dealogic_values))
names(agg_dealogic) = c("Year", "AggMnA")
df <- left_join(df, agg_dealogic, by="Year")



# Add AggEquity to the dataset
# source: https://www.federalreserve.gov/econresdata/releases/corpsecure/corpsecure20090131.htm, Table 1.46 row 8
equity_federal_reserve <- na.omit(read.xlsx("AggEquity.xlsx", 1))
df <- left_join(df, equity_federal_reserve, by="Year")

# Add AggIPO to the dataset
# source: https://site.warrington.ufl.edu/ritter/files/2018/01/IPOs2017Statistics_January17_2018.pdf, Table 1
aggipo_data <- na.omit(read.xlsx("AggIPO.xlsx", 1))
df <- left_join(df, aggipo_data, by="Year")


################## Add the GDP ##################
gdpYears <- 1984:2016
# source: https://www.bea.gov/iTable/iTableHtml.cfm?reqid=19&step=3&isuri=1&1910=x&0=-9&1921=survey&1903=5&1904=1984&1905=2018&1906=a&1911=0
GDP <- c(4040.7,4346.7,4590.2,4870.2,5252.6,5657.7,5979.6,6174.0,6539.3,6878.7,7308.8,7664.1,
         8100.2,8608.5,9089.2,9660.6,10284.8,10621.8,10977.5,11510.7,12274.9,13093.7,13855.9,
         14477.6,14718.6,14418.7,14964.4,15517.9,16155.3,16691.5,17427.6,18120.7,18624.5)
gdpDF <- data.frame(gdpYears, GDP)
names(gdpDF) <- c("Year", "GDP")
df <- left_join(df, gdpDF, by="Year")



################################### Creating a few new variables ###################################
# Create Leverage variable
df <- df %>% mutate(EqPartners = EqPartner.Lawyers*Lawyers) %>% 
  mutate(Leverage = (Lawyers - EqPartners)/EqPartners) %>% 
  select(-EqPartner.Lawyers)

# rewrite any NA's with 0's
df[is.na(df)] <- 0


#Aggregated M&A
df <- df %>% group_by(Year) %>% mutate(CumulativeMnA = sum(MnARevenue)) %>% ungroup()

#Aggregated Equity Proceeds
df <- df %>% group_by(Year) %>% mutate(CumulativeEquity = sum(EquityRevenue)) %>% ungroup()

#Aggreagated IPO
df <- df %>% group_by(Year) %>% mutate(CumulativeIPO = sum(IPORevenue))  %>% ungroup()

# Add both GrossRev/Lawyer and GrossRev/EquityPartner
df <- df %>% mutate(GrossRev.Lawyer = GrossRev/Lawyers, GrossRev.eqPart = GrossRev/EqPartners)

# Add both NOI/Lawyer and NOI/EquityPartner
df <- df %>% mutate(NOI.Lawyer = NOI/Lawyers, NOI.eqPart = NOI/EqPartners)
##########################################################################################



################################ Tweaks according to "hits" ###############################
# if a firm has hit > 1, set its hit and rank to 0 (for each: MnA, IPO, Equity)

# MnA
df$MnARank[df$MnAHits > 1] <- 0
df$MnAHits[df$MnAHits > 1] <- 0
# Equity
df$EquityRank[df$EquityHits > 1] <- 0
df$EquityHits[df$EquityHits > 1] <- 0
# IPO
df$IPORank[df$IPOHits > 1] <- 0
df$IPOHits[df$IPOHits > 1] <- 0


##########################################################################################




################################### Reordering of the data frame ###################################


df$FirmID <- as.numeric(as.factor(df$Short))

#Clear for firms with low number of occurrences
minTimesAppearence <- 5
df <- df %>% group_by(FirmID) %>% mutate(OccCount = n()) %>% ungroup() %>% filter(OccCount >= minTimesAppearence) %>% select(-OccCount)


toWrite <- df %>% select(-Short)

# add the "Hits" columns to the end
col_idx <- grep("Lawyers", names(toWrite))
col_idx <- c(col_idx, grep("Firm", names(toWrite)))
col_idx <- c(col_idx, grep("GrossRev", names(toWrite)))
col_idx <- c(col_idx, grep("NOI", names(toWrite)))
col_idx <- c(col_idx, grep("MnA", names(toWrite)))
col_idx <- c(col_idx, grep("Equity", names(toWrite)))
col_idx <- c(col_idx, grep("IPO", names(toWrite)))
toWrite <- toWrite[, c((1:ncol(toWrite))[-col_idx], col_idx)]
##########################################################################################

df <- toWrite
setwd("~/Google Drive/EliteLaw/Data/")
save(df, file="EliteLawDf2016.RData")

# write it to a new excel file
write.xlsx(as.data.frame(df), "2016FirmData.xlsx", row.names=FALSE)


