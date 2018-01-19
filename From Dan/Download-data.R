#Clear environment-------------------------------------------------------------------------
rm(list = ls())

#Load libraries----------------------------------------------------------------------------
library("openxlsx")
library(dplyr)


# Set working directory
setwd("~/Google Drive/Stanford Law Project")


#==========================================================================================
#====================================== GET DATA ==========================================
#==========================================================================================

#Load the raw data-------------------------------------------------------------------------
final_ipo_equity_data = read.xlsx("Data/final_ipo_equity_data.xlsx")
final_ma_data = read.xlsx("Data/final_ma_data.xlsx")
ma_data_newdef = read.xlsx("Data/MA_data_NEWDEF.xlsx")
mainData = read.xlsx("Data/database_2016.xlsx")

#Drop wrong column with same name
mainData = mainData[, -7]

#Fix wrong firm names
mainData[2520, "Firm.Name"] = "Wilmer Cutler Pickering"
mainData[2666, "Firm.Name"] = "Piper Rudnick"
mainData[3228, "Firm.Name"] = "Kirkpatrick & Lockhart"
mainData[3320, "Firm.Name"] = "Wilmer Cutler Pickering Hale and Dorr"
mainData[3347, "Firm.Name"] = "Bradley Arant Boult Cummings"
mainData[3721, "Firm.Name"] = "Wilmer Cutler Pickering Hale and Dorr"
mainData[4640, "Firm.Name"] = "Locke Lord"

final_ipo_equity_data[2520, "FirmName"] = "Wilmer Cutler Pickering"
final_ipo_equity_data[2666, "FirmName"] = "Piper Rudnick"
final_ipo_equity_data[3228, "FirmName"] = "Kirkpatrick & Lockhart"
final_ipo_equity_data[3320, "FirmName"] = "Wilmer Cutler Pickering Hale and Dorr"
final_ipo_equity_data[3347, "FirmName"] = "Bradley Arant Boult Cummings"
final_ipo_equity_data[3721, "FirmName"] = "Wilmer Cutler Pickering Hale and Dorr"
final_ipo_equity_data[4640, "FirmName"] = "Locke Lord"
final_ipo_equity_data[which(final_ipo_equity_data$FirmName=="Venable" & final_ipo_equity_data$year==2010), "equity_proceeds"] = 20686.8

ma_data_newdef[2520, "Firm.Name"] = "Wilmer Cutler Pickering"
ma_data_newdef[3320, "Firm.Name"] = "Wilmer Cutler Pickering Hale and Dorr"
ma_data_newdef[3721, "Firm.Name"] = "Wilmer Cutler Pickering Hale and Dorr"
ma_data_newdef[4640, "Firm.Name"] = "Locke Lord"




#==========================================================================================
#======================================= ETL ==============================================
#==========================================================================================

#Firm-specific data------------------------------------------------------------------------
#Updating date (since publication is one year ld, we need to make it 1 year ealier)
mainData$Year = mainData$Year.of.AmLaw.Publication - 1
final_ipo_equity_data$year = final_ipo_equity_data$year - 1
ma_data_newdef$year = ma_data_newdef$year - 1
final_ma_data$year = final_ma_data$year - 1

#Munging
mainData = mainData %>% 
  mutate(EqPartner.Lawyers = Number.of.Equity.Partners / Number.of.Lawyers) %>%
  select(Year,
         Firm.Name,
         Unique.Firm.Identifier,
         Number.of.Lawyers,
         EqPartner.Lawyers,
         Gross.Revenue,
         Net.Operating.Income, 
         `Am.Law.200.Rank.(FY:Previous.Year)`)

#Renaming columns
names(mainData) = c('Year', 'FirmName', 'FirmID', 'Lawyers', 'EqPartner.Lawyers', 'GrossRevenue', 'NOI', 'Rank')
mapToID = mainData[, c('Year', 'FirmName', 'FirmID')]

#Financial data----------------------------------------------------------------------------
#Munging
final_ipo_equity_data = final_ipo_equity_data %>% 
  select(year,
         FirmName,
         ipo_proceeds,
         equity_proceeds)

#Renaming columns
names(final_ipo_equity_data) = c('Year', 'FirmName', 'IPOProceeds', 'EquityProceeds')

#Getting the FirmID
final_ipo_equity_data = left_join(final_ipo_equity_data, mapToID, by=c('Year', 'FirmName'))

#M&A data ---------------------------------------------------------------------------------
#Munging
ma_data_newdef = ma_data_newdef %>% select(year, Firm.Name, dealvalue_new, no_deals_new)
final_ma_data = final_ma_data %>% select(year, FirmName, ma_deals)

#Renaming columns
names(ma_data_newdef) = c("Year", "FirmName", "MAValueNewDeals", "MANumberDealsNew")
#names(final_ma_data) = c("Year", "Firm", "MADeals")
#Getting the FirmID
ma_data_newdef = left_join(ma_data_newdef, mapToID, by=c('Year', 'FirmName'))


#Merging the dataframes--------------------------------------------------------------------
df = left_join(mainData, final_ipo_equity_data, by=c("Year", "FirmName", "FirmID"))
df = left_join(df, ma_data_newdef, by=c("Year", "FirmName", "FirmID"))
df = df %>% na.omit

#Create the new required variables---------------------------------------------------------
#Squared number of lawyers
df = df %>%
  mutate(Lawyers2 = Lawyers^2) 

#Aggregated MA proceeds
#df = df %>% group_by(Year) %>% mutate(AggMAProceeds = sum(MAValueNewDeals)) %>% ungroup()

#Aggregated MA proceeds with Hodrick's updated values
agg_dealogic_years = c(1988:2015)
agg_dealogic_values = c(336, 292, 225, 137, 124, 225, 347, 519, 659, 919, 1600, 1750, 1770,
                        757, 448, 524, 875,1300,1560, 1570, 925, 767, 875, 997, 980, 1180,
                        1610, 2360)
agg_dealogic = as.data.frame(cbind(agg_dealogic_years, agg_dealogic_values))
names(agg_dealogic) = c("Year", "AggMAProceedsLogic")
df = left_join(df, agg_dealogic, by="Year")

#Aggregated Equity Proceeds
df = df %>% group_by(Year) %>% mutate(AggEquityProceeds = sum(EquityProceeds)) %>% ungroup()

#Aggreagated IPO
df = df %>% group_by(Year) %>% mutate(AggIPOProceeds = sum(IPOProceeds))  %>% ungroup()

#Clear missing data
#df = df %>% na.omit()

#Clear for firms with low number of occurrences
minTimesAppearence = 5
df = df %>% group_by(FirmID) %>% mutate(OccCount = n()) %>% ungroup() %>% filter(OccCount >= minTimesAppearence) %>% select(-OccCount)

# Create Leverage variable
df = df %>% mutate(EqPartners = EqPartner.Lawyers*Lawyers) %>% 
  mutate(Leverage = (Lawyers - EqPartners)/EqPartners, NOI.eqPart = NOI/EqPartners) %>% 
  select(-EqPartner.Lawyers, -EqPartners)

# Rename the columns
names(df) = c("Year", "FirmName", "FirmID", "Lawyers", "GrossRev", "NOI", "Rank", 
              "IPO", "Equity", "MnA", "NumberMnA", "Lawyers2", "AggMnA", "AggEquity", "AggIPO", "Leverage", "NOI.eqPart")

#==========================================================================================
#======================================= SAVE =============================================
#==========================================================================================
save(df, file = 'Data/EliteLawDf.RData')
