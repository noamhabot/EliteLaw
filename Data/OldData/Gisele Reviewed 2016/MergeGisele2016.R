#Clear environment-------------------------------------------------------------------------
rm(list = ls())

# Set working directory
setwd("~/Google Drive/Stanford Law Project/Data/Gisele Reviewed 2016/")

library("openxlsx")
library("dplyr")


#==========================================================================================
#====================================== GET DATA ==========================================
#==========================================================================================

eqDF <- read.xlsx("Merged AmLaw league table data through 2016  - Equity.xlsx", 1)
ipoDF <- read.xlsx("Merged AmLaw league table data through 2016  - IPO.xlsx", 1)
mnaDF <- read.xlsx("Merged AmLaw league table data through 2016  - MnA.xlsx", 1)

df <- left_join(eqDF, ipoDF, by=c("Year", "FirmName", "GrossRevenue", "NOI", "AmLawRank",
                                  "AggMAProceedsLogic", "Lawyers", "EqPartner.Lawyers", "Lawyers2"))
df <- left_join(df, mnaDF, by=c("Year", "FirmName", "GrossRevenue", "NOI", "AmLawRank",
                                "AggMAProceedsLogic", "Lawyers", "EqPartner.Lawyers", "Lawyers2"))

df <- df %>% mutate(EqPartners = EqPartner.Lawyers*Lawyers) %>% 
  mutate(Leverage = (Lawyers - EqPartners)/EqPartners, NOI.eqPart = NOI/EqPartners) %>% 
  select(-EqPartner.Lawyers, -EqPartners)

agg_dealogic_years = c(1988:2016)
agg_dealogic_values = c(336, 292, 225, 137, 124, 225, 347, 519, 659, 919, 1600, 1750, 1770,
                        757, 448, 524, 875,1300,1560, 1570, 925, 767, 875, 997, 980, 1180,
                        1610, 2360, 1670)

agg_dealogic <- as.data.frame(cbind(agg_dealogic_years, agg_dealogic_values))
names(agg_dealogic) <- c("Year", "AggMnA")
df <- left_join(df, agg_dealogic, by="Year")


#Aggregated Equity Proceeds
df <- df %>% group_by(Year) %>% mutate(AggEquity = sum(EquityRevenue)) %>% ungroup()
#Aggreagated IPO
df <- df %>% group_by(Year) %>% mutate(AggIPO = sum(IPORevenue))  %>% ungroup()

save(df, file = '../EliteLawDf2016.RData')
