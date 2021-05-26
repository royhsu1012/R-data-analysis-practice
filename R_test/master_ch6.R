# Load packages
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(stargazer)
library(sandwich)
library(lmtest)
library(zoo)
library(moments)
library(Hmisc)
library(psych)
library(markovchain)
#################
setwd("D:/data/step2_data")
####dfm_daily
dfm_daily <- read.csv("dfm_daily.csv", header = TRUE, sep = ",")
dfm_daily$TradeDate <- as.Date(dfm_daily$TradeDate)
####df_rf_daily
df_rf_daily <- read.csv("df_rf_daily.csv", header = TRUE, sep = ",")
df_rf_daily$TradeDate <- as.Date(df_rf_daily$TradeDate)
####df_factors_daily
df_factors_daily <- read.csv("df_factors_daily2.csv", header = TRUE, sep = ",")
df_factors_daily$TradeDate <- as.Date(df_factors_daily$TradeDate)


####dfm_monthly
dfm_monthly <- read.csv("dfm_monthly.csv", header = TRUE, sep = ",")
dfm_monthly$TradeDate <- as.Date(dfm_monthly$TradeDate)
####df_rf_monthly
df_rf_monthly <- read.csv("df_rf_monthly.csv", header = TRUE, sep = ",")
df_rf_monthly$TradeDate <- as.Date(df_rf_monthly$TradeDate)
####df_factors_monthly
df_factors_monthly <- read.csv("df_factors_monthly.csv", header = TRUE, sep = ",")
df_factors_monthly$TradeDate <- as.Date(df_factors_monthly$TradeDate)
####df_bab_monthly
df_bab_monthly <- read.csv("df_bab_monthly.csv", header = TRUE, sep = ",")
df_bab_monthly$TradeDate <- as.Date(df_bab_monthly$TradeDate)



######
setwd("D:/data/step5_data")
df_daily <- read.csv("df_daily_ch5_1_1.csv", header = TRUE, sep = ",")
df_monthly <- read.csv("df_monthly_ch5_1_1.csv", header = TRUE, sep = ",")
dfma_daily <- read.csv("dfma_daily_ch5_1_1.csv", header = TRUE, sep = ",")
dfma_monthly <- read.csv("dfma_monthly_ch5_1_1.csv", header = TRUE, sep = ",")


df_daily$TradeDate <- as.Date(df_daily$TradeDate)
df_monthly$TradeDate <- as.Date(df_monthly$TradeDate)
dfma_daily$TradeDate <- as.Date(dfma_daily$TradeDate)
dfma_monthly$TradeDate <- as.Date(dfma_monthly$TradeDate)
class(df_daily$TradeDate)
class(df_monthly$TradeDate)
class(dfma_daily$TradeDate)
class(dfma_monthly$TradeDate)
####################################

setwd("D:/data/step6_data")

# Create df_market_daily
df_market_daily <- 
  merge(dfma_daily, dfm_daily, 
        by = "TradeDate", all.x = TRUE, all.y = FALSE)
df_market_daily <- 
  merge(df_market_daily, df_rf_daily, 
        by = "TradeDate", all.x = TRUE, all.y = FALSE)
df_market_daily <- 
  merge(df_market_daily, df_factors_daily, 
    by = "TradeDate", all.x = TRUE, all.y= FALSE)
df_market_daily$Rf <- 
  na.locf(df_market_daily$Rf, fromLast = TRUE ) # Replace NA rf values with next value
write.table(df_market_daily, file = "df_market_daily_6.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
# rm(df_market_monthly)

# Create df_market_monthly
df_market_monthly <- 
  merge(dfma_monthly, dfm_monthly, 
    by = "TradeDate", all.x = TRUE, all.y = FALSE)
df_market_monthly <- merge(df_market_monthly, df_rf_monthly, 
    by = "TradeDate", all.x = TRUE, all.y = FALSE)
df_market_monthly <- merge(df_market_monthly, df_factors_monthly, 
    by = "TradeDate", all.x = TRUE, all.y = FALSE)
df_market_monthly <- merge(df_market_monthly, df_bab_monthly, 
    by = "TradeDate", all.x = TRUE, all.y = FALSE)
write.table(df_market_monthly, file = "df_market_monthly_6.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Merge daily stock data with daily market data

X <- merge(df_daily, df_rf_daily, by = "TradeDate", all.x = TRUE, all.y = FALSE)
X <- merge(X, dfma_daily, by = "TradeDate", all.x = TRUE, all.y = FALSE)
X <- merge(X, df_factors_daily, by = "TradeDate", all.x = TRUE, all.y = FALSE)
#df_daily$Rf <- na.locf(df_daily$Rf, fromLast = TRUE ) # Replace NA rf values with next value
which(is.na(X))
write.table(X, file = "df_daily_ch6_6.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
df_daily = X
rm(X)

# Merge monthly stock data with monthly market data
Y <- merge(df_monthly, df_rf_monthly, by = "TradeDate", all.x = TRUE, all.y = FALSE)
Y <- merge(Y, dfma_monthly, by = "TradeDate", all.x = TRUE, all.y = FALSE)
Y <- merge(Y, df_factors_monthly, by = "TradeDate", all.x = TRUE, all.y = FALSE)
Y <- merge(Y, df_bab_monthly, by = "TradeDate", all.x = TRUE, all.y = FALSE)
#df_monthly_ch6_1_1 plus df_factors_monthly & df_bab_monthly
write.table(Y, file = "df_monthly_ch6_6.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
df_monthly = Y
rm(Y)
rm(dfma_daily , dfm_daily, df_rf_daily, df_factors_daily)
rm(df_factors_monthly, df_rf_monthly, dfm_monthly, dfma_monthly, df_bab_monthly)
###########

# Extract needed variables from df_daily
df_daily <- df_daily %>% 
  select("TradeDate", 
         "Year", 
         "Month", 
         "SecurityId", 
         "Name",
         "ROI", 
         "CLOSE", 
         "MCAP", 
         "LMCAP", 
         "VOLUME" , 
         "M.EW",
         "M.VW" , 
         "Rf", 
         "SMB", 
         "HML",
         "SMB2",
         "CMA",
         "RMW",
         "UMD")
# Extract needed variables from df_monthly
df_monthly <- df_monthly %>% 
  select("TradeDate", 
         "Year",
         "SecurityId", 
         "Name",
         "ROI", 
         "CLOSE", 
         "MCAP", 
         "LMCAP", 
         "VOLUME", 
         "Trading_days_month", 
         "M.EW" , 
         "M.VW", 
         "Rf", 
         "SMB", 
         "HML",
         "SMB2",
         "CMA",
         "RMW",
         "UMD", 
         "CT1","CT2","CT3",
         "TC1","TC2","TC3",
         "UT1","UT2","UT3",
         "TU1","TU2","TU3",
         "CU1","CU2","CU3",
         "UC1","UC2","UC3")
# Calculate Market Returns in excess of the risk-free rate
df_daily <- df_daily %>% mutate(E.ReturnAdjGeneric = ROI - Rf) %>%
  mutate(E.M.EW = M.EW - Rf) %>% mutate(E.M.VW = M.VW - Rf)
df_market_daily <- df_market_daily %>% mutate(E.M.EW = M.EW - Rf) %>%
  mutate(E.M.VW = M.VW - Rf)
df_monthly <- df_monthly %>% mutate(E.ReturnAdjGeneric = ROI - Rf) %>%
  mutate(E.M.EW = M.EW - Rf) %>% mutate(E.M.VW = M.VW - Rf)
df_market_monthly <- df_market_monthly %>% mutate(E.M.EW = M.EW - Rf) %>%
  mutate(E.M.VW = M.VW - Rf)

write.table(df_daily, file = "df_daily_ch6_7.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(df_market_daily, file = "df_market_daily_ch6_7.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(df_monthly, file = "df_monthly_ch6_7.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(df_market_monthly, file = "df_market_monthly_ch6_7.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")


# Create master dataframe
master <- df_monthly
