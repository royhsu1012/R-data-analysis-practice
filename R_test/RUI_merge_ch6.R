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

setwd("D:/data/new_RUI")
####df_factors_daily
df_factors_daily <- read.csv("merge_data.csv", header = TRUE, sep = ",")
df_factors_daily$TradeDate <- as.Date(df_factors_daily$TradeDate)
setwd("D:/data/Daily/ff5_factor_daily")
df_ff5_daily<- read.csv("D:/data/Daily/ff5_factor_daily.csv", header = TRUE, sep = ",")
df_ff5_daily<-df_ff5_daily %>% rename(TradeDate=Date)%>%
  select("TradeDate","SMB", 
         "HML",
         "SMB2",
         "CMA",
         "RMW",
         "UMD")
df_ff5_daily$TradeDate <- as.Date(df_ff5_daily$TradeDate)
######
setwd("D:/data/step5_data")
df_daily <- read.csv("df_daily_ch5_1_1.csv", header = TRUE, sep = ",")
dfma_daily <- read.csv("dfma_daily_ch5_1_1.csv", header = TRUE, sep = ",")

df_daily$TradeDate <- as.Date(df_daily$TradeDate)
dfma_daily$TradeDate <- as.Date(dfma_daily$TradeDate)
class(df_daily$TradeDate)
class(df_monthly$TradeDate)
class(dfma_daily$TradeDate)
class(dfma_monthly$TradeDate)
####################################





X <- merge(df_daily, df_rf_daily, by = "TradeDate", all.x = TRUE, all.y = FALSE)
X <- merge(X, dfma_daily, by = "TradeDate", all.x = TRUE, all.y = FALSE)
X <- merge(X, df_factors_daily, by = "TradeDate", all.x = TRUE, all.y = FALSE)
#df_daily$Rf <- na.locf(df_daily$Rf, fromLast = TRUE ) # Replace NA rf values with next value
which(is.na(X))
write.table(X, file = "df_daily_ch6_RUI.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
df_daily = X
rm(X)

df_daily = df_daily %>% rename(CLOSE=CLOSE.x)
rm(dfma_daily , dfm_daily, df_rf_daily, df_factors_daily)
rm(df_factors_monthly, df_rf_monthly, dfm_monthly, dfma_monthly, df_bab_monthly)
###########

# Extract needed variables from df_daily
Y <- df_daily %>% 
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
         "TC_sum_30","CT_sum_30","UT_sum_30","TU_sum_30","CU_sum_30","UC_sum_30",
         "TC_sum_60","CT_sum_60","UT_sum_60","TU_sum_60","CU_sum_60","UC_sum_60",
         "TC_sum_90","CT_sum_90","UT_sum_90","TU_sum_90","CU_sum_90","UC_sum_90")
# Extract needed variables from df_monthly
Y =  merge(Y, df_ff5_daily, by = "TradeDate", all.x = TRUE, all.y = FALSE)

# Calculate Market Returns in excess of the risk-free rate
Y <- Y %>% mutate(E.ReturnAdjGeneric = ROI - Rf) %>%
  mutate(E.M.EW = M.EW - Rf) %>% mutate(E.M.VW = M.VW - Rf)
df_market_daily <- df_market_daily %>% mutate(E.M.EW = M.EW - Rf) %>%
  mutate(E.M.VW = M.VW - Rf)

write.table(Y, file = "df_daily_ch6_RUI_2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(df_market_daily, file = "df_market_daily_ch6_7.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
getwd()
# Create master dataframe

df_daily = Y
rm(Y,df_ff5_daily)
