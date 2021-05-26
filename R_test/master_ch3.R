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

########## Section 3: Change date format for imported data ##########

setwd("D:/data/step1_data")
df_daily <- read.csv("df_daily.csv", header = TRUE, sep = ",")
dfm_daily <- read.csv("dfm_daily.csv", header = TRUE, sep = ",")
df_factors_daily<- read.csv("df_factors_daily.csv", header = TRUE, sep = ",")
df_rf_daily <- read.csv("df_rf_daily.csv", header = TRUE, sep = ",")
df_monthly <- read.csv("df_monthly.csv", header = TRUE, sep = ",")
dfm_monthly <- read.csv("dfm_monthly.csv", header = TRUE, sep = ",")
df_factors_monthly <- read.csv("df_factors_monthly.csv", header = TRUE, sep = ",")
df_rf_monthly <- read.csv("df_rf_monthly.csv", header = TRUE, sep = ",")
df_bab_monthly <- read.csv("df_bab_monthly.csv", header = TRUE, sep = ",")

# rm(df_rf_monthly.csv,df_bab_monthly.csv)
########## Section 3: Change date format for imported data ##########
setwd("D:/data/step2_data")
# Daily stock data
df_daily <- df_daily %>%
  rename(TradeDate = MDATE)
df_daily$TradeDate <-  as.Date(ymd(df_daily$TradeDate),format ="%Y%m%d")
#df_daily <- df_daily %>% arrange(TradeDate)
write.table(df_daily, file = "df_daily.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Daily market data
dfm_daily <- dfm_daily %>%
  rename(TradeDate = Date)
dfm_daily$TradeDate <- as.Date(strptime(as.factor(dfm_daily$TradeDate),"%Y/%m/%d"),format="%Y%m%d")
#dfm_daily <- dfm_daily %>% arrange(TradeDate)
#write.table(dfm_daily, file = "dfm_daily.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")


# Daily factor data
df_factors_daily <- df_factors_daily %>%
  rename(TradeDate = Date)
df_factors_daily$TradeDate <- as.Date(strptime(as.factor(df_factors_daily$TradeDate),"%Y/%m/%d"),format="%Y%m%d")
df_factors_daily <- df_factors_daily %>% arrange(TradeDate)
write.table(df_factors_daily, file = "df_factors_daily.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Daily rf data
df_rf_daily <- df_rf_daily %>%
  rename(TradeDate = Date)
df_rf_daily$TradeDate <- as.Date(strptime(as.factor(df_rf_daily$TradeDate),"%Y/%m/%d"),format="%Y%m%d")

#df_rf_daily <- df_rf_daily %>% arrange(TradeDate)
#write.table(df_rf_daily, file = "df_rf_daily.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")


# Monthly stock data
df_monthly <- df_monthly %>%
  rename(TradeDate = MDATE)
# Set all dates first day of the month
df_monthly$TradeDate <- as.Date(as.yearmon(as.character(df_monthly$TradeDate),format = "%Y%m"),format ="%Y%m%d")
df_monthly$TradeDate <- floor_date(df_monthly$TradeDate, "month")
#df_monthly <- df_monthly %>% arrange(TradeDate)
#write.table(df_monthly, file = "df_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Monthly market data
dfm_monthly <- dfm_monthly %>%
  rename(TradeDate = Date)
dfm_monthly$TradeDate <- as.Date(as.yearmon(as.character(dfm_monthly$TradeDate),format = "%Y/%m"),format ="%Y%m%d")
dfm_monthly$TradeDate <- floor_date(dfm_monthly$TradeDate, "month")
#dfm_monthly <- dfm_monthly %>% arrange(TradeDate)
#write.table(dfm_monthly, file = "dfm_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Monthly factor
df_factors_monthly <- df_factors_monthly %>%
  rename(TradeDate = Date)
df_factors_monthly$TradeDate <- as.Date(as.character(df_factors_monthly$TradeDate))
df_factors_monthly$TradeDate <- floor_date(df_factors_monthly$TradeDate, "month")
df_factors_monthly <- df_factors_monthly %>% arrange(TradeDate)
write.table(df_factors_monthly, file = "df_factors_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Montly rf
df_rf_monthly <- df_rf_monthly %>%
  rename(TradeDate = Date)
df_rf_monthly$TradeDate <- as.Date(as.yearmon(as.character(df_rf_monthly$TradeDate),format = "%Y/%m"),format ="%Y%m%d")
df_rf_monthly$TradeDate <- floor_date(df_rf_monthly$TradeDate, "month")
#df_rf_monthly <- df_rf_monthly %>% arrange(TradeDate)
write.table(df_rf_monthly, file = "df_rf_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Monthly bab factor
df_bab_monthly <- df_bab_monthly %>%
  rename(TradeDate = Tradedate)
df_bab_monthly$TradeDate <- as.Date(df_bab_monthly$TradeDate)
df_bab_monthly$TradeDate <- floor_date(df_bab_monthly$TradeDate, "month")
#df_bab_monthly <- df_bab_monthly %>% arrange(TradeDate)
write.table(df_bab_monthly, file = "df_bab_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
