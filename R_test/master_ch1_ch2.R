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

########## Section 2: Import data ##########
### Daily data
# Set Working Directory to import daily data
setwd("D:/data/Daily")
## Import data on stocks from BÃ¸rsprosjektet at NHH
# Create empty dataframe with colnames
d <- read.csv("1995.csv", header = TRUE, sep = ",")
df_daily <- d[0,]
# Loop inn stock data for all time periods (could not download more than 50 000 rows of data at a time from BÃ¸rsprosjektet)
data.list <- list.files( pattern = '*.csv')

for (i in 1:(length(data.list)-1)) {
  d <- read.csv(data.list[i], header = TRUE, sep = ",")
  df_daily <- rbind(df_daily,d)}
# Delete excess variables
rm(i, d, data.list)

## Import remaining daily 
e <- read.csv("ff5_factor_daily.csv", header = TRUE, sep = ",")

# Import Data on Market Returns (Professor Bernt Arne ?˜degaard)
dfm_daily <- e %>% select(Date,MKT) %>% arrange(Date)
# Import data on Factors (Professor Bernt Arne ?˜degaard)
df_factors_daily <-  e %>% select(Date,SMB,HML,SMB2,RMW,CMA,UMD) %>% arrange(Date)
# Import data on Risk-free rate (Professor Bernt Arne ?˜degaard)
df_rf_daily <-  e %>% select(Date,Rf) %>% arrange(Date)

rm(e)

### Monthly data
# Set Working Drectory to import monthly data
setwd("D:/data/Monthly")

f <- read.csv("1995.csv", header = TRUE, sep = ",")
df_monthly <- f[0,]

data.list <- list.files( pattern = '*.csv')
for (i in 1:(length(data.list)-2)) {
  f <- read.csv(data.list[i], header = TRUE, sep = ",")
  df_monthly <- rbind(df_monthly,f)}
# Delete excess variables
rm(i, f, data.list)


## Import remaining monthly data
g <- read.csv("ff5_factor_monthly.csv", header = TRUE, sep = ",")

# Import Data on Market Returns (Professor Bernt Arne ?˜degaard)
dfm_monthly <- g %>% select(Date,MKT) %>% arrange(Date)
# Import data on Factors (Professor Bernt Arne ?˜degaard)
df_factors_monthly <- g %>% select(Date,SMB,HML,SMB2,RMW,CMA,UMD) %>% arrange(Date)
# Import data on Risk-free Rate (Professor Bernt Arne ?˜degaard)
df_rf_monthly <- g %>% select(Date,Rf) %>% arrange(Date)
# Import data on the BAB factor from AQR
df_bab_monthly <- read.csv("RUI_monthly.csv")

rm(g)

### New Environment to save output
setwd("D:/data/step1_data")
write.table(df_daily, file = "df_daily.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(dfm_daily, file = "dfm_daily.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(df_factors_daily, file = "df_factors_daily.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(df_rf_daily, file = "df_rf_daily.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

write.table(df_monthly, file = "df_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(dfm_monthly, file = "dfm_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(df_factors_monthly, file = "df_factors_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(df_rf_monthly, file = "df_rf_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(df_bab_monthly, file = "df_bab_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")


# ########################
# ########## Section 3: Change date format for imported data ##########
# setwd("D:/data/step3_data")
# # Daily stock data
# df_daily <- df_daily %>%
#   rename(TradeDate = MDATE)
# df_daily$TradeDate <- as.Date(as.character(df_daily$TradeDate),format="%Y%m%d")
# df_daily <- df_daily %>% arrange(TradeDate)
# #write.table(df_daily, file = "df_daily.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
# 
# # Daily market data
# dfm_daily <- dfm_daily %>%
#   rename(TradeDate = Date)
# dfm_daily$TradeDate <- as.Date(dfm_daily$TradeDate,format="%Y%m%d")
# dfm_daily <- dfm_daily %>% arrange(TradeDate)
# #write.table(dfm_daily, file = "dfm_daily.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
# 
# 
# # Daily factor data
# df_factors_daily$Date <- as.Date(strptime(as.factor(df_factors_daily$Date),"%Y/%m/%d"),format="%Y%m%d")
# df_factors_daily <- df_factors_daily %>%
#   rename(TradeDate = Date)
# df_factors_daily <- df_factors_daily %>% arrange(TradeDate)
# #write.table(df_factors_daily, file = "df_factors_daily.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
# 
# # Daily rf data
# df_rf_daily$Date
# df_rf_daily$Date <- as.Date(strptime(as.factor(df_rf_daily$Date),"%Y/%m/%d"),format="%Y%m%d")
# df_rf_daily <- df_rf_daily %>%
#   rename(TradeDate = Date)
# df_rf_daily <- df_rf_daily %>% arrange(TradeDate)
# #write.table(df_rf_daily, file = "df_rf_daily.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
# 
# 
# # Monthly stock data
# df_monthly <- df_monthly %>%
#   rename(TradeDate = MDATE)
# # Set all dates first day of the month
# df_monthly$TradeDate <- as.Date(as.yearmon(as.character(df_monthly$TradeDate),format = "%Y%m"),format ="%Y%m%d")
# df_monthly$TradeDate <- floor_date(df_monthly$TradeDate, "month")
# df_monthly <- df_monthly %>% arrange(TradeDate)
# 
# #write.table(df_monthly, file = "df_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
# 
# # Monthly market data
# dfm_monthly <- dfm_monthly %>%
#   rename(TradeDate = Date)
# dfm_monthly$TradeDate <- as.Date(as.yearmon(as.character(dfm_monthly$TradeDate),format = "%Y/%m"),format ="%Y%m%d")
# dfm_monthly$TradeDate <- floor_date(dfm_monthly$TradeDate, "month")
# dfm_monthly <- dfm_monthly %>% arrange(TradeDate)
# #write.table(dfm_monthly, file = "dfm_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
# 
# # Monthly factor
# df_factors_monthly <- df_factors_monthly %>%
#   rename(TradeDate = Date)
# df_factors_monthly$TradeDate <- as.Date(as.yearmon(as.character(df_factors_monthly$TradeDate),format = "%Y/%m"),format ="%Y%m%d")
# df_factors_monthly$TradeDate <- floor_date(df_factors_monthly$TradeDate, "month")
# df_factors_monthly <- df_factors_monthly %>% arrange(TradeDate)
# #write.table(df_factors_monthly, file = "df_factors_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
# 
# # Montly rf
# df_rf_monthly <- df_rf_monthly %>%
#   rename(TradeDate = Date)
# df_rf_monthly$TradeDate <- as.Date(as.yearmon(as.character(df_rf_monthly$TradeDate),format = "%Y/%m"),format ="%Y%m%d")
# df_rf_monthly$TradeDate <- floor_date(df_rf_monthly$TradeDate, "month")
# df_rf_monthly <- df_rf_monthly %>% arrange(TradeDate)
# #write.table(df_rf_monthly, file = "df_rf_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
# 
# # Monthly bab factor
# df_bab_monthly <- df_bab_monthly %>%
#   rename(TradeDate = Date)
# df_bab_monthly$TradeDate <- as.Date(ymd(df_bab_monthly$TradeDate),format ="%Y%m%d")
# df_bab_monthly$TradeDate <- floor_date(df_bab_monthly$TradeDate, "month")
# df_bab_monthly <- df_bab_monthly %>% arrange(TradeDate)
# write.table(df_bab_monthly, file = "df_bab_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
# 
# 
# 
# 
# 
# 
# ########## Section 4: Data filtering ##########
# ### Daily data
# ## Preparations
# # Compute year, month, MCAP (Market Capitalization) 
# #and LMCAP (Lagged Market Capitalization) variables
# df_daily <- df_daily %>%
#   rename(SecurityId = COID)
# df_daily
# X <- df_daily %>% mutate(Year = year(TradeDate)) %>% 
#   mutate(Month = floor_date(df_daily$TradeDate, "month")) %>%
#   mutate(Turnover_adj = CLOSE * TURNOVER) %>% 
#   mutate(MCAP = MV) %>%
#   group_by(SecurityId) %>%
#   arrange(TradeDate) %>% mutate(LMCAP = lag(MCAP)) %>% ungroup()
