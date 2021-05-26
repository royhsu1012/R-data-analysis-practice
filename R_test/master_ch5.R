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
setwd("D:/data/step4_data")
df_daily <- read.csv("df_daily_ch4_3_1.csv", header = TRUE, sep = ",")
df_monthly <- read.csv("df_monthly_ch4_3_2.csv", header = TRUE, sep = ",")

df_daily$TradeDate <- as.Date(df_daily$TradeDate)
df_monthly$TradeDate <- as.Date(df_monthly$TradeDate)

setwd("D:/data/step5_data")
########## Section 5: Compute Market Returns ##########
# Note that VW returns are calculated using lagged market capitalization
## 1: Calculate daily market returns using filtered data
# Create LMCAP_return variable

df_daily <- df_daily %>% 
  mutate(LMCAP_return = ROI * LMCAP)

write.table(df_daily, file = "df_daily_ch5_1_1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Create EW and VW Market Returns
dfma_daily <- df_daily %>% 
  filter(!is.na(LMCAP)) %>% 
  filter(LMCAP > 0) %>%
  group_by(TradeDate) %>% 
  summarise(
    SecurityIds = length(ROI),
    M.EW = sum(ROI)/length(ROI),
    M.VW = sum(LMCAP_return)/sum(LMCAP))

write.table(dfma_daily, file = "dfma_daily_ch5_1_1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

## 2: Calculate monthly market returns using filtered data
# Create LMCAP_Return variable

df_monthly <- df_monthly %>% 
  mutate(LMCAP_return = ROI * LMCAP)

write.table(df_monthly, file = "df_monthly_ch5_1_1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute EW and VW Market Returns
dfma_monthly <- df_monthly %>% 
  filter(!is.na(LMCAP_return)) %>%
  filter(LMCAP > 0) %>% 
  group_by(TradeDate) %>% 
  summarise(M.EW = sum(ROI)/length(ROI), 
            M.VW = sum(LMCAP_return)/sum(LMCAP))

write.table(dfma_monthly, file = "dfma_monthly_ch5_1_1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

