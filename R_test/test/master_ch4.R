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
df_daily <- read.csv("df_daily2.csv", header = TRUE, sep = ",")
#"df_daily2.csv plus volume
dfm_daily <- read.csv("dfm_daily.csv", header = TRUE, sep = ",")
df_factors_daily<- read.csv("df_factors_daily2.csv", header = TRUE, sep = ",")
df_rf_daily <- read.csv("df_rf_daily.csv", header = TRUE, sep = ",")
df_monthly <- read.csv("df_monthly2.csv", header = TRUE, sep = ",")
dfm_monthly <- read.csv("dfm_monthly.csv", header = TRUE, sep = ",")
df_factors_monthly <- read.csv("df_factors_monthly.csv", header = TRUE, sep = ",")
df_rf_monthly <- read.csv("df_rf_monthly.csv", header = TRUE, sep = ",")
df_bab_monthly <- read.csv("df_bab_monthly.csv", header = TRUE, sep = ",")


########## 
setwd("D:/data/step4_data")


########## Section 4: Data filtering ##########
### Daily data
## Preparations
# Compute year, month, MCAP (Market Capitalization) 
#and LMCAP (Lagged Market Capitalization) variables
df_daily <- df_daily %>%
  rename(SecurityId = COID)
df_daily$TradeDate = as.Date(df_daily$TradeDate)
class(df_daily$TradeDate)
head(df_daily$TradeDate)
X = df_daily
## Preparations
# Compute year, month, MCAP (Market Capitalization) and LMCAP (Lagged Market Capitalization) variables
X <- X %>% mutate(Year = year(TradeDate)) %>% 
  mutate(Month = floor_date(df_daily$TradeDate, "month")) %>%
  mutate(Turnover_adj = CLOSE * VOLUME) %>% 
  mutate(MCAP = CLOSE * OUTSTANDING) %>%
  group_by(SecurityId) %>%
  arrange(TradeDate) %>% 
  mutate(LMCAP = lag(MCAP)) %>% ungroup()


# Compute yearly variables needed for filtering
X <- X %>% group_by(SecurityId, Year) %>% 
  mutate(Trading_days_year = sum(!is.na(VOLUME))) %>%
  mutate(Turnover_adj_year = mean(Turnover_adj, na.rm = TRUE)) %>% 
  ungroup()

# Compute monthly variables needed for filtering
X <- X %>% group_by(SecurityId, Month) %>% 
  mutate(Trading_days_month = sum(!is.na(VOLUME))) %>%
  mutate(MCAP_month_low = min(MCAP, na.rm = TRUE)) %>% 
  mutate(Generic_month_low = min(CLOSE,na.rm = TRUE)) %>% 
  ungroup()

# Create relative liquidity limit
k <- X %>% select("Year", "SecurityId", "Turnover_adj_year") %>%
  distinct() %>% group_by(Year) %>% 
  mutate(Turnover_adj_limit = quantile(Turnover_adj_year,0.025, type = 3, na.rm = TRUE)) %>%
  ungroup() %>% select("Year", "Turnover_adj_limit") %>% distinct()
X <- merge(X, k, by = "Year", all.x = TRUE)
# Remove excess variable
rm(k)

write.table(X, file = "df_daily_ch4_1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
#############


# Use data post 1985 with observations of ReturnAdjGeneric

X <- X %>% 
  filter(TradeDate >= as.Date("1995-04-01") & TradeDate <= 
  as.Date("2020-03-31")) #%>% filter(!is.na(ROI))
  # Create summary statistics of daily data prior to filters

# summary_statistics_prefilter_daily <- Y %>% 
#   select("CLOSE", "ROI","OUTSTANDING" ,"MCAP", "VOLUME") %>%
#   describe( ., na.rm = TRUE, skew = FALSE, quant = c(0.25,0.75))

# Apply filters
# Y <- Y %>%
#   #filter(SecurityTypeId != 7) %>% # Remove free float shares from sample
#   #filter(Market == "OSE") %>% # Remove stocks listed not listed on OSE from sample
#   #filter(MCAP_month_low >= 1000000) %>% # Remove stocks for months with less than NOK 1m MCAP
#   filter(Generic_month_low >= 1) %>% # Remove stocks for months with less than NOK 1 Generic
#   filter(Trading_days_year >= 20) #%>% # Remove stocks for years with less than 20 trading days
#   #filter(Turnover_adj_year > Turnover_adj_limit) # Remove the 2.5% of stocks for years with the lowest average daily turnover in NOK

## Create summary statistics of daily data post filters
summary_statistics_postfilter_daily <- X %>% 
  select("CLOSE", "ROI","OUTSTANDING" ,"MCAP", "VOLUME") %>%
  describe( ., na.rm = TRUE, skew = FALSE, quant = c(0.25,0.75))

write.table(X, file = "df_daily_ch4_2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

### Monthly Data
## Preparations
# Calculate Year, Month, MCAP, and LMCAP variable
df_monthly <- df_monthly %>%
  rename(SecurityId = COID)

Y = df_monthly
colnames(Y)
Y$TradeDate <- as.Date(Y$TradeDate)
class(Y$TradeDate)
head(Y$TradeDate)

Y <- Y %>% mutate(Year = year(TradeDate)) %>% 
  mutate(MCAP = CLOSE*OUTSTANDING) %>%
  group_by(SecurityId) %>% arrange(TradeDate) %>% 
  mutate(LMCAP = lag(MCAP)) %>% ungroup()
## Filtering
# Use data post 1985
Y <- Y %>%
  filter(TradeDate >= 
      as.Date("1995-04-01") & TradeDate <= as.Date("2020-03-31"))
Y <- Y %>% filter(!is.na(ROI)) 
write.table(Y, file = "df_monthly_ch4_2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Create summary statistics of monthly data prior to filters
summary_statistics_prefilter_monthly <- Y %>%
  select("CLOSE", "ROI","OUTSTANDING" ,"MCAP", "VOLUME") %>%
  describe( ., na.rm = TRUE, skew = FALSE, quant = c(0.25,0.75))

######
####
df_daily <- read.csv("df_daily_ch4_2.csv", header = TRUE, sep = ",")
class(Y$TradeDate)
class(df_daily$TradeDate)
df_daily$TradeDate <- as.Date(df_daily$TradeDate)
head(df_daily$TradeDate)
# Extract unique combinations of SecurityID and Month from daily data
merge1 <- df_daily %>% select("Month","SecurityId","Trading_days_month") %>% distinct() %>%
  rename(TradeDate = Month)
class(merge1$TradeDate)
merge1$TradeDate <- as.Date(merge1$TradeDate)

# Merge the unique combinations of SecurityID and Month from the daily data with the monthly dataset
# This procedure effectively applies the filters applied to the daily data, to the monthly data.
Z <-  merge(merge1, Y, by = c("TradeDate","SecurityId"), all.x = FALSE, all.y = FALSE) # 87 453 - 72 783
#rm(merge1) # Remove excess dataframe
write.table(Z, file = "df_monthly_ch4_3.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Create summary statistics of monthly data post filters
summary_statistics_postfilter_monthly <- Z  %>%
  select("CLOSE", "ROI","OUTSTANDING" ,"MCAP", "VOLUME") %>%
  describe( ., na.rm = TRUE, skew = FALSE, quant = c(0.25,0.75))


