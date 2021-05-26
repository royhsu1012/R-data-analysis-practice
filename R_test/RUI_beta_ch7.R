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
setwd("D:/data/new_RUI/ch6")
df_daily <- read.csv("df_daily_ch6_RUI_2.csv", header = TRUE, sep = ",")
df_daily <- df_daily %>%
  filter(TradeDate >= as.Date("1995-05-01") & TradeDate <= as.Date("2020-03-31") )

date1 <- seq(as.Date("1995-05-01"), as.Date("2020-03-31"), by = "months")
date2 <- seq(as.Date("1995-05-01"), as.Date("2020-03-31"), by = "weeks")
setwd("D:/data/new_RUI/ch7")
### 2: beta.daily: At the end of each month t, for each stock i, estimate market beta based on 12 months of daily return observations
# Create empty dataframe
CT30_daily <- data.frame(SecurityId = integer(), 
                         CT30.daily = numeric(), 
                         Tradedate = as.Date(character()), 
                         stringsAsFactors = FALSE)
## Model
n <- 12 # Number of months with daily returns used to calculate beta
m <- 180 # Number of valid return observations 
# (observations with official turnover>0) 
# needed in the estimation period for stock to be included
for (i in 1:(length(date1)-n)) {
  print(i)
  beta.tab <- df_daily %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+n]) %>%
    filter((!is.na(VOLUME))) %>%
    filter(VOLUME > 0) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= m) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW + SMB + HML + CT_sum_30)) %>% 
    mutate(CT30.daily = coef(ols.model)[5]) %>%
    select("SecurityId", "CT30.daily") %>% 
    mutate(TradeDate = date1[n+i])
  CT30_daily <- rbind(CT30_daily,beta.tab)}

CT30_daily$TradeDate = as.Date(CT30_daily$TradeDate)

write.table(CT30_daily, file = "beta_CT_sum_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
which(is.na(CT30_daily))
rm(beta.tab,i,m,n)

###############
###############
# Create empty dataframe
TC30_daily  <- data.frame(SecurityId = integer(), 
                         TC30.daily = numeric(), 
                         Tradedate = as.Date(character()), 
                         stringsAsFactors = FALSE)
## Model
n <- 12 # Number of months with daily returns used to calculate beta
m <- 180 # Number of valid return observations 
# (observations with official turnover>0) 
# needed in the estimation period for stock to be included
for (i in 1:(length(date1)-n)) {
  print(i)
  beta.tab <- df_daily %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+n]) %>%
    filter((!is.na(VOLUME))) %>%
    filter(VOLUME > 0) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= m) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW + SMB + HML + TC_sum_30)) %>% 
    mutate(TC30.daily = coef(ols.model)[5]) %>%
    select("SecurityId", "TC30.daily") %>% 
    mutate(TradeDate = date1[n+i])
  TC30_daily  <- rbind(TC30_daily ,beta.tab)}

TC30_daily $TradeDate = as.Date(TC30_daily $TradeDate)

write.table(TC30_daily , file = "beta_TC_sum_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
which(is.na(TC30_daily ))
rm(beta.tab,i,m,n)

########
########
# Create empty dataframe
UT30_daily  <- data.frame(SecurityId = integer(), 
                          UT30.daily = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
## Model
n <- 12 # Number of months with daily returns used to calculate beta
m <- 180 # Number of valid return observations 
# (observations with official turnover>0) 
# needed in the estimation period for stock to be included
for (i in 1:(length(date1)-n)) {
  print(i)
  beta.tab <- df_daily %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+n]) %>%
    filter((!is.na(VOLUME))) %>%
    filter(VOLUME > 0) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= m) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW + SMB + HML + UT_sum_30)) %>% 
    mutate(UT30.daily = coef(ols.model)[5]) %>%
    select("SecurityId", "UT30.daily") %>% 
    mutate(TradeDate = date1[n+i])
  UT30_daily  <- rbind(UT30_daily ,beta.tab)}

UT30_daily $TradeDate = as.Date(UT30_daily $TradeDate)

write.table(UT30_daily , file = "beta_UT_sum_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
which(is.na(UT30_daily ))
rm(beta.tab,i,m,n)


###############
###############
# Create empty dataframe
TU30_daily  <- data.frame(SecurityId = integer(), 
                          TU30.daily = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
## Model
n <- 12 # Number of months with daily returns used to calculate beta
m <- 180 # Number of valid return observations 
# (observations with official turnover>0) 
# needed in the estimation period for stock to be included
for (i in 1:(length(date1)-n)) {
  print(i)
  beta.tab <- df_daily %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+n]) %>%
    filter((!is.na(VOLUME))) %>%
    filter(VOLUME > 0) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= m) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW + SMB + HML + TU_sum_30)) %>% 
    mutate(TU30.daily = coef(ols.model)[5]) %>%
    select("SecurityId", "TU30.daily") %>% 
    mutate(TradeDate = date1[n+i])
  TU30_daily  <- rbind(TU30_daily ,beta.tab)}

TU30_daily $TradeDate = as.Date(TU30_daily $TradeDate)

write.table(TU30_daily , file = "beta_TU_sum_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
which(is.na(TU30_daily ))
rm(beta.tab,i,m,n)

###
###
###############
###############
# Create empty dataframe
CU30_daily  <- data.frame(SecurityId = integer(), 
                          CU30.daily = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
## Model
n <- 12 # Number of months with daily returns used to calculate beta
m <- 180 # Number of valid return observations 
# (observations with official turnover>0) 
# needed in the estimation period for stock to be included
for (i in 1:(length(date1)-n)) {
  print(i)
  beta.tab <- df_daily %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+n]) %>%
    filter((!is.na(VOLUME))) %>%
    filter(VOLUME > 0) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= m) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW + SMB + HML + TC_sum_30)) %>% 
    mutate(CU30.daily = coef(ols.model)[5]) %>%
    select("SecurityId", "CU30.daily") %>% 
    mutate(TradeDate = date1[n+i])
  CU30_daily  <- rbind(CU30_daily ,beta.tab)}

CU30_daily $TradeDate = as.Date(CU30_daily $TradeDate)

write.table(CU30_daily , file = "beta_CU_sum_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
which(is.na(CU30_daily ))
rm(beta.tab,i,m,n)
###########
UC30_daily  <- data.frame(SecurityId = integer(), 
                          UC30.daily = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
## Model
n <- 12 # Number of months with daily returns used to calculate beta
m <- 180 # Number of valid return observations 
# (observations with official turnover>0) 
# needed in the estimation period for stock to be included
for (i in 1:(length(date1)-n)) {
  print(i)
  beta.tab <- df_daily %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+n]) %>%
    filter((!is.na(VOLUME))) %>%
    filter(VOLUME > 0) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= m) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW + SMB + HML + TC_sum_30)) %>% 
    mutate(UC30.daily = coef(ols.model)[5]) %>%
    select("SecurityId", "UC30.daily") %>% 
    mutate(TradeDate = date1[n+i])
  UC30_daily  <- rbind(UC30_daily ,beta.tab)}

UC30_daily $TradeDate = as.Date(UC30_daily $TradeDate)

write.table(UC30_daily , file = "beta_UC_sum_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
which(is.na(UC30_daily ))
rm(beta.tab,i,m,n)

