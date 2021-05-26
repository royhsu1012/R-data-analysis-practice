#install.packages("corrplot")
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
library(corrplot)
#################

#
setwd("D:/data/step6_data")
####df_daily
df_daily <- read.csv("df_daily_ch6_7.csv", header = TRUE, sep = ",")
df_daily$TradeDate <- as.Date(df_daily$TradeDate)
####df_market_daily_ch6_2
df_market_daily <- read.csv("df_market_daily_ch6_7.csv", header = TRUE, sep = ",")
df_market_daily$TradeDate <- as.Date(df_market_daily$TradeDate)
####df_monthly_ch6_2
df_monthly <- read.csv("df_monthly_ch6_7.csv", header = TRUE, sep = ",")
df_monthly$TradeDate <- as.Date(df_monthly$TradeDate)
####df_market_monthly_ch6_2
df_market_monthly <- read.csv("df_market_monthly_ch6_7.csv", header = TRUE, sep = ",")
df_market_monthly$TradeDate <- as.Date(df_market_monthly$TradeDate)

class(df_daily$TradeDate)
class(df_market_daily$TradeDate)
class(df_monthly$TradeDate)
class(df_market_monthly$TradeDate)

###########################
setwd("D:/data/step7_data/f5v60w36")

master <- df_monthly
#################
########## Section 7: Estimate Variables ##########
# Note that all variables are estimated at the end of month t and merged with dates for the month t+1, hence in the master dataframe variable estimates for month t will have corresponding excess returns for month t+1
# Create date vector (Will decide when first variables are calculated and must be specified)
date1 <- seq(as.Date("1995-04-01"), as.Date("2020-03-31"), by = "months")

### 1: beta.monthly: At the end of each month t, for each stock i, estimate market beta based on 60 months of monthly return observations
# Create empty dataframe
beta_monthly <- data.frame(SecurityId = integer(), 
                beta.monthly = numeric(), 
                Tradedate = as.Date(character()), 
                stringsAsFactors = FALSE)
## Model
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  beta.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ E.M.VW)) %>% 
    mutate(beta.monthly = coef(ols.model)[2]) %>%
    select("SecurityId", "beta.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  beta_monthly <- rbind(beta_monthly,beta.tab)}
which(is.na(beta_monthly))
write.table(beta_monthly, file = "beta_monthly_4.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

beta_monthly$TradeDate = as.Date(beta_monthly$TradeDate)
rm(beta.tab,v,w,i)
### 2: beta.daily: At the end of each month t, for each stock i, estimate market beta based on 12 months of daily return observations
# Create empty dataframe
beta_daily <- data.frame(SecurityId = integer(), 
                         beta.daily = numeric(), 
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
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ E.M.VW)) %>% 
    mutate(beta.daily = coef(ols.model)[2]) %>%
    select("SecurityId", "beta.daily") %>% 
    mutate(TradeDate = date1[n+i])
  beta_daily <- rbind(beta_daily,beta.tab)}

beta_daily$TradeDate = as.Date(beta_daily$TradeDate)

write.table(beta_daily, file = "beta_daily_4.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
which(is.na(beta_daily))
rm(beta.tab,i,m,n)

###I have no MSCI >> skip

### 4: max: At the end of each month t, for each stock i, estimate max based on 1 month of daily return observations
# Create empty dataframe
max_daily <- data.frame(SecurityId = integer(), 
                        max = numeric(), 
                        Tradedate = as.Date(character()), 
                        stringsAsFactors = FALSE)
## Model
v <- 1 # Number of months with daily returns used to calculate max
w <- 10 # Number of valid return observations needed in the estimation period for stock to be included

for (i in 1:(length(date1)-v)) {
  print(i)
  max.tab <- df_daily %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    group_by(SecurityId) %>% 
    filter(sum(!is.na(VOLUME))>= w) %>% 
    filter(sum((VOLUME > 0)) >= w) %>%
    summarise(max = mean(tail(sort(ROI),5))) %>% 
    select("SecurityId", "max") %>%
    mutate(TradeDate = date1[v+i])
  max_daily <- rbind(max_daily,max.tab)}

which(is.na(max_daily))
write.table(max_daily, file = "max_daily_4.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
max_daily$TradeDate = as.Date(max_daily$TradeDate)
rm(max.tab,i,v,w)

### 5: ivol and iskew: At the end of each month t, for each stock i, estimate ivol and iskew based on 1 month of daily return observations
# Create empty dataframe
ivol_daily <- data.frame(SecurityId = integer(), 
                         ivol = numeric(),
                         iskew = numeric(), 
                         Tradedate = as.Date(character()), 
                         stringsAsFactors = FALSE)

v <- 1  # Number of months with daily returns used to calculate ivol and iskew
w <- 10 # Number of valid return observations needed in the calculation period for stock to be included
## w = 15 is the original setting but usually out of bound
##i=82 118 142 154 178 190 214 238 250 274 286 fail
for (i in 1:(length(date1)-v)) {
  print(i)
  ivol.tab <- df_daily %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter((!is.na(VOLUME))) %>% 
    filter(VOLUME > 0) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ E.M.VW + SMB + HML)) %>%
    mutate(ivol = sd(residuals(ols.model))) %>% 
    mutate(iskew = skewness(residuals(ols.model)))%>%
    select("SecurityId", "ivol", "iskew") %>% 
    mutate(TradeDate = date1[v+i])
  ivol_daily <- rbind(ivol_daily,ivol.tab)}

write.table(ivol_daily, file = "ivol_daily_4.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(ivol.tab,i,v,w)

##############RUI
##############TEST
# Create empty dataframe
RUI_monthly <- data.frame(SecurityId = integer(), 
                          RUI.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)

#rm(RUI_monthly)
## Model
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  RUI.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ E.M.VW + SMB + HML + CT3)) %>% 
    mutate(RUI.monthly = as.numeric(coef(ols.model)[5])) %>%
    select("SecurityId", "RUI.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  RUI_monthly <- rbind(RUI_monthly,RUI.tab)}

write.table(RUI_monthly, file = "RUI_monthly_4.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

rm(RUI.tab,v,w,i)
################
# Create empty dataframe
CT1_monthly <- data.frame(SecurityId = integer(), 
                          CT1.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)

#rm(RUI_monthly)
## Model
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  CT1.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ E.M.VW + SMB2 + HML + RMW + CMA + CT1)) %>% 
    mutate(CT1.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "CT1.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  CT1_monthly <- rbind(CT1_monthly,CT1.tab)}

write.table(CT1_monthly, file = "CT1_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CT1.tab,v,w,i)
########
################
# CT2
CT2_monthly <- data.frame(SecurityId = integer(), 
                          CT2.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  CT2.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ E.M.VW +  SMB2 + HML + RMW + CMA + CT2)) %>% 
    mutate(CT2.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "CT2.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  CT2_monthly <- rbind(CT2_monthly,CT2.tab)}

write.table(CT2_monthly, file = "CT2_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CT2.tab,v,w,i)

################
# CT3
CT3_monthly <- data.frame(SecurityId = integer(), 
                          CT3.monthly = numeric(), ###
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  CT3.tab <- df_monthly %>% ###
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW +  SMB2 + HML + RMW + CMA  + CT3)) %>% ###
    mutate(CT3.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "CT3.monthly") %>% ###
    mutate(TradeDate = date1[v+i])
  CT3_monthly <- rbind(CT3_monthly,CT3.tab)}###
###
write.table(CT3_monthly, file = "CT3_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CT3.tab,v,w,i)###

################
# TC1
TC1_monthly <- data.frame(SecurityId = integer(), 
                          TC1.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  TC1.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ E.M.VW +  SMB2 + HML + RMW + CMA + TC1)) %>% 
    mutate(TC1.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "TC1.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  TC1_monthly <- rbind(TC1_monthly,TC1.tab)}

write.table(TC1_monthly, file = "TC1_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TC1.tab,v,w,i)

################
# TC2
TC2_monthly <- data.frame(SecurityId = integer(), 
                          TC2.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  TC2.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ E.M.VW +  SMB2 + HML + RMW + CMA + TC2)) %>% 
    mutate(TC2.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "TC2.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  TC2_monthly <- rbind(TC2_monthly,TC2.tab)}

write.table(TC2_monthly, file = "TC2_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TC2.tab,v,w,i)

################
# TC3
TC3_monthly <- data.frame(SecurityId = integer(), 
                          TC3.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  TC3.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW +  SMB2 + HML + RMW + CMA  + TC3)) %>% 
    mutate(TC3.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "TC3.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  TC3_monthly <- rbind(TC3_monthly,TC3.tab)}

write.table(TC3_monthly, file = "TC3_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TC3.tab,v,w,i)

###############
# UT1
UT1_monthly <- data.frame(SecurityId = integer(), 
                          UT1.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  UT1.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW +  SMB2 + HML + RMW + CMA  + UT1)) %>% 
    mutate(UT1.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "UT1.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  UT1_monthly <- rbind(UT1_monthly,UT1.tab)}

write.table(UT1_monthly, file = "UT1_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UT1.tab,v,w,i)

###############
# UT2
UT2_monthly <- data.frame(SecurityId = integer(), 
                          UT2.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  UT2.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW +  SMB2 + HML + RMW + CMA  + UT2)) %>% 
    mutate(UT2.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "UT2.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  UT2_monthly <- rbind(UT2_monthly,UT2.tab)}

write.table(UT2_monthly, file = "UT2_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UT2.tab,v,w,i)
###############
# UT2
UT2_monthly <- data.frame(SecurityId = integer(), 
                          UT2.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  UT2.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW +  SMB2 + HML + RMW + CMA  + UT2)) %>% 
    mutate(UT2.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "UT2.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  UT2_monthly <- rbind(UT2_monthly,UT2.tab)}

write.table(UT2_monthly, file = "UT2_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UT2.tab,v,w,i)

###############
# UT3
UT3_monthly <- data.frame(SecurityId = integer(), 
                          UT3.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  UT3.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW +  SMB2 + HML + RMW + CMA  + UT3)) %>% 
    mutate(UT3.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "UT3.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  UT3_monthly <- rbind(UT3_monthly,UT3.tab)}

write.table(UT3_monthly, file = "UT3_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UT3.tab,v,w,i)

###############
# TU1
TU1_monthly <- data.frame(SecurityId = integer(), 
                          TU1.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  TU1.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW +  SMB2 + HML + RMW + CMA  + TU1)) %>% 
    mutate(TU1.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "TU1.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  TU1_monthly <- rbind(TU1_monthly,TU1.tab)}

write.table(TU1_monthly, file = "TU1_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TU1.tab,v,w,i)

###############
# TU2
TU2_monthly <- data.frame(SecurityId = integer(), 
                          TU2.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  TU2.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW +  SMB2 + HML + RMW + CMA  + TU2)) %>% 
    mutate(TU2.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "TU2.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  TU2_monthly <- rbind(TU2_monthly,TU2.tab)}

write.table(TU2_monthly, file = "TU2_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TU2.tab,v,w,i)

###############
# TU3
TU3_monthly <- data.frame(SecurityId = integer(), 
                          TU3.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  TU3.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW +  SMB2 + HML + RMW + CMA  + TU3)) %>% 
    mutate(TU3.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "TU3.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  TU3_monthly <- rbind(TU3_monthly,TU3.tab)}

write.table(TU3_monthly, file = "TU3_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TU3.tab,v,w,i)

###############
# CU1
CU1_monthly <- data.frame(SecurityId = integer(), 
                          CU1.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  CU1.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW +  SMB2 + HML + RMW + CMA  + CU1)) %>% 
    mutate(CU1.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "CU1.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  CU1_monthly <- rbind(CU1_monthly,CU1.tab)}

write.table(CU1_monthly, file = "CU1_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CU1.tab,v,w,i)

###############
# CU2
CU2_monthly <- data.frame(SecurityId = integer(), 
                          CU2.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  CU2.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW +  SMB2 + HML + RMW + CMA  + CU2)) %>% 
    mutate(CU2.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "CU2.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  CU2_monthly <- rbind(CU2_monthly,CU2.tab)}

write.table(CU2_monthly, file = "CU2_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CU2.tab,v,w,i)

###############
# CU3
CU3_monthly <- data.frame(SecurityId = integer(), 
                          CU3.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  CU3.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW +  SMB2 + HML + RMW + CMA  + CU3)) %>% 
    mutate(CU3.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "CU3.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  CU3_monthly <- rbind(CU3_monthly,CU3.tab)}

write.table(CU3_monthly, file = "CU3_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CU3.tab,v,w,i)

###############
# UC1
UC1_monthly <- data.frame(SecurityId = integer(), 
                          UC1.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  UC1.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW +  SMB2 + HML + RMW + CMA  + UC1)) %>% 
    mutate(UC1.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "UC1.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  UC1_monthly <- rbind(UC1_monthly,UC1.tab)}

write.table(UC1_monthly, file = "UC1_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UC1.tab,v,w,i)

###############
# UC2
UC2_monthly <- data.frame(SecurityId = integer(), 
                          UC2.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  UC2.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW +  SMB2 + HML + RMW + CMA  + UC2)) %>% 
    mutate(UC2.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "UC2.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  UC2_monthly <- rbind(UC2_monthly,UC2.tab)}

write.table(UC2_monthly, file = "UC2_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UC2.tab,v,w,i)

###############
# UC3
UC3_monthly <- data.frame(SecurityId = integer(), 
                          UC3.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  UC3.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    #filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ 
                        E.M.VW +  SMB2 + HML + RMW + CMA  + UC3)) %>% 
    mutate(UC3.monthly = as.numeric(coef(ols.model)[7])) %>%
    select("SecurityId", "UC3.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  UC3_monthly <- rbind(UC3_monthly,UC3.tab)}

write.table(UC3_monthly, file = "UC3_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UC3.tab,v,w,i)
master = df_monthly
### Merge variables with master dataframe
master$TradeDate = as.Date(master$TradeDate)
master <- merge(master, beta_daily, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, beta_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, ivol_daily, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, max_daily, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, RUI_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, TC1_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, TC2_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, TC3_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, CT1_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, CT2_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, CT3_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, TU1_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, TU2_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, TU3_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, UT1_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, UT2_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, UT3_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, CU1_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, CU2_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, CU3_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, UC1_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, UC2_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, UC3_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)


write.table(master, file = "master_4.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

### Filter to use have estimates of all variables for same time period
master <- master %>%
  filter(TradeDate >= as.Date("2000-05-01") & TradeDate <= as.Date("2020-03-31") )
  #I think I should filter NA value
  #filter()
#na.fail(master)
write.table(master, file = "master_2000_4.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

########## Section 8: Summary Statistics and Correlation: Variables ##########
master = read.csv("master_2000_4.csv", header = TRUE, sep = ",")
## Create summary statistics for estimated variables
summary_statistics_variables <- master %>%
  select("beta.monthly", "beta.daily", "max","RUI.monthly") %>%
  describe( ., na.rm = TRUE, skew = FALSE, quant = c(0.25,0.75))
## Create correlation matrix with averages of monthly cross-sectional correlations(not same as average correlation)
variable_cor <- master %>%
  select("TradeDate", 
         "beta.monthly", 
         "beta.daily", 
         "max", 
         "CT1.monthly",
         "CT2.monthly",
         "CT3.monthly",
         "TC1.monthly",
         "TC2.monthly",
         "TC3.monthly",
         "UT1.monthly",
         "UT2.monthly",
         "UT3.monthly",
         "TU1.monthly",
         "TU2.monthly",
         "TU3.monthly",
         "CU1.monthly",
         "CU2.monthly",
         "CU3.monthly",
         "UC1.monthly",
         "UC2.monthly",
         "UC3.monthly")
#which(is.na(variable_cor$beta.monthly))
variable_cor <- variable_cor %>% 
  group_by(TradeDate)%>% 
  do(cormat = cor(select(., - matches("TradeDate")), use = "pairwise.complete.obs"))
variable_cor <- Reduce("+", variable_cor$cormat)/length(variable_cor$cormat)

#corrplot(variable_cor,method="shade",shade.col=NA,tl.col="black",tl.srt=45)

rm(summary_statistics_variables, variable_cor)
rm(df_daily,df_market_daily,df_monthly,df_market_monthly)
rm(beta_daily,beta_monthly,ivol_daily,max_daily,RUI_monthly,
   CT1_monthly,CT2_monthly,CT3_monthly,
   TC1_monthly,TC2_monthly,TC3_monthly,
   UT1_monthly,UT2_monthly,UT3_monthly,
   TU1_monthly,TU2_monthly,TU3_monthly,
   CU1_monthly,CU2_monthly,CU3_monthly,
   UC1_monthly,UC2_monthly,UC3_monthly)
rm(date1)
