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

#
setwd("D:/data/step6_data")
####df_daily
df_daily <- read.csv("df_daily_ch6_5.csv", header = TRUE, sep = ",")
df_daily$TradeDate <- as.Date(df_daily$TradeDate)
####df_market_daily_ch6_2
df_market_daily <- read.csv("df_market_daily_ch6_5.csv", header = TRUE, sep = ",")
df_market_daily$TradeDate <- as.Date(df_market_daily$TradeDate)
####df_monthly_ch6_2
df_monthly <- read.csv("df_monthly_ch6_5.csv", header = TRUE, sep = ",")
df_monthly$TradeDate <- as.Date(df_monthly$TradeDate)
####df_market_monthly_ch6_2
df_market_monthly <- read.csv("df_market_monthly_ch6_5.csv", header = TRUE, sep = ",")
df_market_monthly$TradeDate <- as.Date(df_market_monthly$TradeDate)

class(df_daily$TradeDate)
class(df_market_daily$TradeDate)
class(df_monthly$TradeDate)
class(df_market_monthly$TradeDate)

###########################
setwd("D:/data/step7_data")

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

# Create empty dataframe
CT1_monthly <- data.frame(SecurityId = integer(), 
                          CT1.monthly = numeric(), 
                          Tradedate = as.Date(character()), 
                          stringsAsFactors = FALSE)
## Model
v <- 60 # Number of months with monthly returns used to calculate beta
w <- 36 # Number of valid return observations needed in the estimation period for a stock to be included
for (i in 1:(length(date1)-v)) {
  print(i)
  CT1.tab <- df_monthly %>% 
    filter(TradeDate >= date1[i] & TradeDate < date1[i+v]) %>%
    filter(Trading_days_month > 0) %>% 
    filter((!is.na(VOLUME))) %>% 
    group_by(SecurityId) %>% 
    filter(n() >= w) %>%
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ E.M.VW + SMB + HML + CT1)) %>% 
    mutate(CT1.monthly = as.numeric(coef(ols.model)[5])) %>%
    select("SecurityId", "CT1.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  CT1_monthly <- rbind(CT1_monthly,CT1.tab)}

write.table(CT1_monthly, file = "CT1_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

rm(CT1.tab,v,w,i)
################
############

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
    do(ols.model = lm(data = ., formula = E.ReturnAdjGeneric ~ E.M.VW + SMB + HML + CT2)) %>% 
    mutate(CT2.monthly = as.numeric(coef(ols.model)[5])) %>%
    select("SecurityId", "CT1.monthly") %>% 
    mutate(TradeDate = date1[v+i])
  CT2_monthly <- rbind(CT2_monthly,CT2.tab)}

write.table(CT2_monthly, file = "CT2_monthly.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

rm(CT2.tab,v,w,i)
################


###

### Merge variables with master dataframe
master$TradeDate = as.Date(master$TradeDate)
master <- merge(master, beta_daily, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, beta_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, ivol_daily, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, max_daily, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)
master <- merge(master, RUI_monthly, by = c("TradeDate", "SecurityId"), all.x = TRUE, all.y = FALSE)


write.table(master, file = "master_3_1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

### Filter to use have estimates of all variables for same time period
master <- master %>%
  filter(TradeDate >= as.Date("2000-05-01") & TradeDate <= as.Date("2020-03-31") )
  #I think I should filter NA value
  #filter()
na.fail(master)
write.table(master, file = "master_2000_3_1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

########## Section 8: Summary Statistics and Correlation: Variables ##########
master = read.csv("master_2000_3_1.csv", header = TRUE, sep = ",")
## Create summary statistics for estimated variables
summary_statistics_variables <- master %>%
  select("beta.monthly", "beta.daily", "max","RUI.monthly") %>%
  describe( ., na.rm = TRUE, skew = FALSE, quant = c(0.25,0.75))
## Create correlation matrix with averages of monthly cross-sectional correlations(not same as average correlation)
variable_cor <- master %>%
  select("TradeDate", "beta.monthly", "beta.daily", "max", "RUI.monthly")
#which(is.na(variable_cor$beta.monthly))
variable_cor <- variable_cor %>% 
  group_by(TradeDate)%>% 
  do(cormat = cor(select(., - matches("TradeDate")), use = "pairwise.complete.obs"))
variable_cor <- Reduce("+", variable_cor$cormat)/length(variable_cor$cormat)

rm(summary_statistics_variables, variable_cor)
rm(df_daily,df_market_daily,df_monthly,df_market_monthly)
rm(beta_daily,beta_monthly,ivol_daily,max_daily,RUI_monthly)
rm(date1)
