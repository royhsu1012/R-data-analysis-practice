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
setwd("D:/data/step6_data")
df_daily <- read.csv("df_daily_ch6_4_1.csv", header = TRUE, sep = ",")


setwd("D:/data/step9_data")
#"master_ch8_1.csv"
master <- read.csv("master_ch8_3_1.csv", header = TRUE, sep = ",")


setwd("D:/data/step10_data")
#################
########## Section 14: Identify most frequent companies in each portfolio ##########
# Create dataframe with SecurityID and Company Names
company_lookup <- df_daily %>% 
  select(SecurityId, Name)
company_lookup <- distinct(company_lookup)

### 1: Create overview of top 20 companies in portfolio 5
# Extract top 20 companies in beta.monthly portfolio 5
company.bm <- master %>% 
  group_by(SecurityId, Portfolio_b_m) %>% 
  summarise(n())
company.bm <- merge(company.bm, company_lookup, by = "SecurityId", all.x = TRUE)
company.bm <- company.bm %>% 
  filter(Portfolio_b_m == 5) %>% 
  rename(BetaMonthly = Name)%>%
  arrange(-`n()`) %>% select(BetaMonthly) %>% 
  slice(1:20)

# Extract top 20 companies in beta.daily portfolio 5
company.bd <- master %>% 
  group_by(SecurityId, Portfolio_b_d) %>% 
  summarise(n())
company.bd <- merge(company.bd, company_lookup, by = "SecurityId", all.x = TRUE)
company.bd <- company.bd %>% 
  filter(Portfolio_b_d == 5) %>% 
  rename(BetaDaily = Name) %>%
  arrange(-`n()`) %>% 
  select(BetaDaily) %>% 
  slice(1:20)

# Extract top 20 companies in max portfolio 5
company.max <- master %>% 
  group_by(SecurityId, Portfolio_max) %>% 
  summarise(n())
company.max <- merge(company.max, company_lookup, by = "SecurityId", all.x = TRUE)
company.max <- company.max %>% 
  filter(Portfolio_max == 5) %>% 
  rename(Max = Name) %>%
  arrange(-`n()`) %>% 
  select(Max) %>% 
  slice(1:20)

# Extract top 20 companies in RUI.monthly portfolio 5
company.RUI <- master %>% 
  group_by(SecurityId, Portfolio_RUI) %>% 
  summarise(n())
company.RUI <- merge(company.RUI, company_lookup, by = "SecurityId", all.x = TRUE)
company.RUI <- company.RUI %>% 
  filter(Portfolio_RUI == 5) %>% 
  rename(RUIMonthly = Name)%>%
  arrange(-`n()`) %>% select(RUIMonthly) %>% 
  slice(1:20)

# Create combined dataframe with top 20 companies in portfolio 5 for the different sorting variables
Port5_companies <- cbind(company.bm, company.bd, company.max, company.RUI)
rm(company.bm, company.bd, company.max, company.RUI)


### 2: Create overview of top 20 companies in portfolio 1
## Extract top 20 companies in beta.monthly portfolio 1
company.bm <- master %>% 
  group_by(SecurityId, Portfolio_b_m) %>% 
  summarise(n())
company.bm <- merge(company.bm, company_lookup, by = "SecurityId", all.x = TRUE)
company.bm <- company.bm %>% 
  filter(Portfolio_b_m == 1) %>% 
  rename(BetaMonthly = Name)%>%
  arrange(-`n()`) %>% select(BetaMonthly) %>% 
  slice(1:20)

# Extract top 20 companies in beta.daily portfolio 1
company.bd <- master %>% 
  group_by(SecurityId, Portfolio_b_d) %>% 
  summarise(n())
company.bd <- merge(company.bd, company_lookup, by = "SecurityId", all.x = TRUE)
company.bd <- company.bd %>% 
  filter(Portfolio_b_d == 1) %>% 
  rename(BetaDaily = Name) %>%
  arrange(-`n()`) %>% 
  select(BetaDaily) %>% 
  slice(1:20)

# Extract top 20 companies in max portfolio 1
company.max <- master %>% 
  group_by(SecurityId, Portfolio_max) %>% 
  summarise(n())
company.max <- merge(company.max, company_lookup, by = "SecurityId", all.x = TRUE)
company.max <- company.max %>% 
  filter(Portfolio_max == 1) %>% 
  rename(Max = Name) %>%
  arrange(-`n()`) %>% 
  select(Max) %>% 
  slice(1:20)

# Extract top 20 companies in RUI.monthly portfolio 1
company.RUI <- master %>% 
  group_by(SecurityId, Portfolio_RUI) %>% 
  summarise(n())
company.RUI <- merge(company.RUI, company_lookup, by = "SecurityId", all.x = TRUE)
company.RUI <- company.RUI %>% 
  filter(Portfolio_RUI == 1) %>% 
  rename(RUIMonthly = Name)%>%
  arrange(-`n()`) %>% select(RUIMonthly) %>% 
  slice(1:20)

# Create combined dataframe with top 20 companies in portfolio 1 for the different sorting variables
Port1_companies <- cbind(company.bm, company.bd, company.max, company.RUI)
rm(company.bm, company.bd, company.max, company.RUI)
