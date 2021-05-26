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
setwd("D:/data/step8_data/v48w36")
#"master_ch8_1.csv"
master <- read.csv("master_ch8_5.csv", header = TRUE, sep = ",")
setwd("D:/data/step9_data/v48w36")
########## Section 10: Perform Conditional Double-sorts ##########
#skip double sort, later maybe deal with it
########## Section 11: Compute monthly and timer-series average portfolio excess returns, sharpe
#ratios and portfolio characteristics ##########
#### 1: Quintile portfolios sorted on beta.monthly
### Calculate monthly VW (EW) portfolio excess returns and portfolio characteristics
portfolio_month <- master %>% 
  filter(!is.na(Portfolio_b_m)) %>%
  group_by(TradeDate,Portfolio_b_m) %>%
  summarise(length = length(E.ReturnAdjGeneric), 
            mcap = sum(LMCAP, na.rm = TRUE),
            er_month_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric), 
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE),
            beta_daily_ew = mean(beta.daily, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE), 
            er_month_vw = sum(mcap_return, na.rm = TRUE)/sum(LMCAP, na.rm = TRUE),
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP), 
            beta_daily_vw = sum(mcap_beta_daily, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP))
# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_month <- portfolio_month %>% 
                   group_by(TradeDate) %>%arrange(Portfolio_b_m) %>%
                   summarise(Portfolio_b_m = 6, 
                             length = 0, 
                             mcap = first(mcap)-last(mcap), 
                             er_month_ew = first(er_month_ew)-last(er_month_ew),
                             beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew), 
                             beta_daily_ew = first(beta_daily_ew)-last(beta_daily_ew),
                             max_ew = first(max_ew)-last(max_ew), er_month_vw = first(er_month_vw)-last(er_month_vw),
                             beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
                             beta_daily_vw = first(beta_daily_vw)-last(beta_daily_vw),
                             max_vw = first(max_vw)-last(max_vw)) %>% 
                   bind_rows(portfolio_month, .) %>% 
                   arrange(TradeDate)%>% 
                   rename(portfolio = Portfolio_b_m)
which(is.na(portfolio_month))
write.table(portfolio_month, file = "portfolio_month.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

getwd()


# Compute time-series averages
portfolio_month_c <- portfolio_month %>% 
                   group_by(portfolio) %>% 
                     summarise(length = round(mean(length)),
                     mcap = mean(mcap), beta_monthly_ew = mean(beta_monthly_ew), 
                     beta_daily_ew = mean(beta_daily_ew, na.rm = TRUE),
                     max_ew = mean(max_ew, na.rm = TRUE), sd_month_ew = sd(er_month_ew),
                     er_month_ew = mean(er_month_ew), 
                     sr_month_ew = er_month_ew/sd_month_ew, 
                     beta_monthly_vw = mean(beta_monthly_vw, na.rm = TRUE),
                     beta_daily_vw = mean(beta_daily_vw, na.rm = TRUE), 
                     max_vw = mean(max_vw), 
                     sd_month_vw = sd(er_month_vw, na.rm = TRUE),
                     er_month_vw = mean(er_month_vw), 
                     sr_month_vw = er_month_vw/sd_month_vw) %>% 
  t()
write.table(portfolio_month_c, file = "portfolio_month_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

########################
##############
#### 2: Quintile portfolios sorted on beta.daily
### Calculate monthly VW (EW) portfolio excess returns and portfolio characteristics
portfolio_day <- master %>% 
  filter(!is.na(Portfolio_b_d)) %>% 
  group_by(TradeDate, Portfolio_b_d)%>%
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE),
            er_day_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew =mean(beta.monthly, na.rm = TRUE),
            beta_daily_ew = mean(beta.daily, na.rm = TRUE),
            max_ew = mean(max, na.rm = TRUE),
            er_day_vw = sum(mcap_return)/sum(LMCAP),
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            beta_daily_vw = sum(mcap_beta_daily, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_day <- portfolio_day %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_b_d) %>%
  summarise(Portfolio_b_d = 6,
            length = 0,
            mcap = first(mcap)-last(mcap),
            er_day_ew = first(er_day_ew)-last(er_day_ew),
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            beta_daily_ew = first(beta_daily_ew)-last(beta_daily_ew),
            max_ew = first(max_ew)-last(max_ew),
            er_day_vw = first(er_day_vw)-last(er_day_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw),
            beta_daily_vw = first(beta_daily_vw)-last(beta_daily_vw), 
            max_vw = first(max_vw)-last(max_vw))%>%
  bind_rows(portfolio_day, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_b_d)

which(is.na(portfolio_day))
write.table(portfolio_day, file = "portfolio_day.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_day_c <- portfolio_day %>% 
  group_by(portfolio) %>% summarise(length = round(mean(length)),
                                   mcap = mean(mcap),beta_monthly_ew = mean(beta_monthly_ew),beta_daily_ew = mean(beta_daily_ew),
                                   max_ew = mean(max_ew), 
                                   sd_day_ew = sd(er_day_ew),
                                   er_day_ew = mean(er_day_ew),sr_day_ew =  er_day_ew/sd_day_ew,
                                   beta_monthly_vw = mean(beta_monthly_vw),
                                   beta_daily_vw = mean(beta_daily_vw),
                                   max_vw =mean(max_vw),
                                   sd_day_vw = sd(er_day_vw),
                                   er_day_vw = mean(er_day_vw),
                                   sr_day_vw = er_day_vw/sd_day_vw) %>% 
  t()

write.table(portfolio_day_c, file = "portfolio_day_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

######################
### Calculate monthly VW (EW) portfolio excess returns and portfolio characteristics
portfolio_max <- master %>% 
  filter(!is.na(Portfolio_max)) %>%
  group_by(TradeDate, Portfolio_max) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_max_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_max_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_max <- portfolio_max %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_max) %>%
  summarise(Portfolio_max = 6, 
            length = 0, 
            mcap = first(mcap)-last(mcap),
            er_max_ew = first(er_max_ew)-last(er_max_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_max_vw = first(er_max_vw)-last(er_max_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_max, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_max)

which(is.na(portfolio_max))
write.table(portfolio_max, file = "portfolio_max.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_max_c <- portfolio_max %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_max_ew = sd(er_max_ew),
            er_max_ew = mean(er_max_ew), 
            sr_max_ew = er_max_ew/sd_max_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_max_vw = sd(er_max_vw), 
            er_max_vw = mean(er_max_vw),
            sr_max_vw = er_max_vw/sd_max_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_max_c, file = "portfolio_max_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

########################
######################

### Calculate monthly VW (EW) portfolio excess returns and portfolio characteristics
portfolio_RUI <- master %>% 
  filter(!is.na(Portfolio_RUI)) %>%
  group_by(TradeDate, Portfolio_RUI) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_RUI_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_RUI_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_RUI <- portfolio_RUI %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_RUI) %>%
  summarise(Portfolio_RUI = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_RUI_ew = first(er_RUI_ew)-last(er_RUI_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_RUI_vw = first(er_RUI_vw)-last(er_RUI_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_RUI, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_RUI)

which(is.na(portfolio_RUI))
write.table(portfolio_RUI, file = "portfolio_RUI.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_RUI_c <- portfolio_RUI %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_RUI_ew = sd(er_RUI_ew),
            er_RUI_ew = mean(er_RUI_ew), 
            sr_RUI_ew = er_RUI_ew/sd_RUI_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_RUI_vw = sd(er_RUI_vw), 
            er_RUI_vw = mean(er_RUI_vw),
            sr_RUI_vw = er_RUI_vw/sd_RUI_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_RUI_c, file = "portfolio_RUI_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

####################
##CT1
portfolio_CT1 <- master %>% 
  filter(!is.na(Portfolio_CT1)) %>%
  group_by(TradeDate, Portfolio_CT1) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_CT1_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_CT1_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_CT1 <- portfolio_CT1 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_CT1) %>%
  summarise(Portfolio_CT1 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_CT1_ew = first(er_CT1_ew)-last(er_CT1_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_CT1_vw = first(er_CT1_vw)-last(er_CT1_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_CT1, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_CT1)

which(is.na(portfolio_CT1))
write.table(portfolio_CT1, file = "portfolio_CT1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_CT1_c <- portfolio_CT1 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_CT1_ew = sd(er_CT1_ew),
            er_CT1_ew = mean(er_CT1_ew), 
            sr_CT1_ew = er_CT1_ew/sd_CT1_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_CT1_vw = sd(er_CT1_vw), 
            er_CT1_vw = mean(er_CT1_vw),
            sr_CT1_vw = er_CT1_vw/sd_CT1_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_CT1_c, file = "portfolio_CT1_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

##############
####################
##CT2
portfolio_CT2 <- master %>% 
  filter(!is.na(Portfolio_CT2)) %>%
  group_by(TradeDate, Portfolio_CT2) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_CT2_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_CT2_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_CT2 <- portfolio_CT2 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_CT2) %>%
  summarise(Portfolio_CT2 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_CT2_ew = first(er_CT2_ew)-last(er_CT2_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_CT2_vw = first(er_CT2_vw)-last(er_CT2_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_CT2, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_CT2)

which(is.na(portfolio_CT2))
write.table(portfolio_CT2, file = "portfolio_CT2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_CT2_c <- portfolio_CT2 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_CT2_ew = sd(er_CT2_ew),
            er_CT2_ew = mean(er_CT2_ew), 
            sr_CT2_ew = er_CT2_ew/sd_CT2_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_CT2_vw = sd(er_CT2_vw), 
            er_CT2_vw = mean(er_CT2_vw),
            sr_CT2_vw = er_CT2_vw/sd_CT2_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_CT2_c, file = "portfolio_CT2_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

####################
##CT3
portfolio_CT3 <- master %>% 
  filter(!is.na(Portfolio_CT3)) %>%
  group_by(TradeDate, Portfolio_CT3) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_CT3_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_CT3_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_CT3 <- portfolio_CT3 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_CT3) %>%
  summarise(Portfolio_CT3 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_CT3_ew = first(er_CT3_ew)-last(er_CT3_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_CT3_vw = first(er_CT3_vw)-last(er_CT3_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_CT3, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_CT3)

which(is.na(portfolio_CT3))
write.table(portfolio_CT3, file = "portfolio_CT3.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_CT3_c <- portfolio_CT3 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_CT3_ew = sd(er_CT3_ew),
            er_CT3_ew = mean(er_CT3_ew), 
            sr_CT3_ew = er_CT3_ew/sd_CT3_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_CT3_vw = sd(er_CT3_vw), 
            er_CT3_vw = mean(er_CT3_vw),
            sr_CT3_vw = er_CT3_vw/sd_CT3_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_CT3_c, file = "portfolio_CT3_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

####################
##TC1
portfolio_TC1 <- master %>% 
  filter(!is.na(Portfolio_TC1)) %>%
  group_by(TradeDate, Portfolio_TC1) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_TC1_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_TC1_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_TC1 <- portfolio_TC1 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_TC1) %>%
  summarise(Portfolio_TC1 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_TC1_ew = first(er_TC1_ew)-last(er_TC1_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_TC1_vw = first(er_TC1_vw)-last(er_TC1_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_TC1, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_TC1)

which(is.na(portfolio_TC1))
write.table(portfolio_TC1, file = "portfolio_TC1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_TC1_c <- portfolio_TC1 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_TC1_ew = sd(er_TC1_ew),
            er_TC1_ew = mean(er_TC1_ew), 
            sr_TC1_ew = er_TC1_ew/sd_TC1_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_TC1_vw = sd(er_TC1_vw), 
            er_TC1_vw = mean(er_TC1_vw),
            sr_TC1_vw = er_TC1_vw/sd_TC1_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_TC1_c, file = "portfolio_TC1_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

####################
##TC2
portfolio_TC2 <- master %>% 
  filter(!is.na(Portfolio_TC2)) %>%
  group_by(TradeDate, Portfolio_TC2) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_TC2_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_TC2_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_TC2 <- portfolio_TC2 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_TC2) %>%
  summarise(Portfolio_TC2 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_TC2_ew = first(er_TC2_ew)-last(er_TC2_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_TC2_vw = first(er_TC2_vw)-last(er_TC2_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_TC2, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_TC2)

which(is.na(portfolio_TC2))
write.table(portfolio_TC2, file = "portfolio_TC2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_TC2_c <- portfolio_TC2 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_TC2_ew = sd(er_TC2_ew),
            er_TC2_ew = mean(er_TC2_ew), 
            sr_TC2_ew = er_TC2_ew/sd_TC2_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_TC2_vw = sd(er_TC2_vw), 
            er_TC2_vw = mean(er_TC2_vw),
            sr_TC2_vw = er_TC2_vw/sd_TC2_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_TC2_c, file = "portfolio_TC2_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")


####################
##TC3
portfolio_TC3 <- master %>% 
  filter(!is.na(Portfolio_TC3)) %>%
  group_by(TradeDate, Portfolio_TC3) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_TC3_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_TC3_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_TC3 <- portfolio_TC3 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_TC3) %>%
  summarise(Portfolio_TC3 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_TC3_ew = first(er_TC3_ew)-last(er_TC3_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_TC3_vw = first(er_TC3_vw)-last(er_TC3_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_TC3, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_TC3)

which(is.na(portfolio_TC3))
write.table(portfolio_TC3, file = "portfolio_TC3.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_TC3_c <- portfolio_TC3 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_TC3_ew = sd(er_TC3_ew),
            er_TC3_ew = mean(er_TC3_ew), 
            sr_TC3_ew = er_TC3_ew/sd_TC3_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_TC3_vw = sd(er_TC3_vw), 
            er_TC3_vw = mean(er_TC3_vw),
            sr_TC3_vw = er_TC3_vw/sd_TC3_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_TC3_c, file = "portfolio_TC3_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

####################
##UT1
portfolio_UT1 <- master %>% 
  filter(!is.na(Portfolio_UT1)) %>%
  group_by(TradeDate, Portfolio_UT1) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_UT1_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_UT1_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_UT1 <- portfolio_UT1 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_UT1) %>%
  summarise(Portfolio_UT1 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_UT1_ew = first(er_UT1_ew)-last(er_UT1_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_UT1_vw = first(er_UT1_vw)-last(er_UT1_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_UT1, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_UT1)

which(is.na(portfolio_UT1))
write.table(portfolio_UT1, file = "portfolio_UT1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_UT1_c <- portfolio_UT1 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_UT1_ew = sd(er_UT1_ew),
            er_UT1_ew = mean(er_UT1_ew), 
            sr_UT1_ew = er_UT1_ew/sd_UT1_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_UT1_vw = sd(er_UT1_vw), 
            er_UT1_vw = mean(er_UT1_vw),
            sr_UT1_vw = er_UT1_vw/sd_UT1_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_UT1_c, file = "portfolio_UT1_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

###################
##UT2
portfolio_UT2 <- master %>% 
  filter(!is.na(Portfolio_UT2)) %>%
  group_by(TradeDate, Portfolio_UT2) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_UT2_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_UT2_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_UT2 <- portfolio_UT2 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_UT2) %>%
  summarise(Portfolio_UT2 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_UT2_ew = first(er_UT2_ew)-last(er_UT2_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_UT2_vw = first(er_UT2_vw)-last(er_UT2_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_UT2, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_UT2)

which(is.na(portfolio_UT2))
write.table(portfolio_UT2, file = "portfolio_UT2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_UT2_c <- portfolio_UT2 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_UT2_ew = sd(er_UT2_ew),
            er_UT2_ew = mean(er_UT2_ew), 
            sr_UT2_ew = er_UT2_ew/sd_UT2_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_UT2_vw = sd(er_UT2_vw), 
            er_UT2_vw = mean(er_UT2_vw),
            sr_UT2_vw = er_UT2_vw/sd_UT2_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_UT2_c, file = "portfolio_UT2_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

###################
##UT3
portfolio_UT3 <- master %>% 
  filter(!is.na(Portfolio_UT3)) %>%
  group_by(TradeDate, Portfolio_UT3) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_UT3_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_UT3_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_UT3 <- portfolio_UT3 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_UT3) %>%
  summarise(Portfolio_UT3 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_UT3_ew = first(er_UT3_ew)-last(er_UT3_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_UT3_vw = first(er_UT3_vw)-last(er_UT3_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_UT3, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_UT3)

which(is.na(portfolio_UT3))
write.table(portfolio_UT3, file = "portfolio_UT3.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_UT3_c <- portfolio_UT3 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_UT3_ew = sd(er_UT3_ew),
            er_UT3_ew = mean(er_UT3_ew), 
            sr_UT3_ew = er_UT3_ew/sd_UT3_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_UT3_vw = sd(er_UT3_vw), 
            er_UT3_vw = mean(er_UT3_vw),
            sr_UT3_vw = er_UT3_vw/sd_UT3_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_UT3_c, file = "portfolio_UT3_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

####################
##TU1
portfolio_TU1 <- master %>% 
  filter(!is.na(Portfolio_TU1)) %>%
  group_by(TradeDate, Portfolio_TU1) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_TU1_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_TU1_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_TU1 <- portfolio_TU1 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_TU1) %>%
  summarise(Portfolio_TU1 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_TU1_ew = first(er_TU1_ew)-last(er_TU1_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_TU1_vw = first(er_TU1_vw)-last(er_TU1_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_TU1, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_TU1)

which(is.na(portfolio_TU1))
write.table(portfolio_TU1, file = "portfolio_TU1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_TU1_c <- portfolio_TU1 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_TU1_ew = sd(er_TU1_ew),
            er_TU1_ew = mean(er_TU1_ew), 
            sr_TU1_ew = er_TU1_ew/sd_TU1_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_TU1_vw = sd(er_TU1_vw), 
            er_TU1_vw = mean(er_TU1_vw),
            sr_TU1_vw = er_TU1_vw/sd_TU1_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_TU1_c, file = "portfolio_TU1_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

###################
##TU2
portfolio_TU2 <- master %>% 
  filter(!is.na(Portfolio_TU2)) %>%
  group_by(TradeDate, Portfolio_TU2) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_TU2_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_TU2_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_TU2 <- portfolio_TU2 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_TU2) %>%
  summarise(Portfolio_TU2 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_TU2_ew = first(er_TU2_ew)-last(er_TU2_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_TU2_vw = first(er_TU2_vw)-last(er_TU2_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_TU2, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_TU2)

which(is.na(portfolio_TU2))
write.table(portfolio_TU2, file = "portfolio_TU2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_TU2_c <- portfolio_TU2 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_TU2_ew = sd(er_TU2_ew),
            er_TU2_ew = mean(er_TU2_ew), 
            sr_TU2_ew = er_TU2_ew/sd_TU2_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_TU2_vw = sd(er_TU2_vw), 
            er_TU2_vw = mean(er_TU2_vw),
            sr_TU2_vw = er_TU2_vw/sd_TU2_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_TU2_c, file = "portfolio_TU2_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

###################
##TU3
portfolio_TU3 <- master %>% 
  filter(!is.na(Portfolio_TU3)) %>%
  group_by(TradeDate, Portfolio_TU3) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_TU3_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_TU3_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_TU3 <- portfolio_TU3 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_TU3) %>%
  summarise(Portfolio_TU3 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_TU3_ew = first(er_TU3_ew)-last(er_TU3_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_TU3_vw = first(er_TU3_vw)-last(er_TU3_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_TU3, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_TU3)

which(is.na(portfolio_TU3))
write.table(portfolio_TU3, file = "portfolio_TU3.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_TU3_c <- portfolio_TU3 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_TU3_ew = sd(er_TU3_ew),
            er_TU3_ew = mean(er_TU3_ew), 
            sr_TU3_ew = er_TU3_ew/sd_TU3_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_TU3_vw = sd(er_TU3_vw), 
            er_TU3_vw = mean(er_TU3_vw),
            sr_TU3_vw = er_TU3_vw/sd_TU3_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_TU3_c, file = "portfolio_TU3_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

###################
###################
##CU1
portfolio_CU1 <- master %>% 
  filter(!is.na(Portfolio_CU1)) %>%
  group_by(TradeDate, Portfolio_CU1) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_CU1_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_CU1_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_CU1 <- portfolio_CU1 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_CU1) %>%
  summarise(Portfolio_CU1 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_CU1_ew = first(er_CU1_ew)-last(er_CU1_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_CU1_vw = first(er_CU1_vw)-last(er_CU1_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_CU1, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_CU1)

which(is.na(portfolio_CU1))
write.table(portfolio_CU1, file = "portfolio_CU1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_CU1_c <- portfolio_CU1 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_CU1_ew = sd(er_CU1_ew),
            er_CU1_ew = mean(er_CU1_ew), 
            sr_CU1_ew = er_CU1_ew/sd_CU1_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_CU1_vw = sd(er_CU1_vw), 
            er_CU1_vw = mean(er_CU1_vw),
            sr_CU1_vw = er_CU1_vw/sd_CU1_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_CU1_c, file = "portfolio_CU1_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

###################
##CU2
portfolio_CU2 <- master %>% 
  filter(!is.na(Portfolio_CU2)) %>%
  group_by(TradeDate, Portfolio_CU2) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_CU2_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_CU2_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_CU2 <- portfolio_CU2 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_CU2) %>%
  summarise(Portfolio_CU2 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_CU2_ew = first(er_CU2_ew)-last(er_CU2_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_CU2_vw = first(er_CU2_vw)-last(er_CU2_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_CU2, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_CU2)

which(is.na(portfolio_CU2))
write.table(portfolio_CU2, file = "portfolio_CU2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_CU2_c <- portfolio_CU2 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_CU2_ew = sd(er_CU2_ew),
            er_CU2_ew = mean(er_CU2_ew), 
            sr_CU2_ew = er_CU2_ew/sd_CU2_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_CU2_vw = sd(er_CU2_vw), 
            er_CU2_vw = mean(er_CU2_vw),
            sr_CU2_vw = er_CU2_vw/sd_CU2_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_CU2_c, file = "portfolio_CU2_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

###################
##CU3
portfolio_CU3 <- master %>% 
  filter(!is.na(Portfolio_CU3)) %>%
  group_by(TradeDate, Portfolio_CU3) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_CU3_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_CU3_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_CU3 <- portfolio_CU3 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_CU3) %>%
  summarise(Portfolio_CU3 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_CU3_ew = first(er_CU3_ew)-last(er_CU3_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_CU3_vw = first(er_CU3_vw)-last(er_CU3_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_CU3, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_CU3)

which(is.na(portfolio_CU3))
write.table(portfolio_CU3, file = "portfolio_CU3.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_CU3_c <- portfolio_CU3 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_CU3_ew = sd(er_CU3_ew),
            er_CU3_ew = mean(er_CU3_ew), 
            sr_CU3_ew = er_CU3_ew/sd_CU3_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_CU3_vw = sd(er_CU3_vw), 
            er_CU3_vw = mean(er_CU3_vw),
            sr_CU3_vw = er_CU3_vw/sd_CU3_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_CU3_c, file = "portfolio_CU3_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

###################
##UC1
portfolio_UC1 <- master %>% 
  filter(!is.na(Portfolio_UC1)) %>%
  group_by(TradeDate, Portfolio_UC1) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_UC1_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_UC1_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_UC1 <- portfolio_UC1 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_UC1) %>%
  summarise(Portfolio_UC1 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_UC1_ew = first(er_UC1_ew)-last(er_UC1_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_UC1_vw = first(er_UC1_vw)-last(er_UC1_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_UC1, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_UC1)

which(is.na(portfolio_UC1))
write.table(portfolio_UC1, file = "portfolio_UC1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_UC1_c <- portfolio_UC1 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_UC1_ew = sd(er_UC1_ew),
            er_UC1_ew = mean(er_UC1_ew), 
            sr_UC1_ew = er_UC1_ew/sd_UC1_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_UC1_vw = sd(er_UC1_vw), 
            er_UC1_vw = mean(er_UC1_vw),
            sr_UC1_vw = er_UC1_vw/sd_UC1_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_UC1_c, file = "portfolio_UC1_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

###################
##UC2
portfolio_UC2 <- master %>% 
  filter(!is.na(Portfolio_UC2)) %>%
  group_by(TradeDate, Portfolio_UC2) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_UC2_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_UC2_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_UC2 <- portfolio_UC2 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_UC2) %>%
  summarise(Portfolio_UC2 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_UC2_ew = first(er_UC2_ew)-last(er_UC2_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_UC2_vw = first(er_UC2_vw)-last(er_UC2_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_UC2, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_UC2)

which(is.na(portfolio_UC2))
write.table(portfolio_UC2, file = "portfolio_UC2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_UC2_c <- portfolio_UC2 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_UC2_ew = sd(er_UC2_ew),
            er_UC2_ew = mean(er_UC2_ew), 
            sr_UC2_ew = er_UC2_ew/sd_UC2_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_UC2_vw = sd(er_UC2_vw), 
            er_UC2_vw = mean(er_UC2_vw),
            sr_UC2_vw = er_UC2_vw/sd_UC2_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_UC2_c, file = "portfolio_UC2_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

###################
##UC3
portfolio_UC3 <- master %>% 
  filter(!is.na(Portfolio_UC3)) %>%
  group_by(TradeDate, Portfolio_UC3) %>% 
  summarise(length = length(E.ReturnAdjGeneric),
            mcap = sum(LMCAP, na.rm = TRUE), 
            er_UC3_ew = sum(E.ReturnAdjGeneric)/length(E.ReturnAdjGeneric),
            beta_monthly_ew = mean(beta.monthly, na.rm = TRUE), 
            max_ew = mean(max, na.rm = TRUE),
            er_UC3_vw = sum(mcap_return)/sum(LMCAP), 
            beta_monthly_vw = sum(mcap_beta_monthly, na.rm = TRUE)/sum(LMCAP),
            max_vw = sum(mcap_max, na.rm = TRUE)/sum(LMCAP), 
            price = mean(ROI, na.rm = TRUE),
            ivol = mean(ivol, na.rm = TRUE), 
            iskew = mean(iskew, na.rm = TRUE))

# Compute the metrics for the low-high portfolio (portfolio 6)
portfolio_UC3 <- portfolio_UC3 %>% 
  group_by(TradeDate) %>% 
  arrange(Portfolio_UC3) %>%
  summarise(Portfolio_UC3 = 6, length = 0, 
            mcap = first(mcap)-last(mcap),
            er_UC3_ew = first(er_UC3_ew)-last(er_UC3_ew), 
            beta_monthly_ew = first(beta_monthly_ew)-last(beta_monthly_ew),
            max_ew = first(max_ew)-last(max_ew), 
            er_UC3_vw = first(er_UC3_vw)-last(er_UC3_vw),
            beta_monthly_vw = first(beta_monthly_vw)-last(beta_monthly_vw), 
            max_vw = first(max_vw)-last(max_vw),
            price = first(price) - last(price), 
            ivol = first(ivol)-last(ivol), 
            iskew = first(iskew) - last(iskew)) %>%
  bind_rows(portfolio_UC3, .) %>% 
  arrange(TradeDate) %>% 
  rename(portfolio = Portfolio_UC3)

which(is.na(portfolio_UC3))
write.table(portfolio_UC3, file = "portfolio_UC3.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

# Compute time-series averages
portfolio_UC3_c <- portfolio_UC3 %>% 
  group_by(portfolio) %>%
  summarise(length = round(mean(length)),mcap = mean(mcap),
            beta_monthly_ew = mean(beta_monthly_ew), 
            max_ew = mean(max_ew), 
            sd_UC3_ew = sd(er_UC3_ew),
            er_UC3_ew = mean(er_UC3_ew), 
            sr_UC3_ew = er_UC3_ew/sd_UC3_ew, 
            beta_monthly_vw = mean(beta_monthly_vw),
            max_vw = mean(max_vw), 
            sd_UC3_vw = sd(er_UC3_vw), 
            er_UC3_vw = mean(er_UC3_vw),
            sr_UC3_vw = er_UC3_vw/sd_UC3_vw, 
            price = mean(price),
            ivol = mean(ivol), 
            iskew= mean(iskew)) %>%
  t()
write.table(portfolio_UC3_c, file = "portfolio_UC3_c.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

####<<<<<<<<<<<<<<<<<<<<  DOUBLE SORT  >>>>>>>>>>>>>>>>

########## Section 12: Construct dataframe with all monthly portfolio excess returns ##########

portfolio_returns <- merge(portfolio_month[,c("TradeDate", "portfolio", "er_month_ew","er_month_vw")],
                           portfolio_day[,c("TradeDate", "portfolio", "er_day_ew", "er_day_vw")], 
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_max[,c("TradeDate", "portfolio", "er_max_ew", "er_max_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_CT1[,c("TradeDate", "portfolio", "er_CT1_ew", "er_CT1_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_CT2[,c("TradeDate", "portfolio", "er_CT2_ew", "er_CT2_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_CT3[,c("TradeDate", "portfolio", "er_CT3_ew", "er_CT3_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_TC1[,c("TradeDate", "portfolio", "er_TC1_ew", "er_TC1_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_TC2[,c("TradeDate", "portfolio", "er_TC2_ew", "er_TC2_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_TC3[,c("TradeDate", "portfolio", "er_TC3_ew", "er_TC3_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_UT1[,c("TradeDate", "portfolio", "er_UT1_ew", "er_UT1_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_UT2[,c("TradeDate", "portfolio", "er_UT2_ew", "er_UT2_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_UT3[,c("TradeDate", "portfolio", "er_UT3_ew", "er_UT3_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_TU1[,c("TradeDate", "portfolio", "er_TU1_ew", "er_TU1_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_TU2[,c("TradeDate", "portfolio", "er_TU2_ew", "er_TU2_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_TU3[,c("TradeDate", "portfolio", "er_TU3_ew", "er_TU3_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_CU1[,c("TradeDate", "portfolio", "er_CU1_ew", "er_CU1_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_CU2[,c("TradeDate", "portfolio", "er_CU2_ew", "er_CU2_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_CU3[,c("TradeDate", "portfolio", "er_CU3_ew", "er_CU3_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_UC1[,c("TradeDate", "portfolio", "er_UC1_ew", "er_UC1_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_UC2[,c("TradeDate", "portfolio", "er_UC2_ew", "er_UC2_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)
portfolio_returns <- merge(portfolio_returns,
                           portfolio_UC3[,c("TradeDate", "portfolio", "er_UC3_ew", "er_UC3_vw")],
                           by = c("TradeDate","portfolio"), 
                           all.x = TRUE, all.y = TRUE)


colnames(portfolio_returns)
## df_market_monthly
setwd("D:/data/step6_data")
df_market_monthly <- read.csv("df_market_monthly_ch6_7.csv", header = TRUE, sep = ",")
setwd("D:/data/step9_data/v48w36")
# Merge dataframe containing portfolio excess returns with market returns and factor returns to prepare for factor model regressions
portfolio_returns <- merge(portfolio_returns, df_market_monthly, by = "TradeDate")
colnames(portfolio_returns)
which(is.na(portfolio_returns))
write.table(portfolio_returns, file = "portfolio_returns_3.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")



rm(portfolio_returns,
  portfolio_month_c,
  portfolio_day_c,
  portfolio_max_c,
  portfolio_RUI_c,
  portfolio_CT1_c,
  portfolio_CT2_c,
  portfolio_CT3_c,
  portfolio_TC1_c,
  portfolio_TC2_c,
  portfolio_TC3_c,
  portfolio_UT1_c,
  portfolio_UT2_c,
  portfolio_UT3_c,
  portfolio_TU1_c,
  portfolio_TU2_c,
  portfolio_TU3_c,
  portfolio_CU1_c,
  portfolio_CU2_c,
  portfolio_CU3_c,
  portfolio_UC1_c,
  portfolio_UC2_c,
  portfolio_UC3_c)
rm(portfolio_returns,
   portfolio_month,
   portfolio_day,
   portfolio_max,
   portfolio_RUI,
   portfolio_CT1,
   portfolio_CT2,
   portfolio_CT3,
   portfolio_TC1,
   portfolio_TC2,
   portfolio_TC3,
   portfolio_UT1,
   portfolio_UT2,
   portfolio_UT3,
   portfolio_TU1,
   portfolio_TU2,
   portfolio_TU3,
   portfolio_CU1,
   portfolio_CU2,
   portfolio_CU3,
   portfolio_UC1,
   portfolio_UC2,
   portfolio_UC3,
   df_market_monthly)
