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
setwd("D:/data/step7_data/v12w12")

master <- read.csv("master_2000_4.csv", header = TRUE, sep = ",")

#
setwd("D:/data/step8_data/v12w12")

colnames(master)
#which(is.na(master))
########## Section 9: Sort stocks into portfolios ##########
# At the end of each month t, we sort all stocks into portfolios based on and ascending ordering
#of the sorting variables
# In this section it is important to note what filters are applied to the different sorts as they
#have implications for the double-sorts and factor constructions in the following sections
### Calculate additional variables needed to create portfolios

# Note that all measures using mcap are computed using LMCAP.
master <- master %>% 
  mutate(mcap_return = E.ReturnAdjGeneric * LMCAP) %>%
  mutate(mcap_beta_monthly = LMCAP * beta.monthly) %>% 
  mutate(mcap_beta_daily = LMCAP * beta.daily) %>%
  mutate(mcap_max = LMCAP * max) %>%
  mutate(mcap_ivol = LMCAP * ivol)%>%
  mutate(mcap_RUI = LMCAP * RUI.monthly)%>%
  mutate(mcap_CT1 = LMCAP * CT1.monthly)%>%
  mutate(mcap_CT2 = LMCAP * CT2.monthly)%>%
  mutate(mcap_CT3 = LMCAP * CT3.monthly)%>%
  mutate(mcap_TC1 = LMCAP * TC1.monthly)%>%
  mutate(mcap_TC2 = LMCAP * TC2.monthly)%>%
  mutate(mcap_TC3 = LMCAP * TC3.monthly)%>%
  mutate(mcap_UT1 = LMCAP * UT1.monthly)%>%
  mutate(mcap_UT2 = LMCAP * UT2.monthly)%>%
  mutate(mcap_UT3 = LMCAP * UT3.monthly)%>%
  mutate(mcap_TU1 = LMCAP * TU1.monthly)%>%
  mutate(mcap_TU2 = LMCAP * TU2.monthly)%>%
  mutate(mcap_TU3 = LMCAP * TU3.monthly)%>%
  mutate(mcap_CU1 = LMCAP * CU1.monthly)%>%
  mutate(mcap_CU2 = LMCAP * CU2.monthly)%>%
  mutate(mcap_CU3 = LMCAP * CU3.monthly)%>%
  mutate(mcap_UC1 = LMCAP * UC1.monthly)%>%
  mutate(mcap_UC2 = LMCAP * UC2.monthly)%>%
  mutate(mcap_UC3 = LMCAP * UC3.monthly)
write.table(master, file = "master_ch8_4.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

### Construct quintile portfolios sorted on an ascending ordering of beta.monthly
# Only observations with an estimate of beta.monthly are included in the sort
sort_bm <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(beta.monthly)) %>%
  mutate(Portfolio_b_m = ifelse(beta.monthly <= quantile(beta.monthly, 0.20, type = 3), 1,
                         ifelse(beta.monthly > quantile(beta.monthly, 0.20, type = 3)
                         & beta.monthly <= quantile(beta.monthly, 0.40, type = 3), 2,
                         ifelse(beta.monthly > quantile(beta.monthly, 0.40, type = 3)
                         & beta.monthly <= quantile(beta.monthly, 0.60, type = 3), 3,
                         ifelse(beta.monthly > quantile(beta.monthly, 0.60, type = 3)
                         & beta.monthly <= quantile(beta.monthly, 0.80, type = 3), 4,
                         5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_b_m)

write.table(sort_bm, file = "sort_bm_2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")


### Construct quintile portfolios sorted on an ascending ordering of beta.daily
# Only observations with an estimate of beta.daily are included in the sort
sort_bd <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(beta.daily)) %>%
  mutate(Portfolio_b_d = ifelse(beta.daily <= quantile(beta.daily, 0.20, type = 3), 1,
                         ifelse(beta.daily > quantile(beta.daily, 0.20, type = 3) & beta.daily <= quantile(beta.daily,0.40, type = 3), 2,
                         ifelse(beta.daily > quantile(beta.daily, 0.40, type = 3) & beta.daily <= quantile(beta.daily,0.60, type = 3), 3,
                         ifelse(beta.daily > quantile(beta.daily, 0.60, type = 3) & beta.daily <= quantile(beta.daily,0.80, type = 3), 4,
                         5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_b_d)

write.table(sort_bd, file = "sort_bd_2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

### Construct quintile portfolios sorted on an ascending ordering of max
# Only observations with an estimate of max are included in the sort
sort_max <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(max)) %>%
  mutate(Portfolio_max = ifelse(max <= quantile(max, 0.20, type = 3), 1,
                         ifelse(max > quantile(max, 0.20, type = 3) & max <= quantile(max, 0.40, type = 3), 2,
                         ifelse(max > quantile(max, 0.40, type = 3) & max <= quantile(max, 0.60, type = 3), 3,
                         ifelse(max > quantile(max, 0.60, type = 3) & max <= quantile(max, 0.80, type = 3), 4,
                         5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_max)

write.table(sort_max, file = "sort_max_2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

### Construct quintile portfolios sorted on an ascending ordering of ivol
# Only observations with an estimate of ivol are included in the sort
sort_ivol <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(ivol)) %>%
  mutate(Portfolio_ivol = ifelse(ivol <= quantile(ivol, 0.20, type = 3), 1,
                          ifelse(ivol > quantile(ivol, 0.20, type = 3) & ivol <= quantile(ivol, 0.40, type = 3), 2,
                          ifelse(ivol > quantile(ivol, 0.40, type = 3) & ivol <= quantile(ivol, 0.60, type = 3), 3,
                          ifelse(ivol > quantile(ivol, 0.60, type = 3) & ivol <= quantile(ivol, 0.80, type = 3), 4,
                          5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_ivol)

write.table(sort_ivol, file = "sort_ivol_2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

##RUI
### Construct quintile portfolios sorted on an ascending ordering of RUI.monthly
# Only observations with an estimate of RUI are included in the sort
sort_RUI <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(RUI.monthly)) %>%
  mutate(Portfolio_RUI = ifelse(RUI.monthly <= quantile(RUI.monthly, 0.20, type = 3), 1,
                          ifelse(RUI.monthly > quantile(RUI.monthly, 0.20, type = 3) & RUI.monthly <= quantile(RUI.monthly, 0.40, type = 3), 2,
                          ifelse(RUI.monthly > quantile(RUI.monthly, 0.40, type = 3) & RUI.monthly <= quantile(RUI.monthly, 0.60, type = 3), 3,
                          ifelse(RUI.monthly > quantile(RUI.monthly, 0.60, type = 3) & RUI.monthly <= quantile(RUI.monthly, 0.80, type = 3), 4,
                          5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_RUI) 
write.table(sort_RUI, file = "sort_RUI_2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
####################
#CT1
sort_CT1 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(CT1.monthly)) %>%
  mutate(Portfolio_CT1 = ifelse(CT1.monthly <= quantile(CT1.monthly, 0.20, type = 3), 1,
                         ifelse(CT1.monthly > quantile(CT1.monthly, 0.20, type = 3) & 
                                  CT1.monthly <= quantile(CT1.monthly, 0.40, type = 3), 2,
                         ifelse(CT1.monthly > quantile(CT1.monthly, 0.40, type = 3) & 
                                  CT1.monthly <= quantile(CT1.monthly, 0.60, type = 3), 3,
                         ifelse(CT1.monthly > quantile(CT1.monthly, 0.60, type = 3) & 
                                  CT1.monthly <= quantile(CT1.monthly, 0.80, type = 3), 4,
                         5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_CT1) 
write.table(sort_CT1, file = "sort_CT1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

####################
#CT2
sort_CT2 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(CT2.monthly)) %>%
  mutate(Portfolio_CT2 = ifelse(CT2.monthly <= quantile(CT2.monthly, 0.20, type = 3), 1,
                                ifelse(CT2.monthly > quantile(CT2.monthly, 0.20, type = 3) & 
                                         CT2.monthly <= quantile(CT2.monthly, 0.40, type = 3), 2,
                                       ifelse(CT2.monthly > quantile(CT2.monthly, 0.40, type = 3) & 
                                                CT2.monthly <= quantile(CT2.monthly, 0.60, type = 3), 3,
                                              ifelse(CT2.monthly > quantile(CT2.monthly, 0.60, type = 3) & 
                                                       CT2.monthly <= quantile(CT2.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_CT2) 
write.table(sort_CT2, file = "sort_CT2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

####################
#CT3
sort_CT3 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(CT3.monthly)) %>%
  mutate(Portfolio_CT3 = ifelse(CT3.monthly <= quantile(CT3.monthly, 0.20, type = 3), 1,
                                ifelse(CT3.monthly > quantile(CT3.monthly, 0.20, type = 3) & 
                                         CT3.monthly <= quantile(CT3.monthly, 0.40, type = 3), 2,
                                       ifelse(CT3.monthly > quantile(CT3.monthly, 0.40, type = 3) & 
                                                CT3.monthly <= quantile(CT3.monthly, 0.60, type = 3), 3,
                                              ifelse(CT3.monthly > quantile(CT3.monthly, 0.60, type = 3) & 
                                                       CT3.monthly <= quantile(CT3.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_CT3) 
write.table(sort_CT3, file = "sort_CT3.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

####################
#TC1
sort_TC1 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(TC1.monthly)) %>%
  mutate(Portfolio_TC1 = ifelse(TC1.monthly <= quantile(TC1.monthly, 0.20, type = 3), 1,
                                ifelse(TC1.monthly > quantile(TC1.monthly, 0.20, type = 3) & 
                                         TC1.monthly <= quantile(TC1.monthly, 0.40, type = 3), 2,
                                       ifelse(TC1.monthly > quantile(TC1.monthly, 0.40, type = 3) & 
                                                TC1.monthly <= quantile(TC1.monthly, 0.60, type = 3), 3,
                                              ifelse(TC1.monthly > quantile(TC1.monthly, 0.60, type = 3) & 
                                                       TC1.monthly <= quantile(TC1.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_TC1) 
write.table(sort_TC1, file = "sort_TC1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

####################
#TC2
sort_TC2 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(TC2.monthly)) %>%
  mutate(Portfolio_TC2 = ifelse(TC2.monthly <= quantile(TC2.monthly, 0.20, type = 3), 1,
                                ifelse(TC2.monthly > quantile(TC2.monthly, 0.20, type = 3) & 
                                         TC2.monthly <= quantile(TC2.monthly, 0.40, type = 3), 2,
                                       ifelse(TC2.monthly > quantile(TC2.monthly, 0.40, type = 3) & 
                                                TC2.monthly <= quantile(TC2.monthly, 0.60, type = 3), 3,
                                              ifelse(TC2.monthly > quantile(TC2.monthly, 0.60, type = 3) & 
                                                       TC2.monthly <= quantile(TC2.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_TC2) 
write.table(sort_TC2, file = "sort_TC2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

#TC3
sort_TC3 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(TC3.monthly)) %>%
  mutate(Portfolio_TC3 = ifelse(TC3.monthly <= quantile(TC3.monthly, 0.20, type = 3), 1,
                                ifelse(TC3.monthly > quantile(TC3.monthly, 0.20, type = 3) & 
                                         TC3.monthly <= quantile(TC3.monthly, 0.40, type = 3), 2,
                                       ifelse(TC3.monthly > quantile(TC3.monthly, 0.40, type = 3) & 
                                                TC3.monthly <= quantile(TC3.monthly, 0.60, type = 3), 3,
                                              ifelse(TC3.monthly > quantile(TC3.monthly, 0.60, type = 3) & 
                                                       TC3.monthly <= quantile(TC3.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_TC3) 
write.table(sort_TC3, file = "sort_TC3.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

#UT1
sort_UT1 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(UT1.monthly)) %>%
  mutate(Portfolio_UT1 = ifelse(UT1.monthly <= quantile(UT1.monthly, 0.20, type = 3), 1,
                                ifelse(UT1.monthly > quantile(UT1.monthly, 0.20, type = 3) & 
                                         UT1.monthly <= quantile(UT1.monthly, 0.40, type = 3), 2,
                                       ifelse(UT1.monthly > quantile(UT1.monthly, 0.40, type = 3) & 
                                                UT1.monthly <= quantile(UT1.monthly, 0.60, type = 3), 3,
                                              ifelse(UT1.monthly > quantile(UT1.monthly, 0.60, type = 3) & 
                                                       UT1.monthly <= quantile(UT1.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_UT1) 
write.table(sort_UT1, file = "sort_UT1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

#UT2
sort_UT2 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(UT2.monthly)) %>%
  mutate(Portfolio_UT2 = ifelse(UT2.monthly <= quantile(UT2.monthly, 0.20, type = 3), 1,
                                ifelse(UT2.monthly > quantile(UT2.monthly, 0.20, type = 3) & 
                                         UT2.monthly <= quantile(UT2.monthly, 0.40, type = 3), 2,
                                       ifelse(UT2.monthly > quantile(UT2.monthly, 0.40, type = 3) & 
                                                UT2.monthly <= quantile(UT2.monthly, 0.60, type = 3), 3,
                                              ifelse(UT2.monthly > quantile(UT2.monthly, 0.60, type = 3) & 
                                                       UT2.monthly <= quantile(UT2.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_UT2) 
write.table(sort_UT2, file = "sort_UT2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

#UT3
sort_UT3 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(UT3.monthly)) %>%
  mutate(Portfolio_UT3 = ifelse(UT3.monthly <= quantile(UT3.monthly, 0.20, type = 3), 1,
                                ifelse(UT3.monthly > quantile(UT3.monthly, 0.20, type = 3) & 
                                         UT3.monthly <= quantile(UT3.monthly, 0.40, type = 3), 2,
                                       ifelse(UT3.monthly > quantile(UT3.monthly, 0.40, type = 3) & 
                                                UT3.monthly <= quantile(UT3.monthly, 0.60, type = 3), 3,
                                              ifelse(UT3.monthly > quantile(UT3.monthly, 0.60, type = 3) & 
                                                       UT3.monthly <= quantile(UT3.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_UT3) 
write.table(sort_UT3, file = "sort_UT3.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

#####TU1

sort_TU1 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(TU1.monthly)) %>%
  mutate(Portfolio_TU1 = ifelse(TU1.monthly <= quantile(TU1.monthly, 0.20, type = 3), 1,
                                ifelse(TU1.monthly > quantile(TU1.monthly, 0.20, type = 3) & 
                                         TU1.monthly <= quantile(TU1.monthly, 0.40, type = 3), 2,
                                       ifelse(TU1.monthly > quantile(TU1.monthly, 0.40, type = 3) & 
                                                TU1.monthly <= quantile(TU1.monthly, 0.60, type = 3), 3,
                                              ifelse(TU1.monthly > quantile(TU1.monthly, 0.60, type = 3) & 
                                                       TU1.monthly <= quantile(TU1.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_TU1) 
write.table(sort_TU1, file = "sort_TU1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

#####TU2

sort_TU2 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(TU2.monthly)) %>%
  mutate(Portfolio_TU2 = ifelse(TU2.monthly <= quantile(TU2.monthly, 0.20, type = 3), 1,
                                ifelse(TU2.monthly > quantile(TU2.monthly, 0.20, type = 3) & 
                                         TU2.monthly <= quantile(TU2.monthly, 0.40, type = 3), 2,
                                       ifelse(TU2.monthly > quantile(TU2.monthly, 0.40, type = 3) & 
                                                TU2.monthly <= quantile(TU2.monthly, 0.60, type = 3), 3,
                                              ifelse(TU2.monthly > quantile(TU2.monthly, 0.60, type = 3) & 
                                                       TU2.monthly <= quantile(TU2.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_TU2) 
write.table(sort_TU2, file = "sort_TU2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

#####TU3

sort_TU3 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(TU3.monthly)) %>%
  mutate(Portfolio_TU3 = ifelse(TU3.monthly <= quantile(TU3.monthly, 0.20, type = 3), 1,
                                ifelse(TU3.monthly > quantile(TU3.monthly, 0.20, type = 3) & 
                                         TU3.monthly <= quantile(TU3.monthly, 0.40, type = 3), 2,
                                       ifelse(TU3.monthly > quantile(TU3.monthly, 0.40, type = 3) & 
                                                TU3.monthly <= quantile(TU3.monthly, 0.60, type = 3), 3,
                                              ifelse(TU3.monthly > quantile(TU3.monthly, 0.60, type = 3) & 
                                                       TU3.monthly <= quantile(TU3.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_TU3) 
write.table(sort_TU3, file = "sort_TU3.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

#####CU1

sort_CU1 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(CU1.monthly)) %>%
  mutate(Portfolio_CU1 = ifelse(CU1.monthly <= quantile(CU1.monthly, 0.20, type = 3), 1,
                                ifelse(CU1.monthly > quantile(CU1.monthly, 0.20, type = 3) & 
                                         CU1.monthly <= quantile(CU1.monthly, 0.40, type = 3), 2,
                                       ifelse(CU1.monthly > quantile(CU1.monthly, 0.40, type = 3) & 
                                                CU1.monthly <= quantile(CU1.monthly, 0.60, type = 3), 3,
                                              ifelse(CU1.monthly > quantile(CU1.monthly, 0.60, type = 3) & 
                                                       CU1.monthly <= quantile(CU1.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_CU1) 
write.table(sort_CU1, file = "sort_CU1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
#####CU2

sort_CU2 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(CU2.monthly)) %>%
  mutate(Portfolio_CU2 = ifelse(CU2.monthly <= quantile(CU2.monthly, 0.20, type = 3), 1,
                                ifelse(CU2.monthly > quantile(CU2.monthly, 0.20, type = 3) & 
                                         CU2.monthly <= quantile(CU2.monthly, 0.40, type = 3), 2,
                                       ifelse(CU2.monthly > quantile(CU2.monthly, 0.40, type = 3) & 
                                                CU2.monthly <= quantile(CU2.monthly, 0.60, type = 3), 3,
                                              ifelse(CU2.monthly > quantile(CU2.monthly, 0.60, type = 3) & 
                                                       CU2.monthly <= quantile(CU2.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_CU2) 
write.table(sort_CU2, file = "sort_CU2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

#####CU3

sort_CU3 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(CU3.monthly)) %>%
  mutate(Portfolio_CU3 = ifelse(CU3.monthly <= quantile(CU3.monthly, 0.20, type = 3), 1,
                                ifelse(CU3.monthly > quantile(CU3.monthly, 0.20, type = 3) & 
                                         CU3.monthly <= quantile(CU3.monthly, 0.40, type = 3), 2,
                                       ifelse(CU3.monthly > quantile(CU3.monthly, 0.40, type = 3) & 
                                                CU3.monthly <= quantile(CU3.monthly, 0.60, type = 3), 3,
                                              ifelse(CU3.monthly > quantile(CU3.monthly, 0.60, type = 3) & 
                                                       CU3.monthly <= quantile(CU3.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_CU3) 
write.table(sort_CU3, file = "sort_CU3.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

#####UC1
sort_UC1 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(UC1.monthly)) %>%
  mutate(Portfolio_UC1 = ifelse(UC1.monthly <= quantile(UC1.monthly, 0.20, type = 3), 1,
                                ifelse(UC1.monthly > quantile(UC1.monthly, 0.20, type = 3) & 
                                         UC1.monthly <= quantile(UC1.monthly, 0.40, type = 3), 2,
                                       ifelse(UC1.monthly > quantile(UC1.monthly, 0.40, type = 3) & 
                                                UC1.monthly <= quantile(UC1.monthly, 0.60, type = 3), 3,
                                              ifelse(UC1.monthly > quantile(UC1.monthly, 0.60, type = 3) & 
                                                       UC1.monthly <= quantile(UC1.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_UC1) 
write.table(sort_UC1, file = "sort_UC1.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

#####UC2
sort_UC2 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(UC2.monthly)) %>%
  mutate(Portfolio_UC2 = ifelse(UC2.monthly <= quantile(UC2.monthly, 0.20, type = 3), 1,
                                ifelse(UC2.monthly > quantile(UC2.monthly, 0.20, type = 3) & 
                                         UC2.monthly <= quantile(UC2.monthly, 0.40, type = 3), 2,
                                       ifelse(UC2.monthly > quantile(UC2.monthly, 0.40, type = 3) & 
                                                UC2.monthly <= quantile(UC2.monthly, 0.60, type = 3), 3,
                                              ifelse(UC2.monthly > quantile(UC2.monthly, 0.60, type = 3) & 
                                                       UC2.monthly <= quantile(UC2.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_UC2) 
write.table(sort_UC2, file = "sort_UC2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

#####UC3
sort_UC3 <- master %>% 
  group_by(TradeDate) %>% 
  filter(!is.na(UC3.monthly)) %>%
  mutate(Portfolio_UC3 = ifelse(UC3.monthly <= quantile(UC3.monthly, 0.20, type = 3), 1,
                                ifelse(UC3.monthly > quantile(UC3.monthly, 0.20, type = 3) & 
                                         UC3.monthly <= quantile(UC3.monthly, 0.40, type = 3), 2,
                                       ifelse(UC3.monthly > quantile(UC3.monthly, 0.40, type = 3) & 
                                                UC3.monthly <= quantile(UC3.monthly, 0.60, type = 3), 3,
                                              ifelse(UC3.monthly > quantile(UC3.monthly, 0.60, type = 3) & 
                                                       UC3.monthly <= quantile(UC3.monthly, 0.80, type = 3), 4,
                                                     5))))) %>% 
  ungroup() %>% 
  select(SecurityId, TradeDate, Portfolio_UC3) 
write.table(sort_UC3, file = "sort_UC3.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

######################################
#### 3: Sorts into portfolios to be part of a conditional double-sort used for bivariate
#portfolio analysis
### Construct quintile portfolios sorted on an ascending ordering of max to be used in a
#conditional double sort on max then beta.monthly)
# Only observations with an estimate of both max and beta.monthly are included in the sort

### Merge portoflio sorts with master dataframe.
master <- merge(master, sort_bm, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_bd, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_max, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_ivol, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_RUI, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_TC1, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_TC2, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_TC3, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_CT1, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_CT2, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_CT3, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_UT1, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_UT2, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_UT3, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_TU1, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_TU2, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_TU3, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_CU1, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_CU2, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_CU3, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_UC1, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_UC2, by = c("TradeDate", "SecurityId"), all.x = TRUE)
master <- merge(master, sort_UC3, by = c("TradeDate", "SecurityId"), all.x = TRUE)




write.table(master, file = "master_ch8_5.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")


rm(sort_bd,sort_bm,sort_ivol,sort_max,sort_RUI)
rm(sort_CT1,sort_CT2,sort_CT3,sort_TC1,sort_TC2,sort_TC3)
rm(sort_UT1,sort_UT2,sort_UT3,sort_TU1,sort_TU2,sort_TU3)
rm(sort_CU1,sort_CU2,sort_CU3,sort_UC1,sort_UC2,sort_UC3)