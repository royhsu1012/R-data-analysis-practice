#install.packages("tidyquant")
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
library(tidyquant)
#################

setwd("D:/data/step9_data/v12w12")
portfolio_returns <- read.csv("portfolio_returns_3.csv", header = TRUE, sep = ",")
setwd("D:/data/cum_return")
#########################
# Asset tickers
p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_CT1_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_CT1_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_CT1_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_CT1_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_CT1_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CT1_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_CT1_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_CT1_vw) %>% 
  mutate(p2 = 0.01 * p2$er_CT1_vw)%>%
  mutate(p3 = 0.01 * p3$er_CT1_vw)%>%
  mutate(p4 = 0.01 * p4$er_CT1_vw)%>%
  mutate(p5 = 0.01 * p5$er_CT1_vw)%>%
  mutate(p6 = 0.01 * p6$er_CT1_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_CT1_VW = portfolio_CT1_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_CT1_VW$TradeDate = as.Date(portfolio_CT1_VW$TradeDate, "%Y-%m-%d")
class(portfolio_CT1_VW$TradeDate)

CT1_VW =  ggplot(portfolio_CT1_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                      breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                      values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'CT1_VW 5-year Beta') + theme_classic()
print(CT1_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_CT1_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_CT1_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_CT1_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_CT1_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_CT1_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CT1_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_CT1_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_CT1_ew) %>% 
  mutate(p2 = 0.01 * p2$er_CT1_ew)%>%
  mutate(p3 = 0.01 * p3$er_CT1_ew)%>%
  mutate(p4 = 0.01 * p4$er_CT1_ew)%>%
  mutate(p5 = 0.01 * p5$er_CT1_ew)%>%
  mutate(p6 = 0.01 * p6$er_CT1_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_CT1_EW = portfolio_CT1_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_CT1_EW$TradeDate = as.Date(portfolio_CT1_EW$TradeDate, "%Y-%m-%d")
class(portfolio_CT1_EW$TradeDate)

CT1_EW =  ggplot(portfolio_CT1_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'CT1_EW 5-year Beta') + theme_classic()
print(CT1_EW)
################################


p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_CT2_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_CT2_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_CT2_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_CT2_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_CT2_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CT2_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_CT2_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_CT2_vw) %>% 
  mutate(p2 = 0.01 * p2$er_CT2_vw)%>%
  mutate(p3 = 0.01 * p3$er_CT2_vw)%>%
  mutate(p4 = 0.01 * p4$er_CT2_vw)%>%
  mutate(p5 = 0.01 * p5$er_CT2_vw)%>%
  mutate(p6 = 0.01 * p6$er_CT2_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_CT2_VW = portfolio_CT2_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_CT2_VW$TradeDate = as.Date(portfolio_CT2_VW$TradeDate, "%Y-%m-%d")
class(portfolio_CT2_VW$TradeDate)

 
CT2_VW =  ggplot(portfolio_CT2_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'CT2_VW 5-year Beta') + theme_classic()
print(CT2_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_CT2_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_CT2_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_CT2_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_CT2_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_CT2_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CT2_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_CT2_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_CT2_ew) %>% 
  mutate(p2 = 0.01 * p2$er_CT2_ew)%>%
  mutate(p3 = 0.01 * p3$er_CT2_ew)%>%
  mutate(p4 = 0.01 * p4$er_CT2_ew)%>%
  mutate(p5 = 0.01 * p5$er_CT2_ew)%>%
  mutate(p6 = 0.01 * p6$er_CT2_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_CT2_EW = portfolio_CT2_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_CT2_EW$TradeDate = as.Date(portfolio_CT2_EW$TradeDate, "%Y-%m-%d")
class(portfolio_CT2_EW$TradeDate)

CT2_EW =  ggplot(portfolio_CT2_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'CT2_EW 5-year Beta') + theme_classic()
print(CT2_EW)

################################


p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_CT3_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_CT3_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_CT3_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_CT3_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_CT3_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CT3_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_CT3_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_CT3_vw) %>% 
  mutate(p2 = 0.01 * p2$er_CT3_vw)%>%
  mutate(p3 = 0.01 * p3$er_CT3_vw)%>%
  mutate(p4 = 0.01 * p4$er_CT3_vw)%>%
  mutate(p5 = 0.01 * p5$er_CT3_vw)%>%
  mutate(p6 = 0.01 * p6$er_CT3_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_CT3_VW = portfolio_CT3_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_CT3_VW$TradeDate = as.Date(portfolio_CT3_VW$TradeDate, "%Y-%m-%d")
class(portfolio_CT3_VW$TradeDate)


CT3_VW =  ggplot(portfolio_CT3_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'CT3_VW 5-year Beta') + theme_classic()
print(CT3_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_CT3_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_CT3_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_CT3_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_CT3_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_CT3_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CT3_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_CT3_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_CT3_ew) %>% 
  mutate(p2 = 0.01 * p2$er_CT3_ew)%>%
  mutate(p3 = 0.01 * p3$er_CT3_ew)%>%
  mutate(p4 = 0.01 * p4$er_CT3_ew)%>%
  mutate(p5 = 0.01 * p5$er_CT3_ew)%>%
  mutate(p6 = 0.01 * p6$er_CT3_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_CT3_EW = portfolio_CT3_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_CT3_EW$TradeDate = as.Date(portfolio_CT3_EW$TradeDate, "%Y-%m-%d")
class(portfolio_CT3_EW$TradeDate)

CT3_EW =  ggplot(portfolio_CT3_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'CT3_EW 5-year Beta') + theme_classic()
print(CT3_EW)

##################
p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_TC1_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_TC1_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_TC1_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_TC1_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_TC1_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TC1_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_TC1_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_TC1_vw) %>% 
  mutate(p2 = 0.01 * p2$er_TC1_vw)%>%
  mutate(p3 = 0.01 * p3$er_TC1_vw)%>%
  mutate(p4 = 0.01 * p4$er_TC1_vw)%>%
  mutate(p5 = 0.01 * p5$er_TC1_vw)%>%
  mutate(p6 = 0.01 * p6$er_TC1_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_TC1_VW = portfolio_TC1_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_TC1_VW$TradeDate = as.Date(portfolio_TC1_VW$TradeDate, "%Y-%m-%d")
class(portfolio_TC1_VW$TradeDate)


TC1_VW =  ggplot(portfolio_TC1_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'TC1_VW 5-year Beta') + theme_classic()
print(TC1_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_TC1_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_TC1_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_TC1_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_TC1_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_TC1_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TC1_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_TC1_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_TC1_ew) %>% 
  mutate(p2 = 0.01 * p2$er_TC1_ew)%>%
  mutate(p3 = 0.01 * p3$er_TC1_ew)%>%
  mutate(p4 = 0.01 * p4$er_TC1_ew)%>%
  mutate(p5 = 0.01 * p5$er_TC1_ew)%>%
  mutate(p6 = 0.01 * p6$er_TC1_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_TC1_EW = portfolio_TC1_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_TC1_EW$TradeDate = as.Date(portfolio_TC1_EW$TradeDate, "%Y-%m-%d")
class(portfolio_TC1_EW$TradeDate)

TC1_EW =  ggplot(portfolio_TC1_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'TC1_EW 5-year Beta') + theme_classic()
print(TC1_EW)


##################
p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_TC2_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_TC2_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_TC2_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_TC2_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_TC2_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TC2_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_TC2_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_TC2_vw) %>% 
  mutate(p2 = 0.01 * p2$er_TC2_vw)%>%
  mutate(p3 = 0.01 * p3$er_TC2_vw)%>%
  mutate(p4 = 0.01 * p4$er_TC2_vw)%>%
  mutate(p5 = 0.01 * p5$er_TC2_vw)%>%
  mutate(p6 = 0.01 * p6$er_TC2_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_TC2_VW = portfolio_TC2_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_TC2_VW$TradeDate = as.Date(portfolio_TC2_VW$TradeDate, "%Y-%m-%d")
class(portfolio_TC2_VW$TradeDate)


TC2_VW =  ggplot(portfolio_TC2_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'TC2_VW 5-year Beta') + theme_classic()
print(TC2_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_TC2_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_TC2_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_TC2_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_TC2_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_TC2_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TC2_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_TC2_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_TC2_ew) %>% 
  mutate(p2 = 0.01 * p2$er_TC2_ew)%>%
  mutate(p3 = 0.01 * p3$er_TC2_ew)%>%
  mutate(p4 = 0.01 * p4$er_TC2_ew)%>%
  mutate(p5 = 0.01 * p5$er_TC2_ew)%>%
  mutate(p6 = 0.01 * p6$er_TC2_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_TC2_EW = portfolio_TC2_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_TC2_EW$TradeDate = as.Date(portfolio_TC2_EW$TradeDate, "%Y-%m-%d")
class(portfolio_TC2_EW$TradeDate)

TC2_EW =  ggplot(portfolio_TC2_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'TC2_EW 5-year Beta') + theme_classic()
print(TC2_EW)

##################
p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_UT1_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_UT1_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_UT1_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_UT1_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_UT1_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UT1_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_UT1_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_UT1_vw) %>% 
  mutate(p2 = 0.01 * p2$er_UT1_vw)%>%
  mutate(p3 = 0.01 * p3$er_UT1_vw)%>%
  mutate(p4 = 0.01 * p4$er_UT1_vw)%>%
  mutate(p5 = 0.01 * p5$er_UT1_vw)%>%
  mutate(p6 = 0.01 * p6$er_UT1_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_UT1_VW = portfolio_UT1_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_UT1_VW$TradeDate = as.Date(portfolio_UT1_VW$TradeDate, "%Y-%m-%d")
class(portfolio_UT1_VW$TradeDate)


UT1_VW =  ggplot(portfolio_UT1_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'UT1_VW 5-year Beta') + theme_classic()
print(UT1_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_UT1_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_UT1_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_UT1_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_UT1_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_UT1_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UT1_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_UT1_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_UT1_ew) %>% 
  mutate(p2 = 0.01 * p2$er_UT1_ew)%>%
  mutate(p3 = 0.01 * p3$er_UT1_ew)%>%
  mutate(p4 = 0.01 * p4$er_UT1_ew)%>%
  mutate(p5 = 0.01 * p5$er_UT1_ew)%>%
  mutate(p6 = 0.01 * p6$er_UT1_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_UT1_EW = portfolio_UT1_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_UT1_EW$TradeDate = as.Date(portfolio_UT1_EW$TradeDate, "%Y-%m-%d")
class(portfolio_UT1_EW$TradeDate)

UT1_EW =  ggplot(portfolio_UT1_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'UT1_EW 5-year Beta') + theme_classic()
print(UT1_EW)

##################
p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_UT2_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_UT2_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_UT2_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_UT2_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_UT2_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UT2_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_UT2_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_UT2_vw) %>% 
  mutate(p2 = 0.01 * p2$er_UT2_vw)%>%
  mutate(p3 = 0.01 * p3$er_UT2_vw)%>%
  mutate(p4 = 0.01 * p4$er_UT2_vw)%>%
  mutate(p5 = 0.01 * p5$er_UT2_vw)%>%
  mutate(p6 = 0.01 * p6$er_UT2_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_UT2_VW = portfolio_UT2_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_UT2_VW$TradeDate = as.Date(portfolio_UT2_VW$TradeDate, "%Y-%m-%d")
class(portfolio_UT2_VW$TradeDate)


UT2_VW =  ggplot(portfolio_UT2_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'UT2_VW 5-year Beta') + theme_classic()
print(UT2_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_UT2_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_UT2_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_UT2_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_UT2_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_UT2_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UT2_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_UT2_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_UT2_ew) %>% 
  mutate(p2 = 0.01 * p2$er_UT2_ew)%>%
  mutate(p3 = 0.01 * p3$er_UT2_ew)%>%
  mutate(p4 = 0.01 * p4$er_UT2_ew)%>%
  mutate(p5 = 0.01 * p5$er_UT2_ew)%>%
  mutate(p6 = 0.01 * p6$er_UT2_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_UT2_EW = portfolio_UT2_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_UT2_EW$TradeDate = as.Date(portfolio_UT2_EW$TradeDate, "%Y-%m-%d")
class(portfolio_UT2_EW$TradeDate)

UT2_EW =  ggplot(portfolio_UT2_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'UT2_EW 5-year Beta') + theme_classic()
print(UT2_EW)

##################
p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_UT3_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_UT3_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_UT3_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_UT3_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_UT3_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UT3_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_UT3_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_UT3_vw) %>% 
  mutate(p2 = 0.01 * p2$er_UT3_vw)%>%
  mutate(p3 = 0.01 * p3$er_UT3_vw)%>%
  mutate(p4 = 0.01 * p4$er_UT3_vw)%>%
  mutate(p5 = 0.01 * p5$er_UT3_vw)%>%
  mutate(p6 = 0.01 * p6$er_UT3_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_UT3_VW = portfolio_UT3_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_UT3_VW$TradeDate = as.Date(portfolio_UT3_VW$TradeDate, "%Y-%m-%d")
class(portfolio_UT3_VW$TradeDate)


UT3_VW =  ggplot(portfolio_UT3_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'UT3_VW 5-year Beta') + theme_classic()
print(UT3_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_UT3_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_UT3_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_UT3_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_UT3_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_UT3_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UT3_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_UT3_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_UT3_ew) %>% 
  mutate(p2 = 0.01 * p2$er_UT3_ew)%>%
  mutate(p3 = 0.01 * p3$er_UT3_ew)%>%
  mutate(p4 = 0.01 * p4$er_UT3_ew)%>%
  mutate(p5 = 0.01 * p5$er_UT3_ew)%>%
  mutate(p6 = 0.01 * p6$er_UT3_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_UT3_EW = portfolio_UT3_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_UT3_EW$TradeDate = as.Date(portfolio_UT3_EW$TradeDate, "%Y-%m-%d")
class(portfolio_UT3_EW$TradeDate)

UT3_EW =  ggplot(portfolio_UT3_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'UT3_EW 5-year Beta') + theme_classic()
print(UT3_EW)

##################
p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_TU1_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_TU1_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_TU1_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_TU1_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_TU1_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TU1_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_TU1_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_TU1_vw) %>% 
  mutate(p2 = 0.01 * p2$er_TU1_vw)%>%
  mutate(p3 = 0.01 * p3$er_TU1_vw)%>%
  mutate(p4 = 0.01 * p4$er_TU1_vw)%>%
  mutate(p5 = 0.01 * p5$er_TU1_vw)%>%
  mutate(p6 = 0.01 * p6$er_TU1_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_TU1_VW = portfolio_TU1_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_TU1_VW$TradeDate = as.Date(portfolio_TU1_VW$TradeDate, "%Y-%m-%d")
class(portfolio_TU1_VW$TradeDate)


TU1_VW =  ggplot(portfolio_TU1_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'TU1_VW 5-year Beta') + theme_classic()
print(TU1_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_TU1_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_TU1_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_TU1_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_TU1_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_TU1_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TU1_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_TU1_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_TU1_ew) %>% 
  mutate(p2 = 0.01 * p2$er_TU1_ew)%>%
  mutate(p3 = 0.01 * p3$er_TU1_ew)%>%
  mutate(p4 = 0.01 * p4$er_TU1_ew)%>%
  mutate(p5 = 0.01 * p5$er_TU1_ew)%>%
  mutate(p6 = 0.01 * p6$er_TU1_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_TU1_EW = portfolio_TU1_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_TU1_EW$TradeDate = as.Date(portfolio_TU1_EW$TradeDate, "%Y-%m-%d")
class(portfolio_TU1_EW$TradeDate)

TU1_EW =  ggplot(portfolio_TU1_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'TU1_EW 5-year Beta') + theme_classic()
print(TU1_EW)

##################
p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_TU2_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_TU2_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_TU2_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_TU2_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_TU2_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TU2_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_TU2_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_TU2_vw) %>% 
  mutate(p2 = 0.01 * p2$er_TU2_vw)%>%
  mutate(p3 = 0.01 * p3$er_TU2_vw)%>%
  mutate(p4 = 0.01 * p4$er_TU2_vw)%>%
  mutate(p5 = 0.01 * p5$er_TU2_vw)%>%
  mutate(p6 = 0.01 * p6$er_TU2_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_TU2_VW = portfolio_TU2_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_TU2_VW$TradeDate = as.Date(portfolio_TU2_VW$TradeDate, "%Y-%m-%d")
class(portfolio_TU2_VW$TradeDate)


TU2_VW =  ggplot(portfolio_TU2_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'TU2_VW 5-year Beta') + theme_classic()
print(TU2_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_TU2_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_TU2_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_TU2_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_TU2_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_TU2_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TU2_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_TU2_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_TU2_ew) %>% 
  mutate(p2 = 0.01 * p2$er_TU2_ew)%>%
  mutate(p3 = 0.01 * p3$er_TU2_ew)%>%
  mutate(p4 = 0.01 * p4$er_TU2_ew)%>%
  mutate(p5 = 0.01 * p5$er_TU2_ew)%>%
  mutate(p6 = 0.01 * p6$er_TU2_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_TU2_EW = portfolio_TU2_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_TU2_EW$TradeDate = as.Date(portfolio_TU2_EW$TradeDate, "%Y-%m-%d")
class(portfolio_TU2_EW$TradeDate)

TU2_EW =  ggplot(portfolio_TU2_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'TU2_EW 5-year Beta') + theme_classic()
print(TU2_EW)

##################
p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_TU3_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_TU3_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_TU3_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_TU3_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_TU3_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TU3_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_TU3_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_TU3_vw) %>% 
  mutate(p2 = 0.01 * p2$er_TU3_vw)%>%
  mutate(p3 = 0.01 * p3$er_TU3_vw)%>%
  mutate(p4 = 0.01 * p4$er_TU3_vw)%>%
  mutate(p5 = 0.01 * p5$er_TU3_vw)%>%
  mutate(p6 = 0.01 * p6$er_TU3_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_TU3_VW = portfolio_TU3_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_TU3_VW$TradeDate = as.Date(portfolio_TU3_VW$TradeDate, "%Y-%m-%d")
class(portfolio_TU3_VW$TradeDate)


TU3_VW =  ggplot(portfolio_TU3_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'TU3_VW 5-year Beta') + theme_classic()
print(TU3_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_TU3_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_TU3_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_TU3_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_TU3_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_TU3_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TU3_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_TU3_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_TU3_ew) %>% 
  mutate(p2 = 0.01 * p2$er_TU3_ew)%>%
  mutate(p3 = 0.01 * p3$er_TU3_ew)%>%
  mutate(p4 = 0.01 * p4$er_TU3_ew)%>%
  mutate(p5 = 0.01 * p5$er_TU3_ew)%>%
  mutate(p6 = 0.01 * p6$er_TU3_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_TU3_EW = portfolio_TU3_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_TU3_EW$TradeDate = as.Date(portfolio_TU3_EW$TradeDate, "%Y-%m-%d")
class(portfolio_TU3_EW$TradeDate)

TU3_EW =  ggplot(portfolio_TU3_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'TU3_EW 5-year Beta') + theme_classic()
print(TU3_EW)

##################
p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_CU1_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_CU1_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_CU1_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_CU1_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_CU1_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CU1_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_CU1_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_CU1_vw) %>% 
  mutate(p2 = 0.01 * p2$er_CU1_vw)%>%
  mutate(p3 = 0.01 * p3$er_CU1_vw)%>%
  mutate(p4 = 0.01 * p4$er_CU1_vw)%>%
  mutate(p5 = 0.01 * p5$er_CU1_vw)%>%
  mutate(p6 = 0.01 * p6$er_CU1_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_CU1_VW = portfolio_CU1_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_CU1_VW$TradeDate = as.Date(portfolio_CU1_VW$TradeDate, "%Y-%m-%d")
class(portfolio_CU1_VW$TradeDate)


CU1_VW =  ggplot(portfolio_CU1_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'CU1_VW 5-year Beta') + theme_classic()
print(CU1_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_CU1_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_CU1_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_CU1_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_CU1_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_CU1_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CU1_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_CU1_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_CU1_ew) %>% 
  mutate(p2 = 0.01 * p2$er_CU1_ew)%>%
  mutate(p3 = 0.01 * p3$er_CU1_ew)%>%
  mutate(p4 = 0.01 * p4$er_CU1_ew)%>%
  mutate(p5 = 0.01 * p5$er_CU1_ew)%>%
  mutate(p6 = 0.01 * p6$er_CU1_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_CU1_EW = portfolio_CU1_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_CU1_EW$TradeDate = as.Date(portfolio_CU1_EW$TradeDate, "%Y-%m-%d")
class(portfolio_CU1_EW$TradeDate)

CU1_EW =  ggplot(portfolio_CU1_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'CU1_EW 5-year Beta') + theme_classic()
print(CU1_EW)

##################
p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_CU2_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_CU2_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_CU2_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_CU2_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_CU2_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CU2_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_CU2_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_CU2_vw) %>% 
  mutate(p2 = 0.01 * p2$er_CU2_vw)%>%
  mutate(p3 = 0.01 * p3$er_CU2_vw)%>%
  mutate(p4 = 0.01 * p4$er_CU2_vw)%>%
  mutate(p5 = 0.01 * p5$er_CU2_vw)%>%
  mutate(p6 = 0.01 * p6$er_CU2_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_CU2_VW = portfolio_CU2_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_CU2_VW$TradeDate = as.Date(portfolio_CU2_VW$TradeDate, "%Y-%m-%d")
class(portfolio_CU2_VW$TradeDate)


CU2_VW =  ggplot(portfolio_CU2_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'CU2_VW 5-year Beta') + theme_classic()
print(CU2_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_CU2_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_CU2_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_CU2_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_CU2_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_CU2_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CU2_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_CU2_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_CU2_ew) %>% 
  mutate(p2 = 0.01 * p2$er_CU2_ew)%>%
  mutate(p3 = 0.01 * p3$er_CU2_ew)%>%
  mutate(p4 = 0.01 * p4$er_CU2_ew)%>%
  mutate(p5 = 0.01 * p5$er_CU2_ew)%>%
  mutate(p6 = 0.01 * p6$er_CU2_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_CU2_EW = portfolio_CU2_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_CU2_EW$TradeDate = as.Date(portfolio_CU2_EW$TradeDate, "%Y-%m-%d")
class(portfolio_CU2_EW$TradeDate)

CU2_EW =  ggplot(portfolio_CU2_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'CU2_EW 5-year Beta') + theme_classic()
print(CU2_EW)

##################
p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_CU3_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_CU3_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_CU3_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_CU3_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_CU3_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CU3_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_CU3_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_CU3_vw) %>% 
  mutate(p2 = 0.01 * p2$er_CU3_vw)%>%
  mutate(p3 = 0.01 * p3$er_CU3_vw)%>%
  mutate(p4 = 0.01 * p4$er_CU3_vw)%>%
  mutate(p5 = 0.01 * p5$er_CU3_vw)%>%
  mutate(p6 = 0.01 * p6$er_CU3_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_CU3_VW = portfolio_CU3_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_CU3_VW$TradeDate = as.Date(portfolio_CU3_VW$TradeDate, "%Y-%m-%d")
class(portfolio_CU3_VW$TradeDate)


CU3_VW =  ggplot(portfolio_CU3_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'CU3_VW 5-year Beta') + theme_classic()
print(CU3_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_CU3_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_CU3_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_CU3_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_CU3_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_CU3_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CU3_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_CU3_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_CU3_ew) %>% 
  mutate(p2 = 0.01 * p2$er_CU3_ew)%>%
  mutate(p3 = 0.01 * p3$er_CU3_ew)%>%
  mutate(p4 = 0.01 * p4$er_CU3_ew)%>%
  mutate(p5 = 0.01 * p5$er_CU3_ew)%>%
  mutate(p6 = 0.01 * p6$er_CU3_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_CU3_EW = portfolio_CU3_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_CU3_EW$TradeDate = as.Date(portfolio_CU3_EW$TradeDate, "%Y-%m-%d")
class(portfolio_CU3_EW$TradeDate)

CU3_EW =  ggplot(portfolio_CU3_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'CU3_EW 5-year Beta') + theme_classic()
print(CU2_EW)

##################
##################
p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_CU3_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_CU3_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_CU3_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_CU3_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_CU3_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CU3_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_CU3_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_CU3_vw) %>% 
  mutate(p2 = 0.01 * p2$er_CU3_vw)%>%
  mutate(p3 = 0.01 * p3$er_CU3_vw)%>%
  mutate(p4 = 0.01 * p4$er_CU3_vw)%>%
  mutate(p5 = 0.01 * p5$er_CU3_vw)%>%
  mutate(p6 = 0.01 * p6$er_CU3_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_CU3_VW = portfolio_CU3_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_CU3_VW$TradeDate = as.Date(portfolio_CU3_VW$TradeDate, "%Y-%m-%d")
class(portfolio_CU3_VW$TradeDate)


CU3_VW =  ggplot(portfolio_CU3_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'CU3_VW 5-year Beta') + theme_classic()
print(CU3_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_CU3_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_CU3_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_CU3_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_CU3_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_CU3_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CU3_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_CU3_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_CU3_ew) %>% 
  mutate(p2 = 0.01 * p2$er_CU3_ew)%>%
  mutate(p3 = 0.01 * p3$er_CU3_ew)%>%
  mutate(p4 = 0.01 * p4$er_CU3_ew)%>%
  mutate(p5 = 0.01 * p5$er_CU3_ew)%>%
  mutate(p6 = 0.01 * p6$er_CU3_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_CU3_EW = portfolio_CU3_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_CU3_EW$TradeDate = as.Date(portfolio_CU3_EW$TradeDate, "%Y-%m-%d")
class(portfolio_CU3_EW$TradeDate)

CU3_EW =  ggplot(portfolio_CU3_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'CU3_EW 5-year Beta') + theme_classic()
print(CU3_EW)

##################
##################
p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_UC1_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_UC1_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_UC1_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_UC1_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_UC1_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UC1_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_UC1_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_UC1_vw) %>% 
  mutate(p2 = 0.01 * p2$er_UC1_vw)%>%
  mutate(p3 = 0.01 * p3$er_UC1_vw)%>%
  mutate(p4 = 0.01 * p4$er_UC1_vw)%>%
  mutate(p5 = 0.01 * p5$er_UC1_vw)%>%
  mutate(p6 = 0.01 * p6$er_UC1_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_UC1_VW = portfolio_UC1_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_UC1_VW$TradeDate = as.Date(portfolio_UC1_VW$TradeDate, "%Y-%m-%d")
class(portfolio_UC1_VW$TradeDate)


UC1_VW =  ggplot(portfolio_UC1_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'UC1_VW 5-year Beta') + theme_classic()
print(UC1_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_UC1_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_UC1_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_UC1_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_UC1_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_UC1_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UC1_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_UC1_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_UC1_ew) %>% 
  mutate(p2 = 0.01 * p2$er_UC1_ew)%>%
  mutate(p3 = 0.01 * p3$er_UC1_ew)%>%
  mutate(p4 = 0.01 * p4$er_UC1_ew)%>%
  mutate(p5 = 0.01 * p5$er_UC1_ew)%>%
  mutate(p6 = 0.01 * p6$er_UC1_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_UC1_EW = portfolio_UC1_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_UC1_EW$TradeDate = as.Date(portfolio_UC1_EW$TradeDate, "%Y-%m-%d")
class(portfolio_UC1_EW$TradeDate)

UC1_EW =  ggplot(portfolio_UC1_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'UC1_EW 5-year Beta') + theme_classic()
print(UC1_EW)

##################
##################
p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_UC2_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_UC2_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_UC2_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_UC2_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_UC2_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UC2_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_UC2_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_UC2_vw) %>% 
  mutate(p2 = 0.01 * p2$er_UC2_vw)%>%
  mutate(p3 = 0.01 * p3$er_UC2_vw)%>%
  mutate(p4 = 0.01 * p4$er_UC2_vw)%>%
  mutate(p5 = 0.01 * p5$er_UC2_vw)%>%
  mutate(p6 = 0.01 * p6$er_UC2_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_UC2_VW = portfolio_UC2_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_UC2_VW$TradeDate = as.Date(portfolio_UC2_VW$TradeDate, "%Y-%m-%d")
class(portfolio_UC2_VW$TradeDate)


UC2_VW =  ggplot(portfolio_UC2_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'UC2_VW 5-year Beta') + theme_classic()
print(UC2_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_UC2_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_UC2_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_UC2_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_UC2_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_UC2_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UC2_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_UC2_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_UC2_ew) %>% 
  mutate(p2 = 0.01 * p2$er_UC2_ew)%>%
  mutate(p3 = 0.01 * p3$er_UC2_ew)%>%
  mutate(p4 = 0.01 * p4$er_UC2_ew)%>%
  mutate(p5 = 0.01 * p5$er_UC2_ew)%>%
  mutate(p6 = 0.01 * p6$er_UC2_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_UC2_EW = portfolio_UC2_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_UC2_EW$TradeDate = as.Date(portfolio_UC2_EW$TradeDate, "%Y-%m-%d")
class(portfolio_UC2_EW$TradeDate)

UC2_EW =  ggplot(portfolio_UC2_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'UC2_EW 5-year Beta') + theme_classic()
print(UC2_EW)

##################
##################
p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_UC3_vw)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_UC3_vw)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_UC3_vw)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_UC3_vw)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_UC3_vw)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UC3_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_UC3_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_UC3_vw) %>% 
  mutate(p2 = 0.01 * p2$er_UC3_vw)%>%
  mutate(p3 = 0.01 * p3$er_UC3_vw)%>%
  mutate(p4 = 0.01 * p4$er_UC3_vw)%>%
  mutate(p5 = 0.01 * p5$er_UC3_vw)%>%
  mutate(p6 = 0.01 * p6$er_UC3_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_UC3_VW = portfolio_UC3_VW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_UC3_VW$TradeDate = as.Date(portfolio_UC3_VW$TradeDate, "%Y-%m-%d")
class(portfolio_UC3_VW$TradeDate)


UC3_VW =  ggplot(portfolio_UC3_VW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'UC3_VW 5-year Beta') + theme_classic()
print(UC3_VW)

p1 = portfolio_returns %>% filter(portfolio == 1) %>% 
  select(TradeDate,er_UC3_ew)
p2 = portfolio_returns %>% filter(portfolio == 2) %>% 
  select(TradeDate,er_UC3_ew)
p3 = portfolio_returns %>% filter(portfolio == 3) %>% 
  select(TradeDate,er_UC3_ew)
p4 = portfolio_returns %>% filter(portfolio == 4) %>% 
  select(TradeDate,er_UC3_ew)
p5 = portfolio_returns %>% filter(portfolio == 5) %>% 
  select(TradeDate,er_UC3_ew)
p6 = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UC3_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_UC3_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(p1 = 0.01 * p1$er_UC3_ew) %>% 
  mutate(p2 = 0.01 * p2$er_UC3_ew)%>%
  mutate(p3 = 0.01 * p3$er_UC3_ew)%>%
  mutate(p4 = 0.01 * p4$er_UC3_ew)%>%
  mutate(p5 = 0.01 * p5$er_UC3_ew)%>%
  mutate(p6 = 0.01 * p6$er_UC3_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_UC3_EW = portfolio_UC3_EW %>% 
  mutate(p1_cum = cumprod(1 +p1)) %>% 
  mutate(p2_cum = cumprod(1 +p2))%>%
  mutate(p3_cum = cumprod(1 +p3))%>%
  mutate(p4_cum = cumprod(1 +p4))%>%
  mutate(p5_cum = cumprod(1 +p5))%>%
  mutate(p6_cum = cumprod(1 +p6))%>%
  mutate(MV_cum = cumprod(1 +MV))

portfolio_UC3_EW$TradeDate = as.Date(portfolio_UC3_EW$TradeDate, "%Y-%m-%d")
class(portfolio_UC3_EW$TradeDate)

UC3_EW =  ggplot(portfolio_UC3_EW, aes(x = TradeDate)) +
  geom_line(aes(y = p1_cum, colour = "p1"))+
  geom_line(aes(y = p2_cum, colour = "p2"))+
  geom_line(aes(y = p3_cum, colour = "p3"))+
  geom_line(aes(y = p4_cum, colour = "p4"))+
  geom_line(aes(y = p5_cum, colour = "p5"))+
  geom_line(aes(y = p6_cum, colour = "p6"))+
  geom_line(aes(y = MV_cum, colour = "MV"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("p1", "p2","p3","p4","p5","p6","MV"),
                       values = c("red4", "steelblue2", "blueviolet","darkorange2","plum1","black","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'UC3_EW 5-year Beta') + theme_classic()
print(UC3_EW)
rm(p1,p2,p3,p4,p5,p6)
##################
CT1_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CT1_ew)
CT2_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CT2_ew)
CT3_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CT3_ew)
TC1_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TC1_ew)
TC2_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TC2_ew)
TC3_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TC3_ew)
UT1_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UT1_ew)
UT2_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UT2_ew)
UT3_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UT3_ew)
TU1_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TU1_ew)
TU2_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TU2_ew)
TU3_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TU3_ew)
CU1_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CU1_ew)
CU2_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CU2_ew)
CU3_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CU3_ew)
UC1_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UC1_ew)
UC2_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UC2_ew)
UC3_ew = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UC3_ew)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.EW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_ALL_EW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(CT1_ew = 0.01 * CT1_ew$er_CT1_ew)%>% 
  mutate(CT2_ew = 0.01 * CT2_ew$er_CT2_ew)%>%
  mutate(CT3_ew = 0.01 * CT3_ew$er_CT3_ew)%>%
  mutate(TC1_ew = 0.01 * TC1_ew$er_TC1_ew)%>% 
  mutate(TC2_ew = 0.01 * TC2_ew$er_TC2_ew)%>%
  mutate(TC3_ew = 0.01 * TC3_ew$er_TC3_ew)%>%
  mutate(UT1_ew = 0.01 * UT1_ew$er_UT1_ew) %>% 
  mutate(UT2_ew = 0.01 * UT2_ew$er_UT2_ew)%>%
  mutate(UT3_ew = 0.01 * UT3_ew$er_UT3_ew)%>%
  mutate(TU1_ew = 0.01 * TU1_ew$er_TU1_ew) %>% 
  mutate(TU2_ew = 0.01 * TU2_ew$er_TU2_ew)%>%
  mutate(TU3_ew = 0.01 * TU3_ew$er_TU3_ew)%>%
  mutate(CU1_ew = 0.01 * CU1_ew$er_CU1_ew) %>% 
  mutate(CU2_ew = 0.01 * CU2_ew$er_CU2_ew)%>%
  mutate(CU3_ew = 0.01 * CU3_ew$er_CU3_ew)%>%
  mutate(UC1_ew = 0.01 * UC1_ew$er_UC1_ew) %>% 
  mutate(UC2_ew = 0.01 * UC2_ew$er_UC2_ew)%>%
  mutate(UC3_ew = 0.01 * UC3_ew$er_UC3_ew)%>%
  mutate(MV = 0.01 * MV$M.EW)
portfolio_ALL_EW = portfolio_ALL_EW %>% 
  mutate(CT1_ew_cum = cumprod(1 +CT1_ew))%>% 
  mutate(CT2_ew_cum = cumprod(1 +CT2_ew))%>%
  mutate(CT3_ew_cum = cumprod(1 +CT3_ew))%>%
  mutate(TC1_ew_cum = cumprod(1 +TC1_ew))%>% 
  mutate(TC2_ew_cum = cumprod(1 +TC2_ew))%>%
  mutate(TC3_ew_cum = cumprod(1 +TC3_ew))%>%
  mutate(UT1_ew_cum = cumprod(1 +UT1_ew))%>% 
  mutate(UT2_ew_cum = cumprod(1 +UT2_ew))%>%
  mutate(UT3_ew_cum = cumprod(1 +UT3_ew))%>%
  mutate(TU1_ew_cum = cumprod(1 +TU1_ew))%>% 
  mutate(TU2_ew_cum = cumprod(1 +TU2_ew))%>%
  mutate(TU3_ew_cum = cumprod(1 +TU3_ew))%>%
  mutate(CU1_ew_cum = cumprod(1 +CU1_ew))%>% 
  mutate(CU2_ew_cum = cumprod(1 +CU2_ew))%>%
  mutate(CU3_ew_cum = cumprod(1 +CU3_ew))%>%
  mutate(UC1_ew_cum = cumprod(1 +UC1_ew)) %>% 
  mutate(UC2_ew_cum = cumprod(1 +UC2_ew))%>%
  mutate(UC3_ew_cum = cumprod(1 +UC3_ew))%>%
  mutate(MV_cum = cumprod(1 +MV))


portfolio_ALL_EW$TradeDate = as.Date(portfolio_ALL_EW$TradeDate, "%Y-%m-%d")
class(portfolio_ALL_EW$TradeDate)


ALL_EW =  ggplot(portfolio_ALL_EW, aes(x = TradeDate)) +
  geom_line(aes(y = CT1_ew_cum, colour = "CT1_ew_cum"))+
  geom_line(aes(y = CT2_ew_cum, colour = "CT2_ew_cum"))+
  geom_line(aes(y = CT3_ew_cum, colour = "CT3_ew_cum"))+
  geom_line(aes(y = TC1_ew_cum, colour = "TC1_ew_cum"))+
  geom_line(aes(y = TC2_ew_cum, colour = "TC2_ew_cum"))+
  geom_line(aes(y = TC3_ew_cum, colour = "TC3_ew_cum"))+
  geom_line(aes(y = UT1_ew_cum, colour = "UT1_ew_cum"))+
  geom_line(aes(y = UT2_ew_cum, colour = "UT2_ew_cum"))+
  geom_line(aes(y = UT3_ew_cum, colour = "UT3_ew_cum"))+
  geom_line(aes(y = TU1_ew_cum, colour = "TU1_ew_cum"))+
  geom_line(aes(y = TU2_ew_cum, colour = "TU2_ew_cum"))+
  geom_line(aes(y = TU3_ew_cum, colour = "TU3_ew_cum"))+
  geom_line(aes(y = CU1_ew_cum, colour = "CU1_ew_cum"))+
  geom_line(aes(y = CU2_ew_cum, colour = "CU2_ew_cum"))+
  geom_line(aes(y = CU3_ew_cum, colour = "CU3_ew_cum"))+
  geom_line(aes(y = UC1_ew_cum, colour = "UC1_ew_cum"))+
  geom_line(aes(y = UC2_ew_cum, colour = "UC2_ew_cum"))+
  geom_line(aes(y = UC3_ew_cum, colour = "UC3_ew_cum"))+
  geom_line(aes(y = MV_cum, colour = "MV_cum"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("CT1_ew_cum","CT2_ew_cum","CT3_ew_cum",
                                  "TC1_ew_cum","TC2_ew_cum","TC3_ew_cum",
                                  "UT1_ew_cum","UT2_ew_cum","UT3_ew_cum",
                                  "TU1_ew_cum","TU2_ew_cum","TU3_ew_cum",
                                  "CU1_ew_cum","CU2_ew_cum","CU3_ew_cum",
                                  "UC1_ew_cum","UC2_ew_cum","UC3_ew_cum",
                                  "MV_cum"),
                       values = c("1", "2", "3","4","5","6","7","8","9",
                                  "11","12","13","14","15","16","17","18","19","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'ALL_EW 1-year Beta') + theme_classic()
print(ALL_EW)

##################
CT1_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CT1_vw)
CT2_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CT2_vw)
CT3_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CT3_vw)
TC1_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TC1_vw)
TC2_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TC2_vw)
TC3_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TC3_vw)
UT1_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UT1_vw)
UT2_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UT2_vw)
UT3_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UT3_vw)
TU1_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TU1_vw)
TU2_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TU2_vw)
TU3_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_TU3_vw)
CU1_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CU1_vw)
CU2_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CU2_vw)
CU3_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_CU3_vw)
UC1_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UC1_vw)
UC2_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UC2_vw)
UC3_vw = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate,er_UC3_vw)
MV = portfolio_returns %>% filter(portfolio == 6) %>% 
  select(TradeDate, M.VW)
TradeDate = portfolio_returns %>% 
  select(TradeDate) %>% distinct()
portfolio_ALL_VW = portfolio_returns %>% 
  select(TradeDate) %>% distinct()  %>% 
  mutate(CT1_vw = 0.01 * CT1_vw$er_CT1_vw)%>% 
  mutate(CT2_vw = 0.01 * CT2_vw$er_CT2_vw)%>%
  mutate(CT3_vw = 0.01 * CT3_vw$er_CT3_vw)%>%
  mutate(TC1_vw = 0.01 * TC1_vw$er_TC1_vw)%>% 
  mutate(TC2_vw = 0.01 * TC2_vw$er_TC2_vw)%>%
  mutate(TC3_vw = 0.01 * TC3_vw$er_TC3_vw)%>%
  mutate(UT1_vw = 0.01 * UT1_vw$er_UT1_vw) %>% 
  mutate(UT2_vw = 0.01 * UT2_vw$er_UT2_vw)%>%
  mutate(UT3_vw = 0.01 * UT3_vw$er_UT3_vw)%>%
  mutate(TU1_vw = 0.01 * TU1_vw$er_TU1_vw) %>% 
  mutate(TU2_vw = 0.01 * TU2_vw$er_TU2_vw)%>%
  mutate(TU3_vw = 0.01 * TU3_vw$er_TU3_vw)%>%
  mutate(CU1_vw = 0.01 * CU1_vw$er_CU1_vw) %>% 
  mutate(CU2_vw = 0.01 * CU2_vw$er_CU2_vw)%>%
  mutate(CU3_vw = 0.01 * CU3_vw$er_CU3_vw)%>%
  mutate(UC1_vw = 0.01 * UC1_vw$er_UC1_vw) %>% 
  mutate(UC2_vw = 0.01 * UC2_vw$er_UC2_vw)%>%
  mutate(UC3_vw = 0.01 * UC3_vw$er_UC3_vw)%>%
  mutate(MV = 0.01 * MV$M.VW)
portfolio_ALL_VW = portfolio_ALL_VW %>% 
  mutate(CT1_vw_cum = cumprod(1 +CT1_vw))%>% 
  mutate(CT2_vw_cum = cumprod(1 +CT2_vw))%>%
  mutate(CT3_vw_cum = cumprod(1 +CT3_vw))%>%
  mutate(TC1_vw_cum = cumprod(1 +TC1_vw))%>% 
  mutate(TC2_vw_cum = cumprod(1 +TC2_vw))%>%
  mutate(TC3_vw_cum = cumprod(1 +TC3_vw))%>%
  mutate(UT1_vw_cum = cumprod(1 +UT1_vw))%>% 
  mutate(UT2_vw_cum = cumprod(1 +UT2_vw))%>%
  mutate(UT3_vw_cum = cumprod(1 +UT3_vw))%>%
  mutate(TU1_vw_cum = cumprod(1 +TU1_vw))%>% 
  mutate(TU2_vw_cum = cumprod(1 +TU2_vw))%>%
  mutate(TU3_vw_cum = cumprod(1 +TU3_vw))%>%
  mutate(CU1_vw_cum = cumprod(1 +CU1_vw))%>% 
  mutate(CU2_vw_cum = cumprod(1 +CU2_vw))%>%
  mutate(CU3_vw_cum = cumprod(1 +CU3_vw))%>%
  mutate(UC1_vw_cum = cumprod(1 +UC1_vw)) %>% 
  mutate(UC2_vw_cum = cumprod(1 +UC2_vw))%>%
  mutate(UC3_vw_cum = cumprod(1 +UC3_vw))%>%
  mutate(MV_cum = cumprod(1 +MV))


portfolio_ALL_VW$TradeDate = as.Date(portfolio_ALL_VW$TradeDate, "%Y-%m-%d")
class(portfolio_ALL_VW$TradeDate)


ALL_VW =  ggplot(portfolio_ALL_VW, aes(x = TradeDate)) +
  geom_line(aes(y = CT1_vw_cum, colour = "CT1_vw_cum"))+
  geom_line(aes(y = CT2_vw_cum, colour = "CT2_vw_cum"))+
  geom_line(aes(y = CT3_vw_cum, colour = "CT3_vw_cum"))+
  geom_line(aes(y = TC1_vw_cum, colour = "TC1_vw_cum"))+
  geom_line(aes(y = TC2_vw_cum, colour = "TC2_vw_cum"))+
  geom_line(aes(y = TC3_vw_cum, colour = "TC3_vw_cum"))+
  geom_line(aes(y = UT1_vw_cum, colour = "UT1_vw_cum"))+
  geom_line(aes(y = UT2_vw_cum, colour = "UT2_vw_cum"))+
  geom_line(aes(y = UT3_vw_cum, colour = "UT3_vw_cum"))+
  geom_line(aes(y = TU1_vw_cum, colour = "TU1_vw_cum"))+
  geom_line(aes(y = TU2_vw_cum, colour = "TU2_vw_cum"))+
  geom_line(aes(y = TU3_vw_cum, colour = "TU3_vw_cum"))+
  geom_line(aes(y = CU1_vw_cum, colour = "CU1_vw_cum"))+
  geom_line(aes(y = CU2_vw_cum, colour = "CU2_vw_cum"))+
  geom_line(aes(y = CU3_vw_cum, colour = "CU3_vw_cum"))+
  geom_line(aes(y = UC1_vw_cum, colour = "UC1_vw_cum"))+
  geom_line(aes(y = UC2_vw_cum, colour = "UC2_vw_cum"))+
  geom_line(aes(y = UC3_vw_cum, colour = "UC3_vw_cum"))+
  geom_line(aes(y = MV_cum, colour = "MV_cum"))+
  scale_colour_manual( "portfolio", 
                       breaks = c("CT1_vw_cum","CT2_vw_cum","CT3_vw_cum",
                                  "TC1_vw_cum","TC2_vw_cum","TC3_vw_cum",
                                  "UT1_vw_cum","UT2_vw_cum","UT3_vw_cum",
                                  "TU1_vw_cum","TU2_vw_cum","TU3_vw_cum",
                                  "CU1_vw_cum","CU2_vw_cum","CU3_vw_cum",
                                  "UC1_vw_cum","UC2_vw_cum","UC3_vw_cum",
                                  "MV_cum"),
                       values = c("1", "2", "3","4","5","6","7","8","red",
                                  "11","12","13","14","15","16","17","18","19","gray"))+
  labs(x = 'Date',
       y = 'Cumulative Returns',
       title = 'ALL_VW 1-year Beta') + theme_classic()
print(ALL_VW)

