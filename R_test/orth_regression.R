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
library(far)
#################

setwd("D:/data/step8_data/v60w36")
#"master_ch8_1.csv"
master <- read.csv("master_ch8_5.csv", header = TRUE, sep = ",")
colnames(master)
setwd("D:/data/step9_data/v60w36")
portfolio_returns <- read.csv("portfolio_returns_3.csv", header = TRUE, sep = ",")
colnames(portfolio_returns)
setwd("D:/data/step12_data/v60w36")

HML = master %>% select(TradeDate,HML) %>% distinct()
SMB = master %>% select(TradeDate,SMB2) %>% distinct()
CMA = master %>% select(TradeDate,CMA) %>% distinct()
RMW = master %>% select(TradeDate,RMW) %>% distinct()
UMD = master %>% select(TradeDate,UMD) %>% distinct()
MKT = master %>% select(TradeDate,M.VW) %>% distinct()

HML_a = HML$HML
SMB_a = SMB$SMB2
CMA_a = CMA$CMA
RMW_a = RMW$RMW
UMD_a = UMD$UMD
MKT_a = MKT$M.VW

lm_a <- lm(MKT_a ~ 1 + SMB_a + HML_a + CMA_a + RMW_a + UMD_a )
summary(lm_a)





mat_a <- matrix(c(HML$HML,SMB$SMB2,CMA$CMA,RMW$RMW,UMD$UMD),nrow=239,ncol=5)
orth_a <- orthonormalization(mat_a, basis=FALSE, norm=FALSE)


