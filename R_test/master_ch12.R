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
colnames(master)
setwd("D:/data/step9_data/v48w36")
portfolio_returns <- read.csv("portfolio_returns_3.csv", header = TRUE, sep = ",")
colnames(portfolio_returns)
setwd("D:/data/step12_data/v48w36")

########## Section 16: Factor model regressions ##########
# Note, we have not included the regression models reporting the factor loadings.
# The following presents the code used to generate the ex-post betas, portfolio alphas and
#corresponding t-statistics reported in the main paper
# Create vector equal to the number of portfolios to be used in loop. Portfolio 6 equals the lowhigh portfolio
k = c(1,2,3,4,5,6)
# Create empty dataframe to store market beta from CAPM regressions(ex-post portfolio betas),
#factor model alphas and corresponding t-statistics
alfa <- data.frame(P1.a = numeric(), 
                   P2.a = numeric(), 
                   P3.a = numeric(), 
                   P4.a = numeric(), 
                   P5.a = numeric(), 
                   P6.a = numeric(),
                   P1.t = numeric(), 
                   P2.t = numeric(), 
                   P3.t = numeric(), 
                   P4.t = numeric(), 
                   P5.t = numeric(), 
                   P6.t = numeric())
### Estimate the number of lags to use in Newey West Adjustment (lag=4)
m <- portfolio_returns %>% filter(portfolio == 6)
m <- count(m)
m <- round(0.75*(m^(1/3)))
lag <- m-1
rm(m)
#### 1.a: Portfolios sorted on beta.monthly
## beta.monthly sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_month_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_month_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_month_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_month_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_month_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
  }
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_b_m_VW <- alfa
rm(p)


#####################
#### 1.b: beta.monthly sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_month_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_month_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_month_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_month_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_month_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_b_m_EW <- alfa
rm(p)
#########################
#####################
#### 2.a: beta.day sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_day_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_day_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_day_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_day_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_day_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_b_d_VW <- alfa
rm(p)
####################### 
#### 2.b: beta.day sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_day_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_day_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_day_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_day_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_day_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_b_d_EW <- alfa
rm(p)
####################### 
#### 3.a: CT1 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_CT1_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_CT1_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_CT1_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_CT1_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_CT1_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_CT1_VW <- alfa
rm(p)
####################### 
####################### 
#### 3.b: CT2 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_CT2_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_CT2_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_CT2_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_CT2_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_CT2_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_CT2_VW <- alfa
rm(p)
#######################
####################### 
#### 3.c: CT3 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_CT3_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_CT3_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_CT3_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_CT3_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_CT3_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_CT3_VW <- alfa
rm(p)
#######################
####################### 
#### 3.a.1: CT1 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_CT1_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_CT1_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_CT1_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_CT1_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_CT1_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_CT1_EW <- alfa
rm(p)
####################### 
####################### 
#### 3.b.1: CT2 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_CT2_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_CT2_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_CT2_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_CT2_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_CT2_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_CT2_EW <- alfa
rm(p)
#######################
####################### 
#### 3.c.1: CT3 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_CT3_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_CT3_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_CT3_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_CT3_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_CT3_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_CT3_EW <- alfa
rm(p)
#######################
####################### 
#### 4.a: TC1 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_TC1_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_TC1_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_TC1_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_TC1_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_TC1_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_TC1_VW <- alfa
rm(p)
####################### 
####################### 
#### 4.b: TC2 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_TC2_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_TC2_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_TC2_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_TC2_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_TC2_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_TC2_VW <- alfa
rm(p)
#######################
####################### 
#### 4.c: TC3 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_TC3_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_TC3_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_TC3_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_TC3_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_TC3_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_TC3_VW <- alfa
rm(p)
#######################
####################### 
#### 4.a.1: TC1 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_TC1_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_TC1_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_TC1_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_TC1_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_TC1_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_TC1_EW <- alfa
rm(p)
####################### 
####################### 
#### 4.b.1: TC2 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_TC2_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_TC2_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_TC2_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_TC2_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_TC2_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_TC2_EW <- alfa
rm(p)
#######################
####################### 
#### 4.c.1: TC3 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_TC3_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_TC3_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_TC3_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_TC3_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_TC3_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_TC3_EW <- alfa
rm(p)
#######################
####################### 
#### 4.a: UT1 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_UT1_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_UT1_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_UT1_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_UT1_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_UT1_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_UT1_VW <- alfa
rm(p)
####################### 
####################### 
#### 4.b: UT2 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_UT2_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_UT2_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_UT2_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_UT2_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_UT2_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_UT2_VW <- alfa
rm(p)
#######################
####################### 
#### 5.c: UT3 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_UT3_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_UT3_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_UT3_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_UT3_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_UT3_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_UT3_VW <- alfa
rm(p)
#######################
####################### 
#### 5.a.1: UT1 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_UT1_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_UT1_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_UT1_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_UT1_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_UT1_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_UT1_EW <- alfa
rm(p)
####################### 
####################### 
#### 5.b.1: UT2 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_UT2_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_UT2_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_UT2_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_UT2_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_UT2_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_UT2_EW <- alfa
rm(p)
#######################
####################### 
#### 5.c.1: UT3 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_UT3_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_UT3_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_UT3_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_UT3_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_UT3_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_UT3_EW <- alfa
rm(p)
#######################
####################### 
#### 6.a: TU1 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_TU1_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_TU1_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_TU1_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_TU1_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_TU1_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_TU1_VW <- alfa
rm(p)
####################### 
####################### 
#### 6.b: TU2 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_TU2_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_TU2_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_TU2_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_TU2_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_TU2_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_TU2_VW <- alfa
rm(p)
#######################
####################### 
#### 6.c: TU3 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_TU3_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_TU3_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_TU3_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_TU3_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_TU3_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_TU3_VW <- alfa
rm(p)
#######################
####################### 
#### 5.a.1: TU1 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_TU1_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_TU1_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_TU1_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_TU1_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_TU1_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_TU1_EW <- alfa
rm(p)
####################### 
####################### 
#### 6.b.1: TU2 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_TU2_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_TU2_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_TU2_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_TU2_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_TU2_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_TU2_EW <- alfa
rm(p)
#######################
####################### 
#### 6.c.1: TU3 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_TU3_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_TU3_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_TU3_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_TU3_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_TU3_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_TU3_EW <- alfa
rm(p)
#######################
####################### 
#### 7.a: CU1 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_CU1_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_CU1_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_CU1_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_CU1_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_CU1_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_CU1_VW <- alfa
rm(p)
####################### 
####################### 
#### 7.b: CU2 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_CU2_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_CU2_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_CU2_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_CU2_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_CU2_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_CU2_VW <- alfa
rm(p)
#######################
####################### 
#### 7.c: CU3 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_CU3_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_CU3_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_CU3_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_CU3_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_CU3_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_CU3_VW <- alfa
rm(p)
#######################
####################### 
#### 7.a.1: CU1 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_CU1_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_CU1_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_CU1_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_CU1_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_CU1_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_CU1_EW <- alfa
rm(p)
####################### 
####################### 
#### 6.b.1: CU2 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_CU2_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_CU2_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_CU2_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_CU2_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_CU2_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_CU2_EW <- alfa
rm(p)
#######################
####################### 
#### 7.c.1: CU3 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_CU3_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_CU3_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_CU3_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_CU3_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_CU3_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_CU3_EW <- alfa
rm(p)
#######################
####################### 
#### 8.a: UC1 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_UC1_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_UC1_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_UC1_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_UC1_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_UC1_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_UC1_VW <- alfa
rm(p)
####################### 
####################### 
#### 8.b: UC2 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_UC2_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_UC2_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_UC2_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_UC2_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_UC2_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_UC2_VW <- alfa
rm(p)
#######################
####################### 
#### 8.c: UC3 sorted VW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_UC3_vw ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_UC3_vw ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_UC3_vw ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_UC3_vw ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_UC3_vw ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_UC3_VW <- alfa
rm(p)
#######################
####################### 
#### 8.a.1: UC1 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_UC1_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_UC1_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_UC1_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_UC1_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_UC1_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_UC1_EW <- alfa
rm(p)
####################### 
####################### 
#### 8.b.1: UC2 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_UC2_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_UC2_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_UC2_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_UC2_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_UC2_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_UC2_EW <- alfa
rm(p)
#######################
####################### 
#### 8.c.1: UC3 sorted EW portfolios
for (i in 1:length(k)) {
  
  p <- portfolio_returns %>%
    filter(portfolio == k[i])
  
  # Perform factor model regressions
  reg1 <- lm(er_UC3_ew ~ E.M.VW, data = p) # CAPM
  reg2 <- lm(er_UC3_ew ~ E.M.VW + SMB + HML, data = p) # FF3
  reg3 <- lm(er_UC3_ew ~ E.M.VW + SMB + HML + UMD, data = p) # FF3 + MOM
  reg4 <- lm(er_UC3_ew ~ E.M.VW + SMB2 + HML + CMA + RMW, data = p) # FF3 + MOM + LIQ
  reg5 <- lm(er_UC3_ew ~ E.M.VW + SMB2 + HML + CMA + RMW + UMD, data = p)
  # Estimate Newey West correlation Matrix
  NW1 <- NeweyWest(reg1, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW2 <- NeweyWest(reg2, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW3 <- NeweyWest(reg3, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW4 <- NeweyWest(reg4, lag = 4, prewhite = FALSE, adjust = TRUE)
  NW5 <- NeweyWest(reg5, lag = 4, prewhite = FALSE, adjust = TRUE)
  # Estimate coefficients using Newey West correlation matrix
  Coef1 <- coeftest(reg1, vcov. = NW1)
  Coef2 <- coeftest(reg2, vcov. = NW2)
  Coef3 <- coeftest(reg3, vcov. = NW3)
  Coef4 <- coeftest(reg4, vcov. = NW4)
  Coef5 <- coeftest(reg5, vcov. = NW5)
  # Extract portfolio Ex-post betas and Aplhas
  alfa[1,i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,i] = Coef1[1,1] # Alpha CAPM reg adj. newey west
  alfa[3,i] = Coef2[1,1] # Alpha FF3 reg adj. newey west
  alfa[4,i] = Coef3[1,1] # Alpha FF4 reg adj. newey west
  alfa[5,i] = Coef4[1,1] # Aplha FF5 reg adj. wewey west
  alfa[6,i] = Coef5[1,1] # Aplha FF6 reg adj. wewey west
  
  alfa[1,6+i] = Coef1[2,1] # Ex-post beta (Beta from CAPM regression) adj. newey west
  alfa[2,6+i] = Coef1[1,3] # t-stat alpha CAPM reg adj. newey west
  alfa[3,6+i] = Coef2[1,3] # t-stat alpha FF3 reg adj. newey west
  alfa[4,6+i] = Coef3[1,3] # t-stat alpha FF4 reg adj. newey west
  alfa[5,6+i] = Coef4[1,3] # t-stat aplha FF5 reg adj. wewey west
  alfa[6,6+i] = Coef5[1,3] # t-stat aplha FF6 reg adj. wewey west
}
rownames(alfa) <- c( "Beta",
                     "MKT", 
                     "FF3",
                     "FF4",
                     "FF5",
                     "FF6")
alfa_UC3_EW <- alfa
rm(p)
#######################
write.table(alfa_b_d_EW, file = "alfa_b_d_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_b_d_VW, file = "alfa_b_d_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_b_m_EW, file = "alfa_m_d_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_b_m_VW, file = "alfa_m_d_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_CT1_EW, file = "alfa_CT1_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_CT1_VW, file = "alfa_CT1_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_CT2_EW, file = "alfa_CT2_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_CT2_VW, file = "alfa_CT2_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_CT3_EW, file = "alfa_CT3_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_CT3_VW, file = "alfa_CT3_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_CU1_EW, file = "alfa_CU1_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_CU1_VW, file = "alfa_CU1_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_CU2_EW, file = "alfa_CU2_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_CU2_VW, file = "alfa_CU2_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_CU3_EW, file = "alfa_CU3_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_CU3_VW, file = "alfa_CU3_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_TC1_EW, file = "alfa_TC1_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_TC1_VW, file = "alfa_TC1_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_TC2_EW, file = "alfa_TC2_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_TC2_VW, file = "alfa_TC2_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_TC3_EW, file = "alfa_TC3_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_TC3_VW, file = "alfa_TC3_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_UC1_EW, file = "alfa_UC1_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_UC1_VW, file = "alfa_UC1_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_UC2_EW, file = "alfa_UC2_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_UC2_VW, file = "alfa_UC2_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_UC3_EW, file = "alfa_UC3_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_UC3_VW, file = "alfa_UC3_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_CU1_EW, file = "alfa_TU1_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_TU1_VW, file = "alfa_TU1_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_TU2_EW, file = "alfa_TU2_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_TU2_VW, file = "alfa_TU2_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_TU3_EW, file = "alfa_TU3_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_TU3_VW, file = "alfa_TU3_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_UT1_EW, file = "alfa_UT1_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_UT1_VW, file = "alfa_UT1_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_UT2_EW, file = "alfa_UT2_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_UT2_VW, file = "alfa_UT2_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_UT3_EW, file = "alfa_UT3_EW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(alfa_UT3_VW, file = "alfa_UT3_VW.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

