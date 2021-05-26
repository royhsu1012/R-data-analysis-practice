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

setwd("D:/data/step12_data/v60w36")


# alfa_b_d_EW = read.csv("alfa_b_d_EW.csv",  header = TRUE, sep = ",")
# alfa_b_d_VW = read.csv("alfa_b_d_VW.csv",  header = TRUE, sep = ",")
# alfa_m_d_EW = read.csv("alfa_m_d_EW.csv",  header = TRUE, sep = ",")
# alfa_m_d_VW = read.csv("alfa_m_d_VW.csv",  header = TRUE, sep = ",")
alfa_CT1_EW = read.csv("alfa_CT1_EW.csv",  header = TRUE, sep = ",")
alfa_CT1_VW = read.csv("alfa_CT1_VW.csv",  header = TRUE, sep = ",")
alfa_CT2_EW = read.csv("alfa_CT2_EW.csv",  header = TRUE, sep = ",")
alfa_CT2_VW = read.csv("alfa_CT2_VW.csv",  header = TRUE, sep = ",")
alfa_CT3_EW = read.csv("alfa_CT3_EW.csv",  header = TRUE, sep = ",")
alfa_CT3_VW = read.csv("alfa_CT3_VW.csv",  header = TRUE, sep = ",")
alfa_CU1_EW = read.csv("alfa_CU1_EW.csv",  header = TRUE, sep = ",")
alfa_CU1_VW = read.csv("alfa_CU1_VW.csv",  header = TRUE, sep = ",")
alfa_CU2_EW = read.csv("alfa_CU2_EW.csv",  header = TRUE, sep = ",")
alfa_CU2_VW = read.csv("alfa_CU2_VW.csv",  header = TRUE, sep = ",")
alfa_CU3_EW = read.csv("alfa_CU3_EW.csv",  header = TRUE, sep = ",")
alfa_CU3_VW = read.csv("alfa_CU3_VW.csv",  header = TRUE, sep = ",")
alfa_TC1_EW = read.csv("alfa_TC1_EW.csv",  header = TRUE, sep = ",")
alfa_TC1_VW = read.csv("alfa_TC1_VW.csv",  header = TRUE, sep = ",")
alfa_TC2_EW = read.csv("alfa_TC2_EW.csv",  header = TRUE, sep = ",")
alfa_TC2_VW = read.csv("alfa_TC2_VW.csv",  header = TRUE, sep = ",")
alfa_TC3_EW = read.csv("alfa_TC3_EW.csv",  header = TRUE, sep = ",")
alfa_TC3_VW = read.csv("alfa_TC3_VW.csv",  header = TRUE, sep = ",")
alfa_UC1_EW = read.csv("alfa_UC1_EW.csv",  header = TRUE, sep = ",")
alfa_UC1_VW = read.csv("alfa_UC1_VW.csv",  header = TRUE, sep = ",")
alfa_UC2_EW = read.csv("alfa_UC2_EW.csv",  header = TRUE, sep = ",")
alfa_UC2_VW = read.csv("alfa_UC2_VW.csv",  header = TRUE, sep = ",")
alfa_UC3_EW = read.csv("alfa_UC3_EW.csv",  header = TRUE, sep = ",")
alfa_UC3_VW = read.csv("alfa_UC3_VW.csv",  header = TRUE, sep = ",")
alfa_TU1_EW = read.csv("alfa_TU1_EW.csv",  header = TRUE, sep = ",")
alfa_TU1_VW = read.csv("alfa_TU1_VW.csv",  header = TRUE, sep = ",")
alfa_TU2_EW = read.csv("alfa_TU2_EW.csv",  header = TRUE, sep = ",")
alfa_TU2_VW = read.csv("alfa_TU2_VW.csv",  header = TRUE, sep = ",")
alfa_TU3_EW = read.csv("alfa_TU3_EW.csv",  header = TRUE, sep = ",")
alfa_TU3_VW = read.csv("alfa_TU3_VW.csv",  header = TRUE, sep = ",")
alfa_UT1_EW = read.csv("alfa_UT1_EW.csv",  header = TRUE, sep = ",")
alfa_UT1_VW = read.csv("alfa_UT1_VW.csv",  header = TRUE, sep = ",")
alfa_UT2_EW = read.csv("alfa_UT2_EW.csv",  header = TRUE, sep = ",")
alfa_UT2_VW = read.csv("alfa_UT2_VW.csv",  header = TRUE, sep = ",")
alfa_UT3_EW = read.csv("alfa_UT3_EW.csv",  header = TRUE, sep = ",")
alfa_UT3_VW = read.csv("alfa_UT3_VW.csv",  header = TRUE, sep = ",")


names_ew = c("alfa_CT1_EW","alfa_CT2_EW","alfa_CT3_EW","alfa_TC1_EW","alfa_TC2_EW","alfa_TC3_EW",
          "alfa_UT1_EW","alfa_UT2_EW","alfa_UT3_EW","alfa_TU1_EW","alfa_TU2_EW","alfa_TU3_EW",
          "alfa_CU1_EW","alfa_CU2_EW","alfa_CU3_EW","alfa_UC1_EW","alfa_UC2_EW","alfa_UC3_EW")
FF6_alfa_ew = c(alfa_CT1_EW[6,12],alfa_CT2_EW[6,12],alfa_CT3_EW[6,12],alfa_TC1_EW[6,12],alfa_TC2_EW[6,12],alfa_TC3_EW[6,12],
              alfa_UT1_EW[6,12],alfa_UT2_EW[6,12],alfa_UT3_EW[6,12],alfa_TU1_EW[6,12],alfa_TU2_EW[6,12],alfa_TU3_EW[6,12],
              alfa_CU1_EW[6,12],alfa_CU2_EW[6,12],alfa_CU3_EW[6,12],alfa_UC1_EW[6,12],alfa_UC2_EW[6,12],alfa_UC3_EW[6,12])

tb_ew <- tibble(names_ew,FF6_alfa_ew)

names_vw = c("alfa_CT1_VW","alfa_CT2_VW","alfa_CT3_VW","alfa_TC1_VW","alfa_TC2_VW","alfa_TC3_VW",
             "alfa_UT1_VW","alfa_UT2_VW","alfa_UT3_VW","alfa_TU1_VW","alfa_TU2_VW","alfa_TU3_VW",
             "alfa_CU1_VW","alfa_CU2_VW","alfa_CU3_VW","alfa_UC1_VW","alfa_UC2_VW","alfa_UC3_VW")
FF6_alfa_vw = c(alfa_CT1_VW[6,12],alfa_CT2_VW[6,12],alfa_CT3_VW[6,12],alfa_TC1_VW[6,12],alfa_TC2_VW[6,12],alfa_TC3_VW[6,12],
                alfa_UT1_VW[6,12],alfa_UT2_VW[6,12],alfa_UT3_VW[6,12],alfa_TU1_VW[6,12],alfa_TU2_VW[6,12],alfa_TU3_VW[6,12],
                alfa_CU1_VW[6,12],alfa_CU2_VW[6,12],alfa_CU3_VW[6,12],alfa_UC1_VW[6,12],alfa_UC2_VW[6,12],alfa_UC3_VW[6,12])

tb_vw <- tibble(names_vw,FF6_alfa_vw)


rm(alfa_CT1_EW,alfa_CT2_EW,alfa_CT3_EW,alfa_TC1_EW,alfa_TC2_EW,alfa_TC3_EW,
alfa_UT1_EW,alfa_UT2_EW,alfa_UT3_EW,alfa_TU1_EW,alfa_TU2_EW,alfa_TU3_EW,
alfa_CU1_EW,alfa_CU2_EW,alfa_CU3_EW,alfa_UC1_EW,alfa_UC2_EW,alfa_UC3_EW)

rm(alfa_CT1_VW,alfa_CT2_VW,alfa_CT3_VW,alfa_TC1_VW,alfa_TC2_VW,alfa_TC3_VW,
   alfa_UT1_VW,alfa_UT2_VW,alfa_UT3_VW,alfa_TU1_VW,alfa_TU2_VW,alfa_TU3_VW,
   alfa_CU1_VW,alfa_CU2_VW,alfa_CU3_VW,alfa_UC1_VW,alfa_UC2_VW,alfa_UC3_VW)

write.table(tb_ew, file = "tb_ew.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(tb_vw, file = "tb_vw.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

