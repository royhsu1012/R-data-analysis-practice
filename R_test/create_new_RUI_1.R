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
library(reshape2)
#################
setwd("D:/EPUEPU/data/data3/total")

CT <- read.csv("CT_total_2.csv", header = TRUE, sep = ",")
TC <- read.csv("TC_total_2.csv", header = TRUE, sep = ",")
CU <- read.csv("CU_total_2.csv", header = TRUE, sep = ",")
UC <- read.csv("UC_total_2.csv", header = TRUE, sep = ",")
UT <- read.csv("UT_total_2.csv", header = TRUE, sep = ",")
TU <- read.csv("TU_total_2.csv", header = TRUE, sep = ",")

setwd("D:/data/new_RUI")
######################


CT <- CT %>% rename(TradeDate = eventdate) 
TC <- TC %>% rename(TradeDate = eventdate)
CU <- CU %>% rename(TradeDate = eventdate)
UC <- UC %>% rename(TradeDate = eventdate)
TU <- TU %>% rename(TradeDate = eventdate)
UT <- UT %>% rename(TradeDate = eventdate)

CT <- CT %>% select("TradeDate","intensity")
TC <- TC %>% select("TradeDate","intensity") 
CU <- CU %>% select("TradeDate","intensity") 
UC <- UC %>% select("TradeDate","intensity") 
TU <- TU %>% select("TradeDate","intensity") 
UT <- UT %>% select("TradeDate","intensity") 

CT$TradeDate <- as.Date(CT$TradeDate)
TC$TradeDate <- as.Date(TC$TradeDate) 
CU$TradeDate <- as.Date(CU$TradeDate)
UC$TradeDate <- as.Date(UC$TradeDate)
TU$TradeDate <- as.Date(TU$TradeDate)
UT$TradeDate <- as.Date(UT$TradeDate)

##RUI with filter only positive and summarize value in same day
#(2/(1+exp(-i+10))) sigmoid left
#(2/(1+exp(-i))-1)  sigmoid right side
####################
s_weight = c()
for(i in 1:100){
  j = i-1
  s_weight[j+1]=2/(1+exp(j))
}
#################
hl_weight_30 =c()
H= 30
for(i in 1:(H)){
  j = i-1
  hl_weight_30[j+1]=(2^(-j/(H/2)))}
hl_weight_30 = rev(hl_weight_30)
###################
hl_weight_60 =c()
H= 60
for(i in 1:(H)){
  j = i-1
  hl_weight_60[j+1]=(2^(-j/(H/2)))}
hl_weight_60 = rev(hl_weight_60)
#########################
hl_weight_90 =c()
H= 90
for(i in 1:(H)){
  j = i-1
  hl_weight_90[j+1]=(2^(-j/(H/2)))}
hl_weight_90 = rev(hl_weight_90)
########################
########################
#calculate sum of daily data
CT_pos <- CT %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    CT_pos_counts = n(),
    CT_pos_max = max(intensity),
    CT_pos_intensity = sum(intensity),
    CT_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, CT_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 30
CT_pos_daily_30 <- data.frame(CT_pos_sum = numeric(), 
                           CT_pos_counts = numeric(), 
                           Tradedate = as.Date(character()), 
                           stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  CT_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(CT_pos_sum_30 = (CT_pos_iw * hl_weight_30)) %>%
    select("CT_pos_sum_30","CT_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  CT_pos_daily_30 <- rbind(CT_pos_daily_30,CT_pos.tab)}
CT_pos_daily_30 = CT_pos_daily_30 %>% 
  group_by(TradeDate) %>%
  summarise(CT_pos_sum_30 = sum(CT_pos_sum_30), 
            CT_pos_counts_sum = sum(CT_pos_counts),)%>%
  mutate(CT_pos_final = CT_pos_sum_30*log(1+CT_pos_counts_sum))

write.table(CT_pos_daily_30 , file = "CT_pos_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CT_pos.tab, data_pos)
########################
########################
#calculate sum of daily data
TC_pos <- TC %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    TC_pos_counts = n(),
    TC_pos_max = max(intensity),
    TC_pos_intensity = sum(intensity),
    TC_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, TC_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 30
TC_pos_daily_30 <- data.frame(TC_pos_sum = numeric(), 
                           TC_pos_counts = numeric(), 
                           Tradedate = as.Date(character()), 
                           stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  TC_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(TC_pos_sum_30 = (TC_pos_iw * hl_weight_30)) %>%
    select("TC_pos_sum_30","TC_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  TC_pos_daily_30 <- rbind(TC_pos_daily_30,TC_pos.tab)}
TC_pos_daily_30 = TC_pos_daily_30 %>% 
  group_by(TradeDate) %>%
  summarise(TC_pos_sum_30 = sum(TC_pos_sum_30), 
            TC_pos_counts_sum = sum(TC_pos_counts),)%>%
  mutate(TC_pos_final = TC_pos_sum_30*log(1+TC_pos_counts_sum))

write.table(TC_pos_daily_30 , file = "TC_pos_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TC_pos.tab, data_pos)
########################
########################
########################
#calculate sum of daily data
UT_pos <- UT %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    UT_pos_counts = n(),
    UT_pos_max = max(intensity),
    UT_pos_intensity = sum(intensity),
    UT_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, UT_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 30
UT_pos_daily_30 <- data.frame(UT_pos_sum = numeric(), 
                           UT_pos_counts = numeric(), 
                           Tradedate = as.Date(character()), 
                           stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  UT_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(UT_pos_sum_30 = (UT_pos_iw * hl_weight_30)) %>%
    select("UT_pos_sum_30","UT_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  UT_pos_daily_30 <- rbind(UT_pos_daily_30,UT_pos.tab)}
UT_pos_daily_30 = UT_pos_daily_30 %>% 
  group_by(TradeDate) %>%
  summarise(UT_pos_sum_30 = sum(UT_pos_sum_30), 
            UT_pos_counts_sum = sum(UT_pos_counts),)%>%
  mutate(UT_pos_final = UT_pos_sum_30*log(1+UT_pos_counts_sum))

write.table(UT_pos_daily_30 , file = "UT_pos_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UT_pos.tab, data_pos)
########################
########################
########################
#calculate sum of daily data
TU_pos <- TU %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    TU_pos_counts = n(),
    TU_pos_max = max(intensity),
    TU_pos_intensity = sum(intensity),
    TU_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, TU_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 30
TU_pos_daily_30 <- data.frame(TU_pos_sum = numeric(), 
                           TU_pos_counts = numeric(), 
                           Tradedate = as.Date(character()), 
                           stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  TU_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(TU_pos_sum_30 = (TU_pos_iw * hl_weight_30)) %>%
    select("TU_pos_sum_30","TU_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  TU_pos_daily_30 <- rbind(TU_pos_daily_30,TU_pos.tab)}
TU_pos_daily_30 = TU_pos_daily_30 %>% 
  group_by(TradeDate) %>%
  summarise(TU_pos_sum_30 = sum(TU_pos_sum_30), 
            TU_pos_counts_sum = sum(TU_pos_counts),)%>%
  mutate(TU_pos_final = TU_pos_sum_30*log(1+TU_pos_counts_sum))

write.table(TU_pos_daily_30 , file = "TU_pos_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TU_pos.tab, data_pos)
########################
########################
########################
#calculate sum of daily data
CU_pos <- CU %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    CU_pos_counts = n(),
    CU_pos_max = max(intensity),
    CU_pos_intensity = sum(intensity),
    CU_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, CU_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 30
CU_pos_daily_30 <- data.frame(CU_pos_sum = numeric(), 
                           CU_pos_counts = numeric(), 
                           Tradedate = as.Date(character()), 
                           stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  CU_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(CU_pos_sum_30 = (CU_pos_iw * hl_weight_30)) %>%
    select("CU_pos_sum_30","CU_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  CU_pos_daily_30 <- rbind(CU_pos_daily_30,CU_pos.tab)}
CU_pos_daily_30 = CU_pos_daily_30 %>% 
  group_by(TradeDate) %>%
  summarise(CU_pos_sum_30 = sum(CU_pos_sum_30), 
            CU_pos_counts_sum = sum(CU_pos_counts),)%>%
  mutate(CU_pos_final = CU_pos_sum_30*log(1+CU_pos_counts_sum))

write.table(CU_pos_daily_30 , file = "CU_pos_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CU_pos.tab, data_pos)
########################
########################
########################
#calculate sum of daily data
UC_pos <- UC %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    UC_pos_counts = n(),
    UC_pos_max = max(intensity),
    UC_pos_intensity = sum(intensity),
    UC_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, UC_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 30
UC_pos_daily_30 <- data.frame(UC_pos_sum = numeric(), 
                           UC_pos_counts = numeric(), 
                           Tradedate = as.Date(character()), 
                           stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  UC_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(UC_pos_sum_30 = (UC_pos_iw * hl_weight_30)) %>%
    select("UC_pos_sum_30","UC_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  UC_pos_daily_30 <- rbind(UC_pos_daily_30,UC_pos.tab)}
UC_pos_daily_30 = UC_pos_daily_30 %>% 
  group_by(TradeDate) %>%
  summarise(UC_pos_sum_30 = sum(UC_pos_sum_30), 
            UC_pos_counts_sum = sum(UC_pos_counts),)%>%
  mutate(UC_pos_final = UC_pos_sum_30*log(1+UC_pos_counts_sum))

write.table(UC_pos_daily_30 , file = "UC_pos_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UC_pos.tab, data_pos)
########################
#########################
############################
##RUI with filter only negative and summarize value in same day
#calculate sum of daily data
CT_neg <- CT %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>% 
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    CT_neg_counts = n(),
    CT_neg_max = min(intensity)*(-1),
    CT_neg_intensity = sum(intensity)*(-1),
    CT_neg_iw = sum(intensity_weight)*(-1) )

########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, CT_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 30
CT_neg_daily_30 <- data.frame(CT_neg_sum = numeric(), 
                           CT_neg_counts = numeric(), 
                           Tradedate = as.Date(character()), 
                           stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  CT_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(CT_neg_sum_30 = (CT_neg_iw * hl_weight_30)) %>%
    select("CT_neg_sum_30","CT_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  CT_neg_daily_30 <- rbind(CT_neg_daily_30,CT_neg.tab)}
CT_neg_daily_30 = CT_neg_daily_30 %>% 
  group_by(TradeDate) %>%
  summarise(CT_neg_sum_30 = sum(CT_neg_sum_30), 
            CT_neg_counts_sum = sum(CT_neg_counts),)%>%
  mutate(CT_neg_final = CT_neg_sum_30*log(1+CT_neg_counts_sum))

write.table(CT_neg_daily_30 , file = "CT_neg_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CT_neg.tab, data_neg)
########################
########################
#calculate sum of daily data
TC_neg <- TC %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>%  
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    TC_neg_counts = n(),
    TC_neg_max = min(intensity)*(-1),
    TC_neg_intensity = sum(intensity)*(-1),
    TC_neg_iw = sum(intensity_weight)*(-1) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, TC_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 30
TC_neg_daily_30 <- data.frame(TC_neg_sum = numeric(), 
                           TC_neg_counts = numeric(), 
                           Tradedate = as.Date(character()), 
                           stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  TC_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(TC_neg_sum_30 = (TC_neg_iw * hl_weight_30)) %>%
    select("TC_neg_sum_30","TC_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  TC_neg_daily_30 <- rbind(TC_neg_daily_30,TC_neg.tab)}
TC_neg_daily_30 = TC_neg_daily_30 %>% 
  group_by(TradeDate) %>%
  summarise(TC_neg_sum_30 = sum(TC_neg_sum_30), 
            TC_neg_counts_sum = sum(TC_neg_counts),)%>%
  mutate(TC_neg_final = TC_neg_sum_30*log(1+TC_neg_counts_sum))

write.table(TC_neg_daily_30 , file = "TC_neg_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TC_neg.tab, data_neg)
########################
########################
########################
#calculate sum of daily data
UT_neg <- UT %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>%  
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    UT_neg_counts = n(),
    UT_neg_max = min(intensity)*(-1),
    UT_neg_intensity = sum(intensity)*(-1),
    UT_neg_iw = sum(intensity_weight)*(-1) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, UT_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 30
UT_neg_daily_30 <- data.frame(UT_neg_sum = numeric(), 
                           UT_neg_counts = numeric(), 
                           Tradedate = as.Date(character()), 
                           stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  UT_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(UT_neg_sum_30 = (UT_neg_iw * hl_weight_30)) %>%
    select("UT_neg_sum_30","UT_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  UT_neg_daily_30 <- rbind(UT_neg_daily_30,UT_neg.tab)}
UT_neg_daily_30 = UT_neg_daily_30 %>% 
  group_by(TradeDate) %>%
  summarise(UT_neg_sum_30 = sum(UT_neg_sum_30), 
            UT_neg_counts_sum = sum(UT_neg_counts),)%>%
  mutate(UT_neg_final = UT_neg_sum_30*log(1+UT_neg_counts_sum))

write.table(UT_neg_daily_30 , file = "UT_neg_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UT_neg.tab, data_neg)
########################
########################
########################
#calculate sum of daily data
TU_neg <- TU %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>%  
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    TU_neg_counts = n(),
    TU_neg_max = min(intensity)*(-1),
    TU_neg_intensity = sum(intensity)*(-1),
    TU_neg_iw = sum(intensity_weight)*(-1) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, TU_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 30
TU_neg_daily_30 <- data.frame(TU_neg_sum = numeric(), 
                           TU_neg_counts = numeric(), 
                           Tradedate = as.Date(character()), 
                           stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  TU_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(TU_neg_sum_30 = (TU_neg_iw * hl_weight_30)) %>%
    select("TU_neg_sum_30","TU_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  TU_neg_daily_30 <- rbind(TU_neg_daily_30,TU_neg.tab)}
TU_neg_daily_30 = TU_neg_daily_30 %>% 
  group_by(TradeDate) %>%
  summarise(TU_neg_sum_30 = sum(TU_neg_sum_30), 
            TU_neg_counts_sum = sum(TU_neg_counts),)%>%
  mutate(TU_neg_final = TU_neg_sum_30*log(1+TU_neg_counts_sum))

write.table(TU_neg_daily_30 , file = "TU_neg_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TU_neg.tab, data_neg)
########################
########################
########################
#calculate sum of daily data
CU_neg <- CU %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>%  
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    CU_neg_counts = n(),
    CU_neg_max = min(intensity)*(-1),
    CU_neg_intensity = sum(intensity)*(-1),
    CU_neg_iw = sum(intensity_weight)*(-1) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, CU_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 30
CU_neg_daily_30 <- data.frame(CU_neg_sum = numeric(), 
                           CU_neg_counts = numeric(), 
                           Tradedate = as.Date(character()), 
                           stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  CU_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(CU_neg_sum_30 = (CU_neg_iw * hl_weight_30)) %>%
    select("CU_neg_sum_30","CU_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  CU_neg_daily_30 <- rbind(CU_neg_daily_30,CU_neg.tab)}
CU_neg_daily_30 = CU_neg_daily_30 %>% 
  group_by(TradeDate) %>%
  summarise(CU_neg_sum_30 = sum(CU_neg_sum_30), 
            CU_neg_counts_sum = sum(CU_neg_counts),)%>%
  mutate(CU_neg_final = CU_neg_sum_30*log(1+CU_neg_counts_sum))

write.table(CU_neg_daily_30 , file = "CU_neg_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CU_neg.tab, data_neg)
########################
########################
########################
#calculate sum of daily data
UC_neg <- UC %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>%  
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    UC_neg_counts = n(),
    UC_neg_max = min(intensity)*(-1),
    UC_neg_intensity = sum(intensity)*(-1),
    UC_neg_iw = sum(intensity_weight)*(-1) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, UC_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 30
UC_neg_daily_30 <- data.frame(UC_neg_sum = numeric(), 
                           UC_neg_counts = numeric(), 
                           Tradedate = as.Date(character()), 
                           stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  UC_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(UC_neg_sum_30 = (UC_neg_iw * hl_weight_30)) %>%
    select("UC_neg_sum_30","UC_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  UC_neg_daily_30 <- rbind(UC_neg_daily_30,UC_neg.tab)}
UC_neg_daily_30 = UC_neg_daily_30 %>% 
  group_by(TradeDate) %>%
  summarise(UC_neg_sum_30 = sum(UC_neg_sum_30), 
            UC_neg_counts_sum = sum(UC_neg_counts),)%>%
  mutate(UC_neg_final = UC_neg_sum_30*log(1+UC_neg_counts_sum))

write.table(UC_neg_daily_30 , file = "UC_neg_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UC_neg.tab, data_neg)
########################
#########################

CT_all_daily_30 = full_join(CT_pos_daily_30, CT_neg_daily_30, by = "TradeDate")
CT_all_daily_30 = CT_all_daily_30 %>% 
  mutate(CT_diff = CT_pos_final - CT_neg_final)
TC_all_daily_30 = full_join(TC_pos_daily_30, TC_neg_daily_30, by = "TradeDate")
TC_all_daily_30 = TC_all_daily_30 %>% 
  mutate(TC_diff = TC_pos_final - TC_neg_final)
UT_all_daily_30 = full_join(UT_pos_daily_30, UT_neg_daily_30, by = "TradeDate")
UT_all_daily_30 = UT_all_daily_30 %>% 
  mutate(UT_diff = UT_pos_final - UT_neg_final)
TU_all_daily_30 = full_join(TU_pos_daily_30, TU_neg_daily_30, by = "TradeDate")
TU_all_daily_30 = TU_all_daily_30 %>% 
  mutate(TU_diff = TU_pos_final - TU_neg_final)
CU_all_daily_30 = full_join(CU_pos_daily_30, CU_neg_daily_30, by = "TradeDate")
CU_all_daily_30 = CU_all_daily_30 %>% 
  mutate(CU_diff = CU_pos_final - CU_neg_final)
UC_all_daily_30 = full_join(UC_pos_daily_30, UC_neg_daily_30, by = "TradeDate")
UC_all_daily_30 = UC_all_daily_30 %>% 
  mutate(UC_diff = UC_pos_final - UC_neg_final)
  
write.table(CT_all_daily_30 , file = "CT_all_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(TC_all_daily_30 , file = "TC_all_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(CU_all_daily_30 , file = "CU_all_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(UC_all_daily_30 , file = "UC_all_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(TU_all_daily_30 , file = "TU_all_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(UT_all_daily_30 , file = "UT_all_daily_30.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
#
##
###
####
#####
########################
########################
#calculate sum of daily data
CT_pos <- CT %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    CT_pos_counts = n(),
    CT_pos_max = max(intensity),
    CT_pos_intensity = sum(intensity),
    CT_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, CT_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 60
CT_pos_daily_60 <- data.frame(CT_pos_sum = numeric(), 
                              CT_pos_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  CT_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(CT_pos_sum_60 = (CT_pos_iw * hl_weight_60)) %>%
    select("CT_pos_sum_60","CT_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  CT_pos_daily_60 <- rbind(CT_pos_daily_60,CT_pos.tab)}
CT_pos_daily_60 = CT_pos_daily_60 %>% 
  group_by(TradeDate) %>%
  summarise(CT_pos_sum_60 = sum(CT_pos_sum_60), 
            CT_pos_counts_sum = sum(CT_pos_counts),)%>%
  mutate(CT_pos_final = CT_pos_sum_60*log(1+CT_pos_counts_sum))

write.table(CT_pos_daily_60 , file = "CT_pos_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CT_pos.tab, data_pos)
########################
########################
#calculate sum of daily data
TC_pos <- TC %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    TC_pos_counts = n(),
    TC_pos_max = max(intensity),
    TC_pos_intensity = sum(intensity),
    TC_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, TC_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 60
TC_pos_daily_60 <- data.frame(TC_pos_sum = numeric(), 
                              TC_pos_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  TC_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(TC_pos_sum_60 = (TC_pos_iw * hl_weight_60)) %>%
    select("TC_pos_sum_60","TC_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  TC_pos_daily_60 <- rbind(TC_pos_daily_60,TC_pos.tab)}
TC_pos_daily_60 = TC_pos_daily_60 %>% 
  group_by(TradeDate) %>%
  summarise(TC_pos_sum_60 = sum(TC_pos_sum_60), 
            TC_pos_counts_sum = sum(TC_pos_counts),)%>%
  mutate(TC_pos_final = TC_pos_sum_60*log(1+TC_pos_counts_sum))

write.table(TC_pos_daily_60 , file = "TC_pos_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TC_pos.tab, data_pos)
########################
########################
########################
#calculate sum of daily data
UT_pos <- UT %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    UT_pos_counts = n(),
    UT_pos_max = max(intensity),
    UT_pos_intensity = sum(intensity),
    UT_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, UT_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 60
UT_pos_daily_60 <- data.frame(UT_pos_sum = numeric(), 
                              UT_pos_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  UT_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(UT_pos_sum_60 = (UT_pos_iw * hl_weight_60)) %>%
    select("UT_pos_sum_60","UT_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  UT_pos_daily_60 <- rbind(UT_pos_daily_60,UT_pos.tab)}
UT_pos_daily_60 = UT_pos_daily_60 %>% 
  group_by(TradeDate) %>%
  summarise(UT_pos_sum_60 = sum(UT_pos_sum_60), 
            UT_pos_counts_sum = sum(UT_pos_counts),)%>%
  mutate(UT_pos_final = UT_pos_sum_60*log(1+UT_pos_counts_sum))

write.table(UT_pos_daily_60 , file = "UT_pos_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UT_pos.tab, data_pos)
########################
########################
########################
#calculate sum of daily data
TU_pos <- TU %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    TU_pos_counts = n(),
    TU_pos_max = max(intensity),
    TU_pos_intensity = sum(intensity),
    TU_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, TU_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 60
TU_pos_daily_60 <- data.frame(TU_pos_sum = numeric(), 
                              TU_pos_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  TU_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(TU_pos_sum_60 = (TU_pos_iw * hl_weight_60)) %>%
    select("TU_pos_sum_60","TU_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  TU_pos_daily_60 <- rbind(TU_pos_daily_60,TU_pos.tab)}
TU_pos_daily_60 = TU_pos_daily_60 %>% 
  group_by(TradeDate) %>%
  summarise(TU_pos_sum_60 = sum(TU_pos_sum_60), 
            TU_pos_counts_sum = sum(TU_pos_counts),)%>%
  mutate(TU_pos_final = TU_pos_sum_60*log(1+TU_pos_counts_sum))

write.table(TU_pos_daily_60 , file = "TU_pos_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TU_pos.tab, data_pos)
########################
########################
########################
#calculate sum of daily data
CU_pos <- CU %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    CU_pos_counts = n(),
    CU_pos_max = max(intensity),
    CU_pos_intensity = sum(intensity),
    CU_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, CU_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 60
CU_pos_daily_60 <- data.frame(CU_pos_sum = numeric(), 
                              CU_pos_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  CU_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(CU_pos_sum_60 = (CU_pos_iw * hl_weight_60)) %>%
    select("CU_pos_sum_60","CU_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  CU_pos_daily_60 <- rbind(CU_pos_daily_60,CU_pos.tab)}
CU_pos_daily_60 = CU_pos_daily_60 %>% 
  group_by(TradeDate) %>%
  summarise(CU_pos_sum_60 = sum(CU_pos_sum_60), 
            CU_pos_counts_sum = sum(CU_pos_counts),)%>%
  mutate(CU_pos_final = CU_pos_sum_60*log(1+CU_pos_counts_sum))

write.table(CU_pos_daily_60 , file = "CU_pos_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CU_pos.tab, data_pos)
########################
########################
########################
#calculate sum of daily data
UC_pos <- UC %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    UC_pos_counts = n(),
    UC_pos_max = max(intensity),
    UC_pos_intensity = sum(intensity),
    UC_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, UC_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 60
UC_pos_daily_60 <- data.frame(UC_pos_sum = numeric(), 
                              UC_pos_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  UC_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(UC_pos_sum_60 = (UC_pos_iw * hl_weight_60)) %>%
    select("UC_pos_sum_60","UC_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  UC_pos_daily_60 <- rbind(UC_pos_daily_60,UC_pos.tab)}
UC_pos_daily_60 = UC_pos_daily_60 %>% 
  group_by(TradeDate) %>%
  summarise(UC_pos_sum_60 = sum(UC_pos_sum_60), 
            UC_pos_counts_sum = sum(UC_pos_counts),)%>%
  mutate(UC_pos_final = UC_pos_sum_60*log(1+UC_pos_counts_sum))

write.table(UC_pos_daily_60 , file = "UC_pos_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UC_pos.tab, data_pos)
########################
#########################
############################
##RUI with filter only negative and summarize value in same day
#calculate sum of daily data
CT_neg <- CT %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>% 
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    CT_neg_counts = n(),
    CT_neg_max = min(intensity)*(-1),
    CT_neg_intensity = sum(intensity)*(-1),
    CT_neg_iw = sum(intensity_weight)*(-1) )

########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, CT_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 60
CT_neg_daily_60 <- data.frame(CT_neg_sum = numeric(), 
                              CT_neg_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  CT_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(CT_neg_sum_60 = (CT_neg_iw * hl_weight_60)) %>%
    select("CT_neg_sum_60","CT_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  CT_neg_daily_60 <- rbind(CT_neg_daily_60,CT_neg.tab)}
CT_neg_daily_60 = CT_neg_daily_60 %>% 
  group_by(TradeDate) %>%
  summarise(CT_neg_sum_60 = sum(CT_neg_sum_60), 
            CT_neg_counts_sum = sum(CT_neg_counts),)%>%
  mutate(CT_neg_final = CT_neg_sum_60*log(1+CT_neg_counts_sum))

write.table(CT_neg_daily_60 , file = "CT_neg_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CT_neg.tab, data_neg)
########################
########################
#calculate sum of daily data
TC_neg <- TC %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>%  
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    TC_neg_counts = n(),
    TC_neg_max = min(intensity)*(-1),
    TC_neg_intensity = sum(intensity)*(-1),
    TC_neg_iw = sum(intensity_weight)*(-1) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, TC_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 60
TC_neg_daily_60 <- data.frame(TC_neg_sum = numeric(), 
                              TC_neg_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  TC_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(TC_neg_sum_60 = (TC_neg_iw * hl_weight_60)) %>%
    select("TC_neg_sum_60","TC_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  TC_neg_daily_60 <- rbind(TC_neg_daily_60,TC_neg.tab)}
TC_neg_daily_60 = TC_neg_daily_60 %>% 
  group_by(TradeDate) %>%
  summarise(TC_neg_sum_60 = sum(TC_neg_sum_60), 
            TC_neg_counts_sum = sum(TC_neg_counts),)%>%
  mutate(TC_neg_final = TC_neg_sum_60*log(1+TC_neg_counts_sum))

write.table(TC_neg_daily_60 , file = "TC_neg_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TC_neg.tab, data_neg)
########################
########################
########################
#calculate sum of daily data
UT_neg <- UT %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>%  
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    UT_neg_counts = n(),
    UT_neg_max = min(intensity)*(-1),
    UT_neg_intensity = sum(intensity)*(-1),
    UT_neg_iw = sum(intensity_weight)*(-1) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, UT_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 60
UT_neg_daily_60 <- data.frame(UT_neg_sum = numeric(), 
                              UT_neg_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  UT_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(UT_neg_sum_60 = (UT_neg_iw * hl_weight_60)) %>%
    select("UT_neg_sum_60","UT_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  UT_neg_daily_60 <- rbind(UT_neg_daily_60,UT_neg.tab)}
UT_neg_daily_60 = UT_neg_daily_60 %>% 
  group_by(TradeDate) %>%
  summarise(UT_neg_sum_60 = sum(UT_neg_sum_60), 
            UT_neg_counts_sum = sum(UT_neg_counts),)%>%
  mutate(UT_neg_final = UT_neg_sum_60*log(1+UT_neg_counts_sum))

write.table(UT_neg_daily_60 , file = "UT_neg_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UT_neg.tab, data_neg)
########################
########################
########################
#calculate sum of daily data
TU_neg <- TU %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>%  
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    TU_neg_counts = n(),
    TU_neg_max = min(intensity)*(-1),
    TU_neg_intensity = sum(intensity)*(-1),
    TU_neg_iw = sum(intensity_weight)*(-1) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, TU_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 60
TU_neg_daily_60 <- data.frame(TU_neg_sum = numeric(), 
                              TU_neg_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  TU_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(TU_neg_sum_60 = (TU_neg_iw * hl_weight_60)) %>%
    select("TU_neg_sum_60","TU_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  TU_neg_daily_60 <- rbind(TU_neg_daily_60,TU_neg.tab)}
TU_neg_daily_60 = TU_neg_daily_60 %>% 
  group_by(TradeDate) %>%
  summarise(TU_neg_sum_60 = sum(TU_neg_sum_60), 
            TU_neg_counts_sum = sum(TU_neg_counts),)%>%
  mutate(TU_neg_final = TU_neg_sum_60*log(1+TU_neg_counts_sum))

write.table(TU_neg_daily_60 , file = "TU_neg_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TU_neg.tab, data_neg)
########################
########################
########################
#calculate sum of daily data
CU_neg <- CU %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>%  
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    CU_neg_counts = n(),
    CU_neg_max = min(intensity)*(-1),
    CU_neg_intensity = sum(intensity)*(-1),
    CU_neg_iw = sum(intensity_weight)*(-1) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, CU_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 60
CU_neg_daily_60 <- data.frame(CU_neg_sum = numeric(), 
                              CU_neg_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  CU_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(CU_neg_sum_60 = (CU_neg_iw * hl_weight_60)) %>%
    select("CU_neg_sum_60","CU_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  CU_neg_daily_60 <- rbind(CU_neg_daily_60,CU_neg.tab)}
CU_neg_daily_60 = CU_neg_daily_60 %>% 
  group_by(TradeDate) %>%
  summarise(CU_neg_sum_60 = sum(CU_neg_sum_60), 
            CU_neg_counts_sum = sum(CU_neg_counts),)%>%
  mutate(CU_neg_final = CU_neg_sum_60*log(1+CU_neg_counts_sum))

write.table(CU_neg_daily_60 , file = "CU_neg_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CU_neg.tab, data_neg)
########################
########################
########################
#calculate sum of daily data
UC_neg <- UC %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>%  
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    UC_neg_counts = n(),
    UC_neg_max = min(intensity)*(-1),
    UC_neg_intensity = sum(intensity)*(-1),
    UC_neg_iw = sum(intensity_weight)*(-1) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, UC_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 60
UC_neg_daily_60 <- data.frame(UC_neg_sum = numeric(), 
                              UC_neg_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  UC_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(UC_neg_sum_60 = (UC_neg_iw * hl_weight_60)) %>%
    select("UC_neg_sum_60","UC_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  UC_neg_daily_60 <- rbind(UC_neg_daily_60,UC_neg.tab)}
UC_neg_daily_60 = UC_neg_daily_60 %>% 
  group_by(TradeDate) %>%
  summarise(UC_neg_sum_60 = sum(UC_neg_sum_60), 
            UC_neg_counts_sum = sum(UC_neg_counts),)%>%
  mutate(UC_neg_final = UC_neg_sum_60*log(1+UC_neg_counts_sum))

write.table(UC_neg_daily_60 , file = "UC_neg_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UC_neg.tab, data_neg)
########################
#########################

CT_all_daily_60 = full_join(CT_pos_daily_60, CT_neg_daily_60, by = "TradeDate")
CT_all_daily_60 = CT_all_daily_60 %>% 
  mutate(CT_diff = CT_pos_final - CT_neg_final)
TC_all_daily_60 = full_join(TC_pos_daily_60, TC_neg_daily_60, by = "TradeDate")
TC_all_daily_60 = TC_all_daily_60 %>% 
  mutate(TC_diff = TC_pos_final - TC_neg_final)
UT_all_daily_60 = full_join(UT_pos_daily_60, UT_neg_daily_60, by = "TradeDate")
UT_all_daily_60 = UT_all_daily_60 %>% 
  mutate(UT_diff = UT_pos_final - UT_neg_final)
TU_all_daily_60 = full_join(TU_pos_daily_60, TU_neg_daily_60, by = "TradeDate")
TU_all_daily_60 = TU_all_daily_60 %>% 
  mutate(TU_diff = TU_pos_final - TU_neg_final)
CU_all_daily_60 = full_join(CU_pos_daily_60, CU_neg_daily_60, by = "TradeDate")
CU_all_daily_60 = CU_all_daily_60 %>% 
  mutate(CU_diff = CU_pos_final - CU_neg_final)
UC_all_daily_60 = full_join(UC_pos_daily_60, UC_neg_daily_60, by = "TradeDate")
UC_all_daily_60 = UC_all_daily_60 %>% 
  mutate(UC_diff = UC_pos_final - UC_neg_final)

write.table(CT_all_daily_60 , file = "CT_all_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(TC_all_daily_60 , file = "TC_all_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(CU_all_daily_60 , file = "CU_all_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(UC_all_daily_60 , file = "UC_all_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(TU_all_daily_60 , file = "TU_all_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(UT_all_daily_60 , file = "UT_all_daily_60.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
#
##
###
####
#####
########################
########################
#calculate sum of daily data
CT_pos <- CT %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    CT_pos_counts = n(),
    CT_pos_max = max(intensity),
    CT_pos_intensity = sum(intensity),
    CT_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, CT_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 90
CT_pos_daily_90 <- data.frame(CT_pos_sum = numeric(), 
                              CT_pos_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  CT_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(CT_pos_sum_90 = (CT_pos_iw * hl_weight_90)) %>%
    select("CT_pos_sum_90","CT_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  CT_pos_daily_90 <- rbind(CT_pos_daily_90,CT_pos.tab)}
CT_pos_daily_90 = CT_pos_daily_90 %>% 
  group_by(TradeDate) %>%
  summarise(CT_pos_sum_90 = sum(CT_pos_sum_90), 
            CT_pos_counts_sum = sum(CT_pos_counts),)%>%
  mutate(CT_pos_final = CT_pos_sum_90*log(1+CT_pos_counts_sum))

write.table(CT_pos_daily_90 , file = "CT_pos_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CT_pos.tab, data_pos)
########################
########################
#calculate sum of daily data
TC_pos <- TC %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    TC_pos_counts = n(),
    TC_pos_max = max(intensity),
    TC_pos_intensity = sum(intensity),
    TC_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, TC_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 90
TC_pos_daily_90 <- data.frame(TC_pos_sum = numeric(), 
                              TC_pos_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  TC_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(TC_pos_sum_90 = (TC_pos_iw * hl_weight_90)) %>%
    select("TC_pos_sum_90","TC_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  TC_pos_daily_90 <- rbind(TC_pos_daily_90,TC_pos.tab)}
TC_pos_daily_90 = TC_pos_daily_90 %>% 
  group_by(TradeDate) %>%
  summarise(TC_pos_sum_90 = sum(TC_pos_sum_90), 
            TC_pos_counts_sum = sum(TC_pos_counts),)%>%
  mutate(TC_pos_final = TC_pos_sum_90*log(1+TC_pos_counts_sum))

write.table(TC_pos_daily_90 , file = "TC_pos_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TC_pos.tab, data_pos)
########################
########################
########################
#calculate sum of daily data
UT_pos <- UT %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    UT_pos_counts = n(),
    UT_pos_max = max(intensity),
    UT_pos_intensity = sum(intensity),
    UT_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, UT_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 90
UT_pos_daily_90 <- data.frame(UT_pos_sum = numeric(), 
                              UT_pos_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  UT_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(UT_pos_sum_90 = (UT_pos_iw * hl_weight_90)) %>%
    select("UT_pos_sum_90","UT_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  UT_pos_daily_90 <- rbind(UT_pos_daily_90,UT_pos.tab)}
UT_pos_daily_90 = UT_pos_daily_90 %>% 
  group_by(TradeDate) %>%
  summarise(UT_pos_sum_90 = sum(UT_pos_sum_90), 
            UT_pos_counts_sum = sum(UT_pos_counts),)%>%
  mutate(UT_pos_final = UT_pos_sum_90*log(1+UT_pos_counts_sum))

write.table(UT_pos_daily_90 , file = "UT_pos_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UT_pos.tab, data_pos)
########################
########################
########################
#calculate sum of daily data
TU_pos <- TU %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    TU_pos_counts = n(),
    TU_pos_max = max(intensity),
    TU_pos_intensity = sum(intensity),
    TU_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, TU_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 90
TU_pos_daily_90 <- data.frame(TU_pos_sum = numeric(), 
                              TU_pos_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  TU_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(TU_pos_sum_90 = (TU_pos_iw * hl_weight_90)) %>%
    select("TU_pos_sum_90","TU_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  TU_pos_daily_90 <- rbind(TU_pos_daily_90,TU_pos.tab)}
TU_pos_daily_90 = TU_pos_daily_90 %>% 
  group_by(TradeDate) %>%
  summarise(TU_pos_sum_90 = sum(TU_pos_sum_90), 
            TU_pos_counts_sum = sum(TU_pos_counts),)%>%
  mutate(TU_pos_final = TU_pos_sum_90*log(1+TU_pos_counts_sum))

write.table(TU_pos_daily_90 , file = "TU_pos_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TU_pos.tab, data_pos)
########################
########################
########################
#calculate sum of daily data
CU_pos <- CU %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    CU_pos_counts = n(),
    CU_pos_max = max(intensity),
    CU_pos_intensity = sum(intensity),
    CU_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, CU_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 90
CU_pos_daily_90 <- data.frame(CU_pos_sum = numeric(), 
                              CU_pos_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  CU_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(CU_pos_sum_90 = (CU_pos_iw * hl_weight_90)) %>%
    select("CU_pos_sum_90","CU_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  CU_pos_daily_90 <- rbind(CU_pos_daily_90,CU_pos.tab)}
CU_pos_daily_90 = CU_pos_daily_90 %>% 
  group_by(TradeDate) %>%
  summarise(CU_pos_sum_90 = sum(CU_pos_sum_90), 
            CU_pos_counts_sum = sum(CU_pos_counts),)%>%
  mutate(CU_pos_final = CU_pos_sum_90*log(1+CU_pos_counts_sum))

write.table(CU_pos_daily_90 , file = "CU_pos_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CU_pos.tab, data_pos)
########################
########################
########################
#calculate sum of daily data
UC_pos <- UC %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity >= 0) %>% group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    UC_pos_counts = n(),
    UC_pos_max = max(intensity),
    UC_pos_intensity = sum(intensity),
    UC_pos_iw = sum(intensity_weight) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_pos <- data.frame(TradeDate = date)
data_pos = full_join(data_pos, UC_pos, by = "TradeDate")
data_pos[is.na(data_pos)] <- 0
# data_pos = data_pos%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_pos$TradeDate
#############
v= 90
UC_pos_daily_90 <- data.frame(UC_pos_sum = numeric(), 
                              UC_pos_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  UC_pos.tab <- data_pos %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(UC_pos_sum_90 = (UC_pos_iw * hl_weight_90)) %>%
    select("UC_pos_sum_90","UC_pos_counts") %>% 
    mutate(TradeDate = date[v+i])
  UC_pos_daily_90 <- rbind(UC_pos_daily_90,UC_pos.tab)}
UC_pos_daily_90 = UC_pos_daily_90 %>% 
  group_by(TradeDate) %>%
  summarise(UC_pos_sum_90 = sum(UC_pos_sum_90), 
            UC_pos_counts_sum = sum(UC_pos_counts),)%>%
  mutate(UC_pos_final = UC_pos_sum_90*log(1+UC_pos_counts_sum))

write.table(UC_pos_daily_90 , file = "UC_pos_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UC_pos.tab, data_pos)
########################
#########################
############################
##RUI with filter only negative and summarize value in same day
#calculate sum of daily data
CT_neg <- CT %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>% 
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    CT_neg_counts = n(),
    CT_neg_max = min(intensity)*(-1),
    CT_neg_intensity = sum(intensity)*(-1),
    CT_neg_iw = sum(intensity_weight)*(-1) )

########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, CT_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 90
CT_neg_daily_90 <- data.frame(CT_neg_sum = numeric(), 
                              CT_neg_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  CT_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(CT_neg_sum_90 = (CT_neg_iw * hl_weight_90)) %>%
    select("CT_neg_sum_90","CT_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  CT_neg_daily_90 <- rbind(CT_neg_daily_90,CT_neg.tab)}
CT_neg_daily_90 = CT_neg_daily_90 %>% 
  group_by(TradeDate) %>%
  summarise(CT_neg_sum_90 = sum(CT_neg_sum_90), 
            CT_neg_counts_sum = sum(CT_neg_counts),)%>%
  mutate(CT_neg_final = CT_neg_sum_90*log(1+CT_neg_counts_sum))

write.table(CT_neg_daily_90 , file = "CT_neg_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CT_neg.tab, data_neg)
########################
########################
#calculate sum of daily data
TC_neg <- TC %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>%  
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    TC_neg_counts = n(),
    TC_neg_max = min(intensity)*(-1),
    TC_neg_intensity = sum(intensity)*(-1),
    TC_neg_iw = sum(intensity_weight)*(-1) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, TC_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 90
TC_neg_daily_90 <- data.frame(TC_neg_sum = numeric(), 
                              TC_neg_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  TC_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(TC_neg_sum_90 = (TC_neg_iw * hl_weight_90)) %>%
    select("TC_neg_sum_90","TC_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  TC_neg_daily_90 <- rbind(TC_neg_daily_90,TC_neg.tab)}
TC_neg_daily_90 = TC_neg_daily_90 %>% 
  group_by(TradeDate) %>%
  summarise(TC_neg_sum_90 = sum(TC_neg_sum_90), 
            TC_neg_counts_sum = sum(TC_neg_counts),)%>%
  mutate(TC_neg_final = TC_neg_sum_90*log(1+TC_neg_counts_sum))

write.table(TC_neg_daily_90 , file = "TC_neg_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TC_neg.tab, data_neg)
########################
########################
########################
#calculate sum of daily data
UT_neg <- UT %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>%  
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    UT_neg_counts = n(),
    UT_neg_max = min(intensity)*(-1),
    UT_neg_intensity = sum(intensity)*(-1),
    UT_neg_iw = sum(intensity_weight)*(-1) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, UT_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 90
UT_neg_daily_90 <- data.frame(UT_neg_sum = numeric(), 
                              UT_neg_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  UT_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(UT_neg_sum_90 = (UT_neg_iw * hl_weight_90)) %>%
    select("UT_neg_sum_90","UT_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  UT_neg_daily_90 <- rbind(UT_neg_daily_90,UT_neg.tab)}
UT_neg_daily_90 = UT_neg_daily_90 %>% 
  group_by(TradeDate) %>%
  summarise(UT_neg_sum_90 = sum(UT_neg_sum_90), 
            UT_neg_counts_sum = sum(UT_neg_counts),)%>%
  mutate(UT_neg_final = UT_neg_sum_90*log(1+UT_neg_counts_sum))

write.table(UT_neg_daily_90 , file = "UT_neg_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UT_neg.tab, data_neg)
########################
########################
########################
#calculate sum of daily data
TU_neg <- TU %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>%  
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    TU_neg_counts = n(),
    TU_neg_max = min(intensity)*(-1),
    TU_neg_intensity = sum(intensity)*(-1),
    TU_neg_iw = sum(intensity_weight)*(-1) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, TU_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 90
TU_neg_daily_90 <- data.frame(TU_neg_sum = numeric(), 
                              TU_neg_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  TU_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(TU_neg_sum_90 = (TU_neg_iw * hl_weight_90)) %>%
    select("TU_neg_sum_90","TU_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  TU_neg_daily_90 <- rbind(TU_neg_daily_90,TU_neg.tab)}
TU_neg_daily_90 = TU_neg_daily_90 %>% 
  group_by(TradeDate) %>%
  summarise(TU_neg_sum_90 = sum(TU_neg_sum_90), 
            TU_neg_counts_sum = sum(TU_neg_counts),)%>%
  mutate(TU_neg_final = TU_neg_sum_90*log(1+TU_neg_counts_sum))

write.table(TU_neg_daily_90 , file = "TU_neg_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(TU_neg.tab, data_neg)
########################
########################
########################
#calculate sum of daily data
CU_neg <- CU %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>%  
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    CU_neg_counts = n(),
    CU_neg_max = min(intensity)*(-1),
    CU_neg_intensity = sum(intensity)*(-1),
    CU_neg_iw = sum(intensity_weight)*(-1) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, CU_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 90
CU_neg_daily_90 <- data.frame(CU_neg_sum = numeric(), 
                              CU_neg_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  CU_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(CU_neg_sum_90 = (CU_neg_iw * hl_weight_90)) %>%
    select("CU_neg_sum_90","CU_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  CU_neg_daily_90 <- rbind(CU_neg_daily_90,CU_neg.tab)}
CU_neg_daily_90 = CU_neg_daily_90 %>% 
  group_by(TradeDate) %>%
  summarise(CU_neg_sum_90 = sum(CU_neg_sum_90), 
            CU_neg_counts_sum = sum(CU_neg_counts),)%>%
  mutate(CU_neg_final = CU_neg_sum_90*log(1+CU_neg_counts_sum))

write.table(CU_neg_daily_90 , file = "CU_neg_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(CU_neg.tab, data_neg)
########################
########################
########################
#calculate sum of daily data
UC_neg <- UC %>% arrange(TradeDate,desc(intensity))%>% 
  filter(intensity <0) %>%  
  group_by(TradeDate) %>%
  mutate(sigmoid_weight = rep(s_weight[1:n()], length.out = n()))%>%
  mutate(intensity_weight= intensity*sigmoid_weight)%>%
  summarise(
    UC_neg_counts = n(),
    UC_neg_max = min(intensity)*(-1),
    UC_neg_intensity = sum(intensity)*(-1),
    UC_neg_iw = sum(intensity_weight)*(-1) )
########
date <- seq(as.Date("1995-01-01"), as.Date("2020-03-31"), by = "days")
data_neg <- data.frame(TradeDate = date)
data_neg = full_join(data_neg, UC_neg, by = "TradeDate")
data_neg[is.na(data_neg)] <- 0
# data_neg = data_neg%>%
#   filter(TradeDate >= as.Date("2019-05-01") & TradeDate <= as.Date("2020-03-31") )
date = data_neg$TradeDate
#############
v= 90
UC_neg_daily_90 <- data.frame(UC_neg_sum = numeric(), 
                              UC_neg_counts = numeric(), 
                              Tradedate = as.Date(character()), 
                              stringsAsFactors = FALSE)
for (i in 1:(length(date)-v)) {
  print(i)
  UC_neg.tab <- data_neg %>% 
    filter(TradeDate >= date[i] & TradeDate < date[i+v]) %>%
    mutate(UC_neg_sum_90 = (UC_neg_iw * hl_weight_90)) %>%
    select("UC_neg_sum_90","UC_neg_counts") %>% 
    mutate(TradeDate = date[v+i])
  UC_neg_daily_90 <- rbind(UC_neg_daily_90,UC_neg.tab)}
UC_neg_daily_90 = UC_neg_daily_90 %>% 
  group_by(TradeDate) %>%
  summarise(UC_neg_sum_90 = sum(UC_neg_sum_90), 
            UC_neg_counts_sum = sum(UC_neg_counts),)%>%
  mutate(UC_neg_final = UC_neg_sum_90*log(1+UC_neg_counts_sum))

write.table(UC_neg_daily_90 , file = "UC_neg_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
rm(UC_neg.tab, data_neg)
########################
#########################

CT_all_daily_90 = full_join(CT_pos_daily_90, CT_neg_daily_90, by = "TradeDate")
CT_all_daily_90 = CT_all_daily_90 %>% 
  mutate(CT_diff = CT_pos_final - CT_neg_final)
TC_all_daily_90 = full_join(TC_pos_daily_90, TC_neg_daily_90, by = "TradeDate")
TC_all_daily_90 = TC_all_daily_90 %>% 
  mutate(TC_diff = TC_pos_final - TC_neg_final)
UT_all_daily_90 = full_join(UT_pos_daily_90, UT_neg_daily_90, by = "TradeDate")
UT_all_daily_90 = UT_all_daily_90 %>% 
  mutate(UT_diff = UT_pos_final - UT_neg_final)
TU_all_daily_90 = full_join(TU_pos_daily_90, TU_neg_daily_90, by = "TradeDate")
TU_all_daily_90 = TU_all_daily_90 %>% 
  mutate(TU_diff = TU_pos_final - TU_neg_final)
CU_all_daily_90 = full_join(CU_pos_daily_90, CU_neg_daily_90, by = "TradeDate")
CU_all_daily_90 = CU_all_daily_90 %>% 
  mutate(CU_diff = CU_pos_final - CU_neg_final)
UC_all_daily_90 = full_join(UC_pos_daily_90, UC_neg_daily_90, by = "TradeDate")
UC_all_daily_90 = UC_all_daily_90 %>% 
  mutate(UC_diff = UC_pos_final - UC_neg_final)

write.table(CT_all_daily_90 , file = "CT_all_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(TC_all_daily_90 , file = "TC_all_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(CU_all_daily_90 , file = "CU_all_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(UC_all_daily_90 , file = "UC_all_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(TU_all_daily_90 , file = "TU_all_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
write.table(UT_all_daily_90 , file = "UT_all_daily_90.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
