#VAR
#QQA
#OLS
#BVAR
library(ggplot2)
library(dplyr)
library(vars)
library(bvar)
library(forecast)
library(car)
library(far)
library(tseries)
#################
setwd("D:/data/Data_for_analysis")
tw_data <-read.table("D:/data/Data_for_analysis/tw_stock_return.csv", header = T, sep = ",",fill = TRUE)

setwd("D:/data/new_RUI")
CT_data_30 <-read.table("D:/data/new_RUI/CT_all_daily_30.csv", header = T, sep = ",",fill = TRUE)
TC_data_30 <-read.table("D:/data/new_RUI/TC_all_daily_30.csv", header = T, sep = ",",fill = TRUE)
UT_data_30 <-read.table("D:/data/new_RUI/UT_all_daily_30.csv", header = T, sep = ",",fill = TRUE)
TU_data_30 <-read.table("D:/data/new_RUI/TU_all_daily_30.csv", header = T, sep = ",",fill = TRUE)
CU_data_30 <-read.table("D:/data/new_RUI/CU_all_daily_30.csv", header = T, sep = ",",fill = TRUE)
UC_data_30 <-read.table("D:/data/new_RUI/UC_all_daily_30.csv", header = T, sep = ",",fill = TRUE)

CT_data_60 <-read.table("D:/data/new_RUI/CT_all_daily_60.csv", header = T, sep = ",",fill = TRUE)
TC_data_60 <-read.table("D:/data/new_RUI/TC_all_daily_60.csv", header = T, sep = ",",fill = TRUE)
UT_data_60 <-read.table("D:/data/new_RUI/UT_all_daily_60.csv", header = T, sep = ",",fill = TRUE)
TU_data_60 <-read.table("D:/data/new_RUI/TU_all_daily_60.csv", header = T, sep = ",",fill = TRUE)
CU_data_60 <-read.table("D:/data/new_RUI/CU_all_daily_60.csv", header = T, sep = ",",fill = TRUE)
UC_data_60 <-read.table("D:/data/new_RUI/UC_all_daily_60.csv", header = T, sep = ",",fill = TRUE)

CT_data_90 <-read.table("D:/data/new_RUI/CT_all_daily_90.csv", header = T, sep = ",",fill = TRUE)
TC_data_90 <-read.table("D:/data/new_RUI/TC_all_daily_90.csv", header = T, sep = ",",fill = TRUE)
UT_data_90 <-read.table("D:/data/new_RUI/UT_all_daily_90.csv", header = T, sep = ",",fill = TRUE)
TU_data_90 <-read.table("D:/data/new_RUI/TU_all_daily_90.csv", header = T, sep = ",",fill = TRUE)
CU_data_90 <-read.table("D:/data/new_RUI/CU_all_daily_90.csv", header = T, sep = ",",fill = TRUE)
UC_data_90 <-read.table("D:/data/new_RUI/UC_all_daily_90.csv", header = T, sep = ",",fill = TRUE)

CT_data_30 <- CT_data_30 %>% 
  # # select("TradeDate","CT_pos_final","CT_neg_final","CT_diff")%>%
  rename(CT_pos_final_30 = CT_pos_final)%>%
  rename(CT_neg_final_30 = CT_neg_final)%>%
  rename(CT_pos_counts_30 = CT_pos_counts_sum)%>%
  rename(CT_neg_counts_30 = CT_neg_counts_sum)%>%
  rename(CT_sum_30 = CT_diff)%>%
  mutate(CT_diff_30 = CT_sum_30 - lag(CT_sum_30))
TC_data_30 <- TC_data_30 %>% 
  # select("TradeDate","TC_pos_final","TC_neg_final","TC_diff")%>%
  rename(TC_pos_final_30 = TC_pos_final)%>%
  rename(TC_neg_final_30 = TC_neg_final)%>%
  rename(TC_pos_counts_30 = TC_pos_counts_sum)%>%
  rename(TC_neg_counts_30 = TC_neg_counts_sum)%>%
  rename(TC_sum_30 = TC_diff)%>%
  mutate(TC_diff_30 = TC_sum_30 - lag(TC_sum_30))
UT_data_30 <- UT_data_30 %>% 
  # select("TradeDate","UT_pos_final","UT_neg_final","UT_diff")%>%
  rename(UT_pos_final_30 = UT_pos_final)%>%
  rename(UT_neg_final_30 = UT_neg_final)%>%
  rename(UT_pos_counts_30 = UT_pos_counts_sum)%>%
  rename(UT_neg_counts_30 = UT_neg_counts_sum)%>%
  rename(UT_sum_30 = UT_diff)%>%
  mutate(UT_diff_30 = UT_sum_30-lag(UT_sum_30))
TU_data_30 <- TU_data_30 %>% 
  # select("TradeDate","TU_pos_final","TU_neg_final","TU_diff")%>%
  rename(TU_pos_final_30 = TU_pos_final)%>%
  rename(TU_neg_final_30 = TU_neg_final)%>%
  rename(TU_pos_counts_30 = TU_pos_counts_sum)%>%
  rename(TU_neg_counts_30 = TU_neg_counts_sum)%>%
  rename(TU_sum_30 = TU_diff)%>%
  mutate(TU_diff_30 = TU_sum_30-lag(TU_sum_30))
CU_data_30 <- CU_data_30 %>% 
  # select("TradeDate","CU_pos_final","CU_neg_final","CU_diff")%>%
  rename(CU_pos_final_30 = CU_pos_final)%>%
  rename(CU_neg_final_30 = CU_neg_final)%>%
  rename(CU_pos_counts_30 = CU_pos_counts_sum)%>%
  rename(CU_neg_counts_30 = CU_neg_counts_sum)%>%
  rename(CU_sum_30 = CU_diff)%>%
  mutate(CU_diff_30 = CU_sum_30-lag(CU_sum_30))
UC_data_30 <- UC_data_30 %>% 
  # select("TradeDate","UC_pos_final","UC_neg_final","UC_diff")%>% 
  rename(UC_pos_final_30 = UC_pos_final)%>%
  rename(UC_neg_final_30 = UC_neg_final)%>%
  rename(UC_pos_counts_30 = UC_pos_counts_sum)%>%
  rename(UC_neg_counts_30 = UC_neg_counts_sum)%>%
  rename(UC_sum_30 = UC_diff)%>%
  mutate(UC_diff_30 = UC_sum_30-lag(UC_sum_30))

####60
CT_data_60 <- CT_data_60 %>% 
  # select("TradeDate","CT_pos_final","CT_neg_final","CT_diff")%>%
  rename(CT_pos_final_60 = CT_pos_final)%>%
  rename(CT_neg_final_60 = CT_neg_final)%>%
  rename(CT_pos_counts_60 = CT_pos_counts_sum)%>%
  rename(CT_neg_counts_60 = CT_neg_counts_sum)%>%
  rename(CT_sum_60 = CT_diff)%>%
  mutate(CT_diff_60 = CT_sum_60 - lag(CT_sum_60))
TC_data_60 <- TC_data_60 %>% 
  # select("TradeDate","TC_pos_final","TC_neg_final","TC_diff")%>%
  rename(TC_pos_final_60 = TC_pos_final)%>%
  rename(TC_neg_final_60 = TC_neg_final)%>%
  rename(TC_pos_counts_60 = TC_pos_counts_sum)%>%
  rename(TC_neg_counts_60 = TC_neg_counts_sum)%>%
  rename(TC_sum_60 = TC_diff)%>%
  mutate(TC_diff_60 = TC_sum_60 - lag(TC_sum_60))
UT_data_60 <- UT_data_60 %>% 
  # select("TradeDate","UT_pos_final","UT_neg_final","UT_diff")%>%
  rename(UT_pos_final_60 = UT_pos_final)%>%
  rename(UT_neg_final_60 = UT_neg_final)%>%
  rename(UT_pos_counts_60 = UT_pos_counts_sum)%>%
  rename(UT_neg_counts_60 = UT_neg_counts_sum)%>%
  rename(UT_sum_60 = UT_diff)%>%
  mutate(UT_diff_60 = UT_sum_60-lag(UT_sum_60))
TU_data_60 <- TU_data_60 %>% 
  # select("TradeDate","TU_pos_final","TU_neg_final","TU_diff")%>%
  rename(TU_pos_final_60 = TU_pos_final)%>%
  rename(TU_neg_final_60 = TU_neg_final)%>%
  rename(TU_pos_counts_60 = TU_pos_counts_sum)%>%
  rename(TU_neg_counts_60 = TU_neg_counts_sum)%>%
  rename(TU_sum_60 = TU_diff)%>%
  mutate(TU_diff_60 = TU_sum_60-lag(TU_sum_60))
CU_data_60 <- CU_data_60 %>% 
  # select("TradeDate","CU_pos_final","CU_neg_final","CU_diff")%>%
  rename(CU_pos_final_60 = CU_pos_final)%>%
  rename(CU_neg_final_60 = CU_neg_final)%>%
  rename(CU_pos_counts_60 = CU_pos_counts_sum)%>%
  rename(CU_neg_counts_60 = CU_neg_counts_sum)%>%
  rename(CU_sum_60 = CU_diff)%>%
  mutate(CU_diff_60 = CU_sum_60-lag(CU_sum_60))
UC_data_60 <- UC_data_60 %>% 
  # select("TradeDate","UC_pos_final","UC_neg_final","UC_diff")%>% 
  rename(UC_pos_final_60 = UC_pos_final)%>%
  rename(UC_neg_final_60 = UC_neg_final)%>%
  rename(UC_pos_counts_60 = UC_pos_counts_sum)%>%
  rename(UC_neg_counts_60 = UC_neg_counts_sum)%>%
  rename(UC_sum_60 = UC_diff)%>%
  mutate(UC_diff_60 = UC_sum_60-lag(UC_sum_60))

###########90
CT_data_90 <- CT_data_90 %>% 
  # select("TradeDate","CT_pos_final","CT_neg_final","CT_diff")%>%
  rename(CT_pos_final_90 = CT_pos_final)%>%
  rename(CT_neg_final_90 = CT_neg_final)%>%
  rename(CT_pos_counts_90 = CT_pos_counts_sum)%>%
  rename(CT_neg_counts_90 = CT_neg_counts_sum)%>%
  rename(CT_sum_90 = CT_diff)%>%
  mutate(CT_diff_90 = CT_sum_90 - lag(CT_sum_90))
TC_data_90 <- TC_data_90 %>% 
  # select("TradeDate","TC_pos_final","TC_neg_final","TC_diff")%>%
  rename(TC_pos_final_90 = TC_pos_final)%>%
  rename(TC_neg_final_90 = TC_neg_final)%>%
  rename(TC_pos_counts_90 = TC_pos_counts_sum)%>%
  rename(TC_neg_counts_90 = TC_neg_counts_sum)%>%
  rename(TC_sum_90 = TC_diff)%>%
  mutate(TC_diff_90 = TC_sum_90 - lag(TC_sum_90))
UT_data_90 <- UT_data_90 %>% 
  # select("TradeDate","UT_pos_final","UT_neg_final","UT_diff")%>%
  rename(UT_pos_final_90 = UT_pos_final)%>%
  rename(UT_neg_final_90 = UT_neg_final)%>%
  rename(UT_pos_counts_90 = UT_pos_counts_sum)%>%
  rename(UT_neg_counts_90 = UT_neg_counts_sum)%>%
  rename(UT_sum_90 = UT_diff)%>%
  mutate(UT_diff_90 = UT_sum_90-lag(UT_sum_90))
TU_data_90 <- TU_data_90 %>% 
  # select("TradeDate","TU_pos_final","TU_neg_final","TU_diff")%>%
  rename(TU_pos_final_90 = TU_pos_final)%>%
  rename(TU_neg_final_90 = TU_neg_final)%>%
  rename(TU_pos_counts_90 = TU_pos_counts_sum)%>%
  rename(TU_neg_counts_90 = TU_neg_counts_sum)%>%
  rename(TU_sum_90 = TU_diff)%>%
  mutate(TU_diff_90 = TU_sum_90-lag(TU_sum_90))
CU_data_90 <- CU_data_90 %>% 
  # select("TradeDate","CU_pos_final","CU_neg_final","CU_diff")%>%
  rename(CU_pos_final_90 = CU_pos_final)%>%
  rename(CU_neg_final_90 = CU_neg_final)%>%
  rename(CU_pos_counts_90 = CU_pos_counts_sum)%>%
  rename(CU_neg_counts_90 = CU_neg_counts_sum)%>%
  rename(CU_sum_90 = CU_diff)%>%
  mutate(CU_diff_90 = CU_sum_90-lag(CU_sum_90))
UC_data_90 <- UC_data_90 %>% 
  # select("TradeDate","UC_pos_final","UC_neg_final","UC_diff")%>% 
  rename(UC_pos_final_90 = UC_pos_final)%>%
  rename(UC_neg_final_90 = UC_neg_final)%>%
  rename(UC_pos_counts_90 = UC_pos_counts_sum)%>%
  rename(UC_neg_counts_90 = UC_neg_counts_sum)%>%
  rename(UC_sum_90 = UC_diff)%>%
  mutate(UC_diff_90 = UC_sum_90-lag(UC_sum_90))


data_all = full_join(CT_data_30,TC_data_30, by = "TradeDate") 
data_all = full_join(data_all, UT_data_30, by = "TradeDate")
data_all = full_join(data_all, TU_data_30, by = "TradeDate")
data_all = full_join(data_all, CU_data_30, by = "TradeDate")
data_all = full_join(data_all, UC_data_30, by = "TradeDate")
data_all = full_join(data_all, CT_data_60, by = "TradeDate")
data_all = full_join(data_all, TC_data_60, by = "TradeDate")
data_all = full_join(data_all, UT_data_60, by = "TradeDate")
data_all = full_join(data_all, TU_data_60, by = "TradeDate")
data_all = full_join(data_all, CU_data_60, by = "TradeDate")
data_all = full_join(data_all, UC_data_60, by = "TradeDate")
data_all = full_join(data_all, CT_data_90, by = "TradeDate")
data_all = full_join(data_all, TC_data_90, by = "TradeDate")
data_all = full_join(data_all, UT_data_90, by = "TradeDate")
data_all = full_join(data_all, TU_data_90, by = "TradeDate")
data_all = full_join(data_all, CU_data_90, by = "TradeDate")
data_all = full_join(data_all, UC_data_90, by = "TradeDate")
data_all$TradeDate = as.Date(data_all$TradeDate)

which(is.na(data_all))
write.table(data_all , file = "all_daily_clean2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")


tw_data = tw_data %>% rename(TradeDate = MDATE)%>% 
  rename(return = ROI) %>% rename(return_log = ROIB)
tw_data = tw_data %>% 
  mutate(return_log_diff = return_log-lag(return_log))
tw_data$TradeDate = as.Date(tw_data$TradeDate)
write.table(tw_data , file = "tw_data2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")

########################
merge_data = merge(tw_data,data_all,by="TradeDate")
merge_data = merge_data%>%
  filter(TradeDate >= as.Date("1995-05-01") & TradeDate <= as.Date("2020-03-31") )
which(is.na(merge_data))

write.table(merge_data , file = "merge_data2.csv", sep = ",", quote = FALSE, append = FALSE, na = "NA")
