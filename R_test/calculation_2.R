library(quantreg)
library(ggplot2)
library(dplyr)
library(vars)
library(forecast)
library(car)
library(far)
library(tseries)
library(grid)
#########
extract_varirf <- function(...){
  
  varirf_object <- list(...) #list one or more varirf input objects
  
  get_vec_length <- function(list_item){nrow(list_item[[1]][[1]])}
  
  if (!("varirf" %in% mapply(class, varirf_object))){
    stop("this function only accepts 'varirf' class objects")
  }
  
  if (length(unique(mapply(class, varirf_object)))!=1){
    stop("all input items must be 'varirf' class objects")
  }    
  if (length(unique(mapply(get_vec_length, varirf_object)))!=1){
    stop("all irf vectors must have the same length")   
  }  
  
  period <- as.data.frame(0:(nrow(varirf_object[[1]][[1]][[1]])-1)) 
  names(period) <- "period"
  
  for (l in 1:length(varirf_object)){
    for (i in 1:3){
      for (j in 1:dim(varirf_object[[l]][[i]][[1]])[2]){
        for (k in 1:length(varirf_object[[l]][[1]])){
          temp_colname <- paste(names(varirf_object[[l]][i]), #vector type (irf, lower, or upper)
                                names(varirf_object[[l]][[i]])[k], #impulse name
                                colnames(varirf_object[[l]][[i]][[k]])[j], #response name
                                sep = "_")
          
          temp <- as.data.frame(varirf_object[[l]][[i]][[k]][, j]) #extracts the vector
          
          names(temp) <- temp_colname #add the column name (vectortype_impulse_reponse)
          period <- cbind(period, temp) 
        }
        
      }
    }
  }
  names(period) <- tolower(names(period))
  return(period)
}
#####


setwd("D:/data/new_RUI")
data =read.table("D:/data/new_RUI/merge_data3.csv", header = T, sep = ",",fill = TRUE)
data$TradeDate = as.Date(data$TradeDate)
#################
sum(is.na(data$TU_neg_counts))
length(data$TradeDate)
data[is.na(data)] <- 0
data_CT = data.frame(
  day = as.Date(data$TradeDate),
  A = data$TU_pos_counts,
  B = data$TU_neg_counts*(-1)
)

ts.plot(data_CT)

p <- ggplot(data = data_CT, aes(x=day, y=A))+ 
  geom_line(aes(x=day, y=B)) +
  geom_line(aes(x=day, y=A)) +
  theme_minimal()

p
#############
#Quantile Regression
x <- data$CT_diff_30 
adf.test(x)
y <- data$return_log 
dat <- data.frame(x,y)
ggplot(dat, aes(x,y)) + geom_point()

qs <- seq(0.05, 0.95, by = 0.05)
qr2 <- rq(y ~ x, data=dat, tau = qs)
coef(qr2)
summary(qr2)
lm.m = lm(y~1+x)
summary(lm.m)


data_set_1 = data %>% 
  dplyr::select("return_log","CT_sum_30","TC_sum_30", "UT_sum_30",
                "TU_sum_30","CU_sum_30","UC_sum_30")
data_set_2 = data %>% 
  dplyr::select("return_log","CT_sum_90","TC_sum_90", "UT_sum_90",
                "TU_sum_90","CU_sum_90","UC_sum_90")
# data_set_1 = data %>% 
#   dplyr::select("return_log","return_log_diff",
#                 "CT_sum_30","CT_sum_60","CT_sum_90","CT_diff_30","CT_diff_60","CT_diff_90",
#                 "TC_sum_30","TC_sum_60","TC_sum_90","TC_diff_30","TC_diff_60","TC_diff_90",
#                 "UT_sum_30","UT_sum_60","UT_sum_90","UT_diff_30","UT_diff_60","UT_diff_90",
#                 "TU_sum_30","TU_sum_60","TU_sum_90","TU_diff_30","TU_diff_60","TU_diff_90",
#                 "CU_sum_30","CU_sum_60","CU_sum_90","CU_diff_30","CU_diff_60","CU_diff_90",
#                 "UC_sum_30","UC_sum_60","UC_sum_90","UC_diff_30","UC_diff_60","UC_diff_90")

which(is.na(data_set))
var <- VAR(data_set_1, type = c("const"), lag.max = 12, ic = c("SC"))
var2 <- VAR(data_set_2, type = c("const"), lag.max = 12, ic = c("SC"))

irf_asy <- irf(var, impulse = "TU_sum_30", response = "return_log", n.ahead = 10, ortho = TRUE,
               cumulative = FALSE, boot = TRUE, ci = 0.9, runs = 100)

irf_asy <- irf(var2, impulse = "UT_sum_90", response = "return_log", n.ahead = 10, ortho = TRUE,
               cumulative = FALSE, boot = TRUE, ci = 0.9, runs = 100)
# png("irf_asy.png", width = 700, height = 500)
# plot(irf_asy)
single_varirf <- extract_varirf(irf_asy)
head(single_varirf)
asy_asy <- single_varirf %>% 
  ggplot(aes(x=period, y=irf_ut_sum_90_return_log, ymin=lower_ut_sum_90_return_log,
             ymax=upper_ut_sum_90_return_log)) +
  geom_hline(yintercept = 0, color="red") +
  geom_ribbon(fill="grey", alpha=.2, color="grey50", linetype="dashed") +
  geom_line() +
  theme_light() +
  # ggtitle("Orthogonal impulse response, TU_sum_30 - return_log")+
  ylab("log(return)")+
  labs(title = "Orthogonal impulse response, TU - return",
       subtitle = "U_sum_30 - return_log",
       caption = "Data source: TEJ, ICEWS")+
  xlab("") +
  theme(plot.title = element_text(size = 11, hjust=0.5),
        # plot.title = element_text(color = "red", size = 12, face = "bold"),
        # plot.subtitle = element_text(color = "blue"),
        # plot.caption = element_text(color = "green", face = "italic"),
  )


ggsave("asy_asy.png", asy_asy, width=6, height=4)

asy_asy
