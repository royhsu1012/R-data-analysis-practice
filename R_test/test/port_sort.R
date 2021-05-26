# Load the portsort package and the pre-loaded data
# install.packages("portsort")
#install.packages("PerformanceAnalytic")

library(portsort)
library(PerformanceAnalytics)
library(xts)
data(Factors)
Factors
F = Factors
write.table(F,file="123123.csv")
# Lagged returns, lagged volumes are stored in the Factors list
R.Forward = Factors[[1]]; R.Lag = Factors[[2]]; V.Lag = Factors[[3]]
Fa = R.Lag; Fb = V.Lag
#Specify the dimension of the sort - let's use terciles
dimA = 0:3/3;dimB = 0:3/3;dimC = c(0,1)
# Run the conditional sort function 
sort.output.con = conditional.sort(Fa,Fb,Fc=NULL,R.Forward,dimA,dimB,dimC)
# Run the unconditional sort function 
sort.output.uncon = unconditional.sort(Fa,Fb,Fc=NULL,R.Forward,dimA,dimB,dimC)

# Investigate mean portfolio size - conditional sort
portfolio.mean.size(sort.output = sort.output.con)