# HW 1 
library(xts)
library(moments)
library(ggplot2)
library(lubridate)
library(data.table)
library(sandwich)


# Problem 1
n <- 600
mu <- 0.00
sd <- 0.063
rt <- rnorm(600, mu, sd)
par(mfrow=c(3,1))
plot(rt, ylab="Log returns", xlab="Month",type="l", main="Log Returns")

# part 2 
# It has fat tail and jumps as compared to normal distribution

# part 3
#simulate using jump model 
mean <- 0.012
sigma <- 0.05
prob <- 0.15
mu_j <- -0.03
sigma_j <- 0.1

set.seed(1234)
#simulate bernoulli
B_t <- rbinom(n, 1, prob)
theta_t <- rnorm(n,0,1)
epsilon_t <- rnorm(n,0,1)
J_t <- B_t * (mu_j + sigma_j * theta_t)
r_t <- mean + sigma * epsilon_t + J_t

# Missing: No Clustering and Jump y_t and y_t+1 no related
plot(r_t, ylab="Log returns", xlab="Month",type="l", main="Simulation With Jump Model")
simmean <- mean(r_t)
simvar <- var(r_t)
simskew <- skewness(r_t)
simkurt <- kurtosis(r_t)

#Market log normal return 
mkt_monthly_raw <- read.csv("CRSP_market_monthly.csv",header = TRUE, sep=",", skip=4)[-3,]
colnames(mkt_monthly_raw) <- c("caldt","a","b","sprtrn","indx")
mkt_monthly <- xts(x=mkt_monthly_raw$sprtrn, ymd(mkt_monthly_raw$caldt))
mkt_logret <- log(mkt_monthly + 1)
# Missing: No Clustering and Jump y_t and y_t+1 no related
plot(mkt_logret[200:800], ylab="Log returns", xlab="Month",type="l", main = "Market Return")

# problem 2, 1(a)
# using adjusted price for getting return
dbv_raw <- read.csv("DBV.csv", header=TRUE, sep=",")
gspc_raw <- read.csv("GSPC.csv", header = TRUE, sep=",")

dbv_ts <- xts(x =dbv_raw$Adj.Close, as.POSIXct(dbv_raw$Date, format="%d/%m/%Y"))
gspc_ts <- xts(x=gspc_raw$Adj.Close, as.POSIXct(gspc_raw$Date, format="%d/%m/%Y"))

dbv_logret <- diff(log(dbv_ts), lag =1)[-1,]
gspc_logret <- diff(log(gspc_ts), lag=1)[-1,]

par(mfrow=c(2,1))
plot(dbv_logret, ylab = "Daily Log Return", xlab="Time",main ="DBV")
plot(gspc_logret, ylab = "Daily Log Return", xlab="Time",main ="GSPC")

#1b
colnames(dbv_logret) <- paste0("Log_Return")
colnames(gspc_logret) <- paste0("Log_Return")
ggplot(dbv_logret, aes(x=Log_Return))  + geom_histogram(colour="black", fill="white",bins=100) + ggtitle("DBV Log Return")
ggplot(gspc_logret, aes(x=Log_Return)) + geom_histogram(colour= "black", fill="white",bins=100)+ ggtitle("GSPC Log Return")

# 2(a) - (c)
testSkewness <- function(skew_val,size){
  return(skew_val/sqrt(6/size))
}

testKurtosis <- function(kurt_val, size){
  return((kurt_val-3)/sqrt(24/size))
}

Jarque_Bera_test <- function(skew_val, kurt_val, size){
  
  return((skew_val^2)/(6/size) + (kurt_val -3)^2/(24/size))
}

tval_dbv_skew <- testSkewness(skewness(dbv_logret), length(dbv_logret))    # reject
tval_gspc_skew <- testSkewness(skewness(gspc_logret), length(gspc_logret)) # reject

tval_dbv_kurt <- testKurtosis(kurtosis(dbv_logret), length(dbv_logret))    # reject
tval_gspc_kurt <- testKurtosis(kurtosis(gspc_logret), length(gspc_logret)) # reject

dbv_jb_result <- Jarque_Bera_test(skew_val = skewness(dbv_logret), kurt_val = kurtosis(dbv_logret), size = length(dbv_logret)) # reject
gspc_jb_result <- Jarque_Bera_test(skew_val = skewness(gspc_logret), kurt_val = kurtosis(gspc_logret), size = length(gspc_logret)) #reject

tval_sp500_skew <- testSkewness(skewness(sp500_logret), length(sp500_logret)) # reject
tval_sp500_kurt <- testKurtosis(kurtosis(sp500_logret), length(sp500_logret)) # reject

# 3
DT <- data.table(
  tests = c("Skewness", "t-test","Kurtosis", "t-test", "JB-test"),
  DBV = c(skewness(dbv_logret), tval_dbv_skew, kurtosis(dbv_logret), tval_dbv_kurt, dbv_jb_result),
  GSPC = c(skewness(gspc_logret), tval_gspc_skew, kurtosis(gspc_logret), tval_gspc_kurt, gspc_jb_result)
)
DT

# 4
# Sharp ratio only uses mean and variance, the first two moments. 
# DBV is more risk because of higher kurtosis which leads a fat tail. 
# Also currency trading is highly leverage.

# 5
# 
reg <- lm(as.numeric(dbv_logret) ~ as.numeric(gspc_logret))
summary(reg)
white_stderr = sqrt(diag(vcovHC(x=reg, type = "HC")))






