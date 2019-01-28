# 1(a)
phi0 <- 0
phi1 <- 1.1
phi2 <- -0.25

size <- 10000
w <- rep(0, size)

w[1] <- 0.5 
w[2] <- 0.1

epsilon <- rnorm(size, 0,1)

for(i in 3:size) {
  w[i] = phi0 + phi1 * w[i-1] + phi2 * w[i-2] + epsilon[i]
}

plot(w, x=1:size, type="l")

acf(w, lag.max = 20)

# 1(b)
x1 <- (phi1 + sqrt(phi1^2 + 4 * phi2))/(-2 * phi2)
x2 <- (phi1 - sqrt(phi1^2 + 4 * phi2))/(-2 * phi2)

w1 <- 1/x1
w2 <- 1/x2

# 1(c)
multiplier1 <- phi1^6

# 1(d)
phi1 <- 0.9
phi2 <- 0.8
x1 <- (phi1 + sqrt(phi1^2 + 4 * phi2))/(-2 * phi2)
x2 <- (phi1 - sqrt(phi1^2 + 4 * phi2))/(-2 * phi2)
multiplier2 <- phi1^6

# 2 
#2.1
library(lubridate)
library(xts)
par(mfrow=c(2,2))
ppi_raw <- read.csv("PPIFGS.csv")
ppi_xts <- xts(x=as.double(ppi_raw$VALUE), ymd(ppi_raw$DATE))
#2.a
plot(ppi_xts, main ="Main PPI in levels")
#2.b
ppi_diff <- diff(ppi_xts)[-1]
plot(ppi_diff, main = "Change in PPI")
#2.c
ppi_log <- log(ppi_xts)
plot(ppi_diff, main = "Log in PPI")
#2.d
ppi_diff_log <- diff(ppi_log)[-1]
plot(ppi_diff_log, main = "Change Log in PPI")



