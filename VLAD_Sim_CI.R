rm(list = ls())# reset variables.
setwd("C:/Users/1998c/OneDrive/Skrivebord/Masteroppgave")
library(spcadjust)
#creating datasets
nbase1 <- 100 
nbase2 <- 500
nbase3 <- 1000
nbase4 <- 1500
set.seed(10)
xlogregbase1 <- data.frame(x1 = rbinom(nbase1, 1, 0.2), x2 = runif(nbase1, 0, 1), x3 = rnorm(nbase1, 0, 2))
xlogregbase2 <- data.frame(x1 = rbinom(nbase2, 1, 0.2), x2 = runif(nbase2, 0, 1), x3 = rnorm(nbase2, 0, 2))
xlogregbase3 <- data.frame(x1 = rbinom(nbase3, 1, 0.2), x2 = runif(nbase3, 0, 1), x3 = rnorm(nbase3, 0, 2))
xlogregbase4 <- data.frame(x1 = rbinom(nbase4, 1, 0.2), x2 = runif(nbase4, 0, 1), x3 = rnorm(nbase4, 0, 2))
mrun <- 1000
xlogregrun <- data.frame(x1 = rbinom(mrun, 1, 0.2), x2 = runif(mrun, 0, 1), x3 = rnorm(mrun, 0, 2))

xbetabase1 <- xlogregbase1$x1 + xlogregbase1$x2 + xlogregbase1$x3
xbetabase2 <- xlogregbase2$x1 + xlogregbase2$x2 + xlogregbase2$x3
xbetabase3 <- xlogregbase3$x1 + xlogregbase3$x2 + xlogregbase3$x3
xbetabase4 <- xlogregbase4$x1 + xlogregbase4$x2 + xlogregbase4$x3
xbetarun <- xlogregrun$x1 + xlogregrun$x2 + xlogregrun$x3

#Creating response for base
xlogregbase1$y <- rbinom(nbase1, 1, exp(xbetabase1)/(1+exp(xbetabase1)))#creating response
xlogregbase2$y <- rbinom(nbase2, 1, exp(xbetabase2)/(1+exp(xbetabase2)))
xlogregbase3$y <- rbinom(nbase3, 1, exp(xbetabase3)/(1+exp(xbetabase3)))
xlogregbase4$y <- rbinom(nbase4, 1, exp(xbetabase4)/(1+exp(xbetabase4)))

#Creating response with change (delta) for rundata
Delta <- -0.41
xlogregrun$y <- rbinom(mrun, 1, exp(Delta+xbetarun)/(1+exp(Delta+xbetarun)))
#Calculate deltas for CUSUM
m1.base <- mean(xlogregbase1$y)
m1.base
m2.base <- mean(xlogregbase2$y)
m2.base
m3.base <- mean(xlogregbase3$y)
m3.base
m4.base <- mean(xlogregbase4$y)
m4.base
m.run <- mean(xlogregrun$y)


#Regression Models
t <- seq(from = 1, to = 1000, by = 1)
glm.reg.risk1 <- glm(y ~ x1+x2+x3, data = xlogregbase1, family = binomial("logit"))
glm.reg.risk2 <- glm(y ~ x1+x2+x3, data = xlogregbase2, family = binomial("logit"))
glm.reg.risk3 <- glm(y ~ x1+x2+x3, data = xlogregbase3, family = binomial("logit"))
glm.reg.risk4 <- glm(y ~ x1+x2+x3, data = xlogregbase4, family = binomial("logit"))
summary(glm.reg.risk1)
summary(glm.reg.risk2)
summary(glm.reg.risk3)
summary(glm.reg.risk4)
glm.reg.norisk1 <- glm(y ~ 1, data = xlogregbase1, family = binomial("logit"))
glm.reg.norisk2 <- glm(y ~ 1, data = xlogregbase2, family = binomial("logit"))
glm.reg.norisk3 <- glm(y ~ 1, data = xlogregbase3, family = binomial("logit"))
glm.reg.norisk4 <- glm(y ~ 1, data = xlogregbase4, family = binomial("logit"))
summary(glm.reg.norisk1)
summary(glm.reg.norisk2)
summary(glm.reg.norisk3)
summary(glm.reg.norisk4)
p.0.norisk1 <- mean(xlogregbase1$y)
p.0.norisk2 <- mean(xlogregbase2$y)
p.0.norisk3 <- mean(xlogregbase3$y)
p.0.norisk4 <- mean(xlogregbase4$y)
glm.reg.run.risk <- glm(y ~ x1+x2+x3, data = xlogregrun, family = binomial("logit"))
summary(glm.reg.run.risk)
p.1.risk <- predict(glm.reg.run.risk, newdata = xlogregrun, type = "response")
glm.reg.run.norisk <- glm(y ~ 1, data = xlogregrun, family = binomial("logit"))
summary(glm.reg.run.norisk)
p.1.norisk <- cumsum(xlogregrun$y)/t

#plots
par(mfrow = c(2,2))
plot(x=t, y=p.1.norisk*(1-p.1.norisk), ylab = "variance", type = "l", main = "p.1 vs p.0", ylim = c(0, 0.5))
abline(h = p.0.norisk1*(1-p.0.norisk1), col = "blue")
plot(x=t, y=p.1.norisk*(1-p.1.norisk), ylab = "variance", type = "l", main = "p.1 vs p.0", ylim = c(0, 0.5))
abline(h = p.0.norisk2*(1-p.0.norisk2), col = "blue")
plot(x=t, y=p.1.norisk*(1-p.1.norisk), ylab = "variance", type = "l", main = "p.1 vs p.0", ylim = c(0, 0.5))
abline(h = p.0.norisk3*(1-p.0.norisk3), col = "blue")
plot(x=t, y=p.1.norisk*(1-p.1.norisk), ylab = "variance", type = "l", main = "p.1 vs p.0", ylim = c(0, 0.5))
abline(h = p.0.norisk4*(1-p.0.norisk4), col = "blue")


#VLAD plots no risk
j1 <- seq(from = 1, to = 1000)
j2 <- seq(from = 1, to = 1000)
j3 <- seq(from = 1, to = 1000)
j4 <- seq(from = 1, to = 1000)
V.nonrisk1 <- c(0*1000)
V.nonrisk2 <- c(0*1000)
V.nonrisk3 <- c(0*1000)
V.nonrisk4 <- c(0*1000)
V.nonrisk1[1] <- m1.base - xlogregrun$y[1]
V.nonrisk2[1] <- m2.base - xlogregrun$y[1]
V.nonrisk3[1] <- m3.base - xlogregrun$y[1]
V.nonrisk4[1] <- m4.base - xlogregrun$y[1]
for(i in 2:1000){
  V.nonrisk1[i] <- V.nonrisk1[i-1] + (m1.base - xlogregrun$y[i])
}
dim(V.nonrisk1) <- c(1000, 1)
for(i in 2:1000){
  V.nonrisk2[i] <- V.nonrisk2[i-1] + (m2.base - xlogregrun$y[i])
}
dim(V.nonrisk2) <- c(1000, 1)
for(i in 2:1000){
  V.nonrisk3[i] <- V.nonrisk3[i-1] + (m3.base - xlogregrun$y[i])
}
dim(V.nonrisk3) <- c(1000, 1)
for(i in 2:1000){
  V.nonrisk4[i] <- V.nonrisk4[i-1] + (m4.base - xlogregrun$y[i])
}
dim(V.nonrisk4) <- c(1000, 1)
par(mfrow = c(2,2))
plot(V.nonrisk1)
plot(V.nonrisk2)
plot(V.nonrisk3)
plot(V.nonrisk4)

#Vlad_plots_risk
p.0.risk1 <- predict(glm.reg.risk1, newdata = xlogregrun, type = "response")
p.0.risk2 <- predict(glm.reg.risk2, newdata = xlogregrun, type = "response")
p.0.risk3 <- predict(glm.reg.risk3, newdata = xlogregrun, type = "response")
p.0.risk4 <- predict(glm.reg.risk4, newdata = xlogregrun, type = "response")

plot(x=t, y=p.1.risk*(1-p.1.risk), ylab = "variance", type = "l", main = "p.1 vs p.0", ylim = c(0, 0.5))
lines(x=t, y = p.0.risk1*(1-p.0.risk1), col = "blue")
plot(x=t, y=p.1.risk*(1-p.1.risk), ylab = "variance", type = "l", main = "p.1 vs p.0", ylim = c(0, 0.5))
lines(x=t, y = p.0.risk2*(1-p.0.risk2), col = "blue")
plot(x=t, y=p.1.risk*(1-p.1.risk), ylab = "variance", type = "l", main = "p.1 vs p.0", ylim = c(0, 0.5))
lines(x=t, y = p.0.risk3*(1-p.0.risk3), col = "blue")
plot(x=t, y=p.1.risk*(1-p.1.risk), ylab = "variance", type = "l", main = "p.1 vs p.0", ylim = c(0, 0.5))
lines(x=t, y = p.0.risk4*(1-p.0.risk4), col = "blue")
V.risk1 <- c(0*1000)
V.risk2 <- c(0*1000)
V.risk3 <- c(0*1000)
V.risk4 <- c(0*1000)
V.risk1[1] <- p.0.risk1[1] - xlogregrun$y[1]
V.risk2[1] <- p.0.risk2[1] - xlogregrun$y[1]
V.risk3[1] <- p.0.risk3[1] - xlogregrun$y[1]
V.risk4[1] <- p.0.risk4[1] - xlogregrun$y[1]
for(i in 2:1000){
  V.risk1[i] <- V.risk1[i-1] + (p.0.risk1[i] - xlogregrun$y[i])
}
dim(V.risk1) <- c(1000, 1)
for(i in 2:1000){
  V.risk2[i] <- V.risk2[i-1] + (p.0.risk2[i] - xlogregrun$y[i])
}
dim(V.risk2) <- c(1000, 1)
for(i in 2:1000){
  V.risk3[i] <- V.risk3[i-1] + (p.0.risk3[i] - xlogregrun$y[i])
}
dim(V.risk3) <- c(1000, 1)
for(i in 2:1000){
  V.risk4[i] <- V.risk4[i-1] + (p.0.risk4[i] - xlogregrun$y[i])
}
dim(V.risk4) <- c(1000, 1)

par(mfrow = c(2,2))
plot(V.risk1)
plot(V.risk2)
plot(V.risk3)
plot(V.risk4)

# Creating CI
#No risk
t <- seq(from = 1, to = length(xlogregrun$y), by =1)
V.nonrisk.mean1 <- cumsum(p.0.norisk1-p.1.norisk)
V.nonrisk.mean2 <- cumsum(p.0.norisk2-p.1.norisk)
V.nonrisk.mean3 <- cumsum(p.0.norisk3-p.1.norisk)
V.nonrisk.mean4 <- cumsum(p.0.norisk4-p.1.norisk)
V.nonrisk.var1 <- t*p.0.norisk1*(1-p.0.norisk1)
V.nonrisk.var2 <- t*p.0.norisk2*(1-p.0.norisk2)
V.nonrisk.var3 <- t*p.0.norisk3*(1-p.0.norisk3)
V.nonrisk.var4 <- t*p.0.norisk4*(1-p.0.norisk4)
V.nonrisk.min1 <- V.nonrisk1-1.96*sqrt(V.nonrisk.var1)
V.nonrisk.max1 <- V.nonrisk1+1.96*sqrt(V.nonrisk.var1)
V.nonrisk.min2 <- V.nonrisk2-1.96*sqrt(V.nonrisk.var2)
V.nonrisk.max2 <- V.nonrisk2+1.96*sqrt(V.nonrisk.var2)
V.nonrisk.min3 <- V.nonrisk3-1.96*sqrt(V.nonrisk.var3)
V.nonrisk.max3 <- V.nonrisk3+1.96*sqrt(V.nonrisk.var3)
V.nonrisk.min4 <- V.nonrisk4-1.96*sqrt(V.nonrisk.var4)
V.nonrisk.max4 <- V.nonrisk4+1.96*sqrt(V.nonrisk.var4)
par(mfrow = c(2,2))
plot(V.nonrisk1, ylim = c(-30, 100), xlab = "t", main = "Baseline with n=100 observations", type = "l")
lines(x=j1, y = V.nonrisk.min1, col = "red")
lines(x=j1, y = V.nonrisk.max1, col = "green")
plot(V.nonrisk2, ylim = c(-30, 100), xlab = "t", main = "Baseline with n=500 observations", type = "l")
lines(x=j2, y = V.nonrisk.min2, col = "red")
lines(x=j2, y = V.nonrisk.max2, col = "green")
plot(V.nonrisk3, ylim = c(-30, 100), xlab = "t", main = "Baseline with n=1000 observations", type = "l")
lines(x=j3, y = V.nonrisk.min3, col = "red")
lines(x=j3, y = V.nonrisk.max3, col = "green")
plot(V.nonrisk4, ylim = c(-30, 100), xlab = "t", main = "Baseline with n=1500 observations", type = "l")
lines(x=j4, y = V.nonrisk.min4, col = "red")
lines(x=j4, y = V.nonrisk.max4, col = "green")

#Risk
V.risk.mean1 <- cumsum(p.0.risk1-p.1.risk)
V.risk.mean2 <- cumsum(p.0.risk2-p.1.risk)
V.risk.mean3 <- cumsum(p.0.risk3-p.1.risk)
V.risk.mean4 <- cumsum(p.0.risk4-p.1.risk)
V.risk.var1 <- cumsum(p.0.risk1*(1-p.0.risk1))
V.risk.var2 <- cumsum(p.0.risk2*(1-p.0.risk2))
V.risk.var3 <- cumsum(p.0.risk3*(1-p.0.risk3))
V.risk.var4 <- cumsum(p.0.risk4*(1-p.0.risk4))
V.risk.min1 <- V.risk1-1.96*sqrt(V.risk.var1)
V.risk.max1 <- V.risk1+1.96*sqrt(V.risk.var1)
V.risk.min2 <- V.risk2-1.96*sqrt(V.risk.var2)
V.risk.max2 <- V.risk2+1.96*sqrt(V.risk.var2)
V.risk.min3 <- V.risk3-1.96*sqrt(V.risk.var3)
V.risk.max3 <- V.risk3+1.96*sqrt(V.risk.var3)
V.risk.min4 <- V.risk4-1.96*sqrt(V.risk.var4)
V.risk.max4 <- V.risk4+1.96*sqrt(V.risk.var4)
par(mfrow = c(2,2))
plot(V.risk1, ylim = c(0, 100), xlab = "t", main = "Baseline with n=100 observations", type = "l")
lines(x=j1, y = V.risk.min1, col = "red")
lines(x=j1, y = V.risk.max1, col = "green")
plot(V.risk2, ylim = c(0, 100), xlab = "t", main = "Baseline with n=500 observations", type = "l")
lines(x=j2, y = V.risk.min2, col = "red")
lines(x=j2, y = V.risk.max2, col = "green")
plot(V.risk3, ylim = c(0, 100), xlab = "t", main = "Baseline with n=1000 observations", type = "l")
lines(x=j3, y = V.risk.min3, col = "red")
lines(x=j3, y = V.risk.max3, col = "green")
plot(V.risk4, ylim = c(0, 100), xlab = "t", main = "Baseline with n=1500 observations", type = "l")
lines(x=j4, y = V.risk.min4, col = "red")
lines(x=j4, y = V.risk.max4, col = "green")

#Type 2 intervals
#No risk
p.0.norisk1.min <- p.0.norisk1-1.96*sqrt((p.0.norisk1*(1-p.0.norisk1))/nbase1)
p.0.norisk2.min <- p.0.norisk2-1.96*sqrt((p.0.norisk2*(1-p.0.norisk2))/nbase2)
p.0.norisk3.min <- p.0.norisk3-1.96*sqrt((p.0.norisk3*(1-p.0.norisk3))/nbase3)
p.0.norisk4.min <- p.0.norisk4-1.96*sqrt((p.0.norisk4*(1-p.0.norisk4))/nbase4)
p.0.norisk1.max <- p.0.norisk1+1.96*sqrt((p.0.norisk1*(1-p.0.norisk1))/nbase1)
p.0.norisk2.max <- p.0.norisk2+1.96*sqrt((p.0.norisk2*(1-p.0.norisk2))/nbase2)
p.0.norisk3.max <- p.0.norisk3+1.96*sqrt((p.0.norisk3*(1-p.0.norisk3))/nbase3)
p.0.norisk4.max <- p.0.norisk4+1.96*sqrt((p.0.norisk4*(1-p.0.norisk4))/nbase4)
V.nonrisk.min1 <- cumsum(p.0.norisk1.min-xlogregrun$y)
V.nonrisk.min2 <- cumsum(p.0.norisk2.min-xlogregrun$y)
V.nonrisk.min3 <- cumsum(p.0.norisk3.min-xlogregrun$y)
V.nonrisk.min4 <- cumsum(p.0.norisk4.min-xlogregrun$y)
V.nonrisk.max1 <- cumsum(p.0.norisk1.max-xlogregrun$y)
V.nonrisk.max2 <- cumsum(p.0.norisk2.max-xlogregrun$y)
V.nonrisk.max3 <- cumsum(p.0.norisk3.max-xlogregrun$y)
V.nonrisk.max4 <- cumsum(p.0.norisk4.max-xlogregrun$y)
par(mfrow = c(2,2))
plot(V.nonrisk1, ylim = c(-100, 150), xlab = "t", main = "Baseline with n=100 observations", type = "l")
lines(x=j1, y = V.nonrisk.min1, col = "red")
lines(x=j1, y = V.nonrisk.max1, col = "green")
plot(V.nonrisk2, ylim = c(-100, 150), xlab = "t", main = "Baseline with n=500 observations", type = "l")
lines(x=j2, y = V.nonrisk.min2, col = "red")
lines(x=j2, y = V.nonrisk.max2, col = "green")
plot(V.nonrisk3, ylim = c(-100, 150), xlab = "t", main = "Baseline with n=1000 observations", type = "l")
lines(x=j3, y = V.nonrisk.min3, col = "red")
lines(x=j3, y = V.nonrisk.max3, col = "green")
plot(V.nonrisk4, ylim = c(-100, 150), xlab = "t", main = "Baseline with n=1500 observations", type = "l")
lines(x=j4, y = V.nonrisk.min4, col = "red")
lines(x=j4, y = V.nonrisk.max4, col = "green")

#Risk
inverse_logit <- function(x)
  1/(1+exp(-x))
fitbase1 <- predict(glm.reg.risk1, newdata = xlogregrun, se.fit = TRUE)
p_est.fit1 <- inverse_logit(fitbase1$fit)
p_est.low1 <- inverse_logit(fitbase1$fit - 1.96*fitbase1$se.fit)
p_est.high1 <- inverse_logit(fitbase1$fit + 1.96*fitbase1$se.fit)
fitbase2 <- predict(glm.reg.risk2, newdata = xlogregrun, se.fit = TRUE)
p_est.fit2 <- inverse_logit(fitbase2$fit)
p_est.low2 <- inverse_logit(fitbase2$fit - 1.96*fitbase2$se.fit)
p_est.high2 <- inverse_logit(fitbase2$fit + 1.96*fitbase2$se.fit)
fitbase3 <- predict(glm.reg.risk3, newdata = xlogregrun, se.fit = TRUE)
p_est.fit3 <- inverse_logit(fitbase3$fit)
p_est.low3 <- inverse_logit(fitbase3$fit - 1.96*fitbase3$se.fit)
p_est.high3 <- inverse_logit(fitbase3$fit + 1.96*fitbase3$se.fit)
fitbase4 <- predict(glm.reg.risk4, newdata = xlogregrun, se.fit = TRUE)
p_est.fit4 <- inverse_logit(fitbase4$fit)
p_est.low4 <- inverse_logit(fitbase4$fit - 1.96*fitbase4$se.fit)
p_est.high4 <- inverse_logit(fitbase4$fit + 1.96*fitbase4$se.fit)
V.risk.min1 <- cumsum(p_est.low1-xlogregrun$y)
V.risk.min2 <- cumsum(p_est.low2-xlogregrun$y)
V.risk.min3 <- cumsum(p_est.low3-xlogregrun$y)
V.risk.min4 <- cumsum(p_est.low4-xlogregrun$y)
V.risk.max1 <- cumsum(p_est.high1-xlogregrun$y)
V.risk.max2 <- cumsum(p_est.high2-xlogregrun$y)
V.risk.max3 <- cumsum(p_est.high3-xlogregrun$y)
V.risk.max4 <- cumsum(p_est.high4-xlogregrun$y)
par(mfrow = c(2,2))
plot(V.risk1, ylim = c(-100, 200), xlab = "t", main = "Baseline with n=100 observations", type = "l")
lines(x=j1, y = V.risk.min1, col = "red")
lines(x=j1, y = V.risk.max1, col = "green")
plot(V.risk2, ylim = c(-100, 200), xlab = "t", main = "Baseline with n=500 observations", type = "l")
lines(x=j2, y = V.risk.min2, col = "red")
lines(x=j2, y = V.risk.max2, col = "green")
plot(V.risk3, ylim = c(-100, 200), xlab = "t", main = "Baseline with n=1000 observations", type = "l")
lines(x=j3, y = V.risk.min3, col = "red")
lines(x=j3, y = V.risk.max3, col = "green")
plot(V.risk4, ylim = c(-100, 200), xlab = "t", main = "Baseline with n=1500 observations", type = "l")
lines(x=j4, y = V.risk.min4, col = "red")
lines(x=j4, y = V.risk.max4, col = "green")

#Type 3
#No-risk
t <- length(xlogregrun$y)
V.nonrisk.var1 <- ((1:t)*((1:t)*p.0.norisk1*(1-p.0.norisk1)+nbase1*(p.0.norisk1*(1-p.0.norisk1))))/(nbase1)
V.nonrisk.var2 <- ((1:t)*((1:t)*p.0.norisk2*(1-p.0.norisk2)+nbase2*(p.0.norisk2*(1-p.0.norisk2))))/(nbase2)
V.nonrisk.var3 <- ((1:t)*((1:t)*p.0.norisk3*(1-p.0.norisk3)+nbase3*(p.0.norisk3*(1-p.0.norisk3))))/(nbase3)
V.nonrisk.var4 <- ((1:t)*((1:t)*p.0.norisk4*(1-p.0.norisk4)+nbase4*(p.0.norisk4*(1-p.0.norisk4))))/(nbase4)
V.nonrisk.min1 <- V.nonrisk1-1.96*sqrt(V.nonrisk.var1)
V.nonrisk.max1 <- V.nonrisk1+1.96*sqrt(V.nonrisk.var1)
V.nonrisk.min2 <- V.nonrisk2-1.96*sqrt(V.nonrisk.var2)
V.nonrisk.max2 <- V.nonrisk2+1.96*sqrt(V.nonrisk.var2)
V.nonrisk.min3 <- V.nonrisk3-1.96*sqrt(V.nonrisk.var3)
V.nonrisk.max3 <- V.nonrisk3+1.96*sqrt(V.nonrisk.var3)
V.nonrisk.min4 <- V.nonrisk4-1.96*sqrt(V.nonrisk.var4)
V.nonrisk.max4 <- V.nonrisk4+1.96*sqrt(V.nonrisk.var4)
par(mfrow = c(2,2))
plot(V.nonrisk1, ylim = c(-100, 150), xlab = "t", main = "Baseline with n=100 observations", type = "l")
lines(x=j1, y = V.nonrisk.min1, col = "red")
lines(x=j1, y = V.nonrisk.max1, col = "green")
plot(V.nonrisk2, ylim = c(-100, 150), xlab = "t", main = "Baseline with n=500 observations", type = "l")
lines(x=j2, y = V.nonrisk.min2, col = "red")
lines(x=j2, y = V.nonrisk.max2, col = "green")
plot(V.nonrisk3, ylim = c(-100, 150), xlab = "t", main = "Baseline with n=1000 observations", type = "l")
lines(x=j3, y = V.nonrisk.min3, col = "red")
lines(x=j3, y = V.nonrisk.max3, col = "green")
plot(V.nonrisk4, ylim = c(-100, 150), xlab = "t", main = "Baseline with n=1500 observations", type = "l")
lines(x=j4, y = V.nonrisk.min4, col = "red")
lines(x=j4, y = V.nonrisk.max4, col = "green")

#Risk
p.0.average.risk1 <- mean(p.0.risk1)
p.0.average.risk2 <- mean(p.0.risk2)
p.0.average.risk3 <- mean(p.0.risk3)
p.0.average.risk4 <- mean(p.0.risk4)
V.risk.var1 <- ((1:t)*((1:t)*p.0.average.risk1*(1-p.0.average.risk1)+nbase1*(p.0.average.risk1*(1-p.0.average.risk1))))/(nbase1)
V.risk.var2 <- ((1:t)*((1:t)*p.0.average.risk2*(1-p.0.average.risk2)+nbase2*(p.0.average.risk2*(1-p.0.average.risk2))))/(nbase2)
V.risk.var3 <- ((1:t)*((1:t)*p.0.average.risk3*(1-p.0.average.risk3)+nbase3*(p.0.average.risk3*(1-p.0.average.risk3))))/(nbase3)
V.risk.var4 <- ((1:t)*((1:t)*p.0.average.risk4*(1-p.0.average.risk4)+nbase4*(p.0.average.risk4*(1-p.0.average.risk4))))/(nbase4)
V.risk.min1 <- V.risk1-1.96*sqrt(V.risk.var1)
V.risk.max1 <- V.risk1+1.96*sqrt(V.risk.var1)
V.risk.min2 <- V.risk2-1.96*sqrt(V.risk.var2)
V.risk.max2 <- V.risk2+1.96*sqrt(V.risk.var2)
V.risk.min3 <- V.risk3-1.96*sqrt(V.risk.var3)
V.risk.max3 <- V.risk3+1.96*sqrt(V.risk.var3)
V.risk.min4 <- V.risk4-1.96*sqrt(V.risk.var4)
V.risk.max4 <- V.risk4+1.96*sqrt(V.risk.var4)
par(mfrow = c(2,2))
plot(V.risk1, ylim = c(-200, 200), xlab = "t", main = "Baseline with n=100 observations", type = "l")
lines(x=j1, y = V.risk.min1, col = "red")
lines(x=j1, y = V.risk.max1, col = "green")
plot(V.risk2, ylim = c(-200, 200), xlab = "t", main = "Baseline with n=500 observations", type = "l")
lines(x=j2, y = V.risk.min2, col = "red")
lines(x=j2, y = V.risk.max2, col = "green")
plot(V.risk3, ylim = c(-200, 200), xlab = "t", main = "Baseline with n=1000 observations", type = "l")
lines(x=j3, y = V.risk.min3, col = "red")
lines(x=j3, y = V.risk.max3, col = "green")
plot(V.risk4, ylim = c(-200, 200), xlab = "t", main = "Baseline with n=1500 observations", type = "l")
lines(x=j4, y = V.risk.min4, col = "red")
lines(x=j4, y = V.risk.max4, col = "green")

#Risk version 2
p.base1 <- predict(glm.reg.risk1, newdata = xlogregbase1, type = "response", se.fit = TRUE)
p.base1.se <- p.base1$se.fit
p.base1.var <- (p.base1.se)^2
p.base1.var.average <- mean(p.base1.var)
V.risk.var1 <- (1:t)*((1:t)*p.base1.var.average + p.0.average.risk1*(1-p.0.average.risk1))
p.base2 <- predict(glm.reg.risk2, newdata = xlogregbase2, type = "response", se.fit = TRUE)
p.base2.se <- p.base2$se.fit
p.base2.var <- (p.base2.se)^2
p.base2.var.average <- mean(p.base2.var)
V.risk.var2 <- (1:t)*((1:t)*p.base2.var.average + p.0.average.risk2*(1-p.0.average.risk2))
p.base3 <- predict(glm.reg.risk3, newdata = xlogregbase3, type = "response", se.fit = TRUE)
p.base3.se <- p.base3$se.fit
p.base3.var <- (p.base3.se)^2
p.base3.var.average <- mean(p.base3.var)
V.risk.var3 <- (1:t)*((1:t)*p.base3.var.average + p.0.average.risk3*(1-p.0.average.risk3))
p.base4 <- predict(glm.reg.risk4, newdata = xlogregbase4, type = "response", se.fit = TRUE)
p.base4.se <- p.base4$se.fit
p.base4.var <- (p.base4.se)^2
p.base4.var.average <- mean(p.base4.var)
V.risk.var4 <- (1:t)*((1:t)*p.base4.var.average + p.0.average.risk4*(1-p.0.average.risk4))
V.risk.min1 <- V.risk1-1.96*sqrt(V.risk.var1)
V.risk.max1 <- V.risk1+1.96*sqrt(V.risk.var1)
V.risk.min2 <- V.risk2-1.96*sqrt(V.risk.var2)
V.risk.max2 <- V.risk2+1.96*sqrt(V.risk.var2)
V.risk.min3 <- V.risk3-1.96*sqrt(V.risk.var3)
V.risk.max3 <- V.risk3+1.96*sqrt(V.risk.var3)
V.risk.min4 <- V.risk4-1.96*sqrt(V.risk.var4)
V.risk.max4 <- V.risk4+1.96*sqrt(V.risk.var4)
par(mfrow = c(2,2))
plot(V.risk1, ylim = c(-200, 250), xlab = "t", main = "Baseline with n=100 observations", type = "l")
lines(x=j1, y = V.risk.min1, col = "red")
lines(x=j1, y = V.risk.max1, col = "green")
plot(V.risk2, ylim = c(-200, 250), xlab = "t", main = "Baseline with n=500 observations", type = "l")
lines(x=j2, y = V.risk.min2, col = "red")
lines(x=j2, y = V.risk.max2, col = "green")
plot(V.risk3, ylim = c(-200, 250), xlab = "t", main = "Baseline with n=1000 observations", type = "l")
lines(x=j3, y = V.risk.min3, col = "red")
lines(x=j3, y = V.risk.max3, col = "green")
plot(V.risk4, ylim = c(-200, 250), xlab = "t", main = "Baseline with n=1500 observations", type = "l")
lines(x=j4, y = V.risk.min4, col = "red")
lines(x=j4, y = V.risk.max4, col = "green")

#Risk version 3 independent
p.0.risk1 <- predict(glm.reg.risk1, newdata = xlogregrun, type = "response", se.fit = TRUE)
p.0.risk1.fit <- p.0.risk1$fit
p.0.risk1.se <- p.0.risk1$se.fit
p.0.risk1.var <- (p.0.risk1.se)^2
V.risk.var1 <- cumsum(p.0.risk1.var)+(1:t)*p.0.average.risk1*(1-p.0.average.risk1)
p.0.risk2 <- predict(glm.reg.risk2, newdata = xlogregrun, type = "response", se.fit = TRUE)
p.0.risk2.fit <- p.0.risk2$fit
p.0.risk2.se <- p.0.risk2$se.fit
p.0.risk2.var <- (p.0.risk2.se)^2
V.risk.var2 <- cumsum(p.0.risk2.var)+(1:t)*p.0.average.risk2*(1-p.0.average.risk2)
p.0.risk3 <- predict(glm.reg.risk3, newdata = xlogregrun, type = "response", se.fit = TRUE)
p.0.risk3.fit <- p.0.risk3$fit
p.0.risk3.se <- p.0.risk3$se.fit
p.0.risk3.var <- (p.0.risk3.se)^2
V.risk.var3 <- cumsum(p.0.risk3.var)+(1:t)*p.0.average.risk3*(1-p.0.average.risk3)
p.0.risk4 <- predict(glm.reg.risk4, newdata = xlogregrun, type = "response", se.fit = TRUE)
p.0.risk4.fit <- p.0.risk4$fit
p.0.risk4.se <- p.0.risk4$se.fit
p.0.risk4.var <- (p.0.risk4.se)^2
V.risk.var4 <- cumsum(p.0.risk4.var)+(1:t)*p.0.average.risk4*(1-p.0.average.risk4)
V.risk.min1 <- V.risk1-1.96*sqrt(V.risk.var1)
V.risk.max1 <- V.risk1+1.96*sqrt(V.risk.var1)
V.risk.min2 <- V.risk2-1.96*sqrt(V.risk.var2)
V.risk.max2 <- V.risk2+1.96*sqrt(V.risk.var2)
V.risk.min3 <- V.risk3-1.96*sqrt(V.risk.var3)
V.risk.max3 <- V.risk3+1.96*sqrt(V.risk.var3)
V.risk.min4 <- V.risk4-1.96*sqrt(V.risk.var4)
V.risk.max4 <- V.risk4+1.96*sqrt(V.risk.var4)
par(mfrow = c(2,2))
plot(V.risk1, ylim = c(0, 100), xlab = "t", main = "Baseline with n=100 observations", type = "l")
lines(x=j1, y = V.risk.min1, col = "red")
lines(x=j1, y = V.risk.max1, col = "green")
plot(V.risk2, ylim = c(0, 100), xlab = "t", main = "Baseline with n=500 observations", type = "l")
lines(x=j2, y = V.risk.min2, col = "red")
lines(x=j2, y = V.risk.max2, col = "green")
plot(V.risk3, ylim = c(0, 100), xlab = "t", main = "Baseline with n=1000 observations", type = "l")
lines(x=j3, y = V.risk.min3, col = "red")
lines(x=j3, y = V.risk.max3, col = "green")
plot(V.risk4, ylim = c(0, 100), xlab = "t", main = "Baseline with n=1500 observations", type = "l")
lines(x=j4, y = V.risk.min4, col = "red")
lines(x=j4, y = V.risk.max4, col = "green")

