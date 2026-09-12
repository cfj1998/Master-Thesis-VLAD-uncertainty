
rm(list = ls())# reset variables.
library(boot)
setwd("C:/Users/1998c/OneDrive/Skrivebord/Masteroppgave")
set.seed(10)
#creating datasets
nbase1 <- 100 
nbase2 <- 500
nbase3 <- 1000
nbase4 <- 1500
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

#Vlad_plots_risk
pred1 <- predict(glm.reg.risk1, newdata = xlogregrun, type = "response")
pred2 <- predict(glm.reg.risk2, newdata = xlogregrun, type = "response")
pred3 <- predict(glm.reg.risk3, newdata = xlogregrun, type = "response")
pred4 <- predict(glm.reg.risk4, newdata = xlogregrun, type = "response")
V.risk1 <- cumsum(pred1 - xlogregrun$y)
V.risk2 <- cumsum(pred2 - xlogregrun$y)
V.risk3 <- cumsum(pred3 - xlogregrun$y)
V.risk4 <- cumsum(pred4 - xlogregrun$y)


V.norisk <- function(data, index, rundata){
  baseline <- data[index, ]  # Baseline
  yhat <- mean(baseline$y)
  V <- numeric(0)
  V[1] <- yhat - rundata$y[1]
  for(i in 2:length(rundata$y)){
    V[i] <- V[i-1]+(yhat - rundata$y[i])
  }
  return(V)
}

V.risk <- function(data, index, rundata){
  baseline <- data[index, ]  # Baseline
  baselinereg <-glm(y~x1+x2+x3, family=binomial("logit"), data=baseline, 
                 subset = index)
  V <- numeric(0)
  pred <- predict(baselinereg, newdata = rundata, type = "response")
  V[1] <- pred[1] - rundata$y[1]
  for(i in 2:length(rundata$y)){
    V[i] <- V[i - 1] + (pred[i] - rundata$y[i])
  }
  return(V)
}

bootstrap.V.norisk1 <- boot(xlogregbase1, V.norisk, R = 2000, rundata = xlogregrun)
bootstrap.V.risk1 <- boot(xlogregbase1, V.risk, R = 2000, rundata = xlogregrun)
bootstrap.V.norisk2 <- boot(xlogregbase2, V.norisk, R = 2000, rundata = xlogregrun)
bootstrap.V.risk2 <- boot(xlogregbase2, V.risk, R = 2000, rundata = xlogregrun)
bootstrap.V.norisk3 <- boot(xlogregbase3, V.norisk, R = 2000, rundata = xlogregrun)
bootstrap.V.risk3 <- boot(xlogregbase3, V.risk, R = 2000, rundata = xlogregrun)
bootstrap.V.norisk4 <- boot(xlogregbase4, V.norisk, R = 2000, rundata = xlogregrun)
bootstrap.V.risk4 <- boot(xlogregbase4, V.risk, R = 2000, rundata = xlogregrun)

#Basic Intervals
ci.norisk.basic.low1 <- numeric(0)
ci.norisk.basic.up1 <- numeric(0)
for(i in 1:length(xlogregrun$y)){
  basic.int <- boot.ci(bootstrap.V.norisk1, type = c("basic"), index = i)
  ci.norisk.basic.low1[i] <- basic.int$basic[4]
  ci.norisk.basic.up1[i] <- basic.int$basic[5]
}
ci.norisk.basic.low2 <- numeric(0)
ci.norisk.basic.up2 <- numeric(0)
for(i in 1:length(xlogregrun$y)){
  basic.int <- boot.ci(bootstrap.V.norisk2, type = c("basic"), index = i)
  ci.norisk.basic.low2[i] <- basic.int$basic[4]
  ci.norisk.basic.up2[i] <- basic.int$basic[5]
}
ci.norisk.basic.low3 <- numeric(0)
ci.norisk.basic.up3 <- numeric(0)
for(i in 1:length(xlogregrun$y)){
  basic.int <- boot.ci(bootstrap.V.norisk3, type = c("basic"), index = i)
  ci.norisk.basic.low3[i] <- basic.int$basic[4]
  ci.norisk.basic.up3[i] <- basic.int$basic[5]
}
ci.norisk.basic.low4 <- numeric(0)
ci.norisk.basic.up4 <- numeric(0)
for(i in 1:length(xlogregrun$y)){
  basic.int <- boot.ci(bootstrap.V.norisk4, type = c("basic"), index = i)
  ci.norisk.basic.low4[i] <- basic.int$basic[4]
  ci.norisk.basic.up4[i] <- basic.int$basic[5]
}

par(mfrow = c(2,2))
plot(x=j1, y = V.nonrisk1, ylab = "V_t", xlab = "t",
     ylim = c(-100, 200), type = "l", main = "baseline with n = 100")
abline(0,0,lty=2)
lines(x=j1, y = ci.norisk.basic.low1, col = "red")
lines(x=j1, y = ci.norisk.basic.up1, col = "green")
plot(x=j2, y = V.nonrisk2, ylab = "V_t", xlab = "t",
     ylim = c(0, 130), type = "l",  main = "baseline with n = 500")
abline(0,0,lty=2)
lines(x=j2, y = ci.norisk.basic.low2, col = "red")
lines(x=j2, y = ci.norisk.basic.up2, col = "green")
plot(x=j3, y = V.nonrisk3, ylab = "V_t", xlab = "t",
     ylim = c(0, 100), type = "l", main = "baseline with n = 1000")
abline(0,0,lty=2)
lines(x=j3, y = ci.norisk.basic.low3, col = "red")
lines(x=j3, y = ci.norisk.basic.up3, col = "green")
plot(x=j4, y = V.nonrisk4, ylab = "V_t", xlab = "t",
     ylim = c(0, 100), type = "l", main = "baseline with n = 1500")
abline(0,0,lty=2)
lines(x=j4, y = ci.norisk.basic.low4, col = "red")
lines(x=j4, y = ci.norisk.basic.up4, col = "green")

ci.risk.basic.low1 <- numeric(0)
ci.risk.basic.up1 <- numeric(0)
for(i in 1:length(xlogregrun$y)){
  basic.int <- boot.ci(bootstrap.V.risk1, type = c("basic"), index = i)
  ci.risk.basic.low1[i] <- basic.int$basic[4]
  ci.risk.basic.up1[i] <- basic.int$basic[5]
}
ci.risk.basic.low2 <- numeric(0)
ci.risk.basic.up2 <- numeric(0)
for(i in 1:length(xlogregrun$y)){
  basic.int <- boot.ci(bootstrap.V.risk2, type = c("basic"), index = i)
  ci.risk.basic.low2[i] <- basic.int$basic[4]
  ci.risk.basic.up2[i] <- basic.int$basic[5]
}
ci.risk.basic.low3 <- numeric(0)
ci.risk.basic.up3 <- numeric(0)
for(i in 1:length(xlogregrun$y)){
  basic.int <- boot.ci(bootstrap.V.risk3, type = c("basic"), index = i)
  ci.risk.basic.low3[i] <- basic.int$basic[4]
  ci.risk.basic.up3[i] <- basic.int$basic[5]
}
ci.risk.basic.low4 <- numeric(0)
ci.risk.basic.up4 <- numeric(0)
for(i in 1:length(xlogregrun$y)){
  basic.int <- boot.ci(bootstrap.V.risk4, type = c("basic"), index = i)
  ci.risk.basic.low4[i] <- basic.int$basic[4]
  ci.risk.basic.up4[i] <- basic.int$basic[5]
}

par(mfrow = c(2,2))
plot(x=j1, y = V.risk1, ylab = "V_t", xlab = "t",
     ylim = c(-100, 200), type = "l", main = "baseline with n = 100")
abline(0,0,lty=2)
lines(x=j1, y = ci.risk.basic.low1, col = "red")
lines(x=j1, y = ci.risk.basic.up1, col = "green")
plot(x=j2, y = V.risk2, ylab = "V_t", xlab = "t",
     ylim = c(0, 130), type = "l",  main = "baseline with n = 500")
abline(0,0,lty=2)
lines(x=j2, y = ci.risk.basic.low2, col = "red")
lines(x=j2, y = ci.risk.basic.up2, col = "green")
plot(x=j3, y = V.risk3, ylab = "V_t", xlab = "t",
     ylim = c(0, 100), type = "l", main = "baseline with n = 1000")
abline(0,0,lty=2)
lines(x=j3, y = ci.risk.basic.low3, col = "red")
lines(x=j3, y = ci.risk.basic.up3, col = "green")
plot(x=j4, y = V.risk4, ylab = "V_t", xlab = "t",
     ylim = c(0, 100), type = "l", main = "baseline with n = 1500")
abline(0,0,lty=2)
lines(x=j4, y = ci.risk.basic.low4, col = "red")
lines(x=j4, y = ci.risk.basic.up4, col = "green")

#Normal intervals
ci.norisk.norm.low1 <- numeric(0)
ci.norisk.norm.up1 <- numeric(0)
for(i in 1:1000){
  norm.int <- boot.ci(bootstrap.V.norisk1, type = c("norm"), index = i)
  ci.norisk.norm.low1[i] <- norm.int$normal[2]
  ci.norisk.norm.up1[i] <- norm.int$normal[3]
}
ci.norisk.norm.low2 <- numeric(0)
ci.norisk.norm.up2 <- numeric(0)
for(i in 1:1000){
  norm.int <- boot.ci(bootstrap.V.norisk2, type = c("norm"), index = i)
  ci.norisk.norm.low2[i] <- norm.int$normal[2]
  ci.norisk.norm.up2[i] <- norm.int$normal[3]
}

ci.norisk.norm.low3 <- numeric(0)
ci.norisk.norm.up3 <- numeric(0)
for(i in 1:1000){
  norm.int <- boot.ci(bootstrap.V.norisk3, type = c("norm"), index = i)
  ci.norisk.norm.low3[i] <- norm.int$normal[2]
  ci.norisk.norm.up3[i] <- norm.int$normal[3]
}

ci.norisk.norm.low4 <- numeric(0)
ci.norisk.norm.up4 <- numeric(0)
for(i in 1:1000){
  norm.int <- boot.ci(bootstrap.V.norisk4, type = c("norm"), index = i)
  ci.norisk.norm.low4[i] <- norm.int$normal[2]
  ci.norisk.norm.up4[i] <- norm.int$normal[3]
}

par(mfrow = c(2,2))
plot(x=j1, y = V.nonrisk1, ylab = "V_t", xlab = "t",
     ylim = c(-100, 200), type = "l", main = "baseline with n = 100")
abline(0,0,lty=2)
lines(x=j1, y = ci.norisk.norm.low1, col = "red")
lines(x=j1, y = ci.norisk.norm.up1, col = "green")
plot(x=j2, y = V.nonrisk2, ylab = "V_t", xlab = "t",
     ylim = c(0, 130), type = "l",  main = "baseline with n = 500")
abline(0,0,lty=2)
lines(x=j2, y = ci.norisk.norm.low2, col = "red")
lines(x=j2, y = ci.norisk.norm.up2, col = "green")
plot(x=j3, y = V.nonrisk3, ylab = "V_t", xlab = "t",
     ylim = c(0, 100), type = "l", main = "baseline with n = 1000")
abline(0,0,lty=2)
lines(x=j3, y = ci.norisk.norm.low3, col = "red")
lines(x=j3, y = ci.norisk.norm.up3, col = "green")
plot(x=j4, y = V.nonrisk4, ylab = "V_t", xlab = "t",
     ylim = c(0, 100), type = "l", main = "baseline with n = 1500")
abline(0,0,lty=2)
lines(x=j4, y = ci.norisk.norm.low4, col = "red")
lines(x=j4, y = ci.norisk.norm.up4, col = "green")

ci.risk.norm.low1 <- numeric(0)
ci.risk.norm.up1 <- numeric(0)
for(i in 1:1000){
  norm.int <- boot.ci(bootstrap.V.risk1, type = c("norm"), index = i)
  ci.risk.norm.low1[i] <- norm.int$normal[2]
  ci.risk.norm.up1[i] <- norm.int$normal[3]
}
ci.risk.norm.low2 <- numeric(0)
ci.risk.norm.up2 <- numeric(0)
for(i in 1:1000){
  norm.int <- boot.ci(bootstrap.V.risk2, type = c("norm"), index = i)
  ci.risk.norm.low2[i] <- norm.int$normal[2]
  ci.risk.norm.up2[i] <- norm.int$normal[3]
}

ci.risk.norm.low3 <- numeric(0)
ci.risk.norm.up3 <- numeric(0)
for(i in 1:1000){
  norm.int <- boot.ci(bootstrap.V.risk3, type = c("norm"), index = i)
  ci.risk.norm.low3[i] <- norm.int$normal[2]
  ci.risk.norm.up3[i] <- norm.int$normal[3]
}

ci.risk.norm.low4 <- numeric(0)
ci.risk.norm.up4 <- numeric(0)
for(i in 1:1000){
  norm.int <- boot.ci(bootstrap.V.risk4, type = c("norm"), index = i)
  ci.risk.norm.low4[i] <- norm.int$normal[2]
  ci.risk.norm.up4[i] <- norm.int$normal[3]
}

par(mfrow = c(2,2))
plot(x=j1, y = V.risk1, ylab = "V_t", xlab = "t",
     ylim = c(-100, 200), type = "l", main = "baseline with n = 100")
abline(0,0,lty=2)
lines(x=j1, y = ci.risk.norm.low1, col = "red")
lines(x=j1, y = ci.risk.norm.up1, col = "green")
plot(x=j2, y = V.risk2, ylab = "V_t", xlab = "t",
     ylim = c(0, 130), type = "l",  main = "baseline with n = 500")
abline(0,0,lty=2)
lines(x=j2, y = ci.risk.norm.low2, col = "red")
lines(x=j2, y = ci.risk.norm.up2, col = "green")
plot(x=j3, y = V.risk3, ylab = "V_t", xlab = "t",
     ylim = c(0, 100), type = "l", main = "baseline with n = 1000")
abline(0,0,lty=2)
lines(x=j3, y = ci.risk.norm.low3, col = "red")
lines(x=j3, y = ci.risk.norm.up3, col = "green")
plot(x=j4, y = V.risk4, ylab = "V_t", xlab = "t",
     ylim = c(0, 100), type = "l", main = "baseline with n = 1500")
abline(0,0,lty=2)
lines(x=j4, y = ci.risk.norm.low4, col = "red")
lines(x=j4, y = ci.risk.norm.up4, col = "green")

#Percentile Intervals
ci.norisk.perc.low1 <- numeric(0)
ci.norisk.perc.up1 <- numeric(0)
for(i in 1:1000){
  perc.int <- boot.ci(bootstrap.V.norisk1, type = c("perc"), index = i)
  ci.norisk.perc.low1[i] <- perc.int$percent[4]
  ci.norisk.perc.up1[i] <- perc.int$percent[5]
}
ci.norisk.perc.low2 <- numeric(0)
ci.norisk.perc.up2 <- numeric(0)
for(i in 1:1000){
  perc.int <- boot.ci(bootstrap.V.norisk2, type = c("perc"), index = i)
  ci.norisk.perc.low2[i] <- perc.int$percent[4]
  ci.norisk.perc.up2[i] <- perc.int$percent[5]
}
ci.norisk.perc.low3 <- numeric(0)
ci.norisk.perc.up3 <- numeric(0)
for(i in 1:1000){
  perc.int <- boot.ci(bootstrap.V.norisk3, type = c("perc"), index = i)
  ci.norisk.perc.low3[i] <- perc.int$percent[4]
  ci.norisk.perc.up3[i] <- perc.int$percent[5]
}
ci.norisk.perc.low4 <- numeric(0)
ci.norisk.perc.up4 <- numeric(0)
for(i in 1:1000){
  perc.int <- boot.ci(bootstrap.V.norisk4, type = c("perc"), index = i)
  ci.norisk.perc.low4[i] <- perc.int$percent[4]
  ci.norisk.perc.up4[i] <- perc.int$percent[5]
}

par(mfrow = c(2,2))
plot(x=j1, y = V.nonrisk1, ylab = "V_t", xlab = "t",
     ylim = c(-100, 200), type = "l", main = "baseline with n = 100")
abline(0,0,lty=2)
lines(x=j1, y = ci.norisk.perc.low1, col = "red")
lines(x=j1, y = ci.norisk.perc.up1, col = "green")
plot(x=j2, y = V.nonrisk2, ylab = "V_t", xlab = "t",
     ylim = c(0, 130), type = "l",  main = "baseline with n = 500")
abline(0,0,lty=2)
lines(x=j2, y = ci.norisk.perc.low2, col = "red")
lines(x=j2, y = ci.norisk.perc.up2, col = "green")
plot(x=j3, y = V.nonrisk3, ylab = "V_t", xlab = "t",
     ylim = c(0, 130), type = "l", main = "baseline with n = 1000")
abline(0,0,lty=2)
lines(x=j3, y = ci.norisk.perc.low3, col = "red")
lines(x=j3, y = ci.norisk.perc.up3, col = "green")
plot(x=j4, y = V.nonrisk4, ylab = "V_t", xlab = "t",
     ylim = c(0, 100), type = "l", main = "baseline with n = 1500")
abline(0,0,lty=2)
lines(x=j4, y = ci.norisk.perc.low4, col = "red")
lines(x=j4, y = ci.norisk.perc.up4, col = "green")

ci.risk.perc.low1 <- numeric(0)
ci.risk.perc.up1 <- numeric(0)
for(i in 1:1000){
  perc.int <- boot.ci(bootstrap.V.norisk1, type = c("perc"), index = i)
  ci.risk.perc.low1[i] <- perc.int$percent[4]
  ci.risk.perc.up1[i] <- perc.int$percent[5]
}
ci.risk.perc.low2 <- numeric(0)
ci.risk.perc.up2 <- numeric(0)
for(i in 1:1000){
  perc.int <- boot.ci(bootstrap.V.risk2, type = c("perc"), index = i)
  ci.risk.perc.low2[i] <- perc.int$percent[4]
  ci.risk.perc.up2[i] <- perc.int$percent[5]
}
ci.risk.perc.low3 <- numeric(0)
ci.risk.perc.up3 <- numeric(0)
for(i in 1:1000){
  perc.int <- boot.ci(bootstrap.V.risk3, type = c("perc"), index = i)
  ci.risk.perc.low3[i] <- perc.int$percent[4]
  ci.risk.perc.up3[i] <- perc.int$percent[5]
}
ci.risk.perc.low4 <- numeric(0)
ci.risk.perc.up4 <- numeric(0)
for(i in 1:1000){
  perc.int <- boot.ci(bootstrap.V.risk4, type = c("perc"), index = i)
  ci.risk.perc.low4[i] <- perc.int$percent[4]
  ci.risk.perc.up4[i] <- perc.int$percent[5]
}

par(mfrow = c(2,2))
plot(x=j1, y = V.risk1, ylab = "V_t", xlab = "t",
     ylim = c(-100, 200), type = "l", main = "baseline with n = 100")
abline(0,0,lty=2)
lines(x=j1, y = ci.risk.perc.low1, col = "red")
lines(x=j1, y = ci.risk.perc.up1, col = "green")
plot(x=j2, y = V.risk2, ylab = "V_t", xlab = "t",
     ylim = c(0, 130), type = "l",  main = "baseline with n = 500")
abline(0,0,lty=2)
lines(x=j2, y = ci.risk.perc.low2, col = "red")
lines(x=j2, y = ci.risk.perc.up2, col = "green")
plot(x=j3, y = V.risk3, ylab = "V_t", xlab = "t",
     ylim = c(0, 100), type = "l", main = "baseline with n = 1000")
abline(0,0,lty=2)
lines(x=j3, y = ci.risk.perc.low3, col = "red")
lines(x=j3, y = ci.risk.perc.up3, col = "green")
plot(x=j4, y = V.risk4, ylab = "V_t", xlab = "t",
     ylim = c(0, 100), type = "l", main = "baseline with n = 1500")
abline(0,0,lty=2)
lines(x=j4, y = ci.risk.perc.low4, col = "red")
lines(x=j4, y = ci.risk.perc.up4, col = "green")