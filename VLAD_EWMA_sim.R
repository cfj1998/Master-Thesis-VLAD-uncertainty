rm(list = ls())
setwd("C:/Users/1998c/OneDrive/Skrivebord/Masteroppgave")
library(spcadjust)
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
#Calculate lambda for EWMA
m1.base <- mean(xlogregbase1$y)
m1.base
m2.base <- mean(xlogregbase2$y)
m2.base
m3.base <- mean(xlogregbase3$y)
m3.base
m4.base <- mean(xlogregbase4$y)
m4.base
m.run <- mean(xlogregrun$y)
m.run
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

#EWMA norisk
chartlogregEWMA.norisk1 <- new("SPCEWMA", model = SPCModellogregOE(Delta = 0, formula = y ~ 1),
                               lambda = 0.005)
chartlogregEWMA.norisk2 <- new("SPCEWMA", model = SPCModellogregOE(Delta = 0, formula = y ~ 1),
                               lambda = 0.005)
chartlogregEWMA.norisk3 <- new("SPCEWMA", model = SPCModellogregOE(Delta = 0, formula = y ~ 1),
                               lambda = 0.005)
chartlogregEWMA.norisk4 <- new("SPCEWMA", model = SPCModellogregOE(Delta = 0, formula = y ~ 1),
                               lambda = 0.005)
xihat.norisk1 <- xiofdata(chartlogregEWMA.norisk1, xlogregbase1)
xihat.norisk2 <- xiofdata(chartlogregEWMA.norisk2, xlogregbase2)
xihat.norisk3 <- xiofdata(chartlogregEWMA.norisk3, xlogregbase3)
xihat.norisk4 <- xiofdata(chartlogregEWMA.norisk4, xlogregbase4)

calibrate.norisk1 <- SPCproperty(data = xlogregbase1, nrep = 1, chart = chartlogregEWMA.norisk1,
                                 reportdistr = FALSE, property = "calhitprob", 
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))

calibrate.norisk2 <- SPCproperty(data = xlogregbase2, nrep = 1, chart = chartlogregEWMA.norisk2,
                                 reportdistr = FALSE, property = "calhitprob", 
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))

calibrate.norisk3 <- SPCproperty(data = xlogregbase3, nrep = 1, chart = chartlogregEWMA.norisk3,
                                 reportdistr = FALSE, property = "calhitprob", 
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))

calibrate.norisk4 <- SPCproperty(data = xlogregbase4, nrep = 1, chart = chartlogregEWMA.norisk4,
                                 reportdistr = FALSE, property = "calhitprob", 
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))

Q.norisk1 <- runchart(chartlogregEWMA.norisk1, newdata = xlogregrun, xi = xihat.norisk1)
Q.norisk2 <- runchart(chartlogregEWMA.norisk2, newdata = xlogregrun, xi = xihat.norisk2)
Q.norisk3 <- runchart(chartlogregEWMA.norisk3, newdata = xlogregrun, xi = xihat.norisk3)
Q.norisk4 <- runchart(chartlogregEWMA.norisk4, newdata = xlogregrun, xi = xihat.norisk4)

par(mfrow = c(2, 2))
plot(Q.norisk1, ylim = c(-calibrate.norisk1@raw -0.1, calibrate.norisk1@raw +0.1), xlab = "t", main = "Baseline with t = 100 observations")
abline(h = calibrate.norisk1@raw, col = "blue")
abline(h = -calibrate.norisk1@raw, col = "blue")
plot(Q.norisk2, ylim = c(-calibrate.norisk2@raw -0.1, calibrate.norisk2@raw +0.1), xlab = "t", main = "Baseline with t = 500 observations")
abline(h = calibrate.norisk2@raw, col = "blue")
abline(h = -calibrate.norisk2@raw, col = "blue")
plot(Q.norisk3, ylim = c(-calibrate.norisk3@raw -0.1, calibrate.norisk3@raw +0.1), xlab = "t", main = "Baseline with t = 1000 observations")
abline(h = calibrate.norisk3@raw, col = "blue")
abline(h = -calibrate.norisk3@raw, col = "blue")
plot(Q.norisk4, ylim = c(-calibrate.norisk4@raw -0.1, calibrate.norisk4@raw +0.1), xlab = "t", main = "Baseline with t = 1500 observations")
abline(h = calibrate.norisk4@raw, col = "blue")
abline(h = -calibrate.norisk4@raw, col = "blue")

#EWMA risk
chartlogregEWMA.risk1 <- new("SPCEWMA", model = SPCModellogregOE(Delta = 0, formula = y ~ x1+x2+x3),
                               lambda = 0.005)
chartlogregEWMA.risk2 <- new("SPCEWMA", model = SPCModellogregOE(Delta = 0, formula = y ~ x1+x2+x3),
                               lambda = 0.005)
chartlogregEWMA.risk3 <- new("SPCEWMA", model = SPCModellogregOE(Delta = 0, formula = y ~ x1+x2+x3),
                               lambda = 0.005)
chartlogregEWMA.risk4 <- new("SPCEWMA", model = SPCModellogregOE(Delta = 0, formula = y ~ x1+x2+x3),
                               lambda = 0.005)
xihat.risk1 <- xiofdata(chartlogregEWMA.risk1, xlogregbase1)
xihat.risk2 <- xiofdata(chartlogregEWMA.risk2, xlogregbase2)
xihat.risk3 <- xiofdata(chartlogregEWMA.risk3, xlogregbase3)
xihat.risk4 <- xiofdata(chartlogregEWMA.risk4, xlogregbase4)

calibrate.risk1 <- SPCproperty(data = xlogregbase1, nrep = 1, chart = chartlogregEWMA.risk1,
                                 reportdistr = FALSE, property = "calhitprob", 
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))

calibrate.risk2 <- SPCproperty(data = xlogregbase2, nrep = 1, chart = chartlogregEWMA.risk2,
                                 reportdistr = FALSE, property = "calhitprob", 
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))

calibrate.risk3 <- SPCproperty(data = xlogregbase3, nrep = 1, chart = chartlogregEWMA.risk3,
                                 reportdistr = FALSE, property = "calhitprob", 
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))

calibrate.risk4 <- SPCproperty(data = xlogregbase4, nrep = 1, chart = chartlogregEWMA.risk4,
                                 reportdistr = FALSE, property = "calhitprob", 
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))

Q.risk1 <- runchart(chartlogregEWMA.risk1, newdata = xlogregrun, xi = xihat.risk1)
Q.risk2 <- runchart(chartlogregEWMA.risk2, newdata = xlogregrun, xi = xihat.risk2)
Q.risk3 <- runchart(chartlogregEWMA.risk3, newdata = xlogregrun, xi = xihat.risk3)
Q.risk4 <- runchart(chartlogregEWMA.risk4, newdata = xlogregrun, xi = xihat.risk4)

par(mfrow = c(2, 2))
plot(Q.risk1, ylim = c(-calibrate.risk1@raw -0.1, calibrate.risk1@raw +0.1), xlab = "t", main = "Baseline with t = 100 observations")
abline(h = calibrate.risk1@raw, col = "blue")
abline(h = -calibrate.risk1@raw, col = "blue")
plot(Q.risk2, ylim = c(-calibrate.risk2@raw -0.1, calibrate.risk2@raw +0.1), xlab = "t", main = "Baseline with t = 500 observations")
abline(h = calibrate.norisk2@raw, col = "blue")
abline(h = -calibrate.norisk2@raw, col = "blue")
plot(Q.risk3, ylim = c(-calibrate.risk3@raw -0.1, calibrate.risk3@raw +0.1), xlab = "t", main = "Baseline with t = 1000 observations")
abline(h = calibrate.risk3@raw, col = "blue")
abline(h = -calibrate.risk3@raw, col = "blue")
plot(Q.risk4, ylim = c(-calibrate.risk4@raw -0.1, calibrate.risk4@raw +0.1), xlab = "t", main = "Baseline with t = 1500 observations")
abline(h = calibrate.risk4@raw, col = "blue")
abline(h = -calibrate.risk4@raw, col = "blue")
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
plot(V.nonrisk1, xlab = "t")
plot(V.nonrisk2, xlab = "t")
plot(V.nonrisk3, xlab = "t")
plot(V.nonrisk4, xlab = "t")

#Vlad_plots_risk
pred1 <- predict(glm.reg.risk1, newdata = xlogregrun, type = "response")
pred2 <- predict(glm.reg.risk2, newdata = xlogregrun, type = "response")
pred3 <- predict(glm.reg.risk3, newdata = xlogregrun, type = "response")
pred4 <- predict(glm.reg.risk4, newdata = xlogregrun, type = "response")
V.risk1 <- cumsum(pred1 - xlogregrun$y)
V.risk2 <- cumsum(pred2 - xlogregrun$y)
V.risk3 <- cumsum(pred3 - xlogregrun$y)
V.risk4 <- cumsum(pred4 - xlogregrun$y)

par(mfrow = c(2,2))
plot(V.risk1, xlab = "t")
plot(V.risk2, xlab = "t")
plot(V.risk3, xlab = "t")
plot(V.risk4, xlab = "t")

#VLAD with EWMA limits norisk
h_1.1.norisk <- calibrate.norisk1@raw
h_2.1.norisk <- calibrate.norisk2@raw 
h_3.1.norisk <- calibrate.norisk3@raw
h_4.1.norisk <- calibrate.norisk4@raw 
h_1.2.norisk <- -calibrate.norisk1@raw 
h_2.2.norisk <- -calibrate.norisk2@raw
h_3.2.norisk <- -calibrate.norisk3@raw 
h_4.2.norisk <- -calibrate.norisk4@raw
k.norisk1 <- 20/(abs(h_1.1.norisk)+abs(h_1.2.norisk))
k.norisk2 <- 20/(abs(h_2.1.norisk)+abs(h_2.2.norisk))
k.norisk3 <- 20/(abs(h_3.1.norisk)+abs(h_3.2.norisk))
k.norisk4 <- 20/(abs(h_4.1.norisk)+abs(h_4.2.norisk))
LCL.V.norisk1 <- V.nonrisk1 + k.norisk1*(Q.norisk1 - h_1.1.norisk)
LCL.V.norisk2 <- V.nonrisk2 + k.norisk2*(Q.norisk2 - h_2.1.norisk)
LCL.V.norisk3 <- V.nonrisk3 + k.norisk3*(Q.norisk3 - h_3.1.norisk)
LCL.V.norisk4 <- V.nonrisk4 + k.norisk4*(Q.norisk4 - h_4.1.norisk)
UCL.V.norisk1 <- V.nonrisk1 + k.norisk1*(Q.norisk1 - h_1.2.norisk)
UCL.V.norisk2 <- V.nonrisk2 + k.norisk2*(Q.norisk2 - h_2.2.norisk)
UCL.V.norisk3 <- V.nonrisk3 + k.norisk3*(Q.norisk3 - h_3.2.norisk)
UCL.V.norisk4 <- V.nonrisk4 + k.norisk4*(Q.norisk4 - h_4.2.norisk)
par(mfrow = c(2, 2))
plot(V.nonrisk1, xlab = "t", ylim = c(-20, 100), main = "Baseline with t = 100 observations")
lines(x=j1, y = LCL.V.norisk1, col = "red", lwd = 2)
lines(x=j1, y = UCL.V.norisk1, col = "green", lwd = 2)
plot(V.nonrisk2, xlab = "t", ylim = c(-20, 100), main = "Baseline with t = 500 observations")
lines(x=j2, y = LCL.V.norisk2, col = "red", lwd = 2)
lines(x=j2, y = UCL.V.norisk2, col = "green", lwd = 2)
plot(V.nonrisk3, xlab = "t", ylim = c(-20, 100), main = "Baseline with t = 1000 observations")
lines(x=j3, y = LCL.V.norisk3, col = "red", lwd = 2)
lines(x=j3, y = UCL.V.norisk3, col = "green", lwd = 2)
plot(V.nonrisk4, xlab = "t", ylim = c(-20, 100), main = "Baseline with t = 1500 observations")
lines(x=j4, y = LCL.V.norisk4, col = "red", lwd = 2)
lines(x=j4, y = UCL.V.norisk4, col = "green", lwd = 2)

#VLAD with EWMA limits risk
h_1.1.risk <- calibrate.risk1@raw
h_2.1.risk <- calibrate.risk2@raw 
h_3.1.risk <- calibrate.risk3@raw
h_4.1.risk <- calibrate.risk4@raw 
h_1.2.risk <- -calibrate.risk1@raw 
h_2.2.risk <- -calibrate.risk2@raw
h_3.2.risk <- -calibrate.risk3@raw 
h_4.2.risk <- -calibrate.risk4@raw
k.risk1 <- 20/(abs(h_1.1.risk)+abs(h_1.2.risk))
k.risk2 <- 20/(abs(h_2.1.risk)+abs(h_2.2.risk))
k.risk3 <- 20/(abs(h_3.1.risk)+abs(h_3.2.risk))
k.risk4 <- 20/(abs(h_4.1.risk)+abs(h_4.2.risk))
LCL.V.risk1 <- V.risk1 + k.risk1*(Q.risk1 - h_1.1.risk)
LCL.V.risk2 <- V.risk2 + k.risk2*(Q.risk2 - h_2.1.risk)
LCL.V.risk3 <- V.risk3 + k.risk3*(Q.risk3 - h_3.1.risk)
LCL.V.risk4 <- V.risk4 + k.risk4*(Q.risk4 - h_4.1.risk)
UCL.V.risk1 <- V.risk1 + k.risk1*(Q.risk1 - h_1.2.risk)
UCL.V.risk2 <- V.risk2 + k.risk2*(Q.risk2 - h_2.2.risk)
UCL.V.risk3 <- V.risk3 + k.risk3*(Q.risk3 - h_3.2.risk)
UCL.V.risk4 <- V.risk4 + k.risk4*(Q.risk4 - h_4.2.risk)
par(mfrow = c(2, 2))
plot(V.risk1, xlab = "t", ylim = c(-20, 100))
lines(x=j1, y = LCL.V.risk1, col = "red", lwd = 2)
lines(x=j1, y = UCL.V.risk1, col = "green", lwd = 2)
plot(V.risk2, xlab = "t", ylim = c(-20, 100))
lines(x=j2, y = LCL.V.risk2, col = "red", lwd = 2)
lines(x=j2, y = UCL.V.risk2, col = "green", lwd = 2)
plot(V.risk3, xlab = "t", ylim = c(-20, 100))
lines(x=j3, y = LCL.V.risk3, col = "red", lwd = 2)
lines(x=j3, y = UCL.V.risk3, col = "green", lwd = 2)
plot(V.risk4, xlab = "t", ylim = c(-20, 100))
lines(x=j4, y = LCL.V.risk4, col = "red", lwd = 2)
lines(x=j4, y = UCL.V.risk4, col = "green", lwd = 2)