rm(list = ls())# reset variables.
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
xlogregrun$y <- rbinom(mrun, 1, exp(Delta + xbetarun)/(1+exp(Delta + xbetarun)))
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

#risk average probability change
Delta.risk1 = -0.6843
pred_risk1 <- predict(glm.reg.risk1, newdata = xlogregbase1, type = "response")
mean(pred_risk1)
logit_risk1 <- predict(glm.reg.risk1, newdata = xlogregbase1)
pred_risk1_delta <- exp(Delta.risk1+logit_risk1)/(1+exp(Delta.risk1+logit_risk1))
mean(pred_risk1_delta)
mean(pred_risk1) - mean(pred_risk1_delta)

Delta.risk2 = -0.6724
pred_risk2 <- predict(glm.reg.risk2, newdata = xlogregbase2, type = "response")
mean(pred_risk2)
logit_risk2 <- predict(glm.reg.risk2, newdata = xlogregbase2)
pred_risk2_delta <- exp(Delta.risk2+logit_risk2)/(1+exp(Delta.risk2+logit_risk2))
mean(pred_risk2_delta)
mean(pred_risk2) - mean(pred_risk2_delta)

Delta.risk3 = -0.7129
pred_risk3 <- predict(glm.reg.risk3, newdata = xlogregbase3, type = "response")
mean(pred_risk3)
logit_risk3 <- predict(glm.reg.risk3, newdata = xlogregbase3)
pred_risk3_delta <- exp(Delta.risk3+logit_risk3)/(1+exp(Delta.risk3+logit_risk3))
mean(pred_risk3_delta)
mean(pred_risk3) - mean(pred_risk3_delta)

Delta.risk4 = -0.6999
pred_risk4 <- predict(glm.reg.risk4, newdata = xlogregbase4, type = "response")
mean(pred_risk4)
logit_risk4 <- predict(glm.reg.risk4, newdata = xlogregbase4)
pred_risk4_delta <- exp(Delta.risk4+logit_risk4)/(1+exp(Delta.risk4+logit_risk4))
mean(pred_risk4_delta)
mean(pred_risk4) - mean(pred_risk4_delta)

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
plot(V.nonrisk1, xlab = "t", main = "Baseline with t = 100 observations")
plot(V.nonrisk2, xlab = "t", main = "Baseline with t = 500 observations")
plot(V.nonrisk3, xlab = "t", main = "Baseline with t = 1000 observations")
plot(V.nonrisk4, xlab = "t", main = "Baseline with t = 1500 observations")

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
plot(V.risk1, xlab = "t", main = "Baseline with t = 100 observations")
plot(V.risk2, xlab = "t", main = "Baseline with t = 500 observations")
plot(V.risk3, xlab = "t", main = "Baseline with t = 1000 observations")
plot(V.risk4, xlab = "t", main = "Baseline with t = 1500 observations")
#No-risk CUSUM plots
chartlogreg.norisk1 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = -0.4020, 
                                                                      formula = y~1))
chartlogreg.norisk2 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = -0.4073, 
                                                                      formula = y~1))
chartlogreg.norisk3 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = -0.4053, 
                                                                      formula = y~1))
chartlogreg.norisk4 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = -0.4066, 
                                                                      formula = y~1))
xihat.norisk1 <- xiofdata(chartlogreg.norisk1, xlogregbase1)
xihat.norisk2 <- xiofdata(chartlogreg.norisk2, xlogregbase2)
xihat.norisk3 <- xiofdata(chartlogreg.norisk3, xlogregbase3)
xihat.norisk4 <- xiofdata(chartlogreg.norisk4, xlogregbase4)
calibrate.norisk1 <- SPCproperty(data = xlogregbase1, nrep = 1, chart = chartlogreg.norisk1,
                                 reportdistr = FALSE, property = "calhitprob",
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.norisk2 <- SPCproperty(data = xlogregbase2, nrep = 1, chart = chartlogreg.norisk2,
                                 reportdistr = FALSE, property = "calhitprob",
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.norisk3 <- SPCproperty(data = xlogregbase3, nrep = 1, chart = chartlogreg.norisk3,
                                 reportdistr = FALSE, property = "calhitprob",
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.norisk4 <- SPCproperty(data = xlogregbase4, nrep = 1, chart = chartlogreg.norisk4,
                                 reportdistr = FALSE, property = "calhitprob",
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
par(mfrow = c(2,2))
S.norisk1 <- runchart(chartlogreg.norisk1, newdata = xlogregrun, xi = xihat.norisk1)
S.norisk2 <- runchart(chartlogreg.norisk2, newdata = xlogregrun, xi = xihat.norisk2)
S.norisk3 <- runchart(chartlogreg.norisk3, newdata = xlogregrun, xi = xihat.norisk3)
S.norisk4 <- runchart(chartlogreg.norisk4, newdata = xlogregrun, xi = xihat.norisk4)
plot(S.norisk1, ylim = c(0, 30), xlab = "t", main = "Baseline with t = 100 observations")
abline(h = calibrate.norisk1@raw)
plot(S.norisk2, ylim = c(0, 30), xlab = "t", main = "Baseline with t = 500 observations")
abline(h = calibrate.norisk2@raw)
plot(S.norisk3, ylim = c(0, 30), xlab = "t", main = "Baseline with t = 1000 observations")
abline(h = calibrate.norisk3@raw)
plot(S.norisk4, ylim = c(0, 30), xlab = "t", main = "Baseline with t = 1500 observations")
abline(h = calibrate.norisk4@raw)

#No-risk CUSUM plots positive delta
chartlogreg.norisk1.2 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = 0.4020, 
                                                                        formula = y~1))
chartlogreg.norisk2.2 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = 0.4073, 
                                                                        formula = y~1))
chartlogreg.norisk3.2 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = 0.4053, 
                                                                        formula = y~1))
chartlogreg.norisk4.2 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = 0.4066, 
                                                                        formula = y~1))
xihat.norisk1.2 <- xiofdata(chartlogreg.norisk1.2, xlogregbase1)
xihat.norisk2.2 <- xiofdata(chartlogreg.norisk2.2, xlogregbase2)
xihat.norisk3.2 <- xiofdata(chartlogreg.norisk3.2, xlogregbase3)
xihat.norisk4.2 <- xiofdata(chartlogreg.norisk4.2, xlogregbase4)
calibrate.norisk1.2 <- SPCproperty(data = xlogregbase1, nrep = 1, chart = chartlogreg.norisk1.2,
                                   reportdistr = FALSE, property = "calhitprob",
                                   params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.norisk2.2 <- SPCproperty(data = xlogregbase2, nrep = 1, chart = chartlogreg.norisk2.2,
                                   reportdistr = FALSE, property = "calhitprob",
                                   params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.norisk3.2 <- SPCproperty(data = xlogregbase3, nrep = 1, chart = chartlogreg.norisk3.2,
                                   reportdistr = FALSE, property = "calhitprob",
                                   params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.norisk4.2 <- SPCproperty(data = xlogregbase4, nrep = 1, chart = chartlogreg.norisk4.2,
                                   reportdistr = FALSE, property = "calhitprob",
                                   params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
par(mfrow = c(2,2))
S.norisk1.2 <- runchart(chartlogreg.norisk1.2, newdata = xlogregrun, xi = xihat.norisk1.2)
S.norisk2.2 <- runchart(chartlogreg.norisk2.2, newdata = xlogregrun, xi = xihat.norisk2.2)
S.norisk3.2 <- runchart(chartlogreg.norisk3.2, newdata = xlogregrun, xi = xihat.norisk3.2)
S.norisk4.2 <- runchart(chartlogreg.norisk3.2, newdata = xlogregrun, xi = xihat.norisk4.2)
par(mfrow = c(2, 2))
plot(S.norisk1.2, ylim = c(0, 10), xlab = "t", main = "Baseline with t = 100 observations")
abline(h = calibrate.norisk1.2@raw)
plot(S.norisk2.2, ylim = c(0, 10), xlab = "t", main = "Baseline with t = 500 observations")
abline(h = calibrate.norisk2.2@raw)
plot(S.norisk3.2, ylim = c(0, 10), xlab = "t", main = "Baseline with t = 1000 observations")
abline(h = calibrate.norisk3.2@raw)
plot(S.norisk4.2, ylim = c(0, 10), xlab = "t", main = "Baseline with t = 1500 observations")
abline(h = calibrate.norisk4.2@raw)

#Vlad-plots with CUSUM control limits
h_1.norisk <- calibrate.norisk1@raw
h_2.norisk <- calibrate.norisk2@raw
h_3.norisk <- calibrate.norisk3@raw
h_4.norisk <- calibrate.norisk4@raw
h_1.2.norisk <- calibrate.norisk1.2@raw
h_2.2.norisk <- calibrate.norisk2.2@raw
h_3.2.norisk <- calibrate.norisk3.2@raw
h_4.2.norisk <- calibrate.norisk4.2@raw
U_n.norisk1 <- V.nonrisk1 + (S.norisk1-h_1.norisk)/(-(0.4020))
U_n.norisk2 <- V.nonrisk2 + (S.norisk2-h_2.norisk)/(-(0.4073))
U_n.norisk3 <- V.nonrisk3 + (S.norisk3-h_3.norisk)/(-(0.4053))
U_n.norisk4 <- V.nonrisk4 + (S.norisk4-h_4.norisk)/(-(0.4066))
L_n.norisk1 <- V.nonrisk1 + (S.norisk1.2-h_1.2.norisk)/((0.4020))
L_n.norisk2 <- V.nonrisk2 + (S.norisk2.2-h_2.2.norisk)/((0.4073))
L_n.norisk3 <- V.nonrisk3 + (S.norisk3.2-h_3.2.norisk)/((0.4053))
L_n.norisk4 <- V.nonrisk4 + (S.norisk4.2-h_4.2.norisk)/((0.4066))
par(mfrow = c(2,2))
plot(V.nonrisk1, ylim = c(-30, 40), xlab = "t", main = "Baseline with t = 100 observations")
lines(x=j1, y = U_n.norisk1, col = "green", lwd = 2)
lines(x=j1, y = L_n.norisk1, col = "red", lwd = 2)
plot(V.nonrisk2, ylim = c(-10, 120), xlab = "t", main = "Baseline with t = 500 observations")
lines(x=j2, y = U_n.norisk2, col = "green", lwd = 2)
lines(x=j2, y = L_n.norisk2, col = "red", lwd = 2)
plot(V.nonrisk3, xlab = "t", ylim = c(-10, 150), main = "Baseline with t = 1000 observations")
lines(x=j3, y = U_n.norisk3, col = "green", lwd = 2)
lines(x=j3, y = L_n.norisk3, col = "red", lwd = 2)
plot(V.nonrisk4, xlab = "t", ylim = c(-10, 120), main = "Baseline with t = 1500 observations")
lines(x=j4, y = U_n.norisk4, col = "green", lwd = 2)
lines(x=j4, y = L_n.norisk4, col = "red", lwd = 2)

#risk CUSUM plots
chartlogreg.risk1 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = -0.6843, 
                                                                      formula = y~x1+x2+x3))
chartlogreg.risk2 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = -0.6724, 
                                                                      formula = y~x1+x2+x3))
chartlogreg.risk3 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = -0.7129, 
                                                                      formula = y~x1+x2+x3))
chartlogreg.risk4 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = -0.6999, 
                                                                      formula = y~x1+x2+x3))
xihat.risk1 <- xiofdata(chartlogreg.risk1, xlogregbase1)
xihat.risk2 <- xiofdata(chartlogreg.risk2, xlogregbase2)
xihat.risk3 <- xiofdata(chartlogreg.risk3, xlogregbase3)
xihat.risk4 <- xiofdata(chartlogreg.risk4, xlogregbase4)
calibrate.risk1 <- SPCproperty(data = xlogregbase1, nrep = 1, chart = chartlogreg.risk1,
                                 reportdistr = FALSE, property = "calhitprob",
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.risk2 <- SPCproperty(data = xlogregbase2, nrep = 1, chart = chartlogreg.risk2,
                                 reportdistr = FALSE, property = "calhitprob",
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.risk3 <- SPCproperty(data = xlogregbase3, nrep = 1, chart = chartlogreg.risk3,
                                 reportdistr = FALSE, property = "calhitprob",
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.risk4 <- SPCproperty(data = xlogregbase4, nrep = 1, chart = chartlogreg.risk4,
                                 reportdistr = FALSE, property = "calhitprob",
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
par(mfrow = c(2,2))
S.risk1 <- runchart(chartlogreg.risk1, newdata = xlogregrun, xi = xihat.risk1)
S.risk2 <- runchart(chartlogreg.risk2, newdata = xlogregrun, xi = xihat.risk2)
S.risk3 <- runchart(chartlogreg.risk3, newdata = xlogregrun, xi = xihat.risk3)
S.risk4 <- runchart(chartlogreg.risk4, newdata = xlogregrun, xi = xihat.risk4)
plot(S.risk1, ylim = c(0, 40), xlab = "t", main = "Baseline with t = 100 observations")
abline(h = calibrate.risk1@raw)
plot(S.risk2, ylim = c(0, 40), xlab = "t", main = "Baseline with t = 500 observations")
abline(h = calibrate.risk2@raw)
plot(S.risk3, ylim = c(0, 40), xlab = "t", main = "Baseline with t = 1000 observations")
abline(h = calibrate.risk3@raw)
plot(S.risk4, ylim = c(0, 40), xlab = "t", main = "Baseline with t = 1500 observations")
abline(h = calibrate.risk4@raw)


#risk CUSUM plots positive delta
chartlogreg.risk1.2 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = 0.6843, 
                                                                        formula = y~x1+x2+x3))
chartlogreg.risk2.2 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = 0.6724, 
                                                                        formula = y~x1+x2+x3))
chartlogreg.risk3.2 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = 0.7129, 
                                                                        formula = y~x1+x2+x3))
chartlogreg.risk4.2 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = 0.6999, 
                                                                        formula = y~x1+x2+x3))
xihat.risk1.2 <- xiofdata(chartlogreg.risk1.2, xlogregbase1)
xihat.risk2.2 <- xiofdata(chartlogreg.risk2.2, xlogregbase2)
xihat.risk3.2 <- xiofdata(chartlogreg.risk3.2, xlogregbase3)
xihat.risk4.2 <- xiofdata(chartlogreg.risk4.2, xlogregbase4)
calibrate.risk1.2 <- SPCproperty(data = xlogregbase1, nrep = 1, chart = chartlogreg.risk1.2,
                                   reportdistr = FALSE, property = "calhitprob",
                                   params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.risk2.2 <- SPCproperty(data = xlogregbase2, nrep = 1, chart = chartlogreg.risk2.2,
                                   reportdistr = FALSE, property = "calhitprob",
                                   params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.risk3.2 <- SPCproperty(data = xlogregbase3, nrep = 1, chart = chartlogreg.risk3.2,
                                   reportdistr = FALSE, property = "calhitprob",
                                   params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.risk4.2 <- SPCproperty(data = xlogregbase4, nrep = 1, chart = chartlogreg.risk4.2,
                                   reportdistr = FALSE, property = "calhitprob",
                                   params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
par(mfrow = c(2,2))
S.risk1.2 <- runchart(chartlogreg.risk1.2, newdata = xlogregrun, xi = xihat.risk1.2)
S.risk2.2 <- runchart(chartlogreg.risk2.2, newdata = xlogregrun, xi = xihat.risk2.2)
S.risk3.2 <- runchart(chartlogreg.risk3.2, newdata = xlogregrun, xi = xihat.risk3.2)
S.risk4.2 <- runchart(chartlogreg.risk3.2, newdata = xlogregrun, xi = xihat.risk4.2)
plot(S.risk1.2, ylim = c(0, 10), xlab = "t")
abline(h = calibrate.risk1.2@raw)
plot(S.risk2.2, ylim = c(0, 10), xlab = "t")
abline(h = calibrate.risk2.2@raw, ylim = c(0, 20))
plot(S.risk3.2, ylim = c(0, 10), xlab = "t")
abline(h = calibrate.risk3.2@raw)
plot(S.risk4.2, ylim = c(0, 10), xlab = "t")
abline(h = calibrate.risk4.2@raw)



#Vlad-plots risk with CUSUM control limits
h_1.risk <- calibrate.risk1@raw
h_2.risk <- calibrate.risk2@raw
h_3.risk <- calibrate.risk3@raw
h_4.risk <- calibrate.risk4@raw
h_1.2.risk <- calibrate.risk1.2@raw
h_2.2.risk <- calibrate.risk2.2@raw
h_3.2.risk <- calibrate.risk3.2@raw
h_4.2.risk <- calibrate.risk4.2@raw
U_n.risk1 <- V.risk1 + (S.risk1-h_1.risk)/(-(0.6843))
U_n.risk2 <- V.risk2 + (S.risk2-h_2.risk)/(-(0.6724))
U_n.risk3 <- V.risk3 + (S.risk3-h_3.risk)/(-(0.7129))
U_n.risk4 <- V.risk4 + (S.risk4-h_4.risk)/(-(0.6999))
L_n.risk1 <- V.risk1 + (S.risk1.2-h_1.2.risk)/((0.6843))
L_n.risk2 <- V.risk2 + (S.risk2.2-h_2.2.risk)/((0.6724))
L_n.risk3 <- V.risk3 + (S.risk3.2-h_3.2.risk)/((0.7129))
L_n.risk4 <- V.risk4 + (S.risk4.2-h_4.2.risk)/((0.6999))
par(mfrow = c(2,2))
plot(V.risk1, ylim = c(-30, 100), xlab = "t", main = "Baseline with t = 100 observations")
lines(x=j1, y = U_n.risk1, col = "green", lwd = 2)
lines(x=j1, y = L_n.risk1, col = "red", lwd = 2)
plot(V.risk2, ylim = c(-10 , 100), xlab = "t", main = "Baseline with t = 500 observations")
lines(x=j2, y = U_n.risk2, col = "green", lwd = 2)
lines(x=j2, y = L_n.risk2, col = "red", lwd = 2)
plot(V.risk3, xlab = "t", ylim = c(-10, 100),main = "Baseline with t = 1000 observations")
lines(x=j3, y = U_n.risk3, col = "green", lwd = 2)
lines(x=j3, y = L_n.risk3, col = "red", lwd = 2)
plot(V.risk4, xlab = "t", ylim = c(-10, 100), main = "Baseline with t = 1500 observations")
lines(x=j4, y = U_n.risk4, col = "green", lwd = 2)
lines(x=j4, y = L_n.risk4, col = "red", lwd = 2)
#OE
#No-risk CUSUM plots
chartlogreg.norisk1 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = -0.55, 
                                                                      formula = y~1))
chartlogreg.norisk2 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = -0.55, 
                                                                      formula = y~1))
chartlogreg.norisk3 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = -0.55, 
                                                                      formula = y~1))
chartlogreg.norisk4 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = -0.55, 
                                                                      formula = y~1))
xihat.norisk1 <- xiofdata(chartlogreg.norisk1, xlogregbase1)
xihat.norisk2 <- xiofdata(chartlogreg.norisk2, xlogregbase2)
xihat.norisk3 <- xiofdata(chartlogreg.norisk3, xlogregbase3)
xihat.norisk4 <- xiofdata(chartlogreg.norisk4, xlogregbase4)
calibrate.norisk1 <- SPCproperty(data = xlogregbase1, nrep = 1, chart = chartlogreg.norisk1,
                                 reportdistr = FALSE, property = "calhitprob",
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.norisk2 <- SPCproperty(data = xlogregbase2, nrep = 1, chart = chartlogreg.norisk2,
                                 reportdistr = FALSE, property = "calhitprob",
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.norisk3 <- SPCproperty(data = xlogregbase3, nrep = 1, chart = chartlogreg.norisk3,
                                 reportdistr = FALSE, property = "calhitprob",
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.norisk4 <- SPCproperty(data = xlogregbase4, nrep = 1, chart = chartlogreg.norisk4,
                                 reportdistr = FALSE, property = "calhitprob",
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
par(mfrow = c(2,2))
S.norisk1 <- runchart(chartlogreg.norisk1, newdata = xlogregrun, xi = xihat.norisk1)
S.norisk2 <- runchart(chartlogreg.norisk2, newdata = xlogregrun, xi = xihat.norisk2)
S.norisk3 <- runchart(chartlogreg.norisk3, newdata = xlogregrun, xi = xihat.norisk3)
S.norisk4 <- runchart(chartlogreg.norisk4, newdata = xlogregrun, xi = xihat.norisk4)
par(mfrow = c(2,2))
plot(S.norisk1, ylim = c(0, 400), xlab = "t", main = "Baseline with t = 100 observations")
abline(h = calibrate.norisk1@raw)
plot(S.norisk2, ylim = c(0, 400), xlab = "t", main = "Baseline with t = 500 observations")
abline(h = calibrate.norisk2@raw)
plot(S.norisk3, ylim = c(0, 400), xlab = "t", main = "Baseline with t = 1000 observations")
abline(h = calibrate.norisk3@raw)
plot(S.norisk4, ylim = c(0,400), xlab = "t", main = "Baseline with t = 1500 observations")
abline(h = calibrate.norisk4@raw)

#risk CUSUM plots
chartlogreg.risk1 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = -0.25, 
                                                                    formula = y~x1+x2+x3))
chartlogreg.risk2 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = -0.25, 
                                                                    formula = y~x1+x2+x3))
chartlogreg.risk3 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = -0.25, 
                                                                    formula = y~x1+x2+x3))
chartlogreg.risk4 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = -0.25, 
                                                                    formula = y~x1+x2+x3))
xihat.risk1 <- xiofdata(chartlogreg.risk1, xlogregbase1)
xihat.risk2 <- xiofdata(chartlogreg.risk2, xlogregbase2)
xihat.risk3 <- xiofdata(chartlogreg.risk3, xlogregbase3)
xihat.risk4 <- xiofdata(chartlogreg.risk4, xlogregbase4)
calibrate.risk1 <- SPCproperty(data = xlogregbase1, nrep = 1, chart = chartlogreg.risk1,
                               reportdistr = FALSE, property = "calhitprob",
                               params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.risk2 <- SPCproperty(data = xlogregbase2, nrep = 1, chart = chartlogreg.risk2,
                               reportdistr = FALSE, property = "calhitprob",
                               params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.risk3 <- SPCproperty(data = xlogregbase3, nrep = 1, chart = chartlogreg.risk3,
                               reportdistr = FALSE, property = "calhitprob",
                               params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.risk4 <- SPCproperty(data = xlogregbase4, nrep = 1, chart = chartlogreg.risk4,
                               reportdistr = FALSE, property = "calhitprob",
                               params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
par(mfrow = c(2,2))
S.risk1 <- runchart(chartlogreg.risk1, newdata = xlogregrun1, xi = xihat.risk1)
S.risk2 <- runchart(chartlogreg.risk2, newdata = xlogregrun2, xi = xihat.risk2)
S.risk3 <- runchart(chartlogreg.risk3, newdata = xlogregrun3, xi = xihat.risk3)
S.risk4 <- runchart(chartlogreg.risk4, newdata = xlogregrun4, xi = xihat.risk4)
plot(S.risk1, ylim = c(0, 400), xlab = "t", main = "Baseline with t = 100 observations")
abline(h = calibrate.risk1@raw)
plot(S.risk2, ylim = c(0,400), xlab = "t", main = "Baseline with t = 500 observations")
abline(h = calibrate.risk2@raw)
plot(S.risk3, ylim = c(0,400), xlab = "t", main = "Baseline with t = 1000 observations")
abline(h = calibrate.risk3@raw)
plot(S.risk4, ylim = c(0,400), xlab = "t", main = "Baseline with t = 1500 observations")
abline(h = calibrate.risk4@raw)


#No-risk CUSUM plots positive delta
chartlogreg.norisk1.2 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = 0.25, 
                                                                        formula = y~1))
chartlogreg.norisk2.2 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = 0.25, 
                                                                        formula = y~1))
chartlogreg.norisk3.2 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = 0.25, 
                                                                        formula = y~1))
chartlogreg.norisk4.2 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = 0.25, 
                                                                        formula = y~1))
xihat.norisk1.2 <- xiofdata(chartlogreg.norisk1.2, xlogregbase1)
xihat.norisk2.2 <- xiofdata(chartlogreg.norisk2.2, xlogregbase2)
xihat.norisk3.2 <- xiofdata(chartlogreg.norisk3.2, xlogregbase3)
xihat.norisk4.2 <- xiofdata(chartlogreg.norisk4.2, xlogregbase4)
calibrate.norisk1.2 <- SPCproperty(data = xlogregbase1, nrep = 1, chart = chartlogreg.norisk1.2,
                                   reportdistr = FALSE, property = "calhitprob",
                                   params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.norisk2.2 <- SPCproperty(data = xlogregbase2, nrep = 1, chart = chartlogreg.norisk2.2,
                                   reportdistr = FALSE, property = "calhitprob",
                                   params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.norisk3.2 <- SPCproperty(data = xlogregbase3, nrep = 1, chart = chartlogreg.norisk3.2,
                                   reportdistr = FALSE, property = "calhitprob",
                                   params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.norisk4.2 <- SPCproperty(data = xlogregbase4, nrep = 1, chart = chartlogreg.norisk4.2,
                                   reportdistr = FALSE, property = "calhitprob",
                                   params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
par(mfrow = c(2,2))
S.norisk1.2 <- runchart(chartlogreg.norisk1.2, newdata = xlogregrun1, xi = xihat.norisk1.2)
S.norisk2.2 <- runchart(chartlogreg.norisk2.2, newdata = xlogregrun2, xi = xihat.norisk2.2)
S.norisk3.2 <- runchart(chartlogreg.norisk3.2, newdata = xlogregrun3, xi = xihat.norisk3.2)
S.norisk4.2 <- runchart(chartlogreg.norisk3.2, newdata = xlogregrun4, xi = xihat.norisk4.2)
plot(S.norisk1.2, ylim = c(0, 5), xlab = "t", main = "Baseline with t = 100 observations")
abline(h = calibrate.norisk1.2@raw)
plot(S.norisk2.2, ylim = c(0, 5), xlab = "t", main = "Baseline with t = 500 observations")
abline(h = calibrate.norisk2.2@raw)
plot(S.norisk3.2, ylim = c(0,5), xlab = "t", main = "Baseline with t = 1000 observations")
abline(h = calibrate.norisk3.2@raw)
plot(S.norisk4.2, ylim = c(0,5), xlab = "t", main = "Baseline with t = 1500 observations")
abline(h = calibrate.norisk4.2@raw)

#risk CUSUM plots positive delta
chartlogreg.risk1.2 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = 0.25, 
                                                                      formula = y~x1+x2+x3))
chartlogreg.risk2.2 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = 0.25, 
                                                                      formula = y~x1+x2+x3))
chartlogreg.risk3.2 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = 0.25, 
                                                                      formula = y~x1+x2+x3))
chartlogreg.risk4.2 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = 0.25, 
                                                                      formula = y~x1+x2+x3))
xihat.risk1.2 <- xiofdata(chartlogreg.risk1.2, xlogregbase1)
xihat.risk2.2 <- xiofdata(chartlogreg.risk2.2, xlogregbase2)
xihat.risk3.2 <- xiofdata(chartlogreg.risk3.2, xlogregbase3)
xihat.risk4.2 <- xiofdata(chartlogreg.risk4.2, xlogregbase4)
calibrate.risk1.2 <- SPCproperty(data = xlogregbase1, nrep = 1, chart = chartlogreg.risk1.2,
                                 reportdistr = FALSE, property = "calhitprob",
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.risk2.2 <- SPCproperty(data = xlogregbase2, nrep = 1, chart = chartlogreg.risk2.2,
                                 reportdistr = FALSE, property = "calhitprob",
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.risk3.2 <- SPCproperty(data = xlogregbase3, nrep = 1, chart = chartlogreg.risk3.2,
                                 reportdistr = FALSE, property = "calhitprob",
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
calibrate.risk4.2 <- SPCproperty(data = xlogregbase4, nrep = 1, chart = chartlogreg.risk4.2,
                                 reportdistr = FALSE, property = "calhitprob",
                                 params = list(target = 0.05, nsteps = 1000, gridpoints = 1000))
par(mfrow = c(2,2))
S.risk1.2 <- runchart(chartlogreg.risk1.2, newdata = xlogregrun1, xi = xihat.risk1.2)
S.risk2.2 <- runchart(chartlogreg.risk2.2, newdata = xlogregrun2, xi = xihat.risk2.2)
S.risk3.2 <- runchart(chartlogreg.risk3.2, newdata = xlogregrun3, xi = xihat.risk3.2)
S.risk4.2 <- runchart(chartlogreg.risk3.2, newdata = xlogregrun4, xi = xihat.risk4.2)
plot(S.risk1.2, ylim = c(0, 5), xlab = "t", main = "Baseline with t = 100 observations")
abline(h = calibrate.risk1.2@raw)
plot(S.risk2.2, ylim = c(0, 5), xlab = "t", main = "Baseline with t = 500 observations")
abline(h = calibrate.risk2.2@raw)
plot(S.risk3.2, ylim = c(0, 5), xlab = "t", main = "Baseline with t = 1000 observations")
abline(h = calibrate.risk3.2@raw)
plot(S.risk4.2, ylim = c(0, 5), xlab = "t", main = "Baseline with t = 1500 observations")
abline(h = calibrate.risk4.2@raw)

#Vlad-plots with CUSUM control limits
h_1.norisk <- calibrate.norisk1@raw
h_2.norisk <- calibrate.norisk2@raw
h_3.norisk <- calibrate.norisk3@raw
h_4.norisk <- calibrate.norisk4@raw
h_1.2.norisk <- calibrate.norisk1.2@raw
h_2.2.norisk <- calibrate.norisk2.2@raw
h_3.2.norisk <- calibrate.norisk3.2@raw
h_4.2.norisk <- calibrate.norisk4.2@raw
U_n.norisk1 <- V.nonrisk1 + (S.norisk1-h_1.norisk)/-exp(-(0.25))
U_n.norisk2 <- V.nonrisk2 + (S.norisk2-h_2.norisk)/-exp(-(0.25))
U_n.norisk3 <- V.nonrisk3 + (S.norisk3-h_3.norisk)/-exp(-(0.25))
U_n.norisk4 <- V.nonrisk4 + (S.norisk4-h_4.norisk)/-exp(-(0.25))
L_n.norisk1 <- V.nonrisk1 + (S.norisk1.2-h_1.2.norisk)/exp((0.25))
L_n.norisk2 <- V.nonrisk2 + (S.norisk2.2-h_2.2.norisk)/exp((0.25))
L_n.norisk3 <- V.nonrisk3 + (S.norisk3.2-h_3.2.norisk)/exp((0.25))
L_n.norisk4 <- V.nonrisk4 + (S.norisk4.2-h_4.2.norisk)/exp((0.25))
par(mfrow = c(2,2))
plot(V.nonrisk1, ylim = c(-20, 200),  xlab = "t", main = "Baseline with t = 100 observations")
lines(x=j1, y = U_n.norisk1, col = "green", lwd = 2)
lines(x=j1, y = L_n.norisk1, col = "red", lwd = 2)
plot(V.nonrisk2, ylim = c(-15, 300), xlab = "t", main = "Baseline with t = 500 observations")
lines(x=j2, y = U_n.norisk2, col = "green", lwd = 2)
lines(x=j2, y = L_n.norisk2, col = "red", lwd = 2)
plot(V.nonrisk3, ylim = c(-10, 300), xlab = "t", main = "Baseline with t = 1000 observations")
lines(x=j3, y = U_n.norisk3, col = "green", lwd = 2)
lines(x=j3, y = L_n.norisk3, col = "red", lwd = 2)
plot(V.nonrisk4, ylim = c(-10, 1000), xlab = "t", main = "Baseline with t = 1500 observations")
lines(x=j4, y = U_n.norisk4, col = "green", lwd = 2)
lines(x=j4, y = L_n.norisk4, col = "red", lwd = 2)

#Vlad-plots risk with CUSUM control limits
h_1.risk <- calibrate.risk1@raw
h_2.risk <- calibrate.risk2@raw
h_3.risk <- calibrate.risk3@raw
h_4.risk <- calibrate.risk4@raw
h_1.2.risk <- calibrate.risk1.2@raw
h_2.2.risk <- calibrate.risk2.2@raw
h_3.2.risk <- calibrate.risk3.2@raw
h_4.2.risk <- calibrate.risk4.2@raw
U_n.risk1 <- V.risk1 + (S.risk1-h_1.risk)/-exp(-(0.25))
U_n.risk2 <- V.risk2 + (S.risk2-h_2.risk)/-exp(-(0.25))
U_n.risk3 <- V.risk3 + (S.risk3-h_3.risk)/-exp(-(0.25))
U_n.risk4 <- V.risk4 + (S.risk4-h_4.risk)/-exp(-(0.25))
L_n.risk1 <- V.risk1 + (S.risk1.2-h_1.2.risk)/exp((0.25))
L_n.risk2 <- V.risk2 + (S.risk2.2-h_2.2.risk)/exp((0.25))
L_n.risk3 <- V.risk3 + (S.risk3.2-h_3.2.risk)/exp((0.25))
L_n.risk4 <- V.risk4 + (S.risk4.2-h_4.2.risk)/exp((0.25))
par(mfrow = c(2,2))
plot(V.risk1, ylim = c(0, 200), xlab = "t", main = "Baseline with t = 100 observations")
lines(x=j1, y = U_n.risk1, col = "green", lwd = 2)
lines(x=j1, y = L_n.risk1, col = "red", lwd = 2)
plot(V.risk2, ylim = c(0, 250), xlab = "t", main = "Baseline with t = 500 observations")
lines(x=j2, y = U_n.risk2, col = "green", lwd = 2)
lines(x=j2, y = L_n.risk2, col = "red", lwd = 2)
plot(V.risk3, ylim = c(0, 300), xlab = "t", main = "Baseline with t = 1000 observations")
lines(x=j3, y = U_n.risk3, col = "green", lwd = 2)
lines(x=j3, y = L_n.risk3, col = "red", lwd = 2)
plot(V.risk4, ylim = c(-60, 300), xlab = "t", main = "Baseline with t = 1500 observations")
lines(x=j4, y = U_n.risk4, col = "green", lwd = 2)
lines(x=j4, y = L_n.risk4, col = "red", lwd = 2)

