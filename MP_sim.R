rm(list = ls())
#Deciding L for MP in a simulated set
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

xlogregrun<- data.frame(x1 = rbinom(mrun, 1, 0.2), x2 = runif(mrun, 0, 1), x3 = rnorm(mrun, 0, 2))

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

#Creating MP no-risk
w_1 <- 100
w_2 <- 125
w_3 <- 200
w_4 <- 250
L_1 <- 3.456
L_2 <- 3.385
L_3 <- 3.261
L_4 <- 3.272
nobs1 <- dim(xlogregrun)[1]
nobs2 <- dim(xlogregrun)[1]
nobs3 <- dim(xlogregrun)[1]
nobs4 <- dim(xlogregrun)[1]
p.norisk1 <- predict(glm.reg.norisk1, newdata = xlogregrun, type = "response")
p.norisk2 <- predict(glm.reg.norisk2, newdata = xlogregrun, type = "response")
p.norisk3 <- predict(glm.reg.norisk3, newdata = xlogregrun, type = "response")
p.norisk4 <- predict(glm.reg.norisk4, newdata = xlogregrun, type = "response")
MP.norisk1 <- numeric(0)
variance_MP.norisk1 <- numeric(0)
MP.norisk2 <- numeric(0)
variance_MP.norisk2 <- numeric(0)
MP.norisk3 <- numeric(0)
variance_MP.norisk3 <- numeric(0)
MP.norisk4 <- numeric(0)
variance_MP.norisk4 <- numeric(0)
for(l in 1:nobs1){
  MP.norisk1[l] <- (1/(min(w_1,l)))* sum(xlogregrun$y[max(1,l-min(w_1,l)):l]-p.norisk1[max(1,l-min(w_1,l)):l])
  variance_MP.norisk1[l] <- (1/min(w_1,l))*m1.base*(1-m1.base)
}
for(l in 1:nobs2){
  MP.norisk2[l] <- (1/(min(w_2,l)))* sum(xlogregrun$y[max(1,l-min(w_2,l)):l]-p.norisk2[max(1,l-min(w_2,l)):l])
  variance_MP.norisk2[l] <- (1/min(w_2,l))*m2.base*(1-m2.base)
}
for(l in 1:nobs3){
  MP.norisk3[l] <- (1/(min(w_3,l)))* sum(xlogregrun$y[max(1,l-min(w_3,l)):l]-p.norisk3[max(1,l-min(w_3,l)):l])
  variance_MP.norisk3[l] <- (1/min(w_3,l))*m3.base*(1-m3.base)
}
for(l in 1:nobs4){
  MP.norisk4[l] <- (1/(min(w_4,l)))* sum(xlogregrun$y[max(1,l-min(w_4,l)):l]-p.norisk4[max(1,l-min(w_4,l)):l])
  variance_MP.norisk4[l] <- (1/min(w_4,l))*m4.base*(1-m4.base)
}
j_1 <- seq(from = 1, to = 1000, by = 1)
j_2 <- seq(from = 1, to = 1000, by = 1)
j_3 <- seq(from = 1, to = 1000, by = 1)
j_4 <- seq(from = 1, to = 1000, by = 1)
UCL.norisk1 <- 0 + L_1*sqrt(variance_MP.norisk1)
LCL.norisk1 <- 0 - L_1*sqrt(variance_MP.norisk1)
UCL.norisk2 <- 0 + L_2*sqrt(variance_MP.norisk2)
LCL.norisk2 <- 0 - L_2*sqrt(variance_MP.norisk2)
UCL.norisk3 <- 0 + L_3*sqrt(variance_MP.norisk3)
LCL.norisk3 <- 0 - L_3*sqrt(variance_MP.norisk3)
UCL.norisk4 <- 0 + L_4*sqrt(variance_MP.norisk4)
LCL.norisk4 <- 0 - L_4*sqrt(variance_MP.norisk4)
par(mfrow = c(2, 2))
plot(x=j_1, y = MP.norisk1, ylim = c(min(LCL.norisk1) - 0.1, max(UCL.norisk1) + 0.1), xlab = "t", main = "Baseline with n = 100 observations")
lines(x=j_1, y = UCL.norisk1, col = "green", lwd = 2)
lines(x=j_1, y = LCL.norisk1, col = "red", lwd = 2)
plot(x=j_2, y = MP.norisk2, ylim = c(min(LCL.norisk2) - 0.1, max(UCL.norisk2) + 0.1), xlab = "t", main = "Baseline with n = 500 observations")
lines(x=j_2, y = UCL.norisk2, col = "green", lwd = 2)
lines(x=j_2, y = LCL.norisk2, col = "red", lwd = 2)
plot(x=j_3, y = MP.norisk3, ylim = c(min(LCL.norisk3) - 0.1, max(UCL.norisk3) + 0.1), xlab = "t", main = "Baseline with n = 1000 observations")
lines(x=j_3, y = UCL.norisk3, col = "green", lwd = 2)
lines(x=j_3, y = LCL.norisk3, col = "red", lwd = 2)
plot(x=j_4, y = MP.norisk4, ylim = c(min(LCL.norisk4) - 0.1, max(UCL.norisk4) + 0.1), xlab = "t", main = "Baseline with n = 1500 observations")
lines(x=j_4, y = UCL.norisk4, col = "green", lwd = 2)
lines(x=j_4, y = LCL.norisk4, col = "red", lwd = 2)

#Creating MP risk
w_1 <- 100
w_2 <- 125
w_3 <- 200
w_4 <- 250
L_1 <- 3.827
L_2 <- 3.815
L_3 <- 3.570
L_4 <- 3.540
nobs1 <- dim(xlogregrun)[1]
nobs2 <- dim(xlogregrun)[1]
nobs3 <- dim(xlogregrun)[1]
nobs4 <- dim(xlogregrun)[1]
p.risk1 <- predict(glm.reg.risk1, newdata = xlogregrun, type = "response")
p.risk2 <- predict(glm.reg.risk2, newdata = xlogregrun, type = "response")
p.risk3 <- predict(glm.reg.risk3, newdata = xlogregrun, type = "response")
p.risk4 <- predict(glm.reg.risk4, newdata = xlogregrun, type = "response")
pbase1 <- mean(p.risk1)
pbase2 <- mean(p.risk2)
pbase3 <- mean(p.risk3)
pbase4 <- mean(p.risk4)
MP.risk1 <- numeric(0)
variance_MP.risk1 <- numeric(0)
MP.risk2 <- numeric(0)
variance_MP.risk2 <- numeric(0)
MP.risk3 <- numeric(0)
variance_MP.risk3 <- numeric(0)
MP.risk4 <- numeric(0)
variance_MP.risk4 <- numeric(0)
for(l in 1:nobs1){
  MP.risk1[l] <- (1/(min(w_1,l)))* sum(xlogregrun$y[max(1,l-min(w_1,l)):l]-p.risk1[max(1,l-min(w_1,l)):l])
  variance_MP.risk1[l] <- (1/min(w_1,l))*pbase1*(1-pbase1)
}
for(l in 1:nobs2){
  MP.risk2[l] <- (1/(min(w_2,l)))* sum(xlogregrun$y[max(1,l-min(w_2,l)):l]-p.risk2[max(1,l-min(w_2,l)):l])
  variance_MP.risk2[l] <- (1/min(w_2,l))*pbase2*(1-pbase2)
}
for(l in 1:nobs3){
  MP.risk3[l] <- (1/(min(w_3,l)))* sum(xlogregrun$y[max(1,l-min(w_3,l)):l]-p.risk3[max(1,l-min(w_3,l)):l])
  variance_MP.risk3[l] <- (1/min(w_3,l))*pbase3*(1-pbase3)
}
for(l in 1:nobs4){
  MP.risk4[l] <- (1/(min(w_4,l)))* sum(xlogregrun$y[max(1,l-min(w_4,l)):l]-p.risk4[max(1,l-min(w_4,l)):l])
  variance_MP.risk4[l] <- (1/min(w_4,l))*pbase4*(1-pbase4)
}
j_1 <- seq(from = 1, to = 1000, by = 1)
j_2 <- seq(from = 1, to = 1000, by = 1)
j_3 <- seq(from = 1, to = 1000, by = 1)
j_4 <- seq(from = 1, to = 1000, by = 1)
UCL.risk1 <- 0 + L_1*sqrt(variance_MP.risk1)
LCL.risk1 <- 0 - L_1*sqrt(variance_MP.risk1)
UCL.risk2 <- 0 + L_2*sqrt(variance_MP.risk2)
LCL.risk2 <- 0 - L_2*sqrt(variance_MP.risk2)
UCL.risk3 <- 0 + L_3*sqrt(variance_MP.risk3)
LCL.risk3 <- 0 - L_3*sqrt(variance_MP.risk3)
UCL.risk4 <- 0 + L_4*sqrt(variance_MP.risk4)
LCL.risk4 <- 0 - L_4*sqrt(variance_MP.risk4)
par(mfrow = c(2, 2))
plot(x=j_1, y = MP.risk1, ylim = c(min(LCL.risk1) - 0.1, max(UCL.risk1) + 0.1), xlab = "t", main = "Baseline with t = 100 observations")
lines(x=j_1, y = UCL.risk1, col = "green", lwd = 2)
lines(x=j_1, y = LCL.risk1, col = "red", lwd = 2)
plot(x=j_2, y = MP.risk2, ylim = c(min(LCL.risk2) - 0.1, max(UCL.risk2) + 0.1), xlab = "t",  main = "Baseline with t = 500 observations")
lines(x=j_2, y = UCL.risk2, col = "green", lwd = 2)
lines(x=j_2, y = LCL.risk2, col = "red", lwd = 2)
plot(x=j_3, y = MP.risk3, ylim = c(min(LCL.risk3) - 0.1, max(UCL.risk3) + 0.1), xlab = "t",  main = "Baseline with t = 1000 observations")
lines(x=j_3, y = UCL.risk3, col = "green", lwd = 2)
lines(x=j_3, y = LCL.risk3, col = "red", lwd = 2)
plot(x=j_4, y = MP.risk4, ylim = c(min(LCL.risk4) - 0.1, max(UCL.risk4) + 0.1), xlab = "t",  main = "Baseline with t = 1500 observations")
lines(x=j_4, y = UCL.risk4, col = "green", lwd = 2)
lines(x=j_4, y = LCL.risk4, col = "red", lwd = 2)

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

#VLAD with MP limits no risk
h_1.1.norisk <- UCL.norisk1
h_2.1.norisk <- UCL.norisk2
h_3.1.norisk <- UCL.norisk3
h_4.1.norisk <- UCL.norisk4
h_1.2.norisk <- LCL.norisk1 
h_2.2.norisk <- LCL.norisk2
h_3.2.norisk <- LCL.norisk3
h_4.2.norisk <- LCL.norisk4
k.norisk1 <- 80/(abs(h_1.1.norisk)+abs(h_1.2.norisk))
k.norisk2 <- 80/(abs(h_2.1.norisk)+abs(h_2.2.norisk))
k.norisk3 <- 80/(abs(h_3.1.norisk)+abs(h_3.2.norisk))
k.norisk4 <- 80/(abs(h_4.1.norisk)+abs(h_4.2.norisk))
UCL.V.norisk1 <- V.nonrisk1 - k.norisk1*(MP.norisk1 - h_1.1.norisk)
UCL.V.norisk2 <- V.nonrisk2 - k.norisk2*(MP.norisk2 - h_2.1.norisk)
UCL.V.norisk3 <- V.nonrisk3 - k.norisk3*(MP.norisk3 - h_3.1.norisk)
UCL.V.norisk4 <- V.nonrisk4 - k.norisk4*(MP.norisk4 - h_4.1.norisk)
LCL.V.norisk1 <- V.nonrisk1 - k.norisk1*(MP.norisk1 - h_1.2.norisk)
LCL.V.norisk2 <- V.nonrisk2 - k.norisk2*(MP.norisk2 - h_2.2.norisk)
LCL.V.norisk3 <- V.nonrisk3 - k.norisk3*(MP.norisk3 - h_3.2.norisk)
LCL.V.norisk4 <- V.nonrisk4 - k.norisk4*(MP.norisk4 - h_4.2.norisk)
par(mfrow = c(2, 2))
plot(V.nonrisk1, ylim = c(-100, 200), xlab = "t", main = "Baseline with n = 100 observations")
lines(x=j1, y = LCL.V.norisk1, col = "red", lwd = 2)
lines(x=j1, y = UCL.V.norisk1, col = "green", lwd = 2)
plot(V.nonrisk2, ylim = c(-100, 200), xlab = "t", main = "Baseline with n = 500 observations")
lines(x=j2, y = LCL.V.norisk2, col = "red", lwd = 2)
lines(x=j2, y = UCL.V.norisk2, col = "green", lwd = 2)
plot(V.nonrisk3, ylim = c(-100, 200), xlab = "t",, main = "Baseline with n = 1000 observations")
lines(x=j3, y = LCL.V.norisk3, col = "red", lwd = 2)
lines(x=j3, y = UCL.V.norisk3, col = "green", lwd = 2)
plot(V.nonrisk4, ylim = c(-100, 200), xlab = "t", , main = "Baseline with n = 1500 observations")
lines(x=j4, y = LCL.V.norisk4, col = "red", lwd = 2)
lines(x=j4, y = UCL.V.norisk4, col = "green", lwd = 2)

#VLAD with MP limits risk
h_1.1.risk <- UCL.risk1
h_2.1.risk <- UCL.risk2
h_3.1.risk <- UCL.risk3
h_4.1.risk <- UCL.risk4
h_1.2.risk <- LCL.risk1 
h_2.2.risk <- LCL.risk2
h_3.2.risk <- LCL.risk3
h_4.2.risk <- LCL.risk4
k.risk1 <- 80/(abs(h_1.1.risk)+abs(h_1.2.risk))
k.risk2 <- 80/(abs(h_2.1.risk)+abs(h_2.2.risk))
k.risk3 <- 80/(abs(h_3.1.risk)+abs(h_3.2.risk))
k.risk4 <- 80/(abs(h_4.1.risk)+abs(h_4.2.risk))
UCL.V.risk1 <- V.risk1 - k.risk1*(MP.risk1 - h_1.1.risk)
UCL.V.risk2 <- V.risk2 - k.risk2*(MP.risk2 - h_2.1.risk)
UCL.V.risk3 <- V.risk3 - k.risk3*(MP.risk3 - h_3.1.risk)
UCL.V.risk4 <- V.risk4 - k.risk4*(MP.risk4 - h_4.1.risk)
LCL.V.risk1 <- V.risk1 - k.risk1*(MP.risk1 - h_1.2.risk)
LCL.V.risk2 <- V.risk2 - k.risk2*(MP.risk2 - h_2.2.risk)
LCL.V.risk3 <- V.risk3 - k.risk3*(MP.risk3 - h_3.2.risk)
LCL.V.risk4 <- V.risk4 - k.risk4*(MP.risk4 - h_4.2.risk)
par(mfrow = c(2, 2))
plot(V.risk1, ylim = c(-100, 200), xlab = "t")
lines(x=j1, y = LCL.V.risk1, col = "red", lwd = 2)
lines(x=j1, y = UCL.V.risk1, col = "green", lwd = 2)
plot(V.risk2, ylim = c(-100, 200), xlab = "t")
lines(x=j2, y = LCL.V.risk2, col = "red", lwd = 2)
lines(x=j2, y = UCL.V.risk2, col = "green", lwd = 2)
plot(V.risk3, ylim = c(-100, 200), xlab = "t")
lines(x=j3, y = LCL.V.risk3, col = "red", lwd = 2)
lines(x=j3, y = UCL.V.risk3, col = "green", lwd = 2)
plot(V.risk4, ylim = c(-100, 200), xlab = "t")
lines(x=j4, y = LCL.V.risk4, col = "red", lwd = 2)
lines(x=j4, y = UCL.V.risk4, col = "green", lwd = 2)

#Improved MP no-risk
#Creating MP no-risk
w_1 <- 100
w_2 <- 250
w_3 <- 250
w_4 <- 375
L_1 <- 2.632
L_2 <- 2.846
L_3 <- 2.984
L_4 <- 2.945
nobs1 <- dim(xlogregrun)[1]
nobs2 <- dim(xlogregrun)[1]
nobs3 <- dim(xlogregrun)[1]
nobs4 <- dim(xlogregrun)[1]
p.norisk1 <- predict(glm.reg.norisk1, newdata = xlogregrun, type = "response")
p.norisk2 <- predict(glm.reg.norisk2, newdata = xlogregrun, type = "response")
p.norisk3 <- predict(glm.reg.norisk3, newdata = xlogregrun, type = "response")
p.norisk4 <- predict(glm.reg.norisk4, newdata = xlogregrun, type = "response")
MP.norisk1 <- numeric(0)
variance_MP.norisk1 <- numeric(0)
MP.norisk2 <- numeric(0)
variance_MP.norisk2 <- numeric(0)
MP.norisk3 <- numeric(0)
variance_MP.norisk3 <- numeric(0)
MP.norisk4 <- numeric(0)
variance_MP.norisk4 <- numeric(0)
for(l in 1:nobs1){
  MP.norisk1[l] <- (1/(min(w_1,l)))* sum(xlogregrun$y[max(1,l-min(w_1,l)):l]-p.norisk1[max(1,l-min(w_1,l)):l])
  variance_MP.norisk1[l] <- (nbase1*m1.base*(1-m1.base)+min(w_1,l)*m1.base*(1-m1.base))/(nbase1*min(w_1,l))
}
for(l in 1:nobs2){
  MP.norisk2[l] <- (1/(min(w_2,l)))* sum(xlogregrun$y[max(1,l-min(w_2,l)):l]-p.norisk2[max(1,l-min(w_2,l)):l])
  variance_MP.norisk2[l] <- (nbase2*m2.base*(1-m2.base)+min(w_2,l)*m2.base*(1-m2.base))/(nbase2*min(w_2,l))
}
for(l in 1:nobs3){
  MP.norisk3[l] <- (1/(min(w_3,l)))* sum(xlogregrun$y[max(1,l-min(w_3,l)):l]-p.norisk3[max(1,l-min(w_3,l)):l])
  variance_MP.norisk3[l] <- (nbase3*m3.base*(1-m3.base)+min(w_3,l)*m3.base*(1-m3.base))/(nbase3*min(w_3,l))
}
for(l in 1:nobs4){
  MP.norisk4[l] <- (1/(min(w_4,l)))* sum(xlogregrun$y[max(1,l-min(w_4,l)):l]-p.norisk4[max(1,l-min(w_4,l)):l])
  variance_MP.norisk4[l] <- (nbase4*m4.base*(1-m4.base)+min(w_4,l)*m4.base*(1-m4.base))/(nbase4*min(w_4,l))
}
j_1 <- seq(from = 1, to = 1000, by = 1)
j_2 <- seq(from = 1, to = 1000, by = 1)
j_3 <- seq(from = 1, to = 1000, by = 1)
j_4 <- seq(from = 1, to = 1000, by = 1)
UCL.norisk1 <- 0 + L_1*sqrt(variance_MP.norisk1)
LCL.norisk1 <- 0 - L_1*sqrt(variance_MP.norisk1)
UCL.norisk2 <- 0 + L_2*sqrt(variance_MP.norisk2)
LCL.norisk2 <- 0 - L_2*sqrt(variance_MP.norisk2)
UCL.norisk3 <- 0 + L_3*sqrt(variance_MP.norisk3)
LCL.norisk3 <- 0 - L_3*sqrt(variance_MP.norisk3)
UCL.norisk4 <- 0 + L_4*sqrt(variance_MP.norisk4)
LCL.norisk4 <- 0 - L_4*sqrt(variance_MP.norisk4)
par(mfrow = c(2, 2))
plot(x=j_1, y = MP.norisk1, ylim = c(min(LCL.norisk1) - 0.1, max(UCL.norisk1) + 0.1), xlab = "t", main = "Baseline with n = 100 observations")
lines(x=j_1, y = UCL.norisk1, col = "green", lwd = 2)
lines(x=j_1, y = LCL.norisk1, col = "red", lwd = 2)
plot(x=j_2, y = MP.norisk2, ylim = c(min(LCL.norisk2) - 0.1, max(UCL.norisk2) + 0.1), xlab = "t", main = "Baseline with n = 500 observations")
lines(x=j_2, y = UCL.norisk2, col = "green", lwd = 2)
lines(x=j_2, y = LCL.norisk2, col = "red", lwd = 2)
plot(x=j_3, y = MP.norisk3, ylim = c(min(LCL.norisk3) - 0.1, max(UCL.norisk3) + 0.1), xlab = "t", main = "Baseline with n = 1000 observations")
lines(x=j_3, y = UCL.norisk3, col = "green", lwd = 2)
lines(x=j_3, y = LCL.norisk3, col = "red", lwd = 2)
plot(x=j_4, y = MP.norisk4, ylim = c(min(LCL.norisk4) - 0.1, max(UCL.norisk4) + 0.1), xlab = "t", main = "Baseline with n = 1500 observations")
lines(x=j_4, y = UCL.norisk4, col = "green", lwd = 2)
lines(x=j_4, y = LCL.norisk4, col = "red", lwd = 2)

#Creating MP risk
w_1 <- 100
w_2 <- 250
w_3 <- 250
w_4 <- 375
L_1 <- 2.964
L_2 <- 3.181
L_3 <- 3.243
L_4 <- 3.283
nobs1 <- dim(xlogregrun)[1]
nobs2 <- dim(xlogregrun)[1]
nobs3 <- dim(xlogregrun)[1]
nobs4 <- dim(xlogregrun)[1]
p.risk1 <- predict(glm.reg.risk1, newdata = xlogregrun, type = "response")
p.risk2 <- predict(glm.reg.risk2, newdata = xlogregrun, type = "response")
p.risk3 <- predict(glm.reg.risk3, newdata = xlogregrun, type = "response")
p.risk4 <- predict(glm.reg.risk4, newdata = xlogregrun, type = "response")
p.risk1.average <- mean(p.risk1)
p.risk2.average <- mean(p.risk2)
p.risk3.average <- mean(p.risk3)
p.risk4.average <- mean(p.risk4)
pbase1 <- mean(p.risk1)
pbase2 <- mean(p.risk2)
pbase3 <- mean(p.risk3)
pbase4 <- mean(p.risk4)
MP.risk1 <- numeric(0)
variance_MP.risk1 <- numeric(0)
MP.risk2 <- numeric(0)
variance_MP.risk2 <- numeric(0)
MP.risk3 <- numeric(0)
variance_MP.risk3 <- numeric(0)
MP.risk4 <- numeric(0)
variance_MP.risk4 <- numeric(0)
for(l in 1:nobs1){
  MP.risk1[l] <- (1/(min(w_1,l)))* sum(xlogregrun$y[max(1,l-min(w_1,l)):l]-p.risk1[max(1,l-min(w_1,l)):l])
  variance_MP.risk1[l] <- ((p.risk1.average*(1-p.risk1.average))/(nbase1*min(w_1,l)))*(nbase1+min(w_1,l))
}
for(l in 1:nobs2){
  MP.risk2[l] <- (1/(min(w_2,l)))* sum(xlogregrun$y[max(1,l-min(w_2,l)):l]-p.risk2[max(1,l-min(w_2,l)):l])
  variance_MP.risk2[l] <- ((p.risk2.average*(1-p.risk2.average))/(nbase2*min(w_2,l)))*(nbase2+min(w_2,l))
}
for(l in 1:nobs3){
  MP.risk3[l] <- (1/(min(w_3,l)))* sum(xlogregrun$y[max(1,l-min(w_3,l)):l]-p.risk3[max(1,l-min(w_3,l)):l])
  variance_MP.risk3[l] <- ((p.risk3.average*(1-p.risk3.average))/(nbase3*min(w_3,l)))*(nbase3+min(w_3,l))
}
for(l in 1:nobs4){
  MP.risk4[l] <- (1/(min(w_4,l)))* sum(xlogregrun$y[max(1,l-min(w_4,l)):l]-p.risk4[max(1,l-min(w_4,l)):l])
  variance_MP.risk4[l] <- ((p.risk4.average*(1-p.risk4.average))/(nbase4*min(w_4,l)))*(nbase4+min(w_4,l))
}
j_1 <- seq(from = 1, to = 1000, by = 1)
j_2 <- seq(from = 1, to = 1000, by = 1)
j_3 <- seq(from = 1, to = 1000, by = 1)
j_4 <- seq(from = 1, to = 1000, by = 1)
UCL.risk1 <- 0 + L_1*sqrt(variance_MP.risk1)
LCL.risk1 <- 0 - L_1*sqrt(variance_MP.risk1)
UCL.risk2 <- 0 + L_2*sqrt(variance_MP.risk2)
LCL.risk2 <- 0 - L_2*sqrt(variance_MP.risk2)
UCL.risk3 <- 0 + L_3*sqrt(variance_MP.risk3)
LCL.risk3 <- 0 - L_3*sqrt(variance_MP.risk3)
UCL.risk4 <- 0 + L_4*sqrt(variance_MP.risk4)
LCL.risk4 <- 0 - L_4*sqrt(variance_MP.risk4)
par(mfrow = c(2, 2))
plot(x=j_1, y = MP.risk1, ylim = c(min(LCL.risk1) - 0.1, max(UCL.risk1) + 0.1), xlab = "t", main = "Baseline with t = 100 observations")
lines(x=j_1, y = UCL.risk1, col = "green", lwd = 2)
lines(x=j_1, y = LCL.risk1, col = "red", lwd = 2)
plot(x=j_2, y = MP.risk2, ylim = c(min(LCL.risk2) - 0.1, max(UCL.risk2) + 0.1), xlab = "t",  main = "Baseline with t = 500 observations")
lines(x=j_2, y = UCL.risk2, col = "green", lwd = 2)
lines(x=j_2, y = LCL.risk2, col = "red", lwd = 2)
plot(x=j_3, y = MP.risk3, ylim = c(min(LCL.risk3) - 0.1, max(UCL.risk3) + 0.1), xlab = "t",  main = "Baseline with t = 1000 observations")
lines(x=j_3, y = UCL.risk3, col = "green", lwd = 2)
lines(x=j_3, y = LCL.risk3, col = "red", lwd = 2)
plot(x=j_4, y = MP.risk4, ylim = c(min(LCL.risk4) - 0.1, max(UCL.risk4) + 0.1), xlab = "t",  main = "Baseline with t = 1500 observations")
lines(x=j_4, y = UCL.risk4, col = "green", lwd = 2)
lines(x=j_4, y = LCL.risk4, col = "red", lwd = 2)

#VLAD with MP limits no risk
h_1.1.norisk <- UCL.norisk1
h_2.1.norisk <- UCL.norisk2
h_3.1.norisk <- UCL.norisk3
h_4.1.norisk <- UCL.norisk4
h_1.2.norisk <- LCL.norisk1 
h_2.2.norisk <- LCL.norisk2
h_3.2.norisk <- LCL.norisk3
h_4.2.norisk <- LCL.norisk4
k.norisk1 <- 80/(abs(h_1.1.norisk)+abs(h_1.2.norisk))
k.norisk2 <- 80/(abs(h_2.1.norisk)+abs(h_2.2.norisk))
k.norisk3 <- 80/(abs(h_3.1.norisk)+abs(h_3.2.norisk))
k.norisk4 <- 80/(abs(h_4.1.norisk)+abs(h_4.2.norisk))
UCL.V.norisk1 <- V.nonrisk1 - k.norisk1*(MP.norisk1 - h_1.1.norisk)
UCL.V.norisk2 <- V.nonrisk2 - k.norisk2*(MP.norisk2 - h_2.1.norisk)
UCL.V.norisk3 <- V.nonrisk3 - k.norisk3*(MP.norisk3 - h_3.1.norisk)
UCL.V.norisk4 <- V.nonrisk4 - k.norisk4*(MP.norisk4 - h_4.1.norisk)
LCL.V.norisk1 <- V.nonrisk1 - k.norisk1*(MP.norisk1 - h_1.2.norisk)
LCL.V.norisk2 <- V.nonrisk2 - k.norisk2*(MP.norisk2 - h_2.2.norisk)
LCL.V.norisk3 <- V.nonrisk3 - k.norisk3*(MP.norisk3 - h_3.2.norisk)
LCL.V.norisk4 <- V.nonrisk4 - k.norisk4*(MP.norisk4 - h_4.2.norisk)
par(mfrow = c(2, 2))
plot(V.nonrisk1, ylim = c(-100, 200), xlab = "t", main = "Baseline with n = 100 observations")
lines(x=j1, y = LCL.V.norisk1, col = "red", lwd = 2)
lines(x=j1, y = UCL.V.norisk1, col = "green", lwd = 2)
plot(V.nonrisk2, ylim = c(-100, 200), xlab = "t", main = "Baseline with n = 500 observations")
lines(x=j2, y = LCL.V.norisk2, col = "red", lwd = 2)
lines(x=j2, y = UCL.V.norisk2, col = "green", lwd = 2)
plot(V.nonrisk3, ylim = c(-100, 200), xlab = "t",, main = "Baseline with n = 1000 observations")
lines(x=j3, y = LCL.V.norisk3, col = "red", lwd = 2)
lines(x=j3, y = UCL.V.norisk3, col = "green", lwd = 2)
plot(V.nonrisk4, ylim = c(-100, 200), xlab = "t", , main = "Baseline with n = 1500 observations")
lines(x=j4, y = LCL.V.norisk4, col = "red", lwd = 2)
lines(x=j4, y = UCL.V.norisk4, col = "green", lwd = 2)

#VLAD with MP limits risk
h_1.1.risk <- UCL.risk1
h_2.1.risk <- UCL.risk2
h_3.1.risk <- UCL.risk3
h_4.1.risk <- UCL.risk4
h_1.2.risk <- LCL.risk1 
h_2.2.risk <- LCL.risk2
h_3.2.risk <- LCL.risk3
h_4.2.risk <- LCL.risk4
k.risk1 <- 80/(abs(h_1.1.risk)+abs(h_1.2.risk))
k.risk2 <- 80/(abs(h_2.1.risk)+abs(h_2.2.risk))
k.risk3 <- 80/(abs(h_3.1.risk)+abs(h_3.2.risk))
k.risk4 <- 80/(abs(h_4.1.risk)+abs(h_4.2.risk))
UCL.V.risk1 <- V.risk1 - k.risk1*(MP.risk1 - h_1.1.risk)
UCL.V.risk2 <- V.risk2 - k.risk2*(MP.risk2 - h_2.1.risk)
UCL.V.risk3 <- V.risk3 - k.risk3*(MP.risk3 - h_3.1.risk)
UCL.V.risk4 <- V.risk4 - k.risk4*(MP.risk4 - h_4.1.risk)
LCL.V.risk1 <- V.risk1 - k.risk1*(MP.risk1 - h_1.2.risk)
LCL.V.risk2 <- V.risk2 - k.risk2*(MP.risk2 - h_2.2.risk)
LCL.V.risk3 <- V.risk3 - k.risk3*(MP.risk3 - h_3.2.risk)
LCL.V.risk4 <- V.risk4 - k.risk4*(MP.risk4 - h_4.2.risk)
par(mfrow = c(2, 2))
plot(V.risk1, ylim = c(-100, 200), xlab = "t")
lines(x=j1, y = LCL.V.risk1, col = "red", lwd = 2)
lines(x=j1, y = UCL.V.risk1, col = "green", lwd = 2)
plot(V.risk2, ylim = c(-100, 200), xlab = "t")
lines(x=j2, y = LCL.V.risk2, col = "red", lwd = 2)
lines(x=j2, y = UCL.V.risk2, col = "green", lwd = 2)
plot(V.risk3, ylim = c(-100, 200), xlab = "t")
lines(x=j3, y = LCL.V.risk3, col = "red", lwd = 2)
lines(x=j3, y = UCL.V.risk3, col = "green", lwd = 2)
plot(V.risk4, ylim = c(-100, 200), xlab = "t")
lines(x=j4, y = LCL.V.risk4, col = "red", lwd = 2)
lines(x=j4, y = UCL.V.risk4, col = "green", lwd = 2)