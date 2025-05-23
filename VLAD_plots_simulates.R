rm(list = ls())# reset variables.
#setwd("C:/Users/1998c/OneDrive/Skrivebord/Masteroppgave")
nobs <- 1000
set.seed(10)
Delta_values <- c(-1, -0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8, 1.0)
#baseline sets, first case, nbase = nrun, different delta values
xlogregbase1 <-  data.frame(x1 = rbinom(nobs, 1, 0.2), x2 = runif(nobs, 0, 1), x3 = rnorm(nobs, 0, 2))
xbetabase1 <- xlogregbase1$x1 + xlogregbase1$x2 + xlogregbase1$x3
xlogregbase2 <-  data.frame(x1 = rbinom(nobs, 1, 0.2), x2 = runif(nobs, 0, 1), x3 = rnorm(nobs, 0, 2))
xbetabase2 <- xlogregbase2$x1 + xlogregbase2$x2 + xlogregbase2$x3
xlogregbase3 <-  data.frame(x1 = rbinom(nobs, 1, 0.2), x2 = runif(nobs, 0, 1), x3 = rnorm(nobs, 0, 2))
xbetabase3 <- xlogregbase3$x1 + xlogregbase3$x2 + xlogregbase3$x3
xlogregbase4 <-  data.frame(x1 = rbinom(nobs, 1, 0.2), x2 = runif(nobs, 0, 1), x3 = rnorm(nobs, 0, 2))
xbetabase4 <- xlogregbase4$x1 + xlogregbase4$x2 + xlogregbase4$x3
xlogregbase5 <-  data.frame(x1 = rbinom(nobs, 1, 0.2), x2 = runif(nobs, 0, 1), x3 = rnorm(nobs, 0, 2))
xbetabase5 <- xlogregbase5$x1 + xlogregbase5$x2 + xlogregbase5$x3
xlogregbase6 <-  data.frame(x1 = rbinom(nobs, 1, 0.2), x2 = runif(nobs, 0, 1), x3 = rnorm(nobs, 0, 2))
xbetabase6 <- xlogregbase6$x1 + xlogregbase6$x2 + xlogregbase6$x3
xlogregbase7 <-  data.frame(x1 = rbinom(nobs, 1, 0.2), x2 = runif(nobs, 0, 1), x3 = rnorm(nobs, 0, 2))
xbetabase7 <- xlogregbase7$x1 + xlogregbase7$x2 + xlogregbase7$x3
xlogregbase8 <-  data.frame(x1 = rbinom(nobs, 1, 0.2), x2 = runif(nobs, 0, 1), x3 = rnorm(nobs, 0, 2))
xbetabase8 <- xlogregbase8$x1 + xlogregbase8$x2 + xlogregbase8$x3
xlogregbase9 <-  data.frame(x1 = rbinom(nobs, 1, 0.2), x2 = runif(nobs, 0, 1), x3 = rnorm(nobs, 0, 2))
xbetabase9 <- xlogregbase9$x1 + xlogregbase9$x2 + xlogregbase9$x3
xlogregbase1$y_1 <- rbinom(nobs, size = 1, prob = exp(Delta_values[5] + xbetabase1)/(1+exp(Delta_values[5] + xbetabase1)))
xlogregbase2$y_2 <- rbinom(nobs, size = 1, prob = exp(Delta_values[1] + xbetabase2)/(1+exp(Delta_values[1] + xbetabase2)))
xlogregbase3$y_3 <- rbinom(nobs, size = 1, prob = exp(Delta_values[2] + xbetabase3)/(1+exp(Delta_values[2] + xbetabase3)))
xlogregbase4$y_4 <- rbinom(nobs, size = 1, prob = exp(Delta_values[3] + xbetabase4)/(1+exp(Delta_values[3] + xbetabase4)))
xlogregbase5$y_5 <- rbinom(nobs, size = 1, prob = exp(Delta_values[4] + xbetabase5)/(1+exp(Delta_values[4] + xbetabase5)))
xlogregbase6$y_6 <- rbinom(nobs, size = 1, prob = exp(Delta_values[6] + xbetabase6)/(1+exp(Delta_values[6] + xbetabase6)))
xlogregbase7$y_7 <- rbinom(nobs, size = 1, prob = exp(Delta_values[7] + xbetabase7)/(1+exp(Delta_values[7] + xbetabase7)))
xlogregbase8$y_8 <- rbinom(nobs, size = 1, prob = exp(Delta_values[8] + xbetabase8)/(1+exp(Delta_values[8] + xbetabase8)))
xlogregbase9$y_9 <- rbinom(nobs, size = 1, prob = exp(Delta_values[9] + xbetabase9)/(1+exp(Delta_values[9] + xbetabase9)))

#run set
xlogregrun <-  data.frame(x1 = rbinom(nobs, 1, 0.2), x2 = runif(nobs, 0, 1), x3 = rnorm(nobs, 0, 2))
xbetarun <- xlogregrun$x1 + xlogregrun$x2 + xlogregrun$x3
xlogregrun$y <- rbinom(nobs, size = 1, prob = exp(Delta_values[5] + xbetarun)/(1+exp(Delta_values[5] + xbetarun)))



#regression models
glm.reg.risk1 <- glm(y_1~x1+x2+x3, data = xlogregbase1, family = binomial("logit"))
summary(glm.reg.risk1)
pred1 <- predict(glm.reg.risk1, newdata = xlogregrun, type = "response")
glm.reg.risk2 <- glm(y_2~x1+x2+x3, data = xlogregbase2, family = binomial("logit"))
summary(glm.reg.risk2)
pred2 <- predict(glm.reg.risk2, newdata = xlogregrun, type = "response")
glm.reg.risk3 <- glm(y_3~x1+x2+x3, data = xlogregbase3, family = binomial("logit"))
summary(glm.reg.risk3)
pred3 <- predict(glm.reg.risk3, newdata = xlogregrun, type = "response")
glm.reg.risk4 <- glm(y_4~x1+x2+x3, data = xlogregbase4, family = binomial("logit"))
summary(glm.reg.risk4)
pred4 <- predict(glm.reg.risk4, newdata = xlogregrun, type = "response")
glm.reg.risk5 <- glm(y_5~x1+x2+x3, data = xlogregbase5, family = binomial("logit"))
summary(glm.reg.risk5)
pred5 <- predict(glm.reg.risk5, newdata = xlogregrun, type = "response")
glm.reg.risk6 <- glm(y_6~x1+x2+x3, data = xlogregbase6, family = binomial("logit"))
summary(glm.reg.risk6)
pred6 <- predict(glm.reg.risk6, newdata = xlogregrun, type = "response")
glm.reg.risk7 <- glm(y_7~x1+x2+x3, data = xlogregbase7, family = binomial("logit"))
summary(glm.reg.risk7)
pred7 <- predict(glm.reg.risk7, newdata = xlogregrun, type = "response")
glm.reg.risk8 <- glm(y_8~x1+x2+x3, data = xlogregbase8, family = binomial("logit"))
summary(glm.reg.risk8)
pred8 <- predict(glm.reg.risk8, newdata = xlogregrun, type = "response")
glm.reg.risk9 <- glm(y_9~x1+x2+x3, data = xlogregbase9, family = binomial("logit"))
summary(glm.reg.risk9)
pred9 <- predict(glm.reg.risk9, newdata = xlogregrun, type = "response")
#VLAD
j <- seq(from = 1, to = 1000)
V.risk1 <- numeric(0)
V.risk1[1] <- pred1[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk1[i] <- V.risk1[i-1] + (pred1[i] - xlogregrun$y[i])
}
V.risk2 <- numeric(0)
V.risk2[1] <- pred2[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk2[i] <- V.risk2[i-1] + (pred2[i] - xlogregrun$y[i])
}
V.risk3 <- numeric(0)
V.risk3[1] <- pred3[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk3[i] <- V.risk3[i-1] + (pred3[i] - xlogregrun$y[i])
}
V.risk4 <- numeric(0)
V.risk4[1] <- pred4[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk4[i] <- V.risk4[i-1] + (pred4[i] - xlogregrun$y[i])
}
V.risk5 <- numeric(0)
V.risk5[1] <- pred5[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk5[i] <- V.risk5[i-1] + (pred5[i] - xlogregrun$y[i])
}
V.risk6 <- numeric(0)
V.risk6[1] <- pred6[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk6[i] <- V.risk6[i-1] + (pred6[i] - xlogregrun$y[i])
}
V.risk7 <- numeric(0)
V.risk7[1] <- pred7[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk7[i] <- V.risk7[i-1] + (pred7[i] - xlogregrun$y[i])
}
V.risk8 <- numeric(0)
V.risk8[1] <- pred8[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk8[i] <- V.risk8[i-1] + (pred8[i] - xlogregrun$y[i])
}
V.risk9 <- numeric(0)
V.risk9[1] <- pred9[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk9[i] <- V.risk9[i-1] + (pred9[i] - xlogregrun$y[i])
}
par(mfrow = c(1,1))
plot(x=j, y = V.risk1, type = "l", ylim = c(-140, 140), ylab = "V_t", xlab = "t", col = "magenta", 
     lwd = 2, xlim = c(-3, 1000), main = "Case 1")
lines(x = j, y = V.risk2, col = "red", lwd = 2)
lines(x = j, y = V.risk3, col = "blue", lwd = 2)
lines(x = j, y = V.risk4, col = "green", lwd = 2)
lines(x = j, y = V.risk5, col = "pink", lwd = 2)
lines(x = j, y = V.risk6, col = "purple", lwd = 2)
lines(x = j, y = V.risk7, col = "orange", lwd = 2)
lines(x = j, y = V.risk8, col = "brown", lwd = 2)
lines(x = j, y = V.risk9, col = "cyan", lwd = 2)
abline(h = 0, lty = 2)
#legend(x = -3, y = -30, legend = c("1.0", "0.8", "0.5", "0.2", "0", "-0.2", "-0.5", "-0.8", "-1.0"), 
       #col = c("cyan", "brown", "orange", "purple", "magenta", "pink","green","blue","red" ),
       #lty = c(1, 1, 1, 1 ,1, 1, 1, 1, 1))

#Second case, baseline sets with different values of nbase, same delta for all
rm(list = ls())
Delta_values <- c(-1, -0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8, 1.0)
nobs <- 1000
set.seed(10)
xlogregbase1 <-  data.frame(x1 = rbinom(50, 1, 0.2), x2 = runif(50, 0, 1), x3 = rnorm(50, 0, 2))
xbetabase1 <- xlogregbase1$x1 + xlogregbase1$x2 + xlogregbase1$x3
xlogregbase2 <-  data.frame(x1 = rbinom(100, 1, 0.2), x2 = runif(100, 0, 1), x3 = rnorm(100, 0, 2))
xbetabase2 <- xlogregbase2$x1 + xlogregbase2$x2 + xlogregbase2$x3
xlogregbase3 <-  data.frame(x1 = rbinom(200, 1, 0.2), x2 = runif(200, 0, 1), x3 = rnorm(200, 0, 2))
xbetabase3 <- xlogregbase3$x1 + xlogregbase3$x2 + xlogregbase3$x3
xlogregbase4 <-  data.frame(x1 = rbinom(400, 1, 0.2), x2 = runif(400, 0, 1), x3 = rnorm(400, 0, 2))
xbetabase4 <- xlogregbase4$x1 + xlogregbase4$x2 + xlogregbase4$x3
xlogregbase5 <-  data.frame(x1 = rbinom(500, 1, 0.2), x2 = runif(500, 0, 1), x3 = rnorm(500, 0, 2))
xbetabase5 <- xlogregbase5$x1 + xlogregbase5$x2 + xlogregbase5$x3
xlogregbase6 <-  data.frame(x1 = rbinom(800, 1, 0.2), x2 = runif(800, 0, 1), x3 = rnorm(800, 0, 2))
xbetabase6 <- xlogregbase6$x1 + xlogregbase6$x2 + xlogregbase6$x3
xlogregbase7 <-  data.frame(x1 = rbinom(1000, 1, 0.2), x2 = runif(1000, 0, 1), x3 = rnorm(1000, 0, 2))
xbetabase7 <- xlogregbase7$x1 + xlogregbase7$x2 + xlogregbase7$x3
xlogregbase8 <-  data.frame(x1 = rbinom(1200, 1, 0.2), x2 = runif(1200, 0, 1), x3 = rnorm(1200, 0, 2))
xbetabase8 <- xlogregbase8$x1 + xlogregbase8$x2 + xlogregbase8$x3
xlogregbase9 <-  data.frame(x1 = rbinom(1500, 1, 0.2), x2 = runif(1500, 0, 1), x3 = rnorm(1500, 0, 2))
xbetabase9 <- xlogregbase9$x1 + xlogregbase9$x2 + xlogregbase9$x3
xlogregbase1$y_1 <- rbinom(50, size = 1, prob = exp(Delta_values[5] + xbetabase1)/(1+exp(Delta_values[5] + xbetabase1)))
xlogregbase2$y_2 <- rbinom(100, size = 1, prob = exp(Delta_values[5] + xbetabase2)/(1+exp(Delta_values[5] + xbetabase2)))
xlogregbase3$y_3 <- rbinom(200, size = 1, prob = exp(Delta_values[5] + xbetabase3)/(1+exp(Delta_values[5] + xbetabase3)))
xlogregbase4$y_4 <- rbinom(400, size = 1, prob = exp(Delta_values[5] + xbetabase4)/(1+exp(Delta_values[5] + xbetabase4)))
xlogregbase5$y_5 <- rbinom(500, size = 1, prob = exp(Delta_values[5] + xbetabase5)/(1+exp(Delta_values[5] + xbetabase5)))
xlogregbase6$y_6 <- rbinom(800, size = 1, prob = exp(Delta_values[5] + xbetabase6)/(1+exp(Delta_values[5] + xbetabase6)))
xlogregbase7$y_7 <- rbinom(1000, size = 1, prob = exp(Delta_values[5] + xbetabase7)/(1+exp(Delta_values[5] + xbetabase7)))
xlogregbase8$y_8 <- rbinom(1200, size = 1, prob = exp(Delta_values[5] + xbetabase8)/(1+exp(Delta_values[5] + xbetabase8)))
xlogregbase9$y_9 <- rbinom(1500, size = 1, prob = exp(Delta_values[5] + xbetabase9)/(1+exp(Delta_values[5] + xbetabase9)))

#run set
xlogregrun <-  data.frame(x1 = rbinom(nobs, 1, 0.2), x2 = runif(nobs, 0, 1), x3 = rnorm(nobs, 0, 2))
xbetarun <- xlogregrun$x1 + xlogregrun$x2 + xlogregrun$x3
xlogregrun$y <- rbinom(nobs, size = 1, prob = exp(Delta_values[5] + xbetarun)/(1+exp(Delta_values[5] + xbetarun)))



#regression models
glm.reg.risk1 <- glm(y_1~x1+x2+x3, data = xlogregbase1, family = binomial("logit"))
summary(glm.reg.risk1)
pred1 <- predict(glm.reg.risk1, newdata = xlogregrun, type = "response")
glm.reg.risk2 <- glm(y_2~x1+x2+x3, data = xlogregbase2, family = binomial("logit"))
summary(glm.reg.risk2)
pred2 <- predict(glm.reg.risk2, newdata = xlogregrun, type = "response")
glm.reg.risk3 <- glm(y_3~x1+x2+x3, data = xlogregbase3, family = binomial("logit"))
summary(glm.reg.risk3)
pred3 <- predict(glm.reg.risk3, newdata = xlogregrun, type = "response")
glm.reg.risk4 <- glm(y_4~x1+x2+x3, data = xlogregbase4, family = binomial("logit"))
summary(glm.reg.risk4)
pred4 <- predict(glm.reg.risk4, newdata = xlogregrun, type = "response")
glm.reg.risk5 <- glm(y_5~x1+x2+x3, data = xlogregbase5, family = binomial("logit"))
summary(glm.reg.risk5)
pred5 <- predict(glm.reg.risk5, newdata = xlogregrun, type = "response")
glm.reg.risk6 <- glm(y_6~x1+x2+x3, data = xlogregbase6, family = binomial("logit"))
summary(glm.reg.risk6)
pred6 <- predict(glm.reg.risk6, newdata = xlogregrun, type = "response")
glm.reg.risk7 <- glm(y_7~x1+x2+x3, data = xlogregbase7, family = binomial("logit"))
summary(glm.reg.risk7)
pred7 <- predict(glm.reg.risk7, newdata = xlogregrun, type = "response")
glm.reg.risk8 <- glm(y_8~x1+x2+x3, data = xlogregbase8, family = binomial("logit"))
summary(glm.reg.risk8)
pred8 <- predict(glm.reg.risk8, newdata = xlogregrun, type = "response")
glm.reg.risk9 <- glm(y_9~x1+x2+x3, data = xlogregbase9, family = binomial("logit"))
summary(glm.reg.risk9)
pred9 <- predict(glm.reg.risk9, newdata = xlogregrun, type = "response")
#VLAD
j <- seq(from = 1, to = 1000)
V.risk1 <- numeric(0)
V.risk1[1] <- pred1[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk1[i] <- V.risk1[i-1] + (pred1[i] - xlogregrun$y[i])
}
V.risk2 <- numeric(0)
V.risk2[1] <- pred2[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk2[i] <- V.risk2[i-1] + (pred2[i] - xlogregrun$y[i])
}
V.risk3 <- numeric(0)
V.risk3[1] <- pred3[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk3[i] <- V.risk3[i-1] + (pred3[i] - xlogregrun$y[i])
}
V.risk4 <- numeric(0)
V.risk4[1] <- pred4[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk4[i] <- V.risk4[i-1] + (pred4[i] - xlogregrun$y[i])
}
V.risk5 <- numeric(0)
V.risk5[1] <- pred5[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk5[i] <- V.risk5[i-1] + (pred5[i] - xlogregrun$y[i])
}
V.risk6 <- numeric(0)
V.risk6[1] <- pred6[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk6[i] <- V.risk6[i-1] + (pred6[i] - xlogregrun$y[i])
}
V.risk7 <- numeric(0)
V.risk7[1] <- pred7[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk7[i] <- V.risk7[i-1] + (pred7[i] - xlogregrun$y[i])
}
V.risk8 <- numeric(0)
V.risk8[1] <- pred8[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk8[i] <- V.risk8[i-1] + (pred8[i] - xlogregrun$y[i])
}
V.risk9 <- numeric(0)
V.risk9[1] <- pred9[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk9[i] <- V.risk9[i-1] + (pred9[i] - xlogregrun$y[i])
}
par(mfrow = c(1,1))
plot(x=j, y = V.risk1, type = "l", ylim = c(-140, 140), ylab = "V_t", xlab = "t", col = "magenta", 
     lwd = 2, xlim = c(-3, 1000), main = "Case 2")
lines(x = j, y = V.risk2, col = "red", lwd = 2)
lines(x = j, y = V.risk3, col = "blue", lwd = 2)
lines(x = j, y = V.risk4, col = "green", lwd = 2)
lines(x = j, y = V.risk5, col = "pink", lwd = 2)
lines(x = j, y = V.risk6, col = "purple", lwd = 2)
lines(x = j, y = V.risk7, col = "orange", lwd = 2)
lines(x = j, y = V.risk8, col = "brown", lwd = 2)
lines(x = j, y = V.risk9, col = "cyan", lwd = 2)
abline(h = 0, lty = 2)

#Third case, same situation as in case 2 but now the rundata set has now a delta of -0.2
rm(list = ls())
Delta_values <- c(-1, -0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8, 1.0)
nobs <- 1000
set.seed(10)
xlogregbase1 <-  data.frame(x1 = rbinom(50, 1, 0.2), x2 = runif(50, 0, 1), x3 = rnorm(50, 0, 2))
xbetabase1 <- xlogregbase1$x1 + xlogregbase1$x2 + xlogregbase1$x3
xlogregbase2 <-  data.frame(x1 = rbinom(100, 1, 0.2), x2 = runif(100, 0, 1), x3 = rnorm(100, 0, 2))
xbetabase2 <- xlogregbase2$x1 + xlogregbase2$x2 + xlogregbase2$x3
xlogregbase3 <-  data.frame(x1 = rbinom(200, 1, 0.2), x2 = runif(200, 0, 1), x3 = rnorm(200, 0, 2))
xbetabase3 <- xlogregbase3$x1 + xlogregbase3$x2 + xlogregbase3$x3
xlogregbase4 <-  data.frame(x1 = rbinom(400, 1, 0.2), x2 = runif(400, 0, 1), x3 = rnorm(400, 0, 2))
xbetabase4 <- xlogregbase4$x1 + xlogregbase4$x2 + xlogregbase4$x3
xlogregbase5 <-  data.frame(x1 = rbinom(500, 1, 0.2), x2 = runif(500, 0, 1), x3 = rnorm(500, 0, 2))
xbetabase5 <- xlogregbase5$x1 + xlogregbase5$x2 + xlogregbase5$x3
xlogregbase6 <-  data.frame(x1 = rbinom(800, 1, 0.2), x2 = runif(800, 0, 1), x3 = rnorm(800, 0, 2))
xbetabase6 <- xlogregbase6$x1 + xlogregbase6$x2 + xlogregbase6$x3
xlogregbase7 <-  data.frame(x1 = rbinom(1000, 1, 0.2), x2 = runif(1000, 0, 1), x3 = rnorm(1000, 0, 2))
xbetabase7 <- xlogregbase7$x1 + xlogregbase7$x2 + xlogregbase7$x3
xlogregbase8 <-  data.frame(x1 = rbinom(1200, 1, 0.2), x2 = runif(1200, 0, 1), x3 = rnorm(1200, 0, 2))
xbetabase8 <- xlogregbase8$x1 + xlogregbase8$x2 + xlogregbase8$x3
xlogregbase9 <-  data.frame(x1 = rbinom(1500, 1, 0.2), x2 = runif(1500, 0, 1), x3 = rnorm(1500, 0, 2))
xbetabase9 <- xlogregbase9$x1 + xlogregbase9$x2 + xlogregbase9$x3
xlogregbase1$y_1 <- rbinom(50, size = 1, prob = exp(Delta_values[5] + xbetabase1)/(1+exp(Delta_values[5] + xbetabase1)))
xlogregbase2$y_2 <- rbinom(100, size = 1, prob = exp(Delta_values[5] + xbetabase2)/(1+exp(Delta_values[5] + xbetabase2)))
xlogregbase3$y_3 <- rbinom(200, size = 1, prob = exp(Delta_values[5] + xbetabase3)/(1+exp(Delta_values[5] + xbetabase3)))
xlogregbase4$y_4 <- rbinom(400, size = 1, prob = exp(Delta_values[5] + xbetabase4)/(1+exp(Delta_values[5] + xbetabase4)))
xlogregbase5$y_5 <- rbinom(500, size = 1, prob = exp(Delta_values[5] + xbetabase5)/(1+exp(Delta_values[5] + xbetabase5)))
xlogregbase6$y_6 <- rbinom(800, size = 1, prob = exp(Delta_values[5] + xbetabase6)/(1+exp(Delta_values[5] + xbetabase6)))
xlogregbase7$y_7 <- rbinom(1000, size = 1, prob = exp(Delta_values[5] + xbetabase7)/(1+exp(Delta_values[5] + xbetabase7)))
xlogregbase8$y_8 <- rbinom(1200, size = 1, prob = exp(Delta_values[5] + xbetabase8)/(1+exp(Delta_values[5] + xbetabase8)))
xlogregbase9$y_9 <- rbinom(1500, size = 1, prob = exp(Delta_values[5] + xbetabase9)/(1+exp(Delta_values[5] + xbetabase9)))

#run set
xlogregrun <-  data.frame(x1 = rbinom(nobs, 1, 0.2), x2 = runif(nobs, 0, 1), x3 = rnorm(nobs, 0, 2))
xbetarun <- xlogregrun$x1 + xlogregrun$x2 + xlogregrun$x3
xlogregrun$y <- rbinom(nobs, size = 1, prob = exp(Delta_values[4] + xbetarun)/(1+exp(Delta_values[4] + xbetarun)))



#regression models
glm.reg.risk1 <- glm(y_1~x1+x2+x3, data = xlogregbase1, family = binomial("logit"))
summary(glm.reg.risk1)
pred1 <- predict(glm.reg.risk1, newdata = xlogregrun, type = "response")
glm.reg.risk2 <- glm(y_2~x1+x2+x3, data = xlogregbase2, family = binomial("logit"))
summary(glm.reg.risk2)
pred2 <- predict(glm.reg.risk2, newdata = xlogregrun, type = "response")
glm.reg.risk3 <- glm(y_3~x1+x2+x3, data = xlogregbase3, family = binomial("logit"))
summary(glm.reg.risk3)
pred3 <- predict(glm.reg.risk3, newdata = xlogregrun, type = "response")
glm.reg.risk4 <- glm(y_4~x1+x2+x3, data = xlogregbase4, family = binomial("logit"))
summary(glm.reg.risk4)
pred4 <- predict(glm.reg.risk4, newdata = xlogregrun, type = "response")
glm.reg.risk5 <- glm(y_5~x1+x2+x3, data = xlogregbase5, family = binomial("logit"))
summary(glm.reg.risk5)
pred5 <- predict(glm.reg.risk5, newdata = xlogregrun, type = "response")
glm.reg.risk6 <- glm(y_6~x1+x2+x3, data = xlogregbase6, family = binomial("logit"))
summary(glm.reg.risk6)
pred6 <- predict(glm.reg.risk6, newdata = xlogregrun, type = "response")
glm.reg.risk7 <- glm(y_7~x1+x2+x3, data = xlogregbase7, family = binomial("logit"))
summary(glm.reg.risk7)
pred7 <- predict(glm.reg.risk7, newdata = xlogregrun, type = "response")
glm.reg.risk8 <- glm(y_8~x1+x2+x3, data = xlogregbase8, family = binomial("logit"))
summary(glm.reg.risk8)
pred8 <- predict(glm.reg.risk8, newdata = xlogregrun, type = "response")
glm.reg.risk9 <- glm(y_9~x1+x2+x3, data = xlogregbase9, family = binomial("logit"))
summary(glm.reg.risk9)
pred9 <- predict(glm.reg.risk9, newdata = xlogregrun, type = "response")
#VLAD
j <- seq(from = 1, to = 1000)
V.risk1 <- numeric(0)
V.risk1[1] <- pred1[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk1[i] <- V.risk1[i-1] + (pred1[i] - xlogregrun$y[i])
}
V.risk2 <- numeric(0)
V.risk2[1] <- pred2[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk2[i] <- V.risk2[i-1] + (pred2[i] - xlogregrun$y[i])
}
V.risk3 <- numeric(0)
V.risk3[1] <- pred3[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk3[i] <- V.risk3[i-1] + (pred3[i] - xlogregrun$y[i])
}
V.risk4 <- numeric(0)
V.risk4[1] <- pred4[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk4[i] <- V.risk4[i-1] + (pred4[i] - xlogregrun$y[i])
}
V.risk5 <- numeric(0)
V.risk5[1] <- pred5[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk5[i] <- V.risk5[i-1] + (pred5[i] - xlogregrun$y[i])
}
V.risk6 <- numeric(0)
V.risk6[1] <- pred6[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk6[i] <- V.risk6[i-1] + (pred6[i] - xlogregrun$y[i])
}
V.risk7 <- numeric(0)
V.risk7[1] <- pred7[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk7[i] <- V.risk7[i-1] + (pred7[i] - xlogregrun$y[i])
}
V.risk8 <- numeric(0)
V.risk8[1] <- pred8[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk8[i] <- V.risk8[i-1] + (pred8[i] - xlogregrun$y[i])
}
V.risk9 <- numeric(0)
V.risk9[1] <- pred9[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk9[i] <- V.risk9[i-1] + (pred9[i] - xlogregrun$y[i])
}
par(mfrow = c(1,1))
plot(x=j, y = V.risk1, type = "l", ylim = c(-140, 140), ylab = "V_t", xlab = "t", 
     col = "magenta", lwd = 2, xlim = c(-3, 1000), main = "Case 3")
lines(x = j, y = V.risk2, col = "red", lwd = 2)
lines(x = j, y = V.risk3, col = "blue", lwd = 2)
lines(x = j, y = V.risk4, col = "green", lwd = 2)
lines(x = j, y = V.risk5, col = "pink", lwd = 2)
lines(x = j, y = V.risk6, col = "purple", lwd = 2)
lines(x = j, y = V.risk7, col = "orange", lwd = 2)
lines(x = j, y = V.risk8, col = "brown", lwd = 2)
lines(x = j, y = V.risk9, col = "cyan", lwd = 2)
abline(h = 0, lty = 2)

#Case 4, All baselineline have nbase = 50, delta_values varies. Rundata have a delta of -0.2
rm(list = ls())
set.seed(10)
nbase <- 50
nobs <- 1000
Delta_values <- c(-1, -0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8, 1.0)
xlogregbase1 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase1 <- xlogregbase1$x1 + xlogregbase1$x2 + xlogregbase1$x3
xlogregbase2 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase2 <- xlogregbase2$x1 + xlogregbase2$x2 + xlogregbase2$x3
xlogregbase3 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase3 <- xlogregbase3$x1 + xlogregbase3$x2 + xlogregbase3$x3
xlogregbase4 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase4 <- xlogregbase4$x1 + xlogregbase4$x2 + xlogregbase4$x3
xlogregbase5 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase5 <- xlogregbase5$x1 + xlogregbase5$x2 + xlogregbase5$x3
xlogregbase6 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase6 <- xlogregbase6$x1 + xlogregbase6$x2 + xlogregbase6$x3
xlogregbase7 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase7 <- xlogregbase7$x1 + xlogregbase7$x2 + xlogregbase7$x3
xlogregbase8 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase8 <- xlogregbase8$x1 + xlogregbase8$x2 + xlogregbase8$x3
xlogregbase9 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase9 <- xlogregbase9$x1 + xlogregbase9$x2 + xlogregbase9$x3
xlogregbase1$y_1 <- rbinom(nbase, size = 1, prob = exp(Delta_values[1] + xbetabase1)/(1+exp(Delta_values[1] + xbetabase1)))
xlogregbase2$y_2 <- rbinom(nbase, size = 1, prob = exp(Delta_values[2] + xbetabase2)/(1+exp(Delta_values[2] + xbetabase2)))
xlogregbase3$y_3 <- rbinom(nbase, size = 1, prob = exp(Delta_values[3] + xbetabase3)/(1+exp(Delta_values[3] + xbetabase3)))
xlogregbase4$y_4 <- rbinom(nbase, size = 1, prob = exp(Delta_values[4] + xbetabase4)/(1+exp(Delta_values[4] + xbetabase4)))
xlogregbase5$y_5 <- rbinom(nbase, size = 1, prob = exp(Delta_values[5] + xbetabase5)/(1+exp(Delta_values[5] + xbetabase5)))
xlogregbase6$y_6 <- rbinom(nbase, size = 1, prob = exp(Delta_values[6] + xbetabase6)/(1+exp(Delta_values[6] + xbetabase6)))
xlogregbase7$y_7 <- rbinom(nbase, size = 1, prob = exp(Delta_values[7] + xbetabase7)/(1+exp(Delta_values[7] + xbetabase7)))
xlogregbase8$y_8 <- rbinom(nbase, size = 1, prob = exp(Delta_values[8] + xbetabase8)/(1+exp(Delta_values[8] + xbetabase8)))
xlogregbase9$y_9 <- rbinom(nbase, size = 1, prob = exp(Delta_values[9] + xbetabase9)/(1+exp(Delta_values[9] + xbetabase9)))

#run set
xlogregrun <-  data.frame(x1 = rbinom(nobs, 1, 0.2), x2 = runif(nobs, 0, 1), x3 = rnorm(nobs, 0, 2))
xbetarun <- xlogregrun$x1 + xlogregrun$x2 + xlogregrun$x3
xlogregrun$y <- rbinom(nobs, size = 1, prob = exp(Delta_values[4] + xbetarun)/(1+exp(Delta_values[4] + xbetarun)))

#regression models
glm.reg.risk1 <- glm(y_1~x1+x2+x3, data = xlogregbase1, family = binomial("logit"))
summary(glm.reg.risk1)
pred1 <- predict(glm.reg.risk1, newdata = xlogregrun, type = "response")
glm.reg.risk2 <- glm(y_2~x1+x2+x3, data = xlogregbase2, family = binomial("logit"))
summary(glm.reg.risk2)
pred2 <- predict(glm.reg.risk2, newdata = xlogregrun, type = "response")
glm.reg.risk3 <- glm(y_3~x1+x2+x3, data = xlogregbase3, family = binomial("logit"))
summary(glm.reg.risk3)
pred3 <- predict(glm.reg.risk3, newdata = xlogregrun, type = "response")
glm.reg.risk4 <- glm(y_4~x1+x2+x3, data = xlogregbase4, family = binomial("logit"))
summary(glm.reg.risk4)
pred4 <- predict(glm.reg.risk4, newdata = xlogregrun, type = "response")
glm.reg.risk5 <- glm(y_5~x1+x2+x3, data = xlogregbase5, family = binomial("logit"))
summary(glm.reg.risk5)
pred5 <- predict(glm.reg.risk5, newdata = xlogregrun, type = "response")
glm.reg.risk6 <- glm(y_6~x1+x2+x3, data = xlogregbase6, family = binomial("logit"))
summary(glm.reg.risk6)
pred6 <- predict(glm.reg.risk6, newdata = xlogregrun, type = "response")
glm.reg.risk7 <- glm(y_7~x1+x2+x3, data = xlogregbase7, family = binomial("logit"))
summary(glm.reg.risk7)
pred7 <- predict(glm.reg.risk7, newdata = xlogregrun, type = "response")
glm.reg.risk8 <- glm(y_8~x1+x2+x3, data = xlogregbase8, family = binomial("logit"))
summary(glm.reg.risk8)
pred8 <- predict(glm.reg.risk8, newdata = xlogregrun, type = "response")
glm.reg.risk9 <- glm(y_9~x1+x2+x3, data = xlogregbase9, family = binomial("logit"))
summary(glm.reg.risk9)
pred9 <- predict(glm.reg.risk9, newdata = xlogregrun, type = "response")
#VLAD
j <- seq(from = 1, to = 1000)
V.risk1 <- numeric(0)
V.risk1[1] <- pred1[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk1[i] <- V.risk1[i-1] + (pred1[i] - xlogregrun$y[i])
}
V.risk2 <- numeric(0)
V.risk2[1] <- pred2[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk2[i] <- V.risk2[i-1] + (pred2[i] - xlogregrun$y[i])
}
V.risk3 <- numeric(0)
V.risk3[1] <- pred3[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk3[i] <- V.risk3[i-1] + (pred3[i] - xlogregrun$y[i])
}
V.risk4 <- numeric(0)
V.risk4[1] <- pred4[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk4[i] <- V.risk4[i-1] + (pred4[i] - xlogregrun$y[i])
}
V.risk5 <- numeric(0)
V.risk5[1] <- pred5[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk5[i] <- V.risk5[i-1] + (pred5[i] - xlogregrun$y[i])
}
V.risk6 <- numeric(0)
V.risk6[1] <- pred6[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk6[i] <- V.risk6[i-1] + (pred6[i] - xlogregrun$y[i])
}
V.risk7 <- numeric(0)
V.risk7[1] <- pred7[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk7[i] <- V.risk7[i-1] + (pred7[i] - xlogregrun$y[i])
}
V.risk8 <- numeric(0)
V.risk8[1] <- pred8[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk8[i] <- V.risk8[i-1] + (pred8[i] - xlogregrun$y[i])
}
V.risk9 <- numeric(0)
V.risk9[1] <- pred9[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk9[i] <- V.risk9[i-1] + (pred9[i] - xlogregrun$y[i])
}
par(mfrow = c(2,2))
plot(x=j, y = V.risk1, type = "l", ylim = c(-200, 200), ylab = "V_t", xlab = "t", 
     col = "magenta", lwd = 2, xlim = c(-3, 1000), main = "Baseline with n = 50")
lines(x = j, y = V.risk2, col = "red", lwd = 2)
lines(x = j, y = V.risk3, col = "blue", lwd = 2)
lines(x = j, y = V.risk4, col = "green", lwd = 2)
lines(x = j, y = V.risk5, col = "pink", lwd = 2)
lines(x = j, y = V.risk6, col = "purple", lwd = 2)
lines(x = j, y = V.risk7, col = "orange", lwd = 2)
lines(x = j, y = V.risk8, col = "brown", lwd = 2)
lines(x = j, y = V.risk9, col = "cyan", lwd = 2)
abline(h = 0, lty = 2)

#Case 5, All baselineline have nbase = 200, delta_values varies. Rundata have a delta of -0.2
rm(list = ls())
set.seed(10)
nbase <- 200
nobs <- 1000
Delta_values <- c(-1, -0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8, 1.0)
xlogregbase1 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase1 <- xlogregbase1$x1 + xlogregbase1$x2 + xlogregbase1$x3
xlogregbase2 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase2 <- xlogregbase2$x1 + xlogregbase2$x2 + xlogregbase2$x3
xlogregbase3 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase3 <- xlogregbase3$x1 + xlogregbase3$x2 + xlogregbase3$x3
xlogregbase4 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase4 <- xlogregbase4$x1 + xlogregbase4$x2 + xlogregbase4$x3
xlogregbase5 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase5 <- xlogregbase5$x1 + xlogregbase5$x2 + xlogregbase5$x3
xlogregbase6 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase6 <- xlogregbase6$x1 + xlogregbase6$x2 + xlogregbase6$x3
xlogregbase7 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase7 <- xlogregbase7$x1 + xlogregbase7$x2 + xlogregbase7$x3
xlogregbase8 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase8 <- xlogregbase8$x1 + xlogregbase8$x2 + xlogregbase8$x3
xlogregbase9 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase9 <- xlogregbase9$x1 + xlogregbase9$x2 + xlogregbase9$x3
xlogregbase1$y_1 <- rbinom(nbase, size = 1, prob = exp(Delta_values[1] + xbetabase1)/(1+exp(Delta_values[1] + xbetabase1)))
xlogregbase2$y_2 <- rbinom(nbase, size = 1, prob = exp(Delta_values[2] + xbetabase2)/(1+exp(Delta_values[2] + xbetabase2)))
xlogregbase3$y_3 <- rbinom(nbase, size = 1, prob = exp(Delta_values[3] + xbetabase3)/(1+exp(Delta_values[3] + xbetabase3)))
xlogregbase4$y_4 <- rbinom(nbase, size = 1, prob = exp(Delta_values[4] + xbetabase4)/(1+exp(Delta_values[4] + xbetabase4)))
xlogregbase5$y_5 <- rbinom(nbase, size = 1, prob = exp(Delta_values[5] + xbetabase5)/(1+exp(Delta_values[5] + xbetabase5)))
xlogregbase6$y_6 <- rbinom(nbase, size = 1, prob = exp(Delta_values[6] + xbetabase6)/(1+exp(Delta_values[6] + xbetabase6)))
xlogregbase7$y_7 <- rbinom(nbase, size = 1, prob = exp(Delta_values[7] + xbetabase7)/(1+exp(Delta_values[7] + xbetabase7)))
xlogregbase8$y_8 <- rbinom(nbase, size = 1, prob = exp(Delta_values[8] + xbetabase8)/(1+exp(Delta_values[8] + xbetabase8)))
xlogregbase9$y_9 <- rbinom(nbase, size = 1, prob = exp(Delta_values[9] + xbetabase9)/(1+exp(Delta_values[9] + xbetabase9)))

#run set
xlogregrun <-  data.frame(x1 = rbinom(nobs, 1, 0.2), x2 = runif(nobs, 0, 1), x3 = rnorm(nobs, 0, 2))
xbetarun <- xlogregrun$x1 + xlogregrun$x2 + xlogregrun$x3
xlogregrun$y <- rbinom(nobs, size = 1, prob = exp(Delta_values[4] + xbetarun)/(1+exp(Delta_values[4] + xbetarun)))

#regression models
glm.reg.risk1 <- glm(y_1~x1+x2+x3, data = xlogregbase1, family = binomial("logit"))
summary(glm.reg.risk1)
pred1 <- predict(glm.reg.risk1, newdata = xlogregrun, type = "response")
glm.reg.risk2 <- glm(y_2~x1+x2+x3, data = xlogregbase2, family = binomial("logit"))
summary(glm.reg.risk2)
pred2 <- predict(glm.reg.risk2, newdata = xlogregrun, type = "response")
glm.reg.risk3 <- glm(y_3~x1+x2+x3, data = xlogregbase3, family = binomial("logit"))
summary(glm.reg.risk3)
pred3 <- predict(glm.reg.risk3, newdata = xlogregrun, type = "response")
glm.reg.risk4 <- glm(y_4~x1+x2+x3, data = xlogregbase4, family = binomial("logit"))
summary(glm.reg.risk4)
pred4 <- predict(glm.reg.risk4, newdata = xlogregrun, type = "response")
glm.reg.risk5 <- glm(y_5~x1+x2+x3, data = xlogregbase5, family = binomial("logit"))
summary(glm.reg.risk5)
pred5 <- predict(glm.reg.risk5, newdata = xlogregrun, type = "response")
glm.reg.risk6 <- glm(y_6~x1+x2+x3, data = xlogregbase6, family = binomial("logit"))
summary(glm.reg.risk6)
pred6 <- predict(glm.reg.risk6, newdata = xlogregrun, type = "response")
glm.reg.risk7 <- glm(y_7~x1+x2+x3, data = xlogregbase7, family = binomial("logit"))
summary(glm.reg.risk7)
pred7 <- predict(glm.reg.risk7, newdata = xlogregrun, type = "response")
glm.reg.risk8 <- glm(y_8~x1+x2+x3, data = xlogregbase8, family = binomial("logit"))
summary(glm.reg.risk8)
pred8 <- predict(glm.reg.risk8, newdata = xlogregrun, type = "response")
glm.reg.risk9 <- glm(y_9~x1+x2+x3, data = xlogregbase9, family = binomial("logit"))
summary(glm.reg.risk9)
pred9 <- predict(glm.reg.risk9, newdata = xlogregrun, type = "response")
#VLAD
j <- seq(from = 1, to = 1000)
V.risk1 <- numeric(0)
V.risk1[1] <- pred1[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk1[i] <- V.risk1[i-1] + (pred1[i] - xlogregrun$y[i])
}
V.risk2 <- numeric(0)
V.risk2[1] <- pred2[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk2[i] <- V.risk2[i-1] + (pred2[i] - xlogregrun$y[i])
}
V.risk3 <- numeric(0)
V.risk3[1] <- pred3[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk3[i] <- V.risk3[i-1] + (pred3[i] - xlogregrun$y[i])
}
V.risk4 <- numeric(0)
V.risk4[1] <- pred4[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk4[i] <- V.risk4[i-1] + (pred4[i] - xlogregrun$y[i])
}
V.risk5 <- numeric(0)
V.risk5[1] <- pred5[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk5[i] <- V.risk5[i-1] + (pred5[i] - xlogregrun$y[i])
}
V.risk6 <- numeric(0)
V.risk6[1] <- pred6[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk6[i] <- V.risk6[i-1] + (pred6[i] - xlogregrun$y[i])
}
V.risk7 <- numeric(0)
V.risk7[1] <- pred7[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk7[i] <- V.risk7[i-1] + (pred7[i] - xlogregrun$y[i])
}
V.risk8 <- numeric(0)
V.risk8[1] <- pred8[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk8[i] <- V.risk8[i-1] + (pred8[i] - xlogregrun$y[i])
}
V.risk9 <- numeric(0)
V.risk9[1] <- pred9[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk9[i] <- V.risk9[i-1] + (pred9[i] - xlogregrun$y[i])
}
plot(x=j, y = V.risk1, type = "l", ylim = c(-200, 200), ylab = "V_t", xlab = "t", 
     col = "magenta", lwd = 2, xlim = c(-3, 1000), main = "Baseline with n = 200")
lines(x = j, y = V.risk2, col = "red", lwd = 2)
lines(x = j, y = V.risk3, col = "blue", lwd = 2)
lines(x = j, y = V.risk4, col = "green", lwd = 2)
lines(x = j, y = V.risk5, col = "pink", lwd = 2)
lines(x = j, y = V.risk6, col = "purple", lwd = 2)
lines(x = j, y = V.risk7, col = "orange", lwd = 2)
lines(x = j, y = V.risk8, col = "brown", lwd = 2)
lines(x = j, y = V.risk9, col = "cyan", lwd = 2)
abline(h = 0, lty = 2)

#Case 6, All baselineline have nbase = 500, delta_values varies. Rundata have a delta of -0.2
rm(list = ls())
set.seed(10)
nbase <- 500
nobs <- 1000
Delta_values <- c(-1, -0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8, 1.0)
xlogregbase1 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase1 <- xlogregbase1$x1 + xlogregbase1$x2 + xlogregbase1$x3
xlogregbase2 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase2 <- xlogregbase2$x1 + xlogregbase2$x2 + xlogregbase2$x3
xlogregbase3 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase3 <- xlogregbase3$x1 + xlogregbase3$x2 + xlogregbase3$x3
xlogregbase4 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase4 <- xlogregbase4$x1 + xlogregbase4$x2 + xlogregbase4$x3
xlogregbase5 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase5 <- xlogregbase5$x1 + xlogregbase5$x2 + xlogregbase5$x3
xlogregbase6 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase6 <- xlogregbase6$x1 + xlogregbase6$x2 + xlogregbase6$x3
xlogregbase7 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase7 <- xlogregbase7$x1 + xlogregbase7$x2 + xlogregbase7$x3
xlogregbase8 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase8 <- xlogregbase8$x1 + xlogregbase8$x2 + xlogregbase8$x3
xlogregbase9 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase9 <- xlogregbase9$x1 + xlogregbase9$x2 + xlogregbase9$x3
xlogregbase1$y_1 <- rbinom(nbase, size = 1, prob = exp(Delta_values[1] + xbetabase1)/(1+exp(Delta_values[1] + xbetabase1)))
xlogregbase2$y_2 <- rbinom(nbase, size = 1, prob = exp(Delta_values[2] + xbetabase2)/(1+exp(Delta_values[2] + xbetabase2)))
xlogregbase3$y_3 <- rbinom(nbase, size = 1, prob = exp(Delta_values[3] + xbetabase3)/(1+exp(Delta_values[3] + xbetabase3)))
xlogregbase4$y_4 <- rbinom(nbase, size = 1, prob = exp(Delta_values[4] + xbetabase4)/(1+exp(Delta_values[4] + xbetabase4)))
xlogregbase5$y_5 <- rbinom(nbase, size = 1, prob = exp(Delta_values[5] + xbetabase5)/(1+exp(Delta_values[5] + xbetabase5)))
xlogregbase6$y_6 <- rbinom(nbase, size = 1, prob = exp(Delta_values[6] + xbetabase6)/(1+exp(Delta_values[6] + xbetabase6)))
xlogregbase7$y_7 <- rbinom(nbase, size = 1, prob = exp(Delta_values[7] + xbetabase7)/(1+exp(Delta_values[7] + xbetabase7)))
xlogregbase8$y_8 <- rbinom(nbase, size = 1, prob = exp(Delta_values[8] + xbetabase8)/(1+exp(Delta_values[8] + xbetabase8)))
xlogregbase9$y_9 <- rbinom(nbase, size = 1, prob = exp(Delta_values[9] + xbetabase9)/(1+exp(Delta_values[9] + xbetabase9)))

#run set
xlogregrun <-  data.frame(x1 = rbinom(nobs, 1, 0.2), x2 = runif(nobs, 0, 1), x3 = rnorm(nobs, 0, 2))
xbetarun <- xlogregrun$x1 + xlogregrun$x2 + xlogregrun$x3
xlogregrun$y <- rbinom(nobs, size = 1, prob = exp(Delta_values[4] + xbetarun)/(1+exp(Delta_values[4] + xbetarun)))

#regression models
glm.reg.risk1 <- glm(y_1~x1+x2+x3, data = xlogregbase1, family = binomial("logit"))
summary(glm.reg.risk1)
pred1 <- predict(glm.reg.risk1, newdata = xlogregrun, type = "response")
glm.reg.risk2 <- glm(y_2~x1+x2+x3, data = xlogregbase2, family = binomial("logit"))
summary(glm.reg.risk2)
pred2 <- predict(glm.reg.risk2, newdata = xlogregrun, type = "response")
glm.reg.risk3 <- glm(y_3~x1+x2+x3, data = xlogregbase3, family = binomial("logit"))
summary(glm.reg.risk3)
pred3 <- predict(glm.reg.risk3, newdata = xlogregrun, type = "response")
glm.reg.risk4 <- glm(y_4~x1+x2+x3, data = xlogregbase4, family = binomial("logit"))
summary(glm.reg.risk4)
pred4 <- predict(glm.reg.risk4, newdata = xlogregrun, type = "response")
glm.reg.risk5 <- glm(y_5~x1+x2+x3, data = xlogregbase5, family = binomial("logit"))
summary(glm.reg.risk5)
pred5 <- predict(glm.reg.risk5, newdata = xlogregrun, type = "response")
glm.reg.risk6 <- glm(y_6~x1+x2+x3, data = xlogregbase6, family = binomial("logit"))
summary(glm.reg.risk6)
pred6 <- predict(glm.reg.risk6, newdata = xlogregrun, type = "response")
glm.reg.risk7 <- glm(y_7~x1+x2+x3, data = xlogregbase7, family = binomial("logit"))
summary(glm.reg.risk7)
pred7 <- predict(glm.reg.risk7, newdata = xlogregrun, type = "response")
glm.reg.risk8 <- glm(y_8~x1+x2+x3, data = xlogregbase8, family = binomial("logit"))
summary(glm.reg.risk8)
pred8 <- predict(glm.reg.risk8, newdata = xlogregrun, type = "response")
glm.reg.risk9 <- glm(y_9~x1+x2+x3, data = xlogregbase9, family = binomial("logit"))
summary(glm.reg.risk9)
pred9 <- predict(glm.reg.risk9, newdata = xlogregrun, type = "response")
#VLAD
j <- seq(from = 1, to = 1000)
V.risk1 <- numeric(0)
V.risk1[1] <- pred1[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk1[i] <- V.risk1[i-1] + (pred1[i] - xlogregrun$y[i])
}
V.risk2 <- numeric(0)
V.risk2[1] <- pred2[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk2[i] <- V.risk2[i-1] + (pred2[i] - xlogregrun$y[i])
}
V.risk3 <- numeric(0)
V.risk3[1] <- pred3[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk3[i] <- V.risk3[i-1] + (pred3[i] - xlogregrun$y[i])
}
V.risk4 <- numeric(0)
V.risk4[1] <- pred4[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk4[i] <- V.risk4[i-1] + (pred4[i] - xlogregrun$y[i])
}
V.risk5 <- numeric(0)
V.risk5[1] <- pred5[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk5[i] <- V.risk5[i-1] + (pred5[i] - xlogregrun$y[i])
}
V.risk6 <- numeric(0)
V.risk6[1] <- pred6[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk6[i] <- V.risk6[i-1] + (pred6[i] - xlogregrun$y[i])
}
V.risk7 <- numeric(0)
V.risk7[1] <- pred7[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk7[i] <- V.risk7[i-1] + (pred7[i] - xlogregrun$y[i])
}
V.risk8 <- numeric(0)
V.risk8[1] <- pred8[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk8[i] <- V.risk8[i-1] + (pred8[i] - xlogregrun$y[i])
}
V.risk9 <- numeric(0)
V.risk9[1] <- pred9[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk9[i] <- V.risk9[i-1] + (pred9[i] - xlogregrun$y[i])
}
plot(x=j, y = V.risk1, type = "l", ylim = c(-200, 200), ylab = "V_t", xlab = "t", 
     col = "magenta", lwd = 2, xlim = c(-3, 1000), main = "Baseline with n = 500")
lines(x = j, y = V.risk2, col = "red", lwd = 2)
lines(x = j, y = V.risk3, col = "blue", lwd = 2)
lines(x = j, y = V.risk4, col = "green", lwd = 2)
lines(x = j, y = V.risk5, col = "pink", lwd = 2)
lines(x = j, y = V.risk6, col = "purple", lwd = 2)
lines(x = j, y = V.risk7, col = "orange", lwd = 2)
lines(x = j, y = V.risk8, col = "brown", lwd = 2)
lines(x = j, y = V.risk9, col = "cyan", lwd = 2)
abline(h = 0, lty = 2)



#Case 8, All baselineline have nbase = 2000, delta_values varies. Rundata have a delta of -0.2
rm(list = ls())
set.seed(10)
nbase <- 2000
nobs <- 1000
Delta_values <- c(-1, -0.8, -0.5, -0.2, 0, 0.2, 0.5, 0.8, 1.0)
xlogregbase1 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase1 <- xlogregbase1$x1 + xlogregbase1$x2 + xlogregbase1$x3
xlogregbase2 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase2 <- xlogregbase2$x1 + xlogregbase2$x2 + xlogregbase2$x3
xlogregbase3 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase3 <- xlogregbase3$x1 + xlogregbase3$x2 + xlogregbase3$x3
xlogregbase4 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase4 <- xlogregbase4$x1 + xlogregbase4$x2 + xlogregbase4$x3
xlogregbase5 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase5 <- xlogregbase5$x1 + xlogregbase5$x2 + xlogregbase5$x3
xlogregbase6 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase6 <- xlogregbase6$x1 + xlogregbase6$x2 + xlogregbase6$x3
xlogregbase7 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase7 <- xlogregbase7$x1 + xlogregbase7$x2 + xlogregbase7$x3
xlogregbase8 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase8 <- xlogregbase8$x1 + xlogregbase8$x2 + xlogregbase8$x3
xlogregbase9 <-  data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
xbetabase9 <- xlogregbase9$x1 + xlogregbase9$x2 + xlogregbase9$x3
xlogregbase1$y_1 <- rbinom(nbase, size = 1, prob = exp(Delta_values[1] + xbetabase1)/(1+exp(Delta_values[1] + xbetabase1)))
xlogregbase2$y_2 <- rbinom(nbase, size = 1, prob = exp(Delta_values[2] + xbetabase2)/(1+exp(Delta_values[2] + xbetabase2)))
xlogregbase3$y_3 <- rbinom(nbase, size = 1, prob = exp(Delta_values[3] + xbetabase3)/(1+exp(Delta_values[3] + xbetabase3)))
xlogregbase4$y_4 <- rbinom(nbase, size = 1, prob = exp(Delta_values[4] + xbetabase4)/(1+exp(Delta_values[4] + xbetabase4)))
xlogregbase5$y_5 <- rbinom(nbase, size = 1, prob = exp(Delta_values[5] + xbetabase5)/(1+exp(Delta_values[5] + xbetabase5)))
xlogregbase6$y_6 <- rbinom(nbase, size = 1, prob = exp(Delta_values[6] + xbetabase6)/(1+exp(Delta_values[6] + xbetabase6)))
xlogregbase7$y_7 <- rbinom(nbase, size = 1, prob = exp(Delta_values[7] + xbetabase7)/(1+exp(Delta_values[7] + xbetabase7)))
xlogregbase8$y_8 <- rbinom(nbase, size = 1, prob = exp(Delta_values[8] + xbetabase8)/(1+exp(Delta_values[8] + xbetabase8)))
xlogregbase9$y_9 <- rbinom(nbase, size = 1, prob = exp(Delta_values[9] + xbetabase9)/(1+exp(Delta_values[9] + xbetabase9)))

#run set
xlogregrun <-  data.frame(x1 = rbinom(nobs, 1, 0.2), x2 = runif(nobs, 0, 1), x3 = rnorm(nobs, 0, 2))
xbetarun <- xlogregrun$x1 + xlogregrun$x2 + xlogregrun$x3
xlogregrun$y <- rbinom(nobs, size = 1, prob = exp(Delta_values[4] + xbetarun)/(1+exp(Delta_values[4] + xbetarun)))

#regression models
glm.reg.risk1 <- glm(y_1~x1+x2+x3, data = xlogregbase1, family = binomial("logit"))
summary(glm.reg.risk1)
pred1 <- predict(glm.reg.risk1, newdata = xlogregrun, type = "response")
glm.reg.risk2 <- glm(y_2~x1+x2+x3, data = xlogregbase2, family = binomial("logit"))
summary(glm.reg.risk2)
pred2 <- predict(glm.reg.risk2, newdata = xlogregrun, type = "response")
glm.reg.risk3 <- glm(y_3~x1+x2+x3, data = xlogregbase3, family = binomial("logit"))
summary(glm.reg.risk3)
pred3 <- predict(glm.reg.risk3, newdata = xlogregrun, type = "response")
glm.reg.risk4 <- glm(y_4~x1+x2+x3, data = xlogregbase4, family = binomial("logit"))
summary(glm.reg.risk4)
pred4 <- predict(glm.reg.risk4, newdata = xlogregrun, type = "response")
glm.reg.risk5 <- glm(y_5~x1+x2+x3, data = xlogregbase5, family = binomial("logit"))
summary(glm.reg.risk5)
pred5 <- predict(glm.reg.risk5, newdata = xlogregrun, type = "response")
glm.reg.risk6 <- glm(y_6~x1+x2+x3, data = xlogregbase6, family = binomial("logit"))
summary(glm.reg.risk6)
pred6 <- predict(glm.reg.risk6, newdata = xlogregrun, type = "response")
glm.reg.risk7 <- glm(y_7~x1+x2+x3, data = xlogregbase7, family = binomial("logit"))
summary(glm.reg.risk7)
pred7 <- predict(glm.reg.risk7, newdata = xlogregrun, type = "response")
glm.reg.risk8 <- glm(y_8~x1+x2+x3, data = xlogregbase8, family = binomial("logit"))
summary(glm.reg.risk8)
pred8 <- predict(glm.reg.risk8, newdata = xlogregrun, type = "response")
glm.reg.risk9 <- glm(y_9~x1+x2+x3, data = xlogregbase9, family = binomial("logit"))
summary(glm.reg.risk9)
pred9 <- predict(glm.reg.risk9, newdata = xlogregrun, type = "response")
#VLAD
j <- seq(from = 1, to = 1000)
V.risk1 <- numeric(0)
V.risk1[1] <- pred1[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk1[i] <- V.risk1[i-1] + (pred1[i] - xlogregrun$y[i])
}
V.risk2 <- numeric(0)
V.risk2[1] <- pred2[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk2[i] <- V.risk2[i-1] + (pred2[i] - xlogregrun$y[i])
}
V.risk3 <- numeric(0)
V.risk3[1] <- pred3[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk3[i] <- V.risk3[i-1] + (pred3[i] - xlogregrun$y[i])
}
V.risk4 <- numeric(0)
V.risk4[1] <- pred4[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk4[i] <- V.risk4[i-1] + (pred4[i] - xlogregrun$y[i])
}
V.risk5 <- numeric(0)
V.risk5[1] <- pred5[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk5[i] <- V.risk5[i-1] + (pred5[i] - xlogregrun$y[i])
}
V.risk6 <- numeric(0)
V.risk6[1] <- pred6[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk6[i] <- V.risk6[i-1] + (pred6[i] - xlogregrun$y[i])
}
V.risk7 <- numeric(0)
V.risk7[1] <- pred7[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk7[i] <- V.risk7[i-1] + (pred7[i] - xlogregrun$y[i])
}
V.risk8 <- numeric(0)
V.risk8[1] <- pred8[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk8[i] <- V.risk8[i-1] + (pred8[i] - xlogregrun$y[i])
}
V.risk9 <- numeric(0)
V.risk9[1] <- pred9[1] - xlogregrun$y[1]
for(i in 2:length(j)){
  V.risk9[i] <- V.risk9[i-1] + (pred9[i] - xlogregrun$y[i])
}
plot(x=j, y = V.risk1, type = "l", ylim = c(-200, 200), ylab = "V_t", xlab = "t", 
     col = "magenta", lwd = 2, xlim = c(-3, 1000), main = "Baseline with n = 2000")
lines(x = j, y = V.risk2, col = "red", lwd = 2)
lines(x = j, y = V.risk3, col = "blue", lwd = 2)
lines(x = j, y = V.risk4, col = "green", lwd = 2)
lines(x = j, y = V.risk5, col = "pink", lwd = 2)
lines(x = j, y = V.risk6, col = "purple", lwd = 2)
lines(x = j, y = V.risk7, col = "orange", lwd = 2)
lines(x = j, y = V.risk8, col = "brown", lwd = 2)
lines(x = j, y = V.risk9, col = "cyan", lwd = 2)
abline(h = 0, lty = 2)