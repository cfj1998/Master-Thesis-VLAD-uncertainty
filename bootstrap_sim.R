rm(list = ls())
rm(list = ls())
#Deciding L for MP in a simulated set
#creating datasets
nbase1 <- 100 
nbase2 <- 500
nbase3 <- 1000
nbase4 <- 10000
set.seed(10)
xlogregbase1 <- data.frame(x1 = rbinom(nbase1, 1, 0.2), x2 = runif(nbase1, 0, 1), x3 = rnorm(nbase1, 0, 2))
xlogregbase2 <- data.frame(x1 = rbinom(nbase2, 1, 0.2), x2 = runif(nbase2, 0, 1), x3 = rnorm(nbase2, 0, 2))
xlogregbase3 <- data.frame(x1 = rbinom(nbase3, 1, 0.2), x2 = runif(nbase3, 0, 1), x3 = rnorm(nbase3, 0, 2))
xlogregbase4 <- data.frame(x1 = rbinom(nbase4, 1, 0.2), x2 = runif(nbase4, 0, 1), x3 = rnorm(nbase4, 0, 2))
mrun1 <- 100
mrun2 <- 500
mrun3 <- 1000
mrun4 <- 10000
xlogregrun1 <- data.frame(x1 = rbinom(mrun1, 1, 0.2), x2 = runif(mrun1, 0, 1), x3 = rnorm(mrun1, 0, 2))
xlogregrun2 <- data.frame(x1 = rbinom(mrun2, 1, 0.2), x2 = runif(mrun2, 0, 1), x3 = rnorm(mrun2, 0, 2))
xlogregrun3 <- data.frame(x1 = rbinom(mrun3, 1, 0.2), x2 = runif(mrun3, 0, 1), x3 = rnorm(mrun3, 0, 2))
xlogregrun4 <- data.frame(x1 = rbinom(mrun4, 1, 0.2), x2 = runif(mrun4, 0, 1), x3 = rnorm(mrun4, 0, 2))

xbetabase1 <- xlogregbase1$x1 + xlogregbase1$x2 + xlogregbase1$x3
xbetabase2 <- xlogregbase2$x1 + xlogregbase2$x2 + xlogregbase2$x3
xbetabase3 <- xlogregbase3$x1 + xlogregbase3$x2 + xlogregbase3$x3
xbetabase4 <- xlogregbase4$x1 + xlogregbase4$x2 + xlogregbase4$x3
xbetarun1 <- xlogregrun1$x1 + xlogregrun1$x2 + xlogregrun1$x3
xbetarun2 <- xlogregrun2$x1 + xlogregrun2$x2 + xlogregrun2$x3
xbetarun3 <- xlogregrun3$x1 + xlogregrun3$x2 + xlogregrun3$x3
xbetarun4 <- xlogregrun4$x1 + xlogregrun4$x2 + xlogregrun4$x3

#Creating response for base
xlogregbase1$y <- rbinom(nbase1, 1, exp(xbetabase1)/(1+exp(xbetabase1)))#creating response
xlogregbase2$y <- rbinom(nbase2, 1, exp(xbetabase2)/(1+exp(xbetabase2)))
xlogregbase3$y <- rbinom(nbase3, 1, exp(xbetabase3)/(1+exp(xbetabase3)))
xlogregbase4$y <- rbinom(nbase4, 1, exp(xbetabase4)/(1+exp(xbetabase4)))

#Creating response with change (delta) for rundata
Delta <- -0.71
xlogregrun1$y <- rbinom(mrun1, 1, exp(Delta+xbetarun1)/(1+exp(Delta+xbetarun1)))
xlogregrun2$y<- rbinom(mrun2, 1, exp(Delta+xbetarun2)/(1+exp(Delta+xbetarun2)))
xlogregrun3$y<- rbinom(mrun3, 1, exp(Delta+xbetarun3)/(1+exp(Delta+xbetarun3)))
xlogregrun4$y<- rbinom(mrun4, 1, exp(Delta+xbetarun4)/(1+exp(Delta+xbetarun4)))
#Calculate lambda for EWMA
m1.base <- mean(xlogregbase1$y)
m1.base
m2.base <- mean(xlogregbase2$y)
m2.base
m3.base <- mean(xlogregbase3$y)
m3.base
m4.base <- mean(xlogregbase4$y)
m4.base
m1.run <- mean(xlogregrun1$y)
m1.run
m2.run <- mean(xlogregrun2$y)
m2.run
m3.run <- mean(xlogregrun3$y)
m3.run
m4.run <- mean(xlogregrun4$y)
m4.run

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

#Calculating VLADs
#vlad norisk
j1 <- seq(from = 1, to = 100)
j2 <- seq(from = 1, to = 500)
j3 <- seq(from = 1, to = 1000)
j4 <- seq(from = 1, to = 10000)
V.nonrisk1 <- c(0*100)
V.nonrisk2 <- c(0*500)
V.nonrisk3 <- c(0*1000)
V.nonrisk4 <- c(0*10000)
V.nonrisk1[1] <- m1.base - xlogregrun1$y[1]
V.nonrisk2[1] <- m2.base - xlogregrun2$y[1]
V.nonrisk3[1] <- m3.base - xlogregrun3$y[1]
V.nonrisk4[1] <- m4.base - xlogregrun4$y[1]
for(i in 2:100){
  V.nonrisk1[i] <- V.nonrisk1[i-1] + (m1.base - xlogregrun1$y[i])
}
dim(V.nonrisk1) <- c(100, 1)
for(i in 2:500){
  V.nonrisk2[i] <- V.nonrisk2[i-1] + (m2.base - xlogregrun2$y[i])
}
dim(V.nonrisk2) <- c(500, 1)
for(i in 2:1000){
  V.nonrisk3[i] <- V.nonrisk3[i-1] + (m3.base - xlogregrun3$y[i])
}
dim(V.nonrisk3) <- c(1000, 1)
for(i in 2:10000){
  V.nonrisk4[i] <- V.nonrisk4[i-1] + (m4.base - xlogregrun4$y[i])
}
dim(V.nonrisk4) <- c(10000, 1)
#Vlad risk
pred1 <- predict(glm.reg.risk1, newdata = xlogregrun1, type = "response")
pred2 <- predict(glm.reg.risk2, newdata = xlogregrun2, type = "response")
pred3 <- predict(glm.reg.risk3, newdata = xlogregrun3, type = "response")
pred4 <- predict(glm.reg.risk4, newdata = xlogregrun4, type = "response")
V.risk1 <- cumsum(pred1 - xlogregrun1$y)
V.risk2 <- cumsum(pred2 - xlogregrun2$y)
V.risk3 <- cumsum(pred3 - xlogregrun3$y)
V.risk4 <- cumsum(pred4 - xlogregrun4$y)

#bootstrap function norisk
V.norisk1f <- function(data, index){
  baseline <- data[index, ]
  yhat <- mean(baseline$y)
  V <- numeric(0)
  V[1] <- yhat - xlogregrun1$y[1]
  for(i in 2:length(xlogregrun1)){
    V[i] <- V[i-1] + (yhat - xlogregrun1$y[i])
  }
  return(V)
}
V.norisk2f <- function(data, index){
  baseline <- data[index, ]
  yhat <- mean(baseline$y)
  V <- numeric(0)
  V[1] <- yhat - xlogregrun2$y[1]
  for(i in 2:length(xlogregrun2$y)){
    V[i] <- V[i-1] + (yhat - xlogregrun2$y[i])
  }
  return(V)
}
V.norisk3f <- function(data, index){
  baseline <- data[index, ]
  yhat <- mean(baseline$y)
  V <- numeric(0)
  V[1] <- yhat - xlogregrun3$y[1]
  for(i in 2:length(xlogregrun3$y)){
    V[i] <- V[i-1] + (yhat - xlogregrun3$y[i])
  }
  return(V)
}
V.norisk4f <- function(data, index){
  baseline <- data[index, ]
  yhat <- mean(baseline$y)
  V <- numeric(0)
  V[1] <- yhat - xlogregrun4$y[1]
  for(i in 2:length(xlogregrun4$y)){
    V[i] <- V[i-1] + (yhat - xlogregrun4$y[i])
  }
  return(V)
}

#Bootstrap risk functions
V.risk1f <- function(data, index){
  baseline <- data[index, ]
  glm.model <- glm(y~x1+x2+x3, family = binomial("logit"), 
                   data = data, subset = index)
  pred <- predict(glm.model, newdata = xlogregrun1, type = "response")
  V <- numeric(0)
  V[1] <- pred[1] - xlogregrun1$y[1]
  for(i in 2:length(xlogregrun1$y)){
    V[i] <- V[i-1] + (pred[i] - xlogregrun1$y[i])
  }
  return(V)
}
V.risk2f <- function(data, index){
  baseline <- data[index, ]
  glm.model <- glm(y~x1+x2+x3, family = binomial("logit"), 
                   data = data, subset = index)
  pred <- predict(glm.model, newdata = xlogregrun2, type = "response")
  V <- numeric(0)
  V[1] <- pred[1] - xlogregrun2$y[1]
  for(i in 2:length(xlogregrun2$y)){
    V[i] <- V[i-1] + (pred[i] - xlogregrun2$y[i])
  }
  return(V)
}
V.risk3f <- function(data, index){
  baseline <- data[index, ]
  glm.model <- glm(y~x1+x2+x3, family = binomial("logit"), 
                   data = data, subset = index)
  pred <- predict(glm.model, newdata = xlogregrun3, type = "response")
  V <- numeric(0)
  V[1] <- pred[1] - xlogregrun3$y[1]
  for(i in 2:length(xlogregrun3$y)){
    V[i] <- V[i-1] + (pred[i] - xlogregrun3$y[i])
  }
  return(V)
}
V.risk4f <- function(data, index){
  baseline <- data[index, ]
  glm.model <- glm(y~x1+x2+x3, family = binomial("logit"), 
                   data = data, subset = index)
  pred <- predict(glm.model, newdata = xlogregrun4, type = "response")
  V <- numeric(0)
  V[1] <- pred[1] - xlogregrun4$y[1]
  for(i in 2:length(xlogregrun4$y)){
    V[i] <- V[i-1] + (pred[i] - xlogregrun4$y[i])
  }
  return(V)
}