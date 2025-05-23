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

#Creating MP function for no-risk
w_1 <- 100 
w_2 <- 500/4 
w_3 <- 1000/5 
w_4 <- 1500/6 
L_vec1 <-c(3.450, 3.451, 3.452, 3.453, 3.454, 3.455, 3.456, 3.457, 3.458, 3.459, 3.460)
L_vec2 <-c(3.381, 3.382, 3.383, 3.384, 3.385, 3.386, 3.387, 3.388, 3.389)
L_vec3 <-c(3.257, 3.258, 3.259, 3.260, 3.261, 3.262, 3.263, 3.264, 3.265, 3.266, 3.267, 3.268, 3.269)
L_vec4 <-c(3.263, 3.264, 3.265, 3.266, 3.267, 3.268, 3.269, 3.270, 3.271, 3.272, 3.273)
k1 <- length(L_vec1)
k2 <- length(L_vec2)
k3 <- length(L_vec3)
k4 <- length(L_vec4)
L.find <- function(L_vec, n, w, yhat, regmod, k){
  y_1 <- rbinom(n, size=1, prob=yhat)
  data <- data.frame(y_1)
  y_1pred <- predict(regmod, newdata=data, type="response")
  nobs <- length(y_1)
  MP.norisk <- numeric(0)
  variance_MP.norisk <- numeric(0)
  for(l in 1:nobs){
    MP.norisk[l] <- (1/(min(w,l)))*sum(y_1[max(1, l-min(w, l)):l]-y_1pred[max(1,l-min(w, l)):l])
    variance_MP.norisk[l] <- (1/(min(w,l)))*yhat*(1-yhat)
  }
  UCL.matrix <- matrix(nrow = k, ncol = nobs)
  LCL.matrix <- matrix(nrow = k, ncol = nobs)
  events <- numeric(k)
  for(j in 1:k){
    UCL.norisk <- 0 + L_vec[j]*sqrt(variance_MP.norisk)
    UCL.matrix[j, ] <- UCL.norisk
    LCL.norisk <- 0 - L_vec[j]*sqrt(variance_MP.norisk)
    LCL.matrix[j, ] <- LCL.norisk
    events[j] <- ifelse(sum(MP.norisk>UCL.norisk)+sum(MP.norisk<LCL.norisk)>=1, 1,0)
  }
  return(events)
}
iter <- 10000
A.matrix1 <- matrix(nrow = k1, ncol = iter)
for(i in 1:iter){
  A <- L.find(L_vec1, n=1000, w=w_1, yhat=m1.base, regmod = glm.reg.norisk1, k = k1)
  A.matrix1[,i] <- A
}

s1 <- nrow(A.matrix1)
prob1 <- numeric(0)
for(i in 1:s1){
  prob1[i] <- mean(A.matrix1[i, ])
}
prob1

A.matrix2 <- matrix(nrow = k2, ncol = iter)
for(i in 1:iter){
  A <- L.find(L_vec2, n=1000, w=w_2, yhat=m2.base, regmod = glm.reg.norisk2, k = k2)
  A.matrix2[,i] <- A
 }

s2 <- nrow(A.matrix2)
prob2 <- numeric(0)
for(i in 1:s2){
  prob2[i] <- mean(A.matrix2[i, ])
}
prob2

A.matrix3 <- matrix(nrow = k3, ncol = iter)
for(i in 1:iter){
  A <- L.find(L_vec3, n=1000, w=w_3, yhat=m3.base, regmod = glm.reg.norisk3, k = k3)
  A.matrix3[,i] <- A
 }

 s3 <- nrow(A.matrix3)
prob3 <- numeric(0)
 for(i in 1:s3){
  prob3[i] <- mean(A.matrix3[i, ])
}
prob3

A.matrix4 <- matrix(nrow = k4, ncol = iter)
for(i in 1:iter){
  A <- L.find(L_vec4, n=1000, w=w_4, yhat=m4.base, regmod = glm.reg.norisk4, k = k4)
  A.matrix4[,i] <- A
}

s4 <- nrow(A.matrix4)
prob4 <- numeric(0)
for(i in 1:s4){
  prob4[i] <- mean(A.matrix4[i, ])
}
prob4

#Improved MP function
w_1 <- 100 
w_2 <- 500/2 
w_3 <- 1000/4 
w_4 <- 1500/4
L_vec1 <-c(2.630, 2.631, 2.632, 2.633, 2.634, 2.635, 2.636, 2.637, 2.638, 2.639, 2.640)
L_vec2 <-c(2.841, 2.842, 2.843, 2.844, 2.845, 2.846, 2.847, 2.848, 2.849, 2.850)
L_vec3 <-c(2.976, 2.977, 2.978, 2.979, 2.981, 2.982, 2.983, 2.984, 2.985, 2.986)
L_vec4 <-c(2.941, 2.942, 2.943, 2.944, 2.945, 2.946, 2.947, 2.948, 2.949, 2.950, 2.951)
k1 <- length(L_vec1)
k2 <- length(L_vec2)
k3 <- length(L_vec3)
k4 <- length(L_vec4)
L.find <- function(L_vec, n, w, yhat, regmod, k, n_0){
  y_1 <- rbinom(n, size=1, prob=yhat)
  data <- data.frame(y_1)
  y_1pred <- predict(regmod, newdata=data, type="response")
  nobs <- length(y_1)
  MP.norisk <- numeric(0)
  variance_MP.norisk <- numeric(0)
  for(l in 1:nobs){
    MP.norisk[l] <- (1/(min(w,l)))*sum(y_1[max(1, l-min(w, l)):l]-y_1pred[max(1,l-min(w, l)):l])
    variance_MP.norisk[l] <-  (n_0*yhat*(1-yhat)+min(w,l)*yhat*(1-yhat))/(n_0*(min(w, l)))
  }
  UCL.matrix <- matrix(nrow = k, ncol = nobs)
  LCL.matrix <- matrix(nrow = k, ncol = nobs)
  events <- numeric(k)
  for(j in 1:k){
    UCL.norisk <- 0 + L_vec[j]*sqrt(variance_MP.norisk)
    UCL.matrix[j, ] <- UCL.norisk
    LCL.norisk <- 0 - L_vec[j]*sqrt(variance_MP.norisk)
    LCL.matrix[j, ] <- LCL.norisk
    events[j] <- ifelse(sum(MP.norisk>UCL.norisk)+sum(MP.norisk<LCL.norisk)>=1, 1,0)
  }
  return(events)
}
iter <- 10000
A.matrix1 <- matrix(nrow = k1, ncol = iter)
for(i in 1:iter){
  A <- L.find(L_vec1, n=1000, w=w_1, yhat=m1.base, regmod = glm.reg.norisk1, k = k1, n_0 = nbase1)
  A.matrix1[,i] <- A
}

s1 <- nrow(A.matrix1)
prob1 <- numeric(0)
for(i in 1:s1){
  prob1[i] <- mean(A.matrix1[i, ])
}
prob1

A.matrix2 <- matrix(nrow = k2, ncol = iter)
for(i in 1:iter){
  A <- L.find(L_vec2, n=1000, w=w_2, yhat=m2.base, regmod = glm.reg.norisk2, k = k2, n_0 = nbase2)
  A.matrix2[,i] <- A
 }

s2 <- nrow(A.matrix2)
prob2 <- numeric(0)
for(i in 1:s2){
  prob2[i] <- mean(A.matrix2[i, ])
}
prob2

A.matrix3 <- matrix(nrow = k3, ncol = iter)
for(i in 1:iter){
  A <- L.find(L_vec3, n=1000, w=w_3, yhat=m3.base, regmod = glm.reg.norisk3, k = k3, n_0 = nbase3)
  A.matrix3[,i] <- A
 }

 s3 <- nrow(A.matrix3)
prob3 <- numeric(0)
 for(i in 1:s3){
  prob3[i] <- mean(A.matrix3[i, ])
}
prob3

A.matrix4 <- matrix(nrow = k4, ncol = iter)
for(i in 1:iter){
  A <- L.find(L_vec4, n=1000, w=w_4, yhat=m4.base, regmod = glm.reg.norisk4, k = k4, n_0 = nbase4)
  A.matrix4[,i] <- A
}

s4 <- nrow(A.matrix4)
prob4 <- numeric(0)
for(i in 1:s4){
  prob4[i] <- mean(A.matrix4[i, ])
}
prob4