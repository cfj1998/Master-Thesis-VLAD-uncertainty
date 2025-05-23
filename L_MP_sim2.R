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

#Creating MP function for risk
w_1 <- 100 
w_2 <- 500/4 
w_3 <- 1000/5 
w_4 <- 1500/6 
L_vec1 <-c(3.820, 3.821, 3.822, 3.823, 3.824, 3.825, 3.826, 3.827, 3.828, 3.829, 3.830)
L_vec2 <-c(3.811, 3.812, 3.813, 3.814, 3.815, 3.816, 3.817, 3.818, 3.819, 3.820, 3.821)
L_vec3 <-c(3.560, 3.561, 3.562, 3.563, 3.564, 3.565, 3.566, 3.567, 3.568, 3.569, 3.570)
L_vec4 <-c(3.531, 3.532, 3.533, 3.534, 3.535, 3.536, 3.537, 3.538, 3.539, 3.540, 3.541)
k1 <- length(L_vec1)
k2 <- length(L_vec2)
k3 <- length(L_vec3)
k4 <- length(L_vec4)
L.find <- function(L_vec, n, w, baseline, regmod, k){
  runnew <- baseline[sample(1:length(baseline$y), size = n, replace = TRUE), ]
  p.risk <- predict(regmod, newdata = runnew, type = "response")
  p.risk.mean <- mean(p.risk)
  y_1 <- rbinom(n, size = 1, prob = p.risk)
  runnew$y <- y_1
  y_1pred <- rbinom(n, size = 1, prob = p.risk)
  nobs <- length(runnew$y)
  MP.risk <- numeric(0)
  variance_MP.risk <- numeric(0)
  for(l in 1:nobs){
    MP.risk[l] <- (1/min(w,l))*sum(y_1[max(1, l-min(w,l)):l]-y_1pred[max(1,l-min(w,l)):l])
    variance_MP.risk[l] <- (1/min(w,l))*p.risk.mean*(1-p.risk.mean)
  }
  UCL.matrix <- matrix(nrow = k, ncol = nobs)
  LCL.matrix <- matrix(nrow = k, ncol = nobs)
  events <- numeric(k)
  for(j in 1:k){
    UCL.risk <- 0 + L_vec[j]*sqrt(variance_MP.risk)
    UCL.matrix[j, ] <- UCL.risk
    LCL.risk <- 0 - L_vec[j]*sqrt(variance_MP.risk)
    LCL.matrix[j, ] <- LCL.risk
    events[j] <- ifelse(sum(MP.risk>UCL.risk)+sum(MP.risk<LCL.risk)>=1, 1,0)
  }
  return(events)
    
}
iter <- 10000
A.matrix1 <- matrix(nrow = k1, ncol = iter)
for(i in 1:iter){
  A <- L.find(L_vec1, n = 1000, w = w_1, baseline = xlogregbase1, regmod = glm.reg.risk1, k = k1)
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
  A <- L.find(L_vec2, n = 1000, w = w_2, baseline = xlogregbase2, regmod = glm.reg.risk2, k = k2)
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
  A <- L.find(L_vec3, n = 1000, w = w_3, baseline = xlogregbase3, regmod = glm.reg.risk3, k = k3)
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
  A <- L.find(L_vec4, n = 1000, w = w_4, baseline = xlogregbase4, regmod = glm.reg.risk4, k = k4)
  A.matrix4[,i] <- A
}

s4 <- nrow(A.matrix4)
prob4 <- numeric(0)
for(i in 1:s4){
  prob4[i] <- mean(A.matrix4[i, ])
}
prob4

#Improved MP

w_1 <- 100
w_2 <- 500/2
w_3 <- 1000/4 
w_4 <- 1500/4
L_vec1 <-c(2.960, 2.961, 2.962, 2.963, 2.964, 2.965, 2.966, 2.967, 2.968, 2.969, 2.970)
L_vec2 <-c(3.181, 3.182, 3.183, 3.184, 3.185, 3.186, 3.187, 3.188, 3.189, 3.190)
L_vec3 <-c(3.236, 3.237, 3.238, 3.239, 3.240, 3.241, 3.242, 3.243, 3.244, 3.245)
L_vec4 <-c(3.281, 3.282, 3.283, 3.284, 3.285, 3.286, 3.287, 3.288, 3.289, 3.290, 3.291)
k1 <- length(L_vec1)
k2 <- length(L_vec2)
k3 <- length(L_vec3)
k4 <- length(L_vec4)
L.find <- function(L_vec, n, w, baseline, regmod, k, n_0){
  runnew <- baseline[sample(1:length(baseline$y), size = n, replace = TRUE), ]
  p.risk <- predict(regmod, newdata = runnew, type = "response")
  p.risk.mean <- mean(p.risk)
  y_1 <- rbinom(n, size = 1, prob = p.risk)
  runnew$y <- y_1
  y_1pred <- rbinom(n, size = 1, prob = p.risk)
  nobs <- length(runnew$y)
  MP.risk <- numeric(0)
  variance_MP.risk <- numeric(0)
  for(l in 1:nobs){
    MP.risk[l] <- (1/min(w,l))*sum(y_1[max(1, l-min(w,l)):l]-y_1pred[max(1,l-min(w,l)):l])
    variance_MP.risk[l] <-  ((p.risk.mean*(1-p.risk.mean))/(n_0*min(w,l)))*(n_0+min(w,l))
  }
  UCL.matrix <- matrix(nrow = k, ncol = nobs)
  LCL.matrix <- matrix(nrow = k, ncol = nobs)
  events <- numeric(k)
  for(j in 1:k){
    UCL.risk <- 0 + L_vec[j]*sqrt(variance_MP.risk)
    UCL.matrix[j, ] <- UCL.risk
    LCL.risk <- 0 - L_vec[j]*sqrt(variance_MP.risk)
    LCL.matrix[j, ] <- LCL.risk
    events[j] <- ifelse(sum(MP.risk>UCL.risk)+sum(MP.risk<LCL.risk)>=1, 1,0)
  }
  return(events)
  
}
iter <- 10000
A.matrix1 <- matrix(nrow = k1, ncol = iter)
for(i in 1:iter){
  A <- L.find(L_vec1, n = 1000, w = w_1, baseline = xlogregbase1, regmod = glm.reg.risk1, k = k1, n_0 = nbase1)
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
  A <- L.find(L_vec2, n = 1000, w = w_2, baseline = xlogregbase2, regmod = glm.reg.risk2, k = k2, n_0 = nbase2)
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
  A <- L.find(L_vec3, n = 1000, w = w_3, baseline = xlogregbase3, regmod = glm.reg.risk3, k = k3, n_0 = nbase3)
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
  A <- L.find(L_vec4, n = 1000, w = w_4, baseline = xlogregbase4, regmod = glm.reg.risk4, k = k4, n_0 = nbase4)
  A.matrix4[,i] <- A
}

s4 <- nrow(A.matrix4)
prob4 <- numeric(0)
for(i in 1:s4){
  prob4[i] <- mean(A.matrix4[i, ])
}
prob4