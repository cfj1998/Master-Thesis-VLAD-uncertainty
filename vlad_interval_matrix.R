rm(list = ls())
set.seed(10)
nbase1 <- 50
nbase2 <- 200
nbase3 <- 500
nbase4 <- 2000
vlad <- function(nbase){
  A <- matrix(nrow = 50, ncol = 1000)
  B <- matrix(nrow = 50, ncol = 1000)
  C <- matrix(nrow = 50, ncol = 1000)
  for(i in 1:50){
    xlogregbase <- data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
    xlogregrun <- data.frame(x1 = rbinom(1000, 1, 0.2), x2 = runif(1000, 0, 1), x3 = rnorm(1000, 0, 2))
    xbetabase <- xlogregbase$x1 + xlogregbase$x2 + xlogregbase$x3
    xbetarun <- xlogregrun$x1 + xlogregrun$x2 + xlogregrun$x3
    xlogregbase$y <- rbinom(nbase, size = 1, prob = exp(xbetabase)/(1+exp(xbetabase)))
    xlogregrun$y <- rbinom(1000, size = 1, prob = exp(xbetarun)/(1+exp(xbetarun)))
    regmod <- glm(y~1, data <- xlogregbase, family = binomial("logit"))
    pred <- mean(xlogregbase$y)
    pred_low <- pred - 1.96*sqrt((pred*(1-pred))/nbase)
    pred_high <- pred + 1.96*sqrt((pred*(1-pred))/nbase)
    V_low <- numeric(0)
    V <- numeric(0)
    V_high <- numeric(0)
    V_low[1] <- pred_low - xlogregrun$y[1]
    for(j in 2:1000){
      V_low[j] <- V_low[j-1] + (pred_low - xlogregrun$y[j])
    }
    dim(V_low) <- c(1000, 1)
    V[1] <- pred - xlogregrun$y[1]
    for(j in 2:1000){
      V[j] <- V[j-1] + (pred - xlogregrun$y[j])
    }
    dim(V) <- c(1000, 1)
    V_high[1] <- pred_high - xlogregrun$y[1]
    for(j in 2:1000){
      V_high[j] <- V_high[j-1] + (pred_high - xlogregrun$y[j])
    }
    dim(V_high) <- c(1000, 1)
    A[i, ] <- V_low
    B[i, ] <- V
    C[i, ] <- V_high
  }
  V_list <- list(A, B, C)
  return(V_list)
}
V_ci1 <- vlad(nbase1)
V_ci2 <- vlad(nbase2)
V_ci3 <- vlad(nbase3)
V_ci4 <- vlad(nbase4)
V_ci_l1 <- V_ci1[[1]]
V_ci_l2 <- V_ci2[[1]]
V_ci_l3 <- V_ci3[[1]]
V_ci_l4 <- V_ci4[[1]]
V_ci_u1 <- V_ci1[[3]]
V_ci_u2 <- V_ci2[[3]]
V_ci_u3 <- V_ci3[[3]]
V_ci_u4 <- V_ci4[[3]]
V1 <- V_ci1[[2]]
V2 <- V_ci2[[2]]
V3 <- V_ci3[[2]]
V4 <- V_ci4[[2]]
par(mfrow = c(2, 2))
matplot(t(V1), type = "l", lwd = 6, xlab = "t", main = "Baseline with n = 50", ylab = "V_t", 
        lty = 1, ylim = c(-300, 300), col = c("black"))
matlines(t(V_ci_l1), col = "red", lty = 1)
matlines(t(V_ci_u1), col = "green", lty = 1)
matplot(t(V2), type = "l", lwd = 6, xlab = "t", main = "Baseline with n = 200", 
        ylab = "V_t", lty = 1, ylim = c(-300, 300), col = c("black"))
matlines(t(V_ci_l2), col = "red", lty = 1)
matlines(t(V_ci_u2), col = "green", lty = 1)
matplot(t(V3), type = "l", lwd = 6, xlab = "t", main = "Baseline with n = 500", ylab = "V_t", 
        lty = 1, , ylim = c(-300, 300), col = c("black"))
matlines(t(V_ci_l3), col = "red", lty = 1)
matlines(t(V_ci_u3), col = "green", lty = 1)
matplot(t(V4), type = "l", lwd = 6, xlab = "t", main = "Baseline with n = 2000", ylab = "V_t", 
        lty = 1, , ylim = c(-300, 300), col = c("black"))
matlines(t(V_ci_l4), col = "red", lty = 1)
matlines(t(V_ci_u4), col = "green", lty = 1)

