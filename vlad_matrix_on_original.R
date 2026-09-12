rm(list = ls())
set.seed(10)
nbase1 <- 50
nbase2 <- 200
nbase3 <- 500
nbase4 <- 2000
vlad <- function(nbase){
  A <- matrix(nrow = 50, ncol = 1000)
  for(i in 1:50){
    xlogregbase <- data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
    xlogregrun <- data.frame(x1 = rbinom(1000, 1, 0.2), x2 = runif(1000, 0, 1), x3 = rnorm(1000, 0, 2))
    xbetabase <- xlogregbase$x1 + xlogregbase$x2 + xlogregbase$x3
    xbetarun <- xlogregrun$x1 + xlogregrun$x2 + xlogregrun$x3
    xlogregbase$y <- rbinom(nbase, size = 1, prob = exp(xbetabase)/(1+exp(xbetabase)))
    xlogregrun$y <- rbinom(1000, size = 1, prob = exp(xbetarun)/(1+exp(xbetarun)))
    regmod <- glm(y~1, data <- xlogregbase, family = binomial("logit"))
    beta_0 <- summary(regmod)$coefficients[1, 1]
    pred <- exp(beta_0)/(1+exp(beta_0))
    V <- numeric(0)
    V[1] <- pred - xlogregrun$y[1]
    for(j in 2:1000){
      V[j] <- V[j-1] + (pred - xlogregrun$y[j])
    }
    dim(V) <- c(1000, 1)
    A[i, ] <- V
  }
  
  return(A)
}
vlad1 <- vlad(nbase1)
vlad2 <- vlad(nbase2)
vlad3 <- vlad(nbase3)
vlad4 <- vlad(nbase4)
par(mfrow = c(2, 2))
matplot(t(vlad1), type = "l", lwd = 1, main = "Baseline with n = 50", col = 1, 
        ylim = c(-200, 200), ylab = "V_t", xlab = "t")
abline(h = 0, lty = 2)
matplot(t(vlad2), type = "l", lwd = 1, main = "Baseline with n = 200", col = 1,  
        ylim = c(-200, 200), ylab = "V_t", xlab = "t")
abline(h = 0, lty = 2)
matplot(t(vlad3), type = "l", lwd = 1, main = "Baseline with n = 500", col = 1, 
        ylim = c(-200, 200), ylab = "V_t", xlab = "t")
abline(h = 0, lty = 2)
matplot(t(vlad4), type = "l", lwd = 1, main = "Baseline with n = 2000", col = 1,  
        ylim = c(-200, 200), ylab = "V_t", xlab = "t")
abline(h = 0, lty = 2)
