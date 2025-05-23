rm(list = ls())
library(boot)

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
    pred <- predict(regmod, newdata = xlogregrun, type = "response")
    V <- numeric(0)
    V[1] <- pred[1] - xlogregrun$y[1]
    for(j in 2:1000){
      V[j] <- V[j-1] + (pred[j] - xlogregrun$y[j])
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

#Uncertainty in bootstrap Intervals
V.norisk <- function(data, index, rundata){
  baseline <- data[index, ]  # Baseline
  baselinereg <-glm(y~1, family=binomial("logit"), data=baseline, 
                    subset = index)
  V <- numeric(0)
  pred <- predict(baselinereg, newdata = rundata, type = "response")
  V[1] <- pred[1] - rundata$y[1]
  for(k in 2:length(rundata$y)){
    V[k] <- V[k - 1] + (pred[k] - rundata$y[k])
  }
  return(V)
}
bootstrap_f <- function(nbase){
  A <- matrix(nrow = 50, ncol = 1000) #Lower_limit
  B <- matrix(nrow = 50, ncol = 1000) #Upper_limit
  for(i in 1:50){
    xlogregbase <- data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
    xlogregrun <- data.frame(x1 = rbinom(1000, 1, 0.2), x2 = runif(1000, 0, 1), x3 = rnorm(1000, 0, 2))
    xbetabase <- xlogregbase$x1 + xlogregbase$x2 + xlogregbase$x3
    xbetarun <- xlogregrun$x1 + xlogregrun$x2 + xlogregrun$x3
    xlogregbase$y <- rbinom(nbase, size = 1, prob = exp(xbetabase)/(1+exp(xbetabase)))
    xlogregrun$y <- rbinom(1000, size = 1, prob = exp(xbetarun)/(1+exp(xbetarun)))
    regmod <- glm(y~1, data <- xlogregbase, family = binomial("logit"))
    bootstrap.V.norisk <- boot(xlogregbase, V.norisk, R = 2000, rundata = xlogregrun)
    ci.norisk.perc.low <- numeric(0)
    ci.norisk.perc.up <- numeric(0)
    for(j in 1:length(xlogregrun$y)){
      perc.int <- boot.ci(bootstrap.V.norisk, type = c("perc"), index = j)
      ci.norisk.perc.low[j] <- perc.int$percent[4]
      ci.norisk.perc.up[j] <- perc.int$percent[5]
    }
    A[i, ] <- ci.norisk.perc.low
    B[i, ] <- ci.norisk.perc.up
  }
  ci <- list(A, B)
  return(ci)
}
ci1 <- bootstrap_f(nbase1)
lower_limit1 <- ci1[[1]]
upper_limit1 <- ci1[[2]]
ci2 <- bootstrap_f(nbase2)
lower_limit2 <- ci2[[1]]
upper_limit2 <- ci2[[2]]
ci3 <- bootstrap_f(nbase3)
lower_limit3 <- ci3[[1]]
upper_limit3 <- ci3[[2]]
ci4 <- bootstrap_f(nbase4)
lower_limit4 <- ci4[[1]]
upper_limit4 <- ci4[[2]]
par(mfrow = c(2, 2))
matplot(t(lower_limit1), type = "l", col = "red", lwd = 2, ylim = c(-300, 300), ylab = "V_t",
        main = "Baseline with n = 50", xlab = "t", lty = 2)
matlines(t(upper_limit1), col = "green", lwd = 2, lty = 2)
matlines(t(vlad1), col = "black", lwd = 1)
abline(h = 0, lty = 2)
matplot(t(lower_limit2), type = "l", col = "red", lwd = 2, ylim = c(-300, 300), ylab = "V_t",
        main = "Baseline with n = 200", xlab = "t", lty = 2 )
matlines(t(upper_limit2), col = "green", lwd = 2, lty = 2)
matlines(t(vlad2), col = "black", lty = 1)
abline(h = 0, lty = 2)
matplot(t(lower_limit3), type = "l", col = "red", lwd = 2, ylim = c(-300, 300), ylab = "V_t",
        main = "Baseline with n = 500", xlab = "t", lty = 2)
matlines(t(upper_limit3), col = "green", lwd = 2, lty = 2)
matlines(t(vlad3), col = "black", lty = 1, lwd = 1)
abline(h = 0, lty = 2)
matplot(t(lower_limit4), type = "l", col = "red", lwd = 2, ylim = c(-300, 300), ylab = "V_t",
        main = "Baseline with n = 2000", xlab = "t", lty = 2)
matlines(t(upper_limit4), col = "green", lwd = 2, lty = 2)
matlines(t(vlad4), col = "black", lty = 1)
abline(h = 0, lty = 2)