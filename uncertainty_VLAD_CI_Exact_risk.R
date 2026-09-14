rm(list = ls())
set.seed(10)
#number of observations for baseline
nbase1 <- 50
nbase2 <- 200
nbase3 <- 500
nbase4 <- 2000
inverse_logit <- function(x)
  1/(1+exp(-x))
#Vlad function
vlad <- function(nbase){
  A <- matrix(nrow = 50, ncol = 1000)
  for(i in 1:50){
    xlogregbase <- data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
    xlogregrun <- data.frame(x1 = rbinom(1000, 1, 0.2), x2 = runif(1000, 0, 1), x3 = rnorm(1000, 0, 2))
    xbetabase <- xlogregbase$x1 + xlogregbase$x2 + xlogregbase$x3
    xbetarun <- xlogregrun$x1 + xlogregrun$x2 + xlogregrun$x3
    xlogregbase$y <- rbinom(nbase, size = 1, prob = exp(xbetabase)/(1+exp(xbetabase)))
    xlogregrun$y <- rbinom(1000, size = 1, prob = exp(xbetarun)/(1+exp(xbetarun)))
    regmod <- glm(y~x1+x2+x3, data <- xlogregbase, family = binomial("logit"))
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
par(mfrow = c(2, 2))
matplot(t(vlad1), type = "l", lty = 1, main = "Baseline with n = 50", col = 1, 
        ylim = c(-150, 150), ylab = "V_t", xlab = "t")
abline(h = 0, lty = 2)
matplot(t(vlad2), type = "l", lty = 1, main = "Baseline with n = 200", col = 1,  
        ylim = c(-150, 150), ylab = "V_t", xlab = "t")
abline(h = 0, lty = 2)
matplot(t(vlad3), type = "l", lty = 1, main = "Baseline with n = 500", col = 1, 
        ylim = c(-150, 150), ylab = "V_t", xlab = "t")
abline(h = 0, lty = 2)
matplot(t(vlad4), type = "l", lty = 1, main = "Baseline with n = 2000", col = 1,  
        ylim = c(-150, 150), ylab = "V_t", xlab = "t")
abline(h = 0, lty = 2)

#CI_function
vlad_ci <- function(nbase){
  A <- matrix(nrow = 50, ncol = 1000) #Lower_limit
  B <- matrix(nrow = 50, ncol = 1000) #Upper_limit
  for(i in 1:50){
    xlogregbase <- data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
    xlogregrun <- data.frame(x1 = rbinom(1000, 1, 0.2), x2 = runif(1000, 0, 1), x3 = rnorm(1000, 0, 2))
    xbetabase <- xlogregbase$x1 + xlogregbase$x2 + xlogregbase$x3
    xbetarun <- xlogregrun$x1 + xlogregrun$x2 + xlogregrun$x3
    xlogregbase$y <- rbinom(nbase, size = 1, prob = exp(xbetabase)/(1+exp(xbetabase)))
    xlogregrun$y <- rbinom(1000, size = 1, prob = exp(xbetarun)/(1+exp(xbetarun)))
    regmod <- glm(y~x1+x2+x3, data <- xlogregbase, family = binomial("logit"))
    p <- predict(regmod, newdata = xlogregrun, type = "response")
    p.risk.average <- mean(p)
    t <- length(xlogregrun$y)
    n_0 <- length(xlogregbase$y)
    V.risk.var <- numeric(0)
    for(j in 1:length(xlogregrun$y)){
      V.risk.var[j] <- (j*(j*p.risk.average*(1-p.risk.average) + n_0*p.risk.average*(1-p.risk.average)))/n_0
    }
    V.risk.se <- sqrt(V.risk.var)
    V <- numeric(0)
    V[1] <- p[1] - xlogregrun$y[1]
    for(j in 2:1000){
      V[j] <- V[j-1] + (p[j] - xlogregrun$y[j])
    }
    dim(V) <- c(1000, 1)
    V.min <- V - 1.96*V.risk.se
    V.max <- V + 1.96*V.risk.se
    A[i, ] <- V.min
    B[i, ] <- V.max
  }
  ci <- list(A, B)
  return(ci)
}
ci1 <- vlad_ci(nbase1)
lower_limit1 <- ci1[[1]]
upper_limit1 <- ci1[[2]]
ci2 <- vlad_ci(nbase2)
lower_limit2 <- ci2[[1]]
upper_limit2 <- ci2[[2]]
ci3 <- vlad_ci(nbase3)
lower_limit3 <- ci3[[1]]
upper_limit3 <- ci3[[2]]
ci4 <- vlad_ci(nbase4)
lower_limit4 <- ci4[[1]]
upper_limit4 <- ci4[[2]]
par(mfrow = c(2, 2))
matplot(t(lower_limit1), type = "l", col = "red", lwd = 5, ylim = c(-300, 300), ylab = "V_t",
        main = "Baseline with n = 50", xlab = "t", lty = 1)
matlines(t(upper_limit1), col = "green", lwd = 5, lty = 1)
matlines(t(vlad1), col = "black", lwd = 1)
abline(h = 0, lty = 2)
matplot(t(lower_limit2), type = "l", col = "red", lwd = 5, ylim = c(-300, 300), ylab = "V_t",
        main = "Baseline with n = 200", xlab = "t", lty = 1)
matlines(t(upper_limit2), col = "green", lwd = 5, lty = 1)
matlines(t(vlad2), col = "black", lty = 1)
abline(h = 0, lty = 2)
matplot(t(lower_limit3), type = "l", col = "red", lwd = 5, ylim = c(-300, 300), ylab = "V_t",
        main = "Baseline with n = 500", xlab = "t", lty = 1)
matlines(t(upper_limit3), col = "green", lwd = 5, lty = 1)
matlines(t(vlad3), col = "black", lty = 1, lwd = 1)
abline(h = 0, lty = 2)
matplot(t(lower_limit4), type = "l", col = "red", lwd = 5, ylim = c(-300, 300), ylab = "V_t",
        main = "Baseline with n = 2000", xlab = "t", lty = 1)
matlines(t(upper_limit4), col = "green", lwd = 5)
matlines(t(vlad4), col = "black", lty = 1)
abline(h = 0, lty = 2)

#Uncertainty risk version 2
vlad_ci <- function(nbase){
  A <- matrix(nrow = 50, ncol = 1000) #Lower_limit
  B <- matrix(nrow = 50, ncol = 1000) #Upper_limit
  for(i in 1:50){
    xlogregbase <- data.frame(x1 = rbinom(nbase, 1, 0.2), x2 = runif(nbase, 0, 1), x3 = rnorm(nbase, 0, 2))
    xlogregrun <- data.frame(x1 = rbinom(1000, 1, 0.2), x2 = runif(1000, 0, 1), x3 = rnorm(1000, 0, 2))
    xbetabase <- xlogregbase$x1 + xlogregbase$x2 + xlogregbase$x3
    xbetarun <- xlogregrun$x1 + xlogregrun$x2 + xlogregrun$x3
    xlogregbase$y <- rbinom(nbase, size = 1, prob = exp(xbetabase)/(1+exp(xbetabase)))
    xlogregrun$y <- rbinom(1000, size = 1, prob = exp(xbetarun)/(1+exp(xbetarun)))
    regmod <- glm(y~x1+x2+x3, data <- xlogregbase, family = binomial("logit"))
    p <- predict(regmod, newdata = xlogregrun, type = "response")
    p.risk.average <- mean(p)
    p.base <- predict(regmod, newdata = xlogregbase, type = "response", se.fit = TRUE)
    p.base.se <- p.base$se.fit
    p.base.var <- (p.base.se)^2
    p.base.var.average <- mean(p.base.var)
    t <- length(xlogregrun$y)
    n_0 <- length(xlogregbase$y)
    V.risk.var <- numeric(0)
    for(j in 1:length(xlogregrun$y)){
      V.risk.var[j] <- (j*(j*p.base.var.average + p.risk.average*(1-p.risk.average)))
    }
    V.risk.se <- sqrt(V.risk.var)
    V <- numeric(0)
    V[1] <- p[1] - xlogregrun$y[1]
    for(j in 2:1000){
      V[j] <- V[j-1] + (p[j] - xlogregrun$y[j])
    }
    dim(V) <- c(1000, 1)
    V.min <- V - 1.96*V.risk.se
    V.max <- V + 1.96*V.risk.se
    A[i, ] <- V.min
    B[i, ] <- V.max
  }
  ci <- list(A, B)
  return(ci)
}
ci1 <- vlad_ci(nbase1)
lower_limit1 <- ci1[[1]]
upper_limit1 <- ci1[[2]]
ci2 <- vlad_ci(nbase2)
lower_limit2 <- ci2[[1]]
upper_limit2 <- ci2[[2]]
ci3 <- vlad_ci(nbase3)
lower_limit3 <- ci3[[1]]
upper_limit3 <- ci3[[2]]
ci4 <- vlad_ci(nbase4)
lower_limit4 <- ci4[[1]]
upper_limit4 <- ci4[[2]]
par(mfrow = c(2, 2))
matplot(t(lower_limit1), type = "l", col = "red", lwd = 5, ylim = c(-400, 400), ylab = "V_t",
        main = "Baseline with n = 50", xlab = "t", lty = 1)
matlines(t(upper_limit1), col = "green", lwd = 5, lty = 1)
matlines(t(vlad1), col = "black", lwd = 1)
abline(h = 0, lty = 2)
matplot(t(lower_limit2), type = "l", col = "red", lwd = 5, ylim = c(-400, 400), ylab = "V_t",
        main = "Baseline with n = 200", xlab = "t", lty = 1)
matlines(t(upper_limit2), col = "green", lwd = 5, lty = 1)
matlines(t(vlad2), col = "black", lty = 1)
abline(h = 0, lty = 2)
matplot(t(lower_limit3), type = "l", col = "red", lwd = 5, ylim = c(-400, 400), ylab = "V_t",
        main = "Baseline with n = 500", xlab = "t", lty = 1)
matlines(t(upper_limit3), col = "green", lwd = 5, lty = 1)
matlines(t(vlad3), col = "black", lty = 1, lwd = 1)
abline(h = 0, lty = 2)
matplot(t(lower_limit4), type = "l", col = "red", lwd = 5, ylim = c(-400, 400), ylab = "V_t",
        main = "Baseline with n = 2000", xlab = "t", lty = 1)
matlines(t(upper_limit4), col = "green", lwd = 5)
matlines(t(vlad4), col = "black", lty = 1)
abline(h = 0, lty = 2)
