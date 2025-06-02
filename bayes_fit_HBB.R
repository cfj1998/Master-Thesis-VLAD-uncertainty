#Bayesian Interval in the non risk HBB setting
#edited by me
#Orignial Code provided to me by my supervisor. The alpha_0 and beta0 were chosen by me. 
rm(list = ls())

#prior distribution
alpha0 <- 3

beta0 <- 200

curve(dbeta(x,shape1 = alpha0, shape2 = beta0),from=0,to=1,
      
      ylab="f(x)", col="red")


alpha0/(alpha0+beta0)

# Quantiles, prior distribution

qbeta(0.025,shape1 = alpha0, shape2 = beta0)

qbeta(0.975,shape1 = alpha0, shape2 = beta0)

#Baseline information
nbaseline <- 4839

sumybaseline <- 133

# Posterior mean

(sumybaseline+alpha0)/(nbaseline+alpha0+beta0)



# Bayes interval

qbeta(0.025,shape1 = sumybaseline+alpha0, shape2 = nbaseline-sumybaseline+beta0)

qbeta(0.975,shape1 = sumybaseline+alpha0, shape2 = nbaseline-sumybaseline+beta0)
