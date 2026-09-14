rm(list = ls())
setwd("C:/Users/1998c/OneDrive/Skrivebord/Masteroppgave")

#All the code from line 5-60 was provided to me by Jan Terje Kvaloy
library(foreign)#To read SPSS file
library(zoo)
library(forecast)
library(spcadjust)
hbbdata=read.spss("HBB data 30.07.09-31.01.17.sav", use.value.labels = FALSE, to.data.frame = TRUE)
hbbdata=hbbdata[(hbbdata$NEONATAL_OUTCOM<5),]#Removes Neonatal outcome=5 from data
hbbdata=hbbdata[!is.na(hbbdata$NEONATAL_OUTCOM),]#removes Neonatal outcome=NA from data

#Replace Multi so 0 is singular birth and 1 is multiple birth baby
hbbdata$MULTI<- replace(hbbdata$MULTI, hbbdata$MULTI==8,0)
hbbdata$MULTI<- replace(hbbdata$MULTI, hbbdata$MULTI==2,1)
hbbdata$MULTI<- replace(hbbdata$MULTI, hbbdata$MULTI==3,1)
head(hbbdata)

#Calculate number of events per month.
Monthnr=hbbdata$Month-6+12*(hbbdata$Year-2009)
nMonths=tail(Monthnr,1)
nMonth=vector(length=nMonths)
for(i in c(1:nMonths)){
  nMonth[i]=sum(Monthnr==i)
}
nMonth2=nMonth[20:(nMonths-1)]
cnMonth2=cumsum(nMonth2)
cnMonth=cumsum(nMonth)


# Set up a data frame with relevant variables and remove cases with missing data
## Her kan det be  aktuelt for deg to try andre utvalg, men try gjerne first med denne og 
## try to lage et VLAD-plot basert pC% denne modellen. 
hbbdatamod1=hbbdata[c("Perinatal01","BIRTH_WEIGHT","PREG_COMP","FETAL_HEART","PRESENTATION2","BASELINE")]
hbbdatamod1=hbbdatamod1[complete.cases(hbbdatamod1),] # Remove cases with missing data
phaseone <- hbbdatamod1$BASELINE==1
phasetwo <- hbbdatamod1$BASELINE==0
hbbdatamod1 <-data.frame(y=hbbdatamod1$Perinatal01,
                         x1=hbbdatamod1$BIRTH_WEIGHT,
                         x1kg=hbbdatamod1$BIRTH_WEIGHT/1000,
                         x2=2-hbbdatamod1$PREG_COMP,
                         f2=as.integer(hbbdatamod1$FETAL_HEART==2),
                         f3=as.integer(hbbdatamod1$FETAL_HEART==3),
                         f9=as.integer(hbbdatamod1$FETAL_HEART==9),
                         p2=as.integer(hbbdatamod1$PRESENTATION2==2), 
                         p3=as.integer(hbbdatamod1$PRESENTATION2==3), 
                         p4=as.integer(hbbdatamod1$PRESENTATION2==4), 
                         p5=as.integer(hbbdatamod1$PRESENTATION2==5)) 

# Divide data into baseline and monitoring periods
estdata1 <- hbbdatamod1[phaseone,]  # Baseline
rundata1 <- hbbdatamod1[phasetwo,]# Monitoring period
dim(estdata1)
dim(rundata1)

# Estimate GLMs for baseline period
estmod1kg<-glm(y~x1kg+x2+f2+f3+f9+p2+p3+p4+p5, family=binomial("logit"), data=estdata1)
estmod1kg_fit <- summary(estmod1kg)
estmod1kg_fit
estmod1non_risk <- glm(y~1, family = binomial("logit"), data = estdata1)
estmod1non_risk_fit <- summary(estmod1non_risk)
estmod1non_risk_fit
#calculations
total_number_births <- 31122
births_per_date <- 31122/13 #average number of births
dates <- c("Feb_11", "July_11", "Jan_12", "July_12", "Jan_13", "July_13", "Jan_14", 
           "July_14", "Jan_15", "July_15", "Jan_16", "July_16", "Jan_17")
dates_estdata <- c("Jan_10", "July_10","Jan_11")
yhat <- mean(estdata1$y)

#Risk simulation
n <- 5000
w <- 5000
L_vec <-c(3.1, 3.15, 3.2, 3.25, 3.26, 3.27, 3.3) #L-values
k <- length(L_vec)
#L-function
L.find <- function(L_vec, n=500, w = 5000){
  rundata1_new <- estdata1[sample(1:length(estdata1$y), size = n, replace=TRUE), ]
  p.risk <- predict(estmod1kg, newdata = rundata1_new, type="response")
  p.risk.mean <- mean(p.risk)
  y_1 <- rbinom(n, size = 1, prob = p.risk)
  rundata1_new$y <- y_1
  y_1pred <- predict(estmod1kg, newdata=rundata1_new, type="response")
  nobs <- length(rundata1_new$y)
  MP.risk <- numeric(0)
  variance_MP.risk <- numeric(0)
  for(l in 1:nobs){
    MP.risk[l] <- (1/min(w,l))*sum(y_1pred[max(1,l-min(w,l)):l]-y_1[max(1, l-min(w,l)):l])
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
A <- L.find(L_vec, n=5000, w=5000)
k <- length(L_vec)
iter <- 10000

A.matrix <- matrix(nrow = k, ncol = iter)
for(i in 1:iter){
  A <- L.find(L_vec, n=5000, w=5000)
  A.matrix[,i] <- A
}

s <- nrow(A.matrix)
prob <- numeric(0)
for(i in 1:s){
  prob[i] <- mean(A.matrix[i, ])
}
prob

#Improved MP
n <- 5000
w <- 5000
L_vec <-c(3.1, 3.15, 3.2, 3.25, 3.26, 3.27, 3.3)
k <- length(L_vec)
L.find <- function(L_vec, n=500, w = 5000){
  rundata1_new <- estdata1[sample(1:length(estdata1$y), size = n, replace=TRUE), ]
  p.risk <- predict(estmod1kg, newdata = rundata1_new, type="response")
  p.risk.mean <- mean(p.risk)
  n_0 <- length(estdata1$y)
  y_1 <- rbinom(n, size = 1, prob = p.risk)
  rundata1_new$y <- y_1
  y_1pred <- predict(estmod1kg, newdata=rundata1_new, type="response")
  nobs <- length(rundata1_new$y)
  MP.risk <- numeric(0)
  variance_MP.risk <- numeric(0)
  for(l in 1:nobs){
    MP.risk[l] <- (1/min(w,l))*sum(y_1pred[max(1,l-min(w,l)):l]-y_1[max(1, l-min(w,l)):l])
    variance_MP.risk[l] <- ((p.risk.mean*(1-p.risk.mean))/(n_0*min(w,l)))*(n_0+min(w,l))
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
A <- L.find(L_vec, n=5000, w=5000)
k <- length(L_vec)
iter <- 10000

A.matrix <- matrix(nrow = k, ncol = iter)
for(i in 1:iter){
  A <- L.find(L_vec, n=5000, w=5000)
  A.matrix[,i] <- A
}

s <- nrow(A.matrix)
prob <- numeric(0)
for(i in 1:s){
  prob[i] <- mean(A.matrix[i, ])
}
prob

#Improved MP version 2
n <- 5000
w <- 5000
L_vec <-c(0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1.00)
k <- length(L_vec)
L.find <- function(L_vec, n=500, w = 5000){
  rundata1_new <- estdata1[sample(1:length(estdata1$y), size = n, replace=TRUE), ]
  p.risk <- predict(estmod1kg, newdata = rundata1_new, type="response")
  p.risk.mean <- mean(p.risk)
  p.risk.base <- predict(estmod1kg, newdata = estdata1, se.fit = TRUE)
  p.risk.base.se <- p.risk.base$se.fit
  p.risk.base.var <- (p.risk.base.se)^2
  p.risk.base.var.average <- mean(p.risk.base.var)
  n_0 <- length(estdata1$y)
  y_1 <- rbinom(n, size = 1, prob = p.risk)
  rundata1_new$y <- y_1
  y_1pred <- predict(estmod1kg, newdata=rundata1_new, type="response")
  nobs <- length(rundata1_new$y)
  MP.risk <- numeric(0)
  variance_MP.risk <- numeric(0)
  for(l in 1:nobs){
    MP.risk[l] <- (1/min(w,l))*sum(y_1pred[max(1,l-min(w,l)):l]-y_1[max(1, l-min(w,l)):l])
    variance_MP.risk[l] <- (1/min(w, l))*(p.risk.mean*(1-p.risk.mean)+min(w, l)*p.risk.base.var.average)
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
A <- L.find(L_vec, n=5000, w=5000)
k <- length(L_vec)
iter <- 10000

A.matrix <- matrix(nrow = k, ncol = iter)
for(i in 1:iter){
  A <- L.find(L_vec, n=5000, w=5000)
  A.matrix[,i] <- A
}

s <- nrow(A.matrix)
prob <- numeric(0)
for(i in 1:s){
  prob[i] <- mean(A.matrix[i, ])
}
prob

