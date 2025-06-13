rm(list=ls())
setwd("C:/Users/1998c/OneDrive/Skrivebord/Masteroppgave")
#Read data and format correctly for glm
library(foreign)#To read SPSS file #This line of code was provided to me by supervisor
library(zoo)
library(forecast)
library(spcadjust)
#following code provided to me by my supervisor (line 9-59)
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
#end of provided code
#calculations
total_number_births <- 31122
births_per_date <- 31122/13 #average number of births
dates <- c("Feb_11", "July_11", "Jan_12", "July_12", "Jan_13", "July_13", "Jan_14", 
           "July_14", "Jan_15", "July_15", "Jan_16", "July_16", "Jan_17")
dates_estdata <- c("Jan_10", "July_10","Jan_11")
yhat <- mean(estdata1$y)
# #Simulation no risk situation
n <- 500
w <- 5000
L_vec <-c(4.0871, 4.0872, 4.0873, 4.0874, 4.0875, 4.088, 4.089)
k <- length(L_vec)
L.find <- function(L_vec, n=5000, w=5000, yhat){
  y_1 <- rbinom(n, size=1, prob=yhat)
  data <- data.frame(y_1)
  y_1pred <- predict(estmod1non_risk, newdata=data, type="response")
  nobs <- length(y_1)
  MP.norisk <- numeric(0)
  variance_MP.norisk <- numeric(0)
  for(l in 1:nobs){
    MP.norisk[l] <- (1/min(w,l))*sum(y_1[max(1, l-min(w,l)):l]-y_1pred[max(1,l-min(w,l)):l])
    variance_MP.norisk[l] <- (1/min(w,l))*yhat*(1-yhat)
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
A <- L.find(L_vec, n=5000, w=5000, yhat)
k <- length(L_vec)
iter <- 10000

A.matrix <- matrix(nrow = k, ncol = iter)
for(i in 1:iter){
  A <- L.find(L_vec, n=5000, w=5000, yhat)
  A.matrix[,i] <- A
}

s <- nrow(A.matrix)
prob <- numeric(0)
for(i in 1:s){
  prob[i] <- mean(A.matrix[i, ])
}
prob

#Improved MP_function
y_1.hat <- mean(rundata1$y)
L_vec <-c(4.086, 4.0861, 4.0862, 4.0863, 4.0864, 4.0865, 4.0866, 4.0867, 4.0869, 4.087)
k <- length(L_vec)
L.find <- function(L_vec, n=5000, w=5000, yhat){
   y_1 <- rbinom(n, size=1, prob=yhat)
   y_1hat <- mean(y_1)
   data <- data.frame(y_1)
   y_1pred <- predict(estmod1non_risk, newdata=data, type="response")
   nobs <- length(y_1)
   MP.norisk <- numeric(0)
   n_0 <- length(estdata1$y)
   variance_MP.norisk <- numeric(0)
   for(l in 1:nobs){
    MP.norisk[l] <- (1/min(w,l))*sum(y_1[max(1, l-min(w,l)):l]-y_1pred[max(1,l-min(w,l)):l])     
    variance_MP.norisk[l] <- (n_0*yhat*(1-yhat)+min(w,l)*yhat*(1-yhat))/(n_0*(min(w, l)))
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

 A <- L.find(L_vec, n=5000, w=5000, yhat)
 k <- length(L_vec)
 iter <- 10000

A.matrix <- matrix(nrow = k, ncol = iter)
for(i in 1:iter){
  A <- L.find(L_vec, n=5000, w=5000, yhat)
  A.matrix[,i] <- A
 }

s <- nrow(A.matrix)
prob <- numeric(0)
for(i in 1:s){
  prob[i] <- mean(A.matrix[i, ])
}
prob


# #Optimal MP
# L_vec <-c(6, 7, 8, 9, 10)
# k <- length(L_vec)
# L.find <- function(L_vec, n=5000, w=5000, yhat){
#   y_1 <- rbinom(n, size=1, prob=yhat)
#   t <- seq(from = 1, to = length(y_1))
#   y_1hat <- mean(y_1)
#   data <- data.frame(y_1)
#   y_1pred <- predict(estmod1non_risk, newdata=data, type="response")
#   nobs <- length(y_1)
#   p.1.norisk <- cumsum(y_1)/t
#   MP.norisk <- numeric(0)
#   n_0 <- length(estdata1$y)
#   variance.MP.norisk <- numeric(0)
#   for(l in 1:nobs){
#     MP.norisk[l] <- (1/min(w,l))*sum(y_1[max(1, l-min(w,l)):l]-yhat)
#     variance.MP.norisk[l] <- ((1/min(w, l))^2)*sum(p.1.norisk[max(1, l-min(w,l)):l]*(1-p.1.norisk[max(1, l-min(w,l)):l]))
#   }
#   UCL.matrix <- matrix(nrow = k, ncol = nobs)
#   LCL.matrix <- matrix(nrow = k, ncol = nobs)
#   events <- numeric(k)
#   for(j in 1:k){
#     UCL.norisk <- 0 + L_vec[j]*sqrt(variance.MP.norisk)
#     UCL.matrix[j, ] <- UCL.norisk
#     LCL.norisk <- 0 - L_vec[j]*sqrt(variance.MP.norisk)
#     LCL.matrix[j, ] <- LCL.norisk
#     events[j] <- ifelse(sum(MP.norisk>UCL.norisk)+sum(MP.norisk<LCL.norisk)>=1, 1,0)
#   }
#   return(events)
# }
# 
# A <- L.find(L_vec, n=5000, w=5000, yhat)
# k <- length(L_vec)
# iter <- 10000
# 
# A.matrix <- matrix(nrow = k, ncol = iter)
# for(i in 1:iter){
#   A <- L.find(L_vec, n=5000, w=5000, yhat)
#   A.matrix[,i] <- A
# }
# 
# s <- nrow(A.matrix)
# prob <- numeric(0)
# for(i in 1:s){
#   prob[i] <- mean(A.matrix[i, ])
# }
# prob
