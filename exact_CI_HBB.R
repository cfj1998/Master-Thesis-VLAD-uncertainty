rm(list=ls())
setwd("C:/Users/1998c/OneDrive/Skrivebord/Masteroppgave")
library(foreign)#To read SPSS file
library(zoo)
library(forecast)
#All the code from line 7-58 was provided to me by my supervisor
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
#calculations
total_number_births <- 31122
births_per_date <- 31122/13 #average number of births
dates <- c("Feb_11", "July_11", "Jan_12", "July_12", "Jan_13", "July_13", "Jan_14", 
           "July_14", "Jan_15", "July_15", "Jan_16", "July_16", "Jan_17")
dates_estdata <- c("Jan_10", "July_10","Jan_11")
pred_risk <- predict(estmod1kg, newdata = estdata1, type = "response")
mean(pred_risk)
logit_risk <- predict(estmod1kg, newdata = estdata1)
pred_risk_delta <- exp(-0.37+logit_risk)/(1+exp(-0.37+logit_risk))
mean(pred_risk_delta)

#estimate glm for monitoring period
runmod1kg <- glm(y~x1kg+x2+f2+f3+f9+p2+p3+p4+p5, family=binomial("logit"), data=rundata1)
summary(runmod1kg)
p1.risk <- predict(runmod1kg, newdata = rundata1, type = "response", se.fit = TRUE)
p1.risk.fit <- p1.risk[[1]]
p1.risk.se <- p1.risk[[2]]
p1.norisk <- mean(rundata1$y)

#creating vlad plot in non-risk situation
j <- seq(from = 1, to = 25841, by = 1) #25841 is the total number of births inrundata
yhat <- mean(estdata1$y)
yhat
V.nonrisk <- c(0*25841)
V.nonrisk[1] <- yhat - rundata1$y[1]
for(k in 2:25841){
  V.nonrisk[k]<-V.nonrisk[k-1]+(yhat-rundata1$y[k])
}
dim(V.nonrisk) <- c(25841, 1)

#produce plot for non-risk-adjusted situation
par(mfrow = c(1, 2), mar = c(5, 5, 0.5, 0.5))
plot(x=j, y = V.nonrisk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 260), type = "l", main = "Non-risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
#produce vlad plor for risk situation
V.risk<-c(0*25841)
p_0_risk<-predict(estmod1kg, newdata=rundata1 , type= "response")
V.risk<-cumsum(p_0_risk- rundata1$y)

plot(x=j, y = V.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 260), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)

#CI no-risk situation 
p <- mean(estdata1$y)
t <- length(rundata1$y)
n_0 <- length(estdata1$y)
V.norisk.var <- ((1:t)*((1:t)*p*(1-p)+n_0*(p*(1-p))))/(n_0)
V.norisk.se <- sqrt(V.norisk.var)
V.norisk.min <- V.nonrisk - 1.96*V.norisk.se
V.norisk.max <- V.nonrisk + 1.96*V.norisk.se
par(mfrow = c(1, 1))
plot(x=j, y = V.nonrisk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(-10, 300), type = "l", main = "Non-risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x = j, y = V.norisk.min, col = "red")
lines(x = j, y = V.norisk.max, col = "green")

#CI risk situation
#estimate var(average.p.risk)
p.risk <- predict(estmod1kg, newdata= rundata1, type = "response", se.fit = TRUE)
p.risk.average <- mean(p.risk$fit)
average.p.risk.var <- (p.risk.average*(1-p.risk.average))/n_0
# #method 1
# average.p.risk.f <- function(dataset, monotoring_set){
#   k <- length(dataset$y)
#   y <- sample(x = dataset$y, size = k, replace = TRUE)
#   x1kg <- sample(x = dataset$x1kg, size = k, replace = TRUE)
#   x2 <- sample(x = dataset$x2, size = k, replace = TRUE)
#   f2 <- sample(x = dataset$f2, size = k, replace = TRUE)
#   f3 <- sample(x = dataset$f3, size = k, replace = TRUE)
#   f9 <- sample(x = dataset$f9, size = k, replace = TRUE)
#   p2 <- sample(x = dataset$p2, size = k, replace = TRUE)
#   p3 <- sample(x = dataset$p3, size = k, replace = TRUE)
#   p4 <- sample(x = dataset$p4, size = k, replace = TRUE)
#   p5 <- sample(x = dataset$p5, size = k, replace = TRUE)
#   df <- data.frame(y, x1kg, x2, f2, f3, f9, p2, p3, p4, p5)
#   regmod <- glm(y~x1kg+x2+f2+f3+f9+p2+p3+p4+p5, family=binomial("logit"), data=df)
#   pred <- predict(regmod, newdata = monotoring_set, type = "response")
#   average_pred <- mean(pred)
#   return(average_pred)
# }
# average.p.risk.vec <- numeric(0)
# for(i in 1:10000){
#   average.p.risk.vec[i] <- average.p.risk.f(estdata1, rundata1)
# }
# average.p.risk.var <- var(average.p.risk.vec)
V.risk.var <- numeric(0)
for(i in 1:length(rundata1$y)){
  V.risk.var[i] <- (i*(i*p.risk.average*(1-p.risk.average) + n_0*p.risk.average*(1-p.risk.average)))/n_0
}
V.risk.se <- sqrt(V.risk.var)
V.risk.min <- V.risk - 1.96*V.risk.se
V.risk.max <- V.risk + 1.96*V.risk.se
par(mfrow = c(1, 1))
plot(x=j, y = V.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 420), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y = V.risk.min, col = "red")
lines(x=j, y = V.risk.max, col = "green")


#Standard error plot HBB
plot(x=j, y = V.norisk.se, xlab = "t", ylab = "Standard Error V_t", main = "Non risk-adjusted")
plot(x=j, y = V.risk.se, xlab = "t", ylab = "Standard Error V_t", main = "Risk-adjusted")

#error bound for VLAD
k <- 1.96
error_norisk <- k*V.norisk.se
error_risk <- k*V.risk.se
par(mfrow = c(1, 2))
plot(x=j, y = error_norisk, ylab = "error bound V_t", xlab = "t", main = "Non-Risk adjusted")
plot(x=j, y = error_risk, ylab = "error bound V_t", xlab = "t", main = "Risk-adjusted")

#Second method for risk
p_0_risk <- predict(estmod1kg, newdata= estdata1, type = "response", se.fit = TRUE)
p_0_risk.se <- p_0_risk$se.fit
p_0_risk.var <- (p_0_risk.se)^2
p_0_risk.var.average <- mean(p_0_risk.var)

V.risk.var2 <- numeric(0)
for(i in 1:length(rundata1$y)){
  V.risk.var2[i] <- (i*(i*p_0_risk.var.average + p.risk.average*(1-p.risk.average)))
}
V.risk.se2 <- sqrt(V.risk.var2)
V.risk.min2 <- V.risk - 1.96*V.risk.se2
V.risk.max2 <- V.risk + 1.96*V.risk.se2
par(mfrow = c(1,1))
plot(x=j, y = V.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(-700, 1000), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y = V.risk.min2, col = "red")
lines(x=j, y = V.risk.max2, col = "green")

#Third method risk, assuming independence
t <- seq(from = 1, to = length(rundata1$y), by = 1)
V.risk.var3 <- cumsum((p.risk$se.fit)^2) + t*p.risk.average*(1-p.risk.average)
V.risk.se3 <- sqrt(V.risk.var3)
V.risk.min3 <- V.risk - 1.96*V.risk.se3
V.risk.max3 <- V.risk + 1.96*V.risk.se3
par(mfrow = c(1,1))
plot(x=j, y = V.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 350), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y = V.risk.min3, col = "red")
lines(x=j, y = V.risk.max3, col = "green")
#Fourth method
V.risk.var4 <- cumsum((p.risk$se.fit)^2 + p.risk$fit*(1-p.risk$fit))
V.risk.se4 <- sqrt(V.risk.var4)
V.risk.min4 <- V.risk - 1.96*V.risk.se4
V.risk.max4 <- V.risk + 1.96*V.risk.se4
par(mfrow = c(1,1))
plot(x=j, y = V.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 300), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y = V.risk.min4, col = "red")
lines(x=j, y = V.risk.max4, col = "green")


#Adjusted CI norisk
V.norisk.min <- V.nonrisk - (0.2*V.norisk.se)
V.norisk.max <- V.nonrisk + (0.2*V.norisk.se)
par(mfrow = c(1,2))
plot(x=j, y = V.nonrisk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(-10, 300), type = "l", main = "Non-risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x = j, y = V.norisk.min, col = "red")
lines(x = j, y = V.norisk.max, col = "green")
