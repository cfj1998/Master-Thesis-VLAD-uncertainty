rm(list=ls())
setwd("C:/Users/1998c/OneDrive/Skrivebord/Masteroppgave")
#Read data and format correctly for glm
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

#creating vlad plot
j <- seq(from = 1, to = 25841, by = 1) #25841 is the total number of births inrundata
yhat <- mean(estdata1$y)
yhat
V.nonrisk <- numeric(0)
V.nonrisk[1] <- yhat - rundata1$y[1]
for(k in 2:25841){
  V.nonrisk[k]<-V.nonrisk[k-1]+(yhat-rundata1$y[k])
}
dim(V.nonrisk) <- c(25841, 1)

#vlad-plot non-risk
par(mfrow = c(1, 2), mar = c(5, 5, 0.5, 0.5))
plot(x=j, y = V.nonrisk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 260), type = "l", main = "Non-risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
#Vlad-plot for risk-adjusted
V.risk<-c(0*25841)
p_0.risk<-predict(estmod1kg, newdata=rundata1 , type= "response")
V.risk<-cumsum(p_0.risk - rundata1$y)

plot(x=j, y = V.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 260), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)


#Moving proportion
w <- 5000
nobs <- dim(rundata1)[1]
p.risk <- predict(estmod1kg, newdata = rundata1, type="response")
p.base <- mean(p.risk)
p.norisk <- predict(estmod1non_risk, newdata = rundata1, type="response")
MP.risk <- numeric(0)
MP.norisk <- numeric(0)
variance_MP.norisk <- numeric(0)
variance_MP.risk <- numeric(0)
for(l in 1:nobs){
  MP.risk[l] <- (1/(min(w,l)))*sum(rundata1$y[max(1,l-min(w,l)):l]-p.risk[max(1,l-min(w,l)):l])
  MP.norisk[l] <- (1/(min(w,l)))* sum(rundata1$y[max(1,l-min(w,l)):l]-p.norisk[max(1,l-min(w,l)):l])
  variance_MP.norisk[l] <- (1/min(w,l))*yhat*(1-yhat)
  variance_MP.risk[l] <- (1/min(w,l))*p.base*(1-p.base)
}
UCL.risk <- 0 + 3.30*sqrt(variance_MP.risk)
LCL.risk <- 0 - 3.30*sqrt(variance_MP.risk)
UCL.norisk <- 0 + 4.09*sqrt(variance_MP.norisk)
LCL.norisk <- 0 - 4.09*sqrt(variance_MP.norisk)
par(mfrow = c(1,2))
plot(x=j, y=MP.risk, xlab = "t", xaxt = "n", type="l", main="Moving Proportion risk", ylim=c(-0.06, 0.06))
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
lines(x=j, y=UCL.risk, col = "green")
lines(x=j, y=LCL.risk, col="red")
legend("topleft", legend = c("UCL", "LCL", "w = 5000"), col = c("green", "red", "white"), lty = c(1, 1, 0))
plot(x=j, y=MP.norisk, xlab = "t", xaxt = "n", type="l", main="Moving Proportion non-risk", ylim=c(-0.05, 0.05))
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
lines(x=j, y=UCL.norisk, col = "green")
lines(x=j, y=LCL.norisk, col="red")
legend("topleft", legend = c("UCL", "LCL", "w = 5000"), col = c("green", "red", "white"), lty = c(1, 1, 0))

k.risk <- 80/(abs(UCL.risk) + abs(LCL.risk))
k.norisk <- 80/(abs(UCL.norisk) + abs(LCL.norisk))
vlad.risk.UCL <- V.risk - k.risk*(MP.risk - UCL.risk)
vlad.risk.LCL <- V.risk - k.risk*(MP.risk - LCL.norisk)
vlad.norisk.UCL <- V.nonrisk - k.norisk*(MP.norisk - UCL.norisk)
vlad.norisk.LCL <- V.nonrisk - k.norisk*(MP.norisk - LCL.norisk)
par(mfrow=c(1, 2))
plot(x=j, y = V.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(-100, 300), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
legend("topleft", legend = c("UCL", "LCL", "w = 5000"), col = c("green", "red", "white"), lty = c(1, 1, 0))
lines(x=j, y=vlad.risk.UCL, col="green")
lines(x=j, y=vlad.risk.LCL, col="red")
plot(x=j, y = V.nonrisk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(-100, 300), type = "l", main = "Non-risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y=vlad.norisk.UCL, col="green")
lines(x=j, y=vlad.norisk.LCL, col="red")
legend("topleft", legend = c("UCL", "LCL", "w = 5000"), col = c("green", "red", "white"), lty = c(1, 1, 0))


# improved moving proportion
w <- 5000 
n_0 <- length(estdata1$y)
y_1hat <- mean(rundata1$y)
nobs <- dim(rundata1)[1]
p.risk <- predict(estmod1kg, newdata = rundata1, type="response")
p.base <- mean(p.risk)
p.norisk <- predict(estmod1non_risk, newdata = rundata1, type="response")
MP.risk <- numeric(0)
MP.norisk <- numeric(0)
variance_MP.norisk <- numeric(0)
variance_MP.risk <- numeric(0)
for(l in 1:nobs){
  MP.risk[l] <- (1/(min(w,l)))*sum(rundata1$y[max(1,l-min(w,l)):l]-p.risk[max(1,l-min(w,l)):l])
  MP.norisk[l] <- (1/(min(w,l)))* sum(rundata1$y[max(1,l-min(w,l)):l]-p.norisk[max(1,l-min(w,l)):l])
  variance_MP.norisk[l] <- (n_0*yhat*(1-yhat)+min(w,l)*yhat*(1-yhat))/(n_0*(min(w, l)))
  variance_MP.risk[l] <- ((p.base*(1-p.base))/(n_0*min(w,l)))*(n_0+min(w,l))
}
UCL.risk <- 0 + 3.27*sqrt(variance_MP.risk)
LCL.risk <- 0 - 3.27*sqrt(variance_MP.risk)
UCL.norisk <- 0 + 4.09*sqrt(variance_MP.norisk)
LCL.norisk <- 0 - 4.09*sqrt(variance_MP.norisk)
par(mfrow = c(1,2))
plot(x=j, y=MP.risk, xlab = "t", xaxt = "n", type="l", main="Moving Proportion risk", ylim=c(-0.06, 0.06))
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
lines(x=j, y=UCL.risk, col = "green")
lines(x=j, y=LCL.risk, col="red")
legend("topleft", legend = c("UCL", "LCL", "w = 5000"), col = c("green", "red", "white"), lty = c(1, 1, 0))
plot(x=j, y=MP.norisk, xlab = "t", xaxt = "n", type="l", main="Moving Proportion non-risk", ylim=c(-0.05, 0.05))
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
lines(x=j, y=UCL.norisk, col = "green")
lines(x=j, y=LCL.norisk, col="red")
legend("topleft", legend = c("UCL", "LCL", "w = 5000"), col = c("green", "red", "white"), lty = c(1, 1, 0))

k.risk <- 80/(abs(UCL.risk) + abs(LCL.risk))
k.norisk <- 80/(abs(UCL.norisk) + abs(LCL.norisk))
vlad.risk.UCL <- V.risk - k.risk*(MP.risk - UCL.risk)
vlad.risk.LCL <- V.risk - k.risk*(MP.risk - LCL.norisk)
vlad.norisk.UCL <- V.nonrisk - k.norisk*(MP.norisk - UCL.norisk)
vlad.norisk.LCL <- V.nonrisk - k.norisk*(MP.norisk - LCL.norisk)
par(mfrow=c(1, 2))
plot(x=j, y = V.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(-100, 300), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
legend("topleft", legend = c("UCL", "LCL", "w = 5000"), col = c("green", "red", "white"), lty = c(1, 1, 0))
lines(x=j, y=vlad.risk.UCL, col="green")
lines(x=j, y=vlad.risk.LCL, col="red")
plot(x=j, y = V.nonrisk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(-100, 300), type = "l", main = "Non-risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y=vlad.norisk.UCL, col="green")
lines(x=j, y=vlad.norisk.LCL, col="red")
legend("topleft", legend = c("UCL", "LCL", "w = 5000"), col = c("green", "red", "white"), lty = c(1, 1, 0))

#Improved moving proportion risk version 2
p.risk.base <- predict(estmod1kg, newdata = estdata1, type = "response", se.fit = TRUE)
p.risk.base.se <- p.risk.base$se.fit
p.risk.base.var <- (p.risk.base.se)^2
p.risk.base.var.average <- mean(p.risk.base.var)
variance_MP.risk <- numeric(0)
for(l in 1:nobs){
  variance_MP.risk[l] <- (1/min(w, l))*(p.base*(1-p.base)+min(w, l)*p.risk.base.var.average)
}
UCL.risk <- 0 + 0.97*sqrt(variance_MP.risk)
LCL.risk <- 0 - 0.97*sqrt(variance_MP.risk)
par(mfrow = c(1, 1))
plot(x=j, y=MP.risk, xlab = "t", xaxt = "n", type="l", main="Moving Proportion risk", ylim=c(-0.06, 0.06))
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
lines(x=j, y=UCL.risk, col = "green")
lines(x=j, y=LCL.risk, col="red")
legend("topleft", legend = c("UCL", "LCL", "w = 5000"), col = c("green", "red", "white"), lty = c(1, 1, 0))
k.risk <- 80/(abs(UCL.risk) + abs(LCL.risk))
vlad.risk.UCL <- V.risk - k.risk*(MP.risk - UCL.risk)
vlad.risk.LCL <- V.risk - k.risk*(MP.risk - LCL.norisk)
plot(x=j, y = V.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(-100, 350), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
legend("topleft", legend = c("UCL", "LCL", "w = 5000"), col = c("green", "red", "white"), lty = c(1, 1, 0))
lines(x=j, y=vlad.risk.UCL, col="green")
lines(x=j, y=vlad.risk.LCL, col="red")
