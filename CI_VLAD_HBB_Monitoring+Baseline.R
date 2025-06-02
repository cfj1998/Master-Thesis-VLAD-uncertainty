#Using Baseline and Monitoring data to estimate variance and standard error of VLAd
rm(list=ls())
setwd("C:/Users/1998c/OneDrive/Skrivebord/Masteroppgave")
#Read data and format correctly for glm
library(foreign)#To read SPSS file
library(zoo)
library(forecast)
#The following code was provided to me by my supervisor (line 9-59)
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

#Baseline models
estmod1non_risk <- glm(y~1, family = binomial("logit"), data = estdata1)
estmod1non_risk_fit <- summary(estmod1non_risk)
estmod1non_risk_fit
estmod1kg<-glm(y~x1kg+x2+f2+f3+f9+p2+p3+p4+p5, family=binomial("logit"), data=estdata1)
estmod1kg_fit <- summary(estmod1kg)
estmod1kg_fit
#end of provided code
p.0.norisk <- mean(estdata1$y)
pred.risk <- predict(estmod1kg, newdata = rundata1, type = "response", se.fit = TRUE)
p.0.risk <- pred.risk[[1]]
p.0.risk.var <- pred.risk[[2]]^2
p.0.average.risk <- mean(p.0.risk)
t.0 <- length(estdata1$y)
p.0.var.norisk <- numeric(0)
for(i in 1:length(rundata1$y)){
  p.0.var.norisk[i] <- (i^2*p.0.norisk*(1-p.0.norisk))/t.0
}
p.0.var.risk <- cumsum(p.0.risk.var)
#Monitoring models
rundata1kg <- glm(y~x1kg+x2+f2+f3+f9+p2+p3+p4+p5, family=binomial("logit"), data=rundata1)
summary(rundata1kg)
rundata1non_risk <- glm(y~1, family = binomial("logit"), data = rundata1)
summary(rundata1non_risk)
cusum.y_1 <- cumsum(rundata1$y)
p.1.norisk <- numeric(0)
for(i in 1:length(rundata1$y)){
  p.1.norisk[i] <- cusum.y_1[i]/i
}
plot(p.1.norisk)
p.1.risk <- predict(rundata1kg, newdata = rundata1, type = "response")
y.1.var.norisk <- cumsum(p.1.norisk*(1-p.1.norisk))
y.1.var.risk <- cumsum(p.1.risk*(1-p.1.risk))
p.1.average.risk <- numeric(0)
p.1.average.risk[1] <- p.1.risk[1]
for(i in 2:length(rundata1$y)){
  p.1.average.risk[i] <- (p.1.risk[i-1]+p.1.risk[i])/i
}

#Vlad
t <- seq(from = 1, to = 25841, by = 1) #25841 is the total number of births in rundata
par(mfrow = c(1, 1))
plot(t, y = p.1.norisk)
abline(h = p.0.norisk, col = "blue")
plot(t, y = p.1.risk)
lines(t, y=p.0.risk)
V.nonrisk <- numeric(0)
V.nonrisk[1] <- p.0.norisk - rundata1$y[1]
for(k in 2:25841){
  V.nonrisk[k]<-V.nonrisk[k-1]+(p.0.norisk-rundata1$y[k])
}
dim(V.nonrisk) <- c(25841, 1)
V.risk<-c(0*25841)
V.risk[1]<-p.0.risk[1] - rundata1$y[1]
for(l in 2:25841){
  V.risk[l]<-V.risk[l-1]+(p.0.risk[l]-rundata1$y[l])
}
dim(V.risk)<-c(25841,1)
V.var.norisk <- numeric(0)
for(i in 1:length(rundata1$y)){
  V.var.norisk[i] <- p.0.var.norisk[i] + y.1.var.norisk[i]
}
#for comparison
var(V.nonrisk)

V.var.risk <- cumsum(p.0.var.risk + y.1.var.risk)

var(V.risk)

V.nonrisk.min <- V.nonrisk - 1.96*sqrt(V.var.norisk)
V.nonrisk.max <- V.nonrisk + 1.96*sqrt(V.var.norisk)
V.risk.min <- V.risk - 1.96*sqrt(V.var.risk)
V.risk.max <- V.risk + 1.96*sqrt(V.var.risk)
par(mfrow = c(1, 2))
plot(x = t, y = V.nonrisk, xlab = "t", ylab = "V_t", xaxt = "n", type = "l", ylim = c(-100, 400), main = "Not-risk adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=t, y = V.nonrisk.min, col = "red")
lines(x=t, y = V.nonrisk.max, col = "green")
plot(x = t, y = V.risk, xlab = "t", ylab = "V_t", xaxt = "n", type = "l", ylim = c(-100, 400), main = "Risk-adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=t, y = V.risk.min, col = "red")
lines(x=t, y = V.risk.max, col = "green")

#Plots of two variances of VLAD
V.nonrisk.se1 <- sqrt(cumsum(p.1.norisk*(1-p.1.norisk)))
V.nonrisk.se2 <- sqrt(t*(p.0.norisk*(1-p.0.norisk)))
plot(x = t, y = V.nonrisk.se1, xlab = "t", ylab = "V_t", type = "l", main = "Non-Risk-adjusted", ylim = c(0, 30))
lines(x = t, y = V.nonrisk.se2, col = "orange")
legend("bottomright", legend = c("SE(V_t) using p_1", "SE(V_t) using p_0"), col = c("black", "orange"), lty = c(1,1,1))
V.risk.se1 <- sqrt(cumsum(p.1.risk*(1-p.1.risk)))
V.risk.se2 <- sqrt(cumsum(p.0.risk*(1-p.0.risk)))
plot(x = t, y = V.risk.se1, xlab = "t", ylab = "V_t", type = "l", main = "Risk-adjusted", ylim = c(0, 30))
lines(x = t, y = V.risk.se2, col = "orange")
legend("bottomright", legend = c("SE(V_t) using p_1", "SE(V_t) using p_0"), col = c("black", "orange"), lty = c(1,1,1))

#Expectation no-risk
V.nonrisk.mean <- cumsum(p.0.norisk-p.1.norisk)
V.nonrisk.var <- cumsum(p.1.norisk*(1-p.1.norisk))
V.nonrisk.min <- numeric(0)
for(i in 1:length(rundata1$y)){
  V.nonrisk.min[i] <- V.nonrisk[i]-1.96*sqrt(V.nonrisk.var[i])
}
V.nonrisk.max <- numeric(0)
for(i in 1:length(rundata1$y)){
  V.nonrisk.max[i] <- V.nonrisk[i]+1.96*sqrt(V.nonrisk.var[i])
}
plot(x = t, y = V.nonrisk, xlab = "t", ylab = "V_t", xaxt = "n", type = "l", ylim = c(0, 300), main = "Not Risk-Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=t, y = V.nonrisk.min, col = "red")
lines(x=t, y = V.nonrisk.max, col = "green")

#Expectation risk
V.risk.mean <- cumsum(p.0.risk-p.1.risk)
#plot(V.risk.mean)
V.risk.var <- cumsum(p.1.risk*(1-p.1.risk))
V.risk.min <- numeric(0)
for(i in 1:length(rundata1$y)){
  V.risk.min[i] <- V.risk[i]-1.96*sqrt(V.risk.var[i])
}
V.risk.max <- numeric(0)
for(i in 1:length(rundata1$y)){
  V.risk.max[i] <- V.risk[i]+1.96*sqrt(V.risk.var[i])
}
plot(x = t, y = V.risk, xlab = "t", ylab = "V_t", xaxt = "n", type = "l", ylim = c(0, 300), main = "Risk-adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=t, y = V.risk.min, col = "red")
lines(x=t, y = V.risk.max, col = "green")

#Improved Variance and CI
t_0 <- length(estdata1$y)
V.nonrisk.mean <- cumsum(p.0.norisk-p.1.norisk)
V.nonrisk.var <- ((t^2)/(t_0))*p.0.norisk*(1-p.0.norisk)+cumsum(p.0.norisk*(1-p.0.norisk))
V.nonrisk.min <- numeric(0)
for(i in 1:length(rundata1$y)){
  V.nonrisk.min[i] <- V.nonrisk[i]-1.96*sqrt(V.nonrisk.var[i])
}
V.nonrisk.max <- numeric(0)
for(i in 1:length(rundata1$y)){
  V.nonrisk.max[i] <- V.nonrisk[i]+1.96*sqrt(V.nonrisk.var[i])
}
plot(x = t, y = V.nonrisk, xlab = "t", ylab = "V_t", xaxt = "n", type = "l", ylim = c(0, 300), main = "Not Risk-Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=t, y = V.nonrisk.min, col = "red")
lines(x=t, y = V.nonrisk.max, col = "green")

V.risk.mean <- cumsum(p.0.risk-p.1.risk)
#plot(V.risk.mean)
V.risk.var <- ((t^2)/(t_0))*p.0.average.risk*(1-p.0.average.risk)+cumsum(p.0.average.risk*(1-p.0.average.risk))
V.risk.min <- numeric(0)
for(i in 1:length(rundata1$y)){
  V.risk.min[i] <- V.risk[i]-1.96*sqrt(V.risk.var[i])
}
V.risk.max <- numeric(0)
for(i in 1:length(rundata1$y)){
  V.risk.max[i] <- V.risk[i]+1.96*sqrt(V.risk.var[i])
}
plot(x = t, y = V.risk, xlab = "t", ylab = "V_t", xaxt = "n", type = "l", ylim = c(0, 300), main = "Risk-adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=t, y = V.risk.min, col = "red")
lines(x=t, y = V.risk.max, col = "green")


#Average VLAD
V.average.nonrisk <- (1/t)*V.nonrisk
V.average.se.nonrisk <- sqrt((1/(t^2))*V.nonrisk.var)
V.average.nonrisk.min <- V.average.nonrisk-1.96*V.average.se.nonrisk
V.average.nonrisk.max <- V.average.nonrisk+1.96*V.average.se.nonrisk
V.average.risk <- (1/t)*V.risk
V.average.se.risk <- sqrt((1/(t^2))*V.risk.var)
V.average.risk.min <- V.average.risk-1.96*V.average.se.risk
V.average.risk.max <- V.average.nonrisk+1.96*V.average.se.risk
plot(x = t, y = V.average.nonrisk, xlab = "t", ylab = "V_t", xaxt = "n", type = "l", ylim = c(-0.05, 0.05), main = "Not Risk-Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x = t, y = V.average.nonrisk.min, col = "red")
lines(x = t, y = V.average.nonrisk.max, col = "green")
plot(x = t, y = V.average.risk, xlab = "t", ylab = "V_t", xaxt = "n", type = "l", ylim = c(-0.05, 0.05), main = "Risk-adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x = t, y = V.average.risk.min, col = "red")
lines(x = t, y = V.average.risk.max, col = "green")

#Optimal MP
w <- 5000
nobs <- dim(rundata1)[1]
MP.risk <- numeric(0)
MP.norisk <- numeric(0)
variance.MP.norisk <- numeric(0)
variance.MP.risk <- numeric(0)
for(l in 1:nobs){
  MP.risk[l] <- (1/(min(w,l)))*sum(p.0.risk[max(1,l-min(w,l)):l]-rundata1$y[max(1,l-min(w,l)):l])
  MP.norisk[l] <- (1/(min(w,l)))* sum(p.0.norisk-rundata1$y[max(1,l-min(w,l)):l])
  variance.MP.norisk[l] <- ((1/min(w, l))^2)*sum(p.1.norisk[max(1, l-min(w,l)):l]*(1-p.1.norisk[max(1, l-min(w,l)):l]))
  variance.MP.risk[l] <- ((1/min(w, l))^2)*sum(p.1.risk[max(1, l-min(w,l)):l]*(1-p.1.risk[max(1, l-min(w,l)):l]))
}
L.norisk <- 3
L.risk <- 3
h1.norisk <- 0 - L.norisk*sqrt(variance.MP.norisk)
h2.norisk <- 0 + L.norisk*sqrt(variance.MP.norisk)
h1.risk <- 0 - L.risk*sqrt(variance.MP.risk)
h2.risk <- 0 + L.risk*sqrt(variance.MP.risk)
par(mfrow = c(1,2))
plot(x=t, y = MP.norisk, xlab = "t", xaxt = "n", ylab = "MP_t", ylim = c(-0.05, 0.05), main = "Non-risk adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x = t, y = h1.norisk, col = "red")
lines(x = t, y = h2.norisk, col = "green")
plot(x=t, y = MP.risk, xlab = "t", xaxt = "n", ylab = "MP_t", ylim = c(-0.05, 0.05), main = "Risk adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x = t, y = h1.risk, col = "red")
lines(x = t, y = h2.risk, col = "green")
k.risk <- 80/(abs(h1.risk) + abs(h2.risk))
k.norisk <- 80/(abs(h1.norisk) + abs(h2.norisk))
vlad.risk.UCL <- V.risk - k.risk*(MP.risk - h2.risk)
vlad.risk.LCL <- V.risk - k.risk*(MP.risk - h1.risk)
vlad.norisk.UCL <- V.nonrisk - k.norisk*(MP.norisk - h2.norisk)
vlad.norisk.LCL <- V.nonrisk - k.norisk*(MP.norisk - h1.norisk)
par(mfrow=c(1, 2))
plot(x=t, y = V.risk, ylab = "Cumalitive_Number_of_Excess_Survivors", xlab = "t", xaxt = "n",
     ylim = c(-100, 260), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
legend("topleft", legend = c("UCL", "LCL", "w = 5000"), col = c("green", "red", "white"), lty = c(1, 1, 0))
lines(x=t, y=vlad.risk.UCL, col="green")
lines(x=t, y=vlad.risk.LCL, col="red")
plot(x=t, y = V.nonrisk, ylab = "Cumalitive_Number_of_Excess_Survivors", xlab = "t", xaxt = "n",
     ylim = c(-100, 260), type = "l", main = "Non-risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=t, y=vlad.norisk.UCL, col="green")
lines(x=t, y=vlad.norisk.LCL, col="red")
legend("topleft", legend = c("UCL", "LCL", "w = 5000"), col = c("green", "red", "white"), lty = c(1, 1, 0))

