#Bootstrap VLAD CI
#library(boot)
# V_t = sum(E_t - O_t)

rm(list=ls())
set.seed(4)
setwd("C:/Users/1998c/OneDrive/Skrivebord/Masteroppgave")
#Read data and format correctly for glm
#Following code provided to me by Jan Terje Kvaloy
library(foreign)#To read SPSS file
library(zoo)
library(forecast)
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


yhat <- mean(estdata1$y)
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

j <- seq(from = 1, to = 25841, by = 1) #25841 is the total number of births inrundata
yhat <- mean(estdata1$y)
yhat
Vlad.nonrisk <- numeric(0)
Vlad.nonrisk[1] <- yhat - rundata1$y[1]
for(k in 2:25841){
  Vlad.nonrisk[k]<-Vlad.nonrisk[k-1]+(yhat-rundata1$y[k])
}
dim(Vlad.nonrisk) <- c(25841, 1)

p.0.risk<-predict(estmod1kg, newdata=rundata1 , type= "response")
Vlad.risk<-cumsum(p.0.risk - rundata1$y)

V.norisk <- function(data, index){
  estdata1 <- data[index, ]  # Baseline
  yhat <- mean(estdata1$y)
  V <- numeric(0)
  V[1] <- yhat - rundata1$y[1]
  for(i in 2:length(rundata1$y)){
    V[i] <- V[i-1]+(yhat - rundata1$y[i])
  }
  return(V)
}

V.risk <- function(data, index){
  estdata1 <- data[index, ]  # Baseline
  estmod1kg<-glm(y~x1kg+x2+f2+f3+f9+p2+p3+p4+p5, family=binomial("logit"), data=estdata1, 
                 subset = index)
  V <- numeric(0)
  pred <- predict(estmod1kg, newdata = rundata1, type = "response")
  V[1] <- pred[1] - rundata1$y[1]
  for(i in 2:length(rundata1$y)){
    V[i] <- V[i - 1] + (pred[i] - rundata1$y[i])
  }
  return(V)
}
library(boot)
bootstrap.V.norisk <- boot(estdata1, V.norisk, R = 2000)
bootstrap.V.risk <- boot(estdata1, V.risk, R = 2000)

ci.norisk.basic.low <- numeric(0)
ci.norisk.basic.up <- numeric(0)
for(i in 1:25841){
  basic.int <- boot.ci(bootstrap.V.norisk, type = c("basic"), index = i)
  ci.norisk.basic.low[i] <- basic.int$basic[4]
  ci.norisk.basic.up[i] <- basic.int$basic[5]
}
par(mfrow = c(1,2))
plot(x=j, y = Vlad.nonrisk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 350), type = "l", main = "Non-Risk Adjusted, Basic CI")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y = ci.norisk.basic.low, col = "red")
lines(x=j, y = ci.norisk.basic.up, col = "green")
ci.risk.basic.low <- numeric(0)
ci.risk.basic.up <- numeric(0)
for(i in 1:25841){
  basic.int <- boot.ci(bootstrap.V.risk, type = c("basic"), index = i)
  ci.risk.basic.low[i] <- basic.int$basic[4]
  ci.risk.basic.up[i] <- basic.int$basic[5]
}
plot(x=j, y = Vlad.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 550), type = "l", main = "Risk Adjusted, Basic CI")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y = ci.risk.basic.low, col = "red")
lines(x=j, y = ci.risk.basic.up, col = "green")
ci.norisk.norm.low <- numeric(0)
ci.norisk.norm.up <- numeric(0)
for(i in 1:25841){
  norm.int <- boot.ci(bootstrap.V.norisk, type = c("norm"), index = i)
  ci.norisk.norm.low[i] <- norm.int$normal[2]
  ci.norisk.norm.up[i] <- norm.int$normal[3]
}
plot(x=j, y = Vlad.nonrisk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 350), type = "l", main = "Non-Risk Adjusted, Normal CI")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y = ci.norisk.norm.low, col = "red")
lines(x=j, y = ci.norisk.norm.up, col = "green")

ci.risk.norm.low <- numeric(0)
ci.risk.norm.up <- numeric(0)
for(i in 1:25841){
  norm.int <- boot.ci(bootstrap.V.risk, type = c("norm"), index = i)
  ci.risk.norm.low[i] <- norm.int$normal[2]
  ci.risk.norm.up[i] <- norm.int$normal[3]
}
plot(x=j, y = Vlad.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 550), type = "l", main = "Risk Adjusted, Normal CI")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y = ci.risk.norm.low, col = "red")
lines(x=j, y = ci.risk.norm.up, col = "green")
ci.norisk.perc.low <- numeric(0)
ci.norisk.perc.up <- numeric(0)
for(i in 1:25841){
  perc.int <- boot.ci(bootstrap.V.norisk, type = c("perc"), index = i)
  ci.norisk.perc.low[i] <- perc.int$percent[4]
  ci.norisk.perc.up[i] <- perc.int$percent[5]
}
plot(x=j, y = Vlad.nonrisk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 350), type = "l", main = "Non-Risk Adjusted, Percentile CI")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y = ci.norisk.perc.low, col = "red")
lines(x=j, y = ci.norisk.perc.up, col = "green")

ci.risk.perc.low <- numeric(0)
ci.risk.perc.up <- numeric(0)
for(i in 1:25841){
  perc.int <- boot.ci(bootstrap.V.risk, type = c("perc"), index = i)
  ci.risk.perc.low[i] <- perc.int$percent[4]
  ci.risk.perc.up[i] <- perc.int$percent[5]
}
plot(x=j, y = Vlad.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 550), type = "l", main = "Risk Adjusted, Percentile CI")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y = ci.risk.perc.low, col = "red")
lines(x=j, y = ci.risk.perc.up, col = "green")


