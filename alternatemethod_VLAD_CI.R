rm(list=ls())
setwd("C:/Users/1998c/OneDrive/Skrivebord/Masteroppgave")
#Read data and format correctly for glm
#All the code from line 5-60 was provided to me by Jan Terje Kvaloy
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
#creating vlad plot in non-risk situation
j <- seq(from = 1, to = 25841, by = 1) #25841 is the total number of births inrundata
yhat <- mean(estdata1$y)
yhat
V.nonrisk <- numeric(0)
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
p_0.risk<-predict(estmod1kg, newdata=rundata1 , type= "response")
V.risk<-cumsum(p_0.risk - rundata1$y)

plot(x=j, y = V.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 260), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)

#interval for norisk
p_est <- mean(estdata1$y)
n <- length(estdata1$y)
p_low <- p_est - 1.96*sqrt((p_est*(1-p_est))/(n))
p_high <- p_est + 1.96*sqrt((p_est*(1-p_est))/(n))
V.nonrisk.min <- numeric(0)
V.nonrisk.min[1] <- p_low - rundata1$y[1]
for(i in 2:25841){
  V.nonrisk.min[i] <- V.nonrisk.min[i-1] + (p_low - rundata1$y[i])
}
V.nonrisk.max <- numeric(0)
V.nonrisk.max[1] <- p_high - rundata1$y[1]
for(i in 2:25841){
  V.nonrisk.max[i] <- V.nonrisk.max[i-1] + (p_high - rundata1$y[i])
}
plot(x=j, y = V.nonrisk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 300), type = "l", main = "Non-risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(V.nonrisk.min, col = "red")
lines(V.nonrisk.max, col = "green")
legend("topleft", legend = c("V_t", "LCI", "UCI"), col = c("black", "red", "green"), lty = c(1,1,1))

#Interval for risk
# p_est.risk <- predict(estmod1kg, newdata = rundata1, se.fit = TRUE, 
#                       type = "response", interval = "confidence", level = 0.95) #does not give CI, only gives se
# p_est.fit <- p_est.risk[[1]]
# p_est.se <- p_est.risk[[2]]
# p_est.low <- p_est.fit-1.96*p_est.se
# p_est.high <- p_est.fit+1.96*p_est.se

inverse_logit <- function(x)
  1/(1+exp(-x))
fitbase <- predict(estmod1kg, newdata = rundata1, se.fit = TRUE)
p_est.fit <- inverse_logit(fitbase$fit)
p_est.low <- inverse_logit(fitbase$fit - 1.96*fitbase$se.fit)
p_est.high <- inverse_logit(fitbase$fit + 1.96*fitbase$se.fit)

V.risk.min <- cumsum(p_est.low-rundata1$y)
V.risk.max <- cumsum(p_est.high-rundata1$y)

# V.risk.min <- numeric(0)
# V.risk.min[1] <- p_est.low[1] - rundata1$y[1]
# for(i in 2:25841){
#   V.risk.min[i] <- V.risk.min[i-1] + (p_est.low[i]-rundata1$y[i])
# }
# V.risk.max <- numeric(0)
# V.risk.max[1] <- p_est.high[1] - rundata1$y[1]
# for(i in 2:25841){
#   V.risk.max[i] <- V.risk.max[i-1] + (p_est.high[i]-rundata1$y[i])
# }
plot(x=j, y = V.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(-150, 800), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(V.risk.min, col = "red")
lines(V.risk.max, col = "green")
legend("topleft", legend = c("V_t", "LCI", "UCI"), col = c("black", "red", "green"), lty = c(1,1,1))

#Error bounds
# cusum_Y <- cumsum(rundata1$y)
# error_norisk <- numeric(0)
# for(i in 1:length(rundata1$y)){
#   error_norisk[i] <- sqrt(((-1.96^2)/(4*4893))*(((-2*(V.nonrisk[i]+cusum_Y[i])+i)/i)^2)-1)
# }
# plot(x=j, y = error_norisk)
