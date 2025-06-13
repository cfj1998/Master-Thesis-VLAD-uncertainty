rm(list=ls())
setwd("C:/Users/1998c/OneDrive/Skrivebord/Masteroppgave")

library(foreign)#To read SPSS file #This line of code was provided to me by supervisor
library(zoo)
library(forecast)
#All the code from line 8-61 was provided to me by my supervisor to set up the model
#Load the dataset
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


# Model used#. 
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


#chart making
library(spcadjust)
set.seed(1)
#Delta-values -0.37 and -0.21 were provided to me by my supervisor
chartlogreg_baseline <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = -0.37, 
                                                                       formula = 
                                                                         y~x1kg + x2 + f2 +
                                                                         f3 + f9 + p2 +
                                                                         p3 + p4 + p5))
chartlogreg_baseline2 <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = 0.37, 
                                                                       formula = 
                                                                         y~x1kg + x2 + f2 +
                                                                         f3 + f9 + p2 +
                                                                         p3 + p4 + p5))
chartlogreg_nonrisk <- new("SPCCUSUM", model = SPCModellogregLikRatio(Delta = -0.21, formula = y~1
))
chartlogreg_nonrisk2 <- new("SPCCUSUM", model = SPCModellogregLikRatio(
  Delta = 0.21, formula = y~1
))
xihat <- xiofdata(chartlogreg_baseline, estdata1)
xihat
xihat2 <- xiofdata(chartlogreg_baseline2, estdata1)
xihat_nonrisk <- xiofdata(chartlogreg_nonrisk, estdata1)
xihat_nonrisk
xihat_nonrisk2 <- xiofdata(chartlogreg_nonrisk2, estdata1)


#calibrate chart to get calhitprob
calibrate <- SPCproperty(data = estdata1, nrep = 1, chart = chartlogreg_baseline, 
                         reportdistr = FALSE,
                         property = "calhitprob", params = list(target = 0.05, nsteps = 5000, gridpoints = 1000))
calibrate
calibrate@raw
calibrate@res
calibrate2 <- SPCproperty(data = estdata1, nrep = 1, chart = chartlogreg_baseline2, 
                         reportdistr = FALSE,
                         property = "calhitprob", params = list(target = 0.05, nsteps = 5000, gridpoints = 1000))
calibrate_nonrisk <- SPCproperty(data = estdata1, nrep = 1, chart = chartlogreg_nonrisk,
                                 reportdistr = FALSE,
                                 property = "calhitprob", params = list(target = 0.05, nsteps = 5000, gridpoints = 1000))

#calibrate_nonrisk
calibrate_nonrisk2 <- SPCproperty(data = estdata1, nrep = 1, chart = chartlogreg_nonrisk2,
                                 reportdistr = FALSE,
                                 property = "calhitprob", params = list(target = 0.05, nsteps = 5000, gridpoints = 1000))
#run the chart on the monitoring data that is in control
par(mfrow = c(1, 2))
S <- runchart(chartlogreg_baseline, newdata = rundata1, xi = xihat) #CUSUM with negative delta
S2 <- runchart(chartlogreg_baseline2, newdata = rundata1, xi = xihat2) #CUSUM with positive delta
plot(S, xlab = "", ylim = c(0, 70), xlim = c(0, 25841), xaxt = "n", main = "Risk-adjusted")
abline(h = calibrate@raw, col = "blue")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
legend("topright", legend = c("4.383"), col = c("blue"), lty = c(1))
S_nonrisk <- runchart(chartlogreg_nonrisk, newdata = rundata1, xi = xihat_nonrisk #CUSUM nonrisk negative delta
 )
S_nonrisk2 <- runchart(chartlogreg_nonrisk2, newdata = rundata1, xi = xihat_nonrisk2) #CUSUM risk positive delta
plot(S_nonrisk, xlab = "", ylim = c(0, 30), xlim = c(0, 25841), xaxt = "n", main = "Non-risk")
abline(h = calibrate_nonrisk@raw, col = "blue")
legend("topright", legend = c("3.476"), col = c("blue"), lty = c(1))
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)

#Plots with positive delta
par(mfrow = c(1, 2))
plot(S2, xlab = "", ylim = c(0, 5), xlim = c(0, 25841), xaxt = "n", main = "Risk-adjusted")
abline(h = calibrate2@raw, col = "blue")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
legend("topright", legend = c("4.139"), col = c("blue"), lty = c(1))

plot(S_nonrisk2, xlab = "", ylim = c(0, 5), xlim = c(0, 25841), xaxt = "n", main = "Non-risk")
abline(h = calibrate_nonrisk2@raw, col = "blue")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
legend("topright", legend = c("3.307"), col = c("blue"), lty = c(1))
 
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
V.risk<- cumsum(p_0.risk - rundata1$y)

plot(x=j, y = V.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 260), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)

#Control limits for Vlad based of EWMA #analytical, not used in thesis, just for testing purposes
lambda.norisk = 0.0005
lambda.risk = 0.0005
prob_p_risk <- predict(estmod1kg, newdata = rundata1, type = "response")
p_risk <- prob_p_risk
prob_p_norisk <- predict(estmod1non_risk, newdata = rundata1, type = "response")
p_norisk <- prob_p_norisk
O_n <- rundata1$y
Z.risk <- c(0*25841)
Z.risk[1] <- lambda.risk*(rundata1$y[1] - p_risk[1])+(1-lambda.risk)*0
for(l in 2:25841){
  Z.risk[l] <- lambda.risk*(rundata1$y[l] - p_risk[l])+(1-lambda.risk)*Z.risk[l-1]
}
Z.norisk <- c(0*25841)
Z.norisk[1] <- lambda.norisk*(rundata1$y[1] - p_norisk[1])+(1-lambda.norisk)*0
for(l in 2:25841){
  Z.norisk[l] <- lambda.norisk*(rundata1$y[l] - p_norisk[l])+(1-lambda.norisk)*Z.norisk[l-1]
}
L.risk <- 3
L.norisk <- 3
t <- seq(from = 1, to = 25841)
sdvlad.risk <- sd(rundata1$y - p_risk) 
sdvlad.norisk <- sd(rundata1$y - p_norisk)
LCL.risk <- 0 - L.risk*sdvlad.risk*sqrt((lambda.risk/(2-lambda.risk))*(1-(1-lambda.risk)^(2*t)))
UCL.risk <- 0 + L.risk*sdvlad.risk*sqrt((lambda.risk/(2-lambda.risk))*(1-(1-lambda.risk)^(2*t)))
LCL.norisk <- 0 - L.norisk*sdvlad.norisk*sqrt((lambda.norisk/(2-lambda.norisk))*(1-(1-lambda.norisk)^(2*t)))
UCL.norisk <- 0 + L.norisk*sdvlad.norisk*sqrt((lambda.norisk/(2-lambda.norisk))*(1-(1-lambda.norisk)^(2*t)))
par(mfrow = c(1, 2))
plot(x=j, y = Z.risk, type="b", ylim=c(-0.01, 0.01), xlab = "t", xaxt = "n")
lines(x=j, y = UCL.risk, col = "green")
lines(x=j, y = LCL.risk, col = "red")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
legend("topleft", legend = c("lambda = 0.0005", "L = 3", "UCL", "LCL"), col = c("white", "white", "green", "red"),
       lty = c(0, 0, 1, 1))
plot(x=j, y = Z.norisk, type="b", ylim=c(-0.01, 0.01), xaxt = "n", xlab = "t")
lines(x=j, y = UCL.norisk, col = "green")
lines(x=j, y = LCL.norisk, col = "red")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
legend("topleft", legend = c("lambda = 0.0005", "L = 3", "UCL", "LCL"), col = c("white", "white", "green", "red"),
        lty = c(0, 0, 1, 1))
k.risk <- 20/(abs(LCL.risk)+abs(UCL.risk))
k.norisk <- 20/(abs(LCL.norisk)+abs(UCL.norisk))
LCL.Vlad.risk <- V.risk + (k.risk)*(Z.risk-UCL.risk)
UCL.Vlad.risk <- V.risk + (k.risk)*(Z.risk-LCL.risk)
LCL.Vlad.norisk <- V.nonrisk + (k.norisk)*(Z.norisk-UCL.norisk)
UCL.Vlad.norisk <- V.nonrisk + (k.norisk)*(Z.norisk-LCL.norisk)
par(mfrow = c(1, 2), mar = c(5, 5, 0.5, 0.5))
plot(x=j, y = V.nonrisk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 260), type = "l", main = "Non-risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y = LCL.Vlad.norisk, col = "red")
lines(x=j, y = UCL.Vlad.norisk, col = "green")
legend("topleft", legend = c("UCL", "LCL", "V_t"), col = c("green", "red", "black"), lty = c(1, 1, 1))
plot(x=j, y = V.risk, ylab = "Cumalitive_Number_of_Excess_Survivors", xlab = "t", xaxt = "n",
     ylim = c(0, 260), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y = LCL.Vlad.risk, col = "red")
lines(x=j, y = UCL.Vlad.risk, col = "green")
legend("topleft", legend = c("UCL", "LCL", "V_t"), col = c("green", "red", "black"), lty = c(1, 1, 1))
#Control limits for Vlad
vlad_risk <- V.risk
vlad_norisk <- V.nonrisk
h.risk.upper <- calibrate@raw
h.risk.lower <- calibrate2@raw
h.nonrisk.upper <- calibrate_nonrisk@raw
h.nonrisk.lower <- calibrate_nonrisk2@raw
p_risk <- predict(estmod1kg, newdata = rundata1, type = "response")
p_norisk <- predict(estmod1non_risk, newdata = rundata1, type = "response")
O_n <- rundata1$y
E_n.risk <- exp(p_risk)/(1+exp(p_risk))
E_n.norisk <- exp(p_norisk)/(1+exp(p_norisk))

U_n.risk <- vlad_risk+(S-h.risk.upper)/(-(0.37))
U_n.norisk <- vlad_norisk+(S_nonrisk-h.nonrisk.upper)/(-(0.21))
L_n.risk <- vlad_risk+(S2-h.risk.lower)/(0.37)
L_n.norisk <- vlad_norisk+(S_nonrisk2-h.nonrisk.lower)/(0.21)

Centre_line_risk <- mean(vlad_risk)
Centre_line_norisk <- mean(vlad_norisk)

# Vlad Plots with control limits by Andrino
par(mfrow = c(1, 2), mar = c(5, 5, 0.5, 0.5))
plot(x=j, y = V.nonrisk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 260), type = "l", main = "Non-risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y=U_n.norisk, col = "green")
lines(x=j, y=L_n.norisk, col = "red")
legend("bottomright", legend = c("UCL", "LCL", "V_t"), col=c("green", "red", "black"), lty=c(1, 1, 1))

plot(x=j, y = V.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 260), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y=U_n.risk, col = "green")
lines(x=j, y=L_n.risk, col = "red")
legend("bottomright", legend = c("UCL", "LCL", "V_t"), col=c("green", "red", "black"), lty=c(1, 1, 1))


#Control limits for VLAD based on Moving Average Control Chart
w = 500
M_vlad_risk <- TTR::SMA(-rundata1$y + p_risk, n=w)
M_vlad_norisk <- TTR::SMA(-rundata1$y + p_norisk, n=w)
h_1.risk <- 0 + 3*sd(-rundata1$y + p_risk)/sqrt(w)
h_2.risk <- 0 - 3*sd(-rundata1$y + p_risk)/sqrt(w)
h_1.norisk <- 0 + 3*sd(-rundata1$y + p_norisk)/sqrt(w)
h_2.norisk <- 0  - 3*sd(-rundata1$y + p_norisk)/sqrt(w)
par(mfrow=c(1,2))
plot(x=j, y=M_vlad_risk, type="l", xaxt="n", xlab="t", main="Risk-adjusted", 
     ylim=c(-0.10, 0.10))
abline(h=h_1.risk, col="green")
abline(h=h_2.risk, col="red")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
legend("bottomright", legend=c("MA", "UCL = 0.016", "LCL = -0.016"), col=c("black", "green", "red"), lty=c(1,1,1))
plot(x=j, y=M_vlad_norisk, type="l", xaxt="n", xlab="t", main="Non-Risk-adjusted", 
     ylim=c(-0.10, 0.10))
abline(h=h_1.norisk, col="green")
abline(h=h_2.norisk, col="red")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
legend("bottomright", legend=c("MA", "UCL = 0.019", "LCL = -0.019"), col=c("black", "green", "red"), lty=c(1,1,1))
k.risk = 20/(abs(h_1.risk) + abs(h_2.risk))
k.norisk = 20/(abs(h_1.norisk) + abs(h_2.norisk))
Lower_MA_limit.risk <- vlad_risk + k.risk*(M_vlad_risk-h_1.risk)
Upper_MA_limit.risk <- vlad_risk + k.risk*(M_vlad_risk-h_2.risk)
Lower_MA_limit.norisk <- vlad_norisk + k.norisk*(M_vlad_norisk-h_1.norisk)
Upper_MA_limit.norisk <- vlad_norisk + k.norisk*(M_vlad_norisk-h_2.norisk)
par(mfrow = c(1, 2), mar = c(5, 5, 0.5, 0.5))
plot(x=j, y = V.nonrisk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 260), type = "l", main = "Non-risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y=Upper_MA_limit.norisk, col = "green")
lines(x=j, y=Lower_MA_limit.norisk, col = "red")
legend("topleft", inset = 0.05, legend=c("UMACL", "LMACL", "V_t"), col=c("green", "red", "black"), lty=c(1, 1, 1))


plot(x=j, y = V.risk, ylab = "V_t", xlab = "t", xaxt = "n",
     ylim = c(0, 260), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y=Upper_MA_limit.risk, col = "green")
lines(x=j, y=Lower_MA_limit.risk, col = "red")
legend("topleft", inset = 0.05, legend=c("UMACL", "LMACL", "V_t"), col=c("green", "red", "black"), lty=c(1, 1, 1))

#Confidence interal for VLAD by CLT 
vlad_norisk.var <- numeric(0)
vlad_norisk.var[1] <- yhat*(1-yhat)
vlad_risk.var <- numeric(0)
vlad_risk.var[1] <- p_0.risk[1]*(1-p_0.risk[1])
for(i in 2:25841){
  vlad_norisk.var[i] <- i*yhat*(1-yhat)
  vlad_risk.var[i] <- vlad_risk.var[i-1] + p_0.risk[i]*(1-p_0.risk[i])
}
se_V_t.risk <- sqrt(vlad_risk.var)
se_V_t.norisk <- sqrt(vlad_norisk.var)
V_t_CI_L.risk <- -1.96*(se_V_t.risk)
V_t_CI_U.risk <-  1.96*(se_V_t.risk)
V_t_CI_L.norisk <- -1.96*(se_V_t.norisk)
V_t_CI_U.norisk <- 1.96*(se_V_t.norisk)
max(V_t_CI_U.risk)
par(mfrow = c(1, 2))
plot(x=j, y = V.nonrisk, ylab = "Cumalitive_Number_of_Excess_Survivors", xlab = "t", xaxt = "n",
     ylim = c(-100, 350), type = "l", main = "Non-risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y = V_t_CI_U.norisk, col = "green")
lines(x=j, y = V_t_CI_L.norisk, col = "red")
legend("topleft", legend = c("V_t", "LCI", "UCI"), col = c("black", "red", "green"), lty = c(1,1,1))
plot(x=j, y = V.risk, ylab = "Cumalitive_Number_of_Excess_Survivors", xlab = "t", xaxt = "n",
     ylim = c(-100, 350), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y = V_t_CI_U.risk, col = "green")
lines(x=j, y = V_t_CI_L.risk, col = "red")
legend("topleft", legend = c("V_t", "LCI", "UCI"), col = c("black", "red", "green"), lty = c(1,1,1))

V_t_CI_L.risk2 <- V.risk - 1.96*(se_V_t.risk)
V_t_CI_U.risk2 <-  V.risk + 1.96*(se_V_t.risk)
V_t_CI_L.norisk2 <- V.nonrisk -1.96*(se_V_t.norisk)
V_t_CI_U.norisk2 <- V.nonrisk + 1.96*(se_V_t.norisk)

par(mfrow = c(1, 2))
plot(x=j, y = V.nonrisk, ylab = "Cumalitive_Number_of_Excess_Survivors", xlab = "t", xaxt = "n",
     ylim = c(0, 350), type = "l", main = "Non-risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y = V_t_CI_U.norisk2, col = "green")
lines(x=j, y = V_t_CI_L.norisk2, col = "red")
legend("topleft", legend = c("V_t", "LCI", "UCI"), col = c("black", "red", "green"), lty = c(1,1,1))
plot(x=j, y = V.risk, ylab = "Cumalitive_Number_of_Excess_Survivors", xlab = "t", xaxt = "n",
     ylim = c(0, 350), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
lines(x=j, y = V_t_CI_U.risk2, col = "green")
lines(x=j, y = V_t_CI_L.risk2, col = "red")
legend("topleft", legend = c("V_t", "LCI", "UCI"), col = c("black", "red", "green"), lty = c(1,1,1))
error_norisk <- 1.96*se_V_t.norisk
error_risk <- 1.96*se_V_t.risk
par(mfrow = c(2, 2))
plot(x=j, y = error_norisk, ylab = "error", xlab = "t")
plot(x=j, y = error_risk, ylab = "error", xlab = "t")

