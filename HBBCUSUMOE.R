rm(list=ls())
setwd("C:/Users/1998c/OneDrive/Skrivebord/Masteroppgave")
#Read data and format correctly for glm
library(foreign)#To read SPSS file
library(zoo)
library(forecast)
#All the code from line 8-62 was provided to me by my supervisor
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


#delta
delta.risk <- -0.0256
delta.norisk <- -0.0144
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

#chart making
library(spcadjust)
chartlogreg_baseline <- new("SPCCUSUM", model = SPCModellogregOE(Delta = delta.risk, 
                                                                       formula = 
                                                                         y~x1kg + x2 + f2 +
                                                                         f3 + f9 + p2 +
                                                                         p3 + p4 + p5))
chartlogreg_baseline2 <- new("SPCCUSUM", model = SPCModellogregOE(Delta = -delta.risk, 
                                                                        formula = 
                                                                          y~x1kg + x2 + f2 +
                                                                          f3 + f9 + p2 +
                                                                          p3 + p4 + p5))
chartlogreg_nonrisk <- new("SPCCUSUM", model = SPCModellogregOE(Delta = delta.norisk, formula = y~1
))
chartlogreg_nonrisk2 <- new("SPCCUSUM", model = SPCModellogregOE(
  Delta = -delta.norisk, formula = y~1))

xihat <- xiofdata(chartlogreg_baseline, estdata1)
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
S <- runchart(chartlogreg_baseline, newdata = rundata1, xi = xihat)
S2 <- runchart(chartlogreg_baseline2, newdata = rundata1, xi = xihat2)
plot(S, xlab = "", ylim = c(0, 85), xlim = c(0, 25841), xaxt = "n", main = "Risk-adjusted")
abline(h = calibrate@raw, col = "blue")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
legend("bottomright", legend = c(64.406), col = c("blue"), lty = c(1))
S_nonrisk <- runchart(chartlogreg_nonrisk, newdata = rundata1, xi = xihat_nonrisk
)
S_nonrisk2 <- runchart(chartlogreg_nonrisk2, newdata = rundata1, xi = xihat_nonrisk2)
plot(S_nonrisk, xlab = "", ylim = c(0, 70), xlim = c(0, 25841), xaxt = "n", main = "Non-risk")
abline(h = calibrate_nonrisk@raw, col = "blue")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
legend("topright", legend = c(27.570), col = c("blue"), lty = c(1))

#Plots with positive delta
par(mfrow = c(1, 2))
plot(S2, xlab = "", ylim = c(0, 10), xlim = c(0, 25841), xaxt = "n", main = "Risk-adjusted")
abline(h = calibrate2@raw, col = "blue")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
legend("topright", legend = c(5.773), col = c("blue"), lty = c(1))
plot(S_nonrisk2, xlab = "", ylim = c(0, 17), xlim = c(0, 25841), xaxt = "n", main = "Non-risk")
abline(h = calibrate_nonrisk2@raw, col = "blue")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
legend("topright", legend = c(15.681), col = c("blue"), lty = c(1))
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
plot(x=j, y = V.nonrisk, ylab = "Cumalitive_Number_of_Excess_Survivors", xlab = "t", xaxt = "n",
     ylim = c(0, 260), type = "l", main = "Non-risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)
#produce vlad plor for risk situation
V.risk<-c(0*25841)
p_0.risk<-predict(estmod1kg, newdata=rundata1 , type= "response")
V.risk<-cumsum(p_0.risk - rundata1$y)

plot(x=j, y = V.risk, ylab = "Cumalitive_Number_of_Excess_Survivors", xlab = "t", xaxt = "n",
     ylim = c(0, 260), type = "l", main = "Risk Adjusted")
axis(1,las=2, at=c(1,cnMonth2[6],cnMonth2[12],cnMonth2[18],cnMonth2[24],cnMonth2[30],cnMonth2[36],cnMonth2[42],
                   cnMonth2[48],cnMonth2[54],cnMonth2[60],cnMonth2[66],cnMonth2[72]),
     labels=c("Feb 11","July 11","Jan 12","July 12","Jan 13","July 13","Jan 14","July 14","Jan 15","July 15","Jan 16","July 16","Jan 17"))
abline(0,0,lty=2)

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

U_n.risk <- vlad_risk+(S-h.risk.upper)/(-exp(-delta.risk))
U_n.norisk <- vlad_norisk+(S_nonrisk-h.nonrisk.upper)/(-exp(-delta.norisk))
L_n.risk <- vlad_risk+(S2-h.risk.lower)/(exp(delta.risk))
L_n.norisk <- vlad_norisk+(S_nonrisk2-h.nonrisk.lower)/(exp(delta.norisk))

Centre_line_risk <- mean(vlad_risk)
Centre_line_norisk <- mean(vlad_norisk)

# Vlad Plots with control limits based on OE
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
