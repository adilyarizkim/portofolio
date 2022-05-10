data = read.csv("D://SMT 7//2. ANALISIS SURVIVAL//TUGAS//4//Data pasien leukimia.csv", header=T, sep=",")
data

data_TRT1 = data[22:42,1]
data_TRT0 = data[1:21,1]

#Deskriptif data
library(survival)
kmfit=survfit(Surv(data$time, data$event) ~ data$TRT,  type="kaplan-meier", conf.type="log", data=data)
kmfit
summary(kmfit)

#plot kurva
plot(kmfit, lty=c("solid", "solid"), col=c("blue", "green"), xlab="survival time in days", ylab="survival probabilities")
legend("topright", c("TRT=0", "TRT=1"), lty=c("solid","solid"), col=c("blue", "green"))

#Uji Logrank
survdiff(Surv(data$time, data$event) ~ data$TRT, data=data)

#uji asumsi distribusi weibull
library(fitdistrplus)
fitdistr(data_TRT1, "weibull")
ks.test(data_TRT1, "pweibull", scale=19.359774, shape=1.864619)

fitdistr(data_TRT0, "weibull")
ks.test(data_TRT0, "pweibull", scale=9.4817986, shape=1.3705114)

#uji asumsi distribusi exponential
fitdistr(data_TRT1, "exponential")
ks.test(data_TRT1, "pexp", rate=0.05849582)

fitdistr(data_TRT0, "exponential")
ks.test(data_TRT0, "pexp", rate=0.11538462)

#Exponential regresi survival
modpar1=survreg(Surv(data$time,data$event) ~ TRT, data=data, dist="exponential")
summary(modpar1)

#Weibull regresi survival
modpar2=survreg(Surv(data$time,data$event) ~ TRT, data=data, dist="weibull")
summary(modpar2)

#predict function of survival time from exp dist
pattern1=data.frame(TRT=1)
pattern1
st=c(.25,.50,.75)
days=predict(modpar1,newdata=pattern1,type="quantile",p=st)
days
cbind(st,days)

pattern1=data.frame(TRT=0)
st=c(.25,.50,.75)
days=predict(modpar1,newdata=pattern1,type="quantile",p=st)
cbind(st,days)

#predict function of survival time from weibull dist
pattern2=data.frame(TRT=1)
pct=c(.25,.50,.75)
days=predict(modpar2,newdata=pattern2,type="quantile",p=pct)
cbind(pct,days)

pattern2=data.frame(TRT=0)
pct=c(.25,.50,.75)
days=predict(modpar2,newdata=pattern2,type="quantile",p=pct)
cbind(pct,days)
