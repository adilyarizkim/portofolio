#Memanggil data
data=read.csv("D://SMT 7//ANALISIS SURVIVAL//data chronic.csv")
data

#mendefinisikan respon
Y=Surv(data$time, data$status)

#Deskriptif data
library(survival)
kmfit2=survfit(Y ~ data$grup,  type="kaplan-meier", conf.type="log", data=data)
kmfit2
summary(kmfit2)

#plot kurva
plot(kmfit2, lty=c("solid", "solid"), col=c("blue", "green"), xlab="survival time in days", ylab="survival probabilities")
legend("topright", c("No history", "Positive history"), lty=c("solid","solid"), col=c("blue", "green"))

#logrank test
survdiff(Y~data$grup, data=data)

