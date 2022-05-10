library(survival)
library(survminer)
library(MASS)
library(RVAideMemoire)
library(readxl)
data = read_excel("C:/Users/User/Downloads/project 2.xlsx")
head(data)

#Kurva Kaplan-Meier
KM = survfit(Surv(day, event) ~ trt, data = data)
plot(KM, lty = 1:2, xlab = "Survival Time", ylab = "trt")
legend(600, 1, c("Auto", "Allo"), lty = 1:2)
title("Kurva Kaplan-Meier by Treatment")

KM1 = survfit(Surv(day, event) ~ disease, data = data)
plot(KM1, lty = 1:2, xlab = "Survival Time", ylab = "disease")
legend(600, 1, c("NHL", "HOD"), lty = 1:2)
title("Kurva Kaplan-Meier by Disease")

#Uji Log-Rank 
surv_diff <- survdiff(formula = Surv(day, event) ~ trt, data = data)
surv_diff

surv_diff1 <- survdiff(formula = Surv(day, event) ~ disease, data = data)
surv_diff1

#Cox PH Model
coxPH <- coxph(formula = Surv(day, event) ~ trt + disease + karno + waiting_time, data = data)
summary(coxPH)

#Martingale Residual
coxPH2 <- coxph(formula = Surv(day, event) ~ trt + disease + karno + waiting_time, data = data)
win.graph()
cox.resid(coxPH2)

#Uji Grambsch-Therneau
test.ph = cox.zph(coxPH)
print(test.ph)

#Schoenfeld Residual
win.graph()
par(mfrow=c(3,3))
plot(test.ph)

## Hasil Log rank menunjukkan bahwa tidak ada variabel yang time-dependent sehingga model CoxPH adalah model yang sesuai.