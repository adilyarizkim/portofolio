# STRATIFIED
library(survival)
library(ggplot2)
library(psych)
library(cluster)
library(fpc)
library(readxl)
library(survminer)
library(survf)
library(rms)
library(Hmisc)
library(plyr)

data = read_xlsx("D:/Data EAS Analisis Survival.xlsx")
data

# Model Tanpa interaksi
model2 = coxph(formula = Surv(Survt, Relapse) ~ Rx + logWBC + strata(Sex), data = data)
summary(model2)

# Survival Plot
plot(survfit(model2), lwd =3, col =1,lty = c(1,3), cex = 2, lab = c(10,10,7))
legend(0.5, 0.3, c("P", "L"), lty = 1:2)

# KM
survdiff(formula = Surv(Survt, Relapse) ~ Sex, data = data, rho = 0)

# Baseline Hazard
base <- basehaz(model2)
baseline_gg <- ggplot(base, aes(x=time)) +
  geom_step(aes(y=hazard, group=strata)) +
  ylab(expression(hat(Lambda)(t))) + xlab("t")
baseline_gg + aes(col=strata)

# Schoenfeld residual 
par(mfrow = c(1,2))
res_vet = cox.zph(model2)
plot(res_vet)
res_vet

# Martingale Residual
data$resid_mart_str <- residuals(model2, type = "martingale")
ggplot(data = data, mapping = aes(x = Sex, y = resid_mart_str)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Rx") +
  theme_bw() + theme(legend.key = element_blank())

ggplot(data = data, mapping = aes(x = logWBC, y = resid_mart_str)) +
  geom_point() +
  geom_smooth() +
  labs(title = "logWBC") +
  theme_bw() + theme(legend.key = element_blank())



ggplot(data = veteran, mapping = aes(x = prior, y = resid_mart_str)) +
  geom_point() +
  geom_smooth() +
  labs(title = "prior") +
  theme_bw() + theme(legend.key = element_blank())
