library(survival)
library(survminer)
library(MASS)
library(RVAideMemoire)
veteran

#statistika deskriptif
summary(veteran)

Y=Surv(veteran$time, veteran$status)

#Kaplain-Meier CUrve dan Uji Log-Rank Faktor trt
kmfit1=survfit(Y ~ veteran$trt ,  type="kaplan-meier",
              conf.type="log", data=veteran)
plot(kmfit1, lty=c("solid", "solid"), col=c("blue", "green"), 
     xlab="survival time in days", ylab="survival probabilities")
legend("topright", c("Standard", "Test Chemotherapy"), 
       lty=c("solid","solid"), col=c("blue", "green"))
survdiff(Y~veteran$trt, data=veteran)

#Kaplain-Meier CUrve dan Uji Log-Rank Faktor celltype
kmfit2=survfit(Y ~ veteran$celltype ,  type="kaplan-meier",
               conf.type="log", data=veteran)
plot(kmfit2, lty=c("solid", "solid", "solid", "solid"), 
     col=c("blue", "green", "red", "yellow"), 
     xlab="survival time in days", ylab="survival probabilities")
legend("topright", c("squamous", "smallcell", "adeno", "large"), 
       lty=c("solid","solid", "solid", "solid"), 
       col=c("blue", "green", "red", "yellow"))
survdiff(Y~veteran$celltype, data=veteran)

#Kaplain-Meier CUrve dan Uji Log-Rank Faktor celltype
kmfit3=survfit(Y ~ veteran$prior ,  type="kaplan-meier",
               conf.type="log", data=veteran)
plot(kmfit3, lty=c("solid", "solid"), col=c("blue", "red"), 
     xlab="survival time in days", ylab="survival probabilities")
legend("topright", c("No", "Yes"), lty=c("solid","solid"), col=c("blue", "red"))
survdiff(Y~veteran$prior, data=veteran)

#Cox PH Model
coxPHmodel <- coxph(formula = Y ~ trt + celltype + karno + 
                 diagtime + age + prior, data = veteran)
summary(coxPHmodel)
test.coxPHmodel

#Uji Grambsch-Therneau
test.ph = cox.zph(coxPHmodel)
test.ph

#Schoenfeld Residual
win.graph()
par(mfrow=c(3,3))
plot(test.ph)

#Martingale Residual
mresi=resid(coxPHmodel,type="martingale")
##karno
resi1 <- ggplot(data = veteran, mapping = aes(x = karno, y = mresi)) + geom_point() +
        geom_smooth() + labs(title = "Variabel Karno") + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
        theme_bw() + theme(legend.key = element_blank())
resi1

##age
resi2 <- ggplot(data = veteran, mapping = aes(x = age, y = mresi)) + geom_point() +
        geom_smooth() + labs(title = "Variabel age") + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
        theme_bw() + theme(legend.key = element_blank())
resi2

##celltype
celltype2 <- as.factor(veteran$celltype)
celltype2 <- as.numeric(celltype2)
resi3 <- ggplot(data = veteran, mapping = aes(x = celltype2, y = mresi)) + 
        geom_point() +
        geom_smooth() + labs(title = "Variabel celltype") + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
        theme_bw() + theme(legend.key = element_blank())
resi3

##trt
resi4 <- ggplot(data = veteran, mapping = aes(x = trt, y = mresi)) + 
        geom_point() +
        geom_smooth() + labs(title = "Variabel trt") + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
        theme_bw() + theme(legend.key = element_blank())
resi4

##diagtime
resi5 <- ggplot(data = veteran, mapping = aes(x = diagtime, y = mresi)) + 
        geom_point() +
        geom_smooth() + labs(title = "Variabel diagtime") + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
        theme_bw() + theme(legend.key = element_blank())
resi5

##prior
resi6 <- ggplot(data = veteran, mapping = aes(x = prior, y = mresi)) + 
        geom_point() +
        geom_smooth() + labs(title = "Variabel prior") + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
        theme_bw() + theme(legend.key = element_blank())
resi6
