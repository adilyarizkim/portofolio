dt <- read.csv("D://Data.csv")
print(dt)
str(dt)
head(dt)
table (dt$Dataset)
prop.table(table(dt$Dataset))

library(plotrix)
Dataset = table(dt$Dataset) 
Dataset=c(42,57)
label=c("1 = ","2 = ") 
persen=round(Dataset/sum(Dataset)*100) 
label=paste(label,persen)
label=paste(label,'%',sep ='')
pie3D(Dataset,labels=label,col=c('yellow','blue'),
      main="Persentase Pasien")

library(plyr)
mu <- ddply(dt, "Dataset", summarise, grp.mean=mean(Age))
head(mu)

library(ggplot2)
ggplot(dt, aes(x=Age, fill=Dataset, color=Dataset)) +
  geom_histogram(aes(y =..density..), bins=30) +
  geom_density(alpha=.2) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Dataset),
             linetype="dashed") +
  labs(title="Histogram for Age", x="Age", y="Dataset")

#Uji Multikolinearitas
library("PerformanceAnalytics")
chart.Correlation(dt[,1:10], histogram=TRUE, pch=19)

#Membentuk Model
reg<- glm(Dataset~Age+Gender+Total_Bilirubin+Direct_Bilirubin+Alkaline_Phosphotase+
                Alamine_Aminotransferase+Aspartate_Aminotransferase+Total_Protiens+Albumin+
                Albumin_and_Globulin_Ratio, family = binomial)
library(haven)
dt <- read.csv("D://Data.csv")
print(dt)
y<-factor(dt$Dataset)
x1<-factor(dt$Age)
x2<-factor(dt$Gender)
x3<-factor(dt$Total_Bilirubin)
x4<-factor(dt$Direct_Bilirubin)
x5<-factor(dt$Alkaline_Phosphotase)
x6<-factor(dt$Alamine_Aminotransferase)
x7<-factor(dt$Aspartate_Aminotransferase)
x8<-factor(dt$AgeTotal_Protiens)
x9<-factor(dt$Albumin)
x10<-factor(dt$Albumin_and_Globulin)
reg<-glm(y~x1+x2+x5, family = binomial (link="logit"))
summary(reg)

#Uji Overall
library(pscl)
pR2(reg)
qchisq(0.95, 3)

dt <- read.csv("D://Data.csv")
dt
library(plotrix)
Diagnosis = table(y) #melihat jumlah setiap JK
print(Diagnosis)
Diagnosis=c(416,167)
label=c("Disease = ","No Disease = ") 
persen=round(Diagnosis/sum(Diagnosis)*100) ##buat persentase
label=paste(label,persen)
label=paste(label,'%',sep ='')
pie3D(Diagnosis,labels=label,col=c('yellow','blue'),
      main="Persentase Diagnosis")
#uji parsial
summary(logit1)
qnorm(0.95, 1)

#Model Regresi Logstik biner setelah x2 dikeluarkan
logit2 <- glm(as.factor(y)~x1+x5, family = binomial(link = "logit"), data = dt)
summary(logit2)

#Uji kesesuaian model
library(ResourceSelection)
hoslem.test(logit2$y, fitted(logit2))
qchisq(0.95, 8)
pR2(logit2)
logitt <- glm(as.factor(y)~1, family = binomial(link = "logit"), data = dt)
1-as.vector(logLik(logit2)/logLik(logitt))
