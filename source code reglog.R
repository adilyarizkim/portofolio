library (readr)
library(generalhoslem)
library (MLmetrics)
data=read.csv('D://skenario10_test.csv')
str(data)
data$y=as.factor(data$y)
model <- glm(y ~ x1 + x2 + x3 +x4 + x5, data = data, family = "binomial")
model
summary(model)
prediksi<-predict(model,data,type="response")
head(prediksi)
logitgof(data$y, fitted(model))

#prediksi table regresi logistik
prediksi <- ifelse(model$fitted.values>0.5, "YA","Tidak")
head(prediksi)
tab1 <- table(predicted=prediksi, Actual=data$y)
head(tab1)
akurasi <- ((tab1[1,1]+tab1[2,2]/sum(tab1)))
akurasi



