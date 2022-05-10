# set the working directory
setwd("E:/Tugas + Materi/S2/Semester 1/SML S1")

library(randomForest) # fungsi randomforest
library(BDgraph) # manggil data churn
library(caret) # confusion matrix, RMSE
library(e1071) # svr
library(kernlab) #svr

##########################################################
# LATIHAN
x <- seq(-20,20,0.1)
y <- sin(x)/x + rnorm(401,sd=0.03)
plot(x,y)

# train support vector machine
regm <- ksvm(x,y,epsilon=0.01,kpar=list(sigma=16),cross=3)
plot(x,y,type="l")
lines(x,predict(regm,x),col="red")

##########################################################
# load data
regression <- read.csv("D:/AKADEMIK/SEMESTER 7/STATISTIC MACHINE LEARNING/Latihan R/regression.csv")
plot(regression, main ="Scatter Plot")

model=lm(Y~X,regression)
abline(model)

## Prepare Scatter Plot with Predicted Points

#Scatter Plot
plot(regression, pch=16)

#Predict Y using Linear Model
predY <- predict(model, regression)
RMSE(predY,regression$Y)

#Overlay Predictions on Scatter Plot
points(regression$X, predY, col = "blue", pch=16)

#Scatter Plot
plot(regression)

# SVR Linear
linearsvm=svm(Y~X,regression, kernel='linear')
linearsvm
pred.lin.svm <- predict(linearsvm, regression)
RMSE(pred.lin.svm,regression$Y)
line(regression$X, pred.lin.svm)
lines(regression$X, pred.lin.svm)

#Regression with RBF SVM
modelsvm=svm(Y~X,regression)

#Predict using SVM regression
predYsvm <- predict(modelsvm, regression)
RMSE(predYsvm,regression$Y)

##Overlay SVM Predictions on Scatter Plot
points(regression$X, predYsvm, col = "red", pch=16)

## Calculate parameters of the SVR Model
#Find value of W
W=t(modelsvm$coefs) %*% modelsvm$SV

#Find value of b
b=modelsvm$rho  

##  Optimising SVR Model and Selecting Best Model

#Tune the above SVM model
OptModelsvm=tune(svm, Y~X, data=regression,ranges=list(epsilon=seq(0,1,0.1), cost=1:100))

#Print optimum value of parameters
print(OptModelsvm)

#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)

#Find out the best model
BstModel=OptModelsvm$best.model

#Predict Y using best model
PredYBst=predict(BstModel,regression)
RMSE(PredYBst,regression$Y)


## Calculate parameters of the Best SVR Model
#Find value of W
W=t(BstModel$coefs) %*% BstModel$SV

#Find value of b
b=BstModel$rho

## Plotting SVR Model and Tuned Model in same plot

plot(regression, pch=16)
points(regression$X, predYsvm, col = "blue", pch=3)
points(regression$X, PredYBst, col = "red", pch=4)
points(regression$X, predYsvm, col = "blue", pch=3, type="l")
points(regression$X, PredYBst, col = "red", pch=4, type="l")

all.data=data.frame(regression,predY,predYsvm,PredYBst)
colnames(all.data)<-c("Prediktor","Aktual","Regresi.Linear","SVR","Tuned.SVR")
head(all.data)

ggplot(all.data,aes(x=Prediktor,y=Aktual)) + geom_point() + 
  geom_line(aes(x=Prediktor,y=Regresi.Linear, col="lm"), size=1.5) + 
  geom_line(aes(x=Prediktor,y=SVR, col="svr"), size=1.5) +
  geom_line(aes(x=Prediktor,y=Tuned.SVR, col="tuned.svr"), size=1.5) + theme_minimal() +
  scale_colour_manual("", 
                      breaks = c("lm", "svr", "tuned.svr"),
                      values = c("black", "red", "blue"))

# load data
data("churn")

# drop inappropriate attributes
churn = churn[,! names(churn) %in% c("State", "Area.Code", "Account.Length") ]
table(churn$Churn)/length(churn$Churn)

# split data into training set and testing set
set.seed(2)
ind = sample(2, nrow(churn), replace = TRUE, prob=c(0.7, 0.3))
trainset = churn[ind == 1,]
testset = churn[ind == 2,]

# basic random forest
set.seed(123)
churn.rf = randomForest(Churn ~ ., data = trainset, importance = T)
churn.rf
rf.prediction = predict(churn.rf, testset)
confusionMatrix(testset$Churn,rf.prediction,positive = "True")
plot(churn.rf)

# feature importance
importance(churn.rf)
varImpPlot(churn.rf)

# tuning rf
set.seed(123)
train.control <- trainControl(method = "cv", number = 5,search = "random")
rf5<-train(Churn~., data = trainset, method = "rf", 
             trControl=train.control,
             tuneLength = 5)
rf5
rf5.tuned.pred = predict(rf5, testset[, !names(testset)
                                          %in% c("Churn")])
confusionMatrix(rf5.tuned.pred,testset$Churn, positive = "True")

