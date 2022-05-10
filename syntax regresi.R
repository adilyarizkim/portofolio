
library(rpart)  #metode decision tree
library(class)  #metode KNN
library(e1071)  #naive bayes dan SVM
library(caret)  #menghitung confussion matrix

#Load the dataset 
data=read.csv('D://skenario1_test.csv')  #dari library BDGraph
str(data)

#Drop inappropiate attributes
data = data[,!names(data) %in% c("State", "Area.Code", "Account.Length")]

#split data into training set and testing set
set.seed(2)
ind = sample(2, nrow(data), replace = TRUE, prob=c(0.7, 0.3))
trainset = data[ind == 1,]
table(trainset$data)/length(trainset$data)
testset =data[ind == 2,]
table(testset$data)/length(testset$data)

class

##Classifying data with logistic regression
fit = glm(Y~., data=data, family=binomial(link="logit")) #bisa pake family logit
summary(fit)
backwards = step(fit)
summary(backwards)
pred = predict(backwards, testset[,-19], type="response")
class = ifelse(pred>=0.10, "True", "False")
table(class)
class = as.factor(class)
confusionMatrix(table(prediksi, testing$Y))
head(pred)

lr.roc = roc(testset$data, pred)
lr.roc
plot(lr.roc)
auc(lr.roc)

head(testset)
  
