library(C50)  
library(BDgraph)  #load dataset ygmau dipake
library(rpart)  #metode decision tree
library(class)  #metode KNN
library(e1071)  #naive bayes dan SVM
library(caret)  #menghitung confussion matrix
library(pROC)   #menghitung AUC, buat plot ROC

#Load the dataset 
data(churn)  #dari library BDGraph
str(churn)

#Drop inappropiate attributes
churn = churn[,!names(churn) %in% c("State", "Area.Code", "Account.Length")]

#split data into training set and testing set
set.seed(2)
ind = sample(2, nrow(churn), replace = TRUE, prob=c(0.7, 0.3))
trainset = churn[ind == 1,]
table(trainset$churn)/length(trainset$churn)
testset = churn[ind == 2,]
table(testset$churn)/length(testset$churn)

#Building a classification model with recursive partitioning tree
churn.rp = rpart(Churn ~ ., data=trainset)
printcp(churn.rp)
plotcp(churn.rp)
summary(churn.rp)
plot(churn.rp, margin=0.1)
text(churn.rp, all=TRUE, use.n = TRUE)

#Dari Mas Asva
predictions = predict(churn.rp, testset[,-17], type="class")
head(predictions)
confusionMatrix(table(predictions, testset$Churn))

churn.cp = churn.rp$cptable[which.min(churn.rp$cptable[,"xerror"]),"CP"]
prune.tree = prune(churn.rp, cp= churn.cp)
plot(prune.tree, margin= 0.1)
text(prune.tree, all=TRUE , use.n=TRUE)
rp.predictions = predict(prune.tree, testset[,-17], type="class")
confusionMatrix(table(rp.predictions, testset$Churn))

rp.prob<-predict(prune.tree, testset[,-17])
head(rp.prob)
true.rp.prob<-rp.prob[,"True"]
head(true.rp.prob)
rp.roc<-roc(testset$Churn,true.rp.prob)
plot(rp.roc)
auc(rp.roc)

##Classifying data with logistic regression
fit = glm(Churn ~., data=trainset, family=binomial) #bisa pake family logit
summary(fit)
backwards = step(fit)
summary(backwards)
pred = predict(backwards, testset[,-19], type="response")
class = ifelse(pred>=0.5, "True", "False")
class = as.factor(class)
confusionMatrix(table(testset$Churn, class))
head(pred)

lr.roc = roc(testset$Churn, pred)
lr.roc
plot(lr.roc)
auc(lr.roc)

#Naive Bayes
classifier=naiveBayes(trainset[, !names(trainset) %in% c("Churn")], trainset$Churn)
classifier  #conditional probability
bayes.table = table(predict(classifier, testset[,!names(testset) %in% c("Churn")]), testset$Churn)
confusionMatrix(bayes.table)

pred.nb=predict(classifier, testset[,-17], type="raw")
head(pred.nb)
true.pred.nb=pred.nb[,"True"]
roc.nb=roc(testset$Churn,true.pred.nb)
plot(roc.nb)
auc(roc.nb)

#K Nearest Neighbor
levels(trainset$Int.l.Plan) = list("0"="no", "1"="yes")
levels(trainset$VMail.Plan) = list("0"="no", "1"="yes")
levels(testset$Int.l.Plan) = list("0"="no", "1"="yes")
levels(testset$VMail.Plan) = list("0"="no", "1"="yes")
churn.knn = knn(trainset[,! names(trainset) %in% c("Churn")],
                testset[,! names(testset) %in% c("Churn")], trainset$Churn, k=5)
confusionMatrix(table(testset$Churn, churn.knn))

prob.knn<-knn(train = trainset[,-17], test = testset[,-17], 
              cl=trainset$Churn, k=5, prob = TRUE)
head(prob.knn)
roc.knn=roc(testset$Churn,attributes(prob.knn)$prob)
plot(roc.knn)
auc(roc.knn)

# multiple roc curves
mult.roc<-plot(roc(testset$Churn,true.rp.prob), print.auc = TRUE, col = "blue")
mult.roc <- plot(roc(testset$Churn,pred), print.auc = TRUE, 
                 col = "green", print.auc.y = .4, add = TRUE)
mult.roc <- plot(roc(testset$Churn,true.pred.nb), print.auc = TRUE, 
                 col = "red", print.auc.y = .3, add = TRUE)
mult.roc <- plot(roc(testset$Churn,attributes(prob.knn)$prob), print.auc = TRUE, 
                 col = "black", print.auc.y = .2, add = TRUE)
legend(x=-0.1,y=.6,legend = c("Decision Tree", "Logistic Regression",
                              "Naive Bayes","KNN"),
       col = c("blue","green","red","black"),lwd = 3,cex = 0.7, ncol = 1,
       text.width = 0.6)
