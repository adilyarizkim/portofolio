# set the working directory
setwd("E:/Tugas + Materi/S2/Semester 1/SML S1")

library(caret) #confusion matrix, cross-validation, feature selection
library(BDgraph) # data churn
library(e1071) # metode svm
library(rminer) # ranking variable immportance
library(ggcorrplot) # correlation plot
library(doSNOW) # parallel computing
library(parallel) # parallel computing
library(ROCR) # roc-auc
library(pROC) # compare roc-auc

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

# manual k-fold cross-validation
ind = cut(1:nrow(trainset), breaks=10, labels=F)
accuracies = c()
for (i in 1:10) {
  fit = svm(Churn ~., trainset[ind != i,])
  predictions = predict(fit, trainset[ind == i, 
                                        !names(trainset) %in% c("Churn")])
  correct_count = sum(predictions == trainset[ind ==i,c("Churn")])
  accuracies = append(correct_count / nrow(trainset[ind ==i,]), accuracies)
}
accuracies
mean(accuracies)

# built-in svm package cross-validation
tuned = tune.svm(Churn~., data = trainset, gamma = 10^-2, cost =
                   10^2, tunecontrol=tune.control(cross=10))
summary(tuned)
tuned$performances
svmtuned=tuned$best.model
confusionMatrix(predict(svmtuned,testset[,-17]),testset[,17], positive = "True")

# caret's cross-validation
control = trainControl(method="repeatedcv", number=10,repeats=3) # repeated k-cv
rpart.start<-Sys.time()
model = train(Churn~., data=trainset, method="rpart",
              preProcess="scale", trControl=control)
rpart.end<-Sys.time()
rpart.time=rpart.end-rpart.start
model
rpart.imp=varImp(model, scale = F)
plot(rpart.imp)

# Ranking the variable importance with the rminer package
model=fit(Churn~.,trainset,model="svm")
VariableImportance=Importance(model,trainset,method="sensv")
L=list(runs=1,sen=t(VariableImportance$imp),
       sresponses=VariableImportance$sresponses)
mgraph(L,graph="IMP",leg=names(trainset),col="gray",Grid=10)

# Finding highly correlated features with the caret package
new_train = trainset[,! names(trainset) %in% 
                       c("Churn","Int.l.Plan", "VMail.Plan")]
cor_mat = cor(new_train)
p.mat <- cor_pmat(new_train)
ggcorrplot(cor_mat, hc.order = F,type = "lower",lab=T, p.mat = p.mat)
highlyCorrelated = findCorrelation(cor_mat, cutoff=0.75)
names(new_train)[highlyCorrelated]

# Selecting features using recursive feature elimination from the caret package
intl_plan = model.matrix(~ trainset$Int.l.Plan - 1,
                         data=data.frame(trainset$Int.l.Plan)) # create dummy variable
colnames(intl_plan) = c("trainset.international_planno"="intl_no", 
                        "trainset.international_planyes"= "intl_yes")
head(intl_plan)
voice_plan = model.matrix(~ trainset$VMail.Plan - 1,
                          data=data.frame(trainset$VMail.Plan))
colnames(voice_plan) = c("trainset.voice_mail_planno" ="voice_no", 
                         "trainset.voice_mail_planyes"="voidce_yes")
head(voice_plan)
trainset2<-trainset
trainset2$Int.l.Plan = NULL
trainset2$VMail.Plan = NULL
trainset2 = cbind(intl_plan,voice_plan, trainset2)

intl_plan = model.matrix(~ testset$Int.l.Plan - 1,
                         data=data.frame(testset$Int.l.Plan)) # create dummy variable
colnames(intl_plan) = c("testset.international_planno"="intl_no", 
                        "testset.international_planyes"= "intl_yes")
head(intl_plan)
voice_plan = model.matrix(~ testset$VMail.Plan - 1,
                          data=data.frame(testset$VMail.Plan))
colnames(voice_plan) = c("testset.voice_mail_planno" ="voice_no", 
                         "testset.voice_mail_planyes"="voidce_yes")
head(voice_plan)
testset2<-testset
testset2$Int.l.Plan = NULL
testset2$VMail.Plan = NULL
testset2 = cbind(intl_plan,voice_plan, testset2)

ldaControl = rfeControl(functions = ldaFuncs, method = "cv") # feature selection using Linear Discriminant Analysis
# alternative function
# caretFuncs SVM (caret)
# lmFuncs lm (base)
# rfFuncs RF(randomForest)
# treebagFuncs DT (ipred)
# ldaFuncs lda(base)
# nbFuncs NB(klaR)
# gamFuncs gam(gam)

ldaProfile = rfe(trainset2[, !names(trainset2) %in% c("Churn")],
                 trainset2[,c("Churn")],sizes = c(1:18), rfeControl = ldaControl)
ldaProfile
plot(ldaProfile, type = c("o", "g"))
ldaProfile$optVariables
ldaProfile$fit
postResample(predict(ldaProfile, testset2[, !names(testset2) %in%
                                           c("Churn")]), testset2[,c("Churn")])

# Selecting features using univariate filters from the caret package
filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 5)
set.seed(10)
rfWithFilter <- sbf(trainset2[,-19], trainset2[,19], sbfControl = filterCtrl)
rfWithFilter$fit
rfWithFilter$optVariables
rfSBF.pred=predict(rfWithFilter,testset2[,-19])
confusionMatrix(rfSBF.pred$pred,testset$Churn,positive = "True")

# Selecting features using genetic algorithm from the caret package
gaCtrl=gafsControl(functions = rfGA, method = "cv", number = 10, verbose= TRUE)
cl <- makeCluster(detectCores(), type = "SOCK")
registerDoSNOW(cl) 
rf_ga.start=Sys.time()
rf_ga <- gafs(x = trainset[,-17], y = trainset[,17],
              iters = 10, popSize = 20, 
              gafsControl = gaCtrl)
rf_ga.end=Sys.time()
rf_ga.time=rf_ga.end-rf_ga.start
stopCluster(cl)
plot(rf_ga)+theme_bw()
rf_ga$fit
rf_ga$optVariables
rf_ga.pred=predict(rf_ga,testset[,-17])
confusionMatrix(rf_ga.pred$pred,testset$Churn,positive = "True")

# Selecting features using simulated annealing from the caret package
saCtrl=safsControl(functions = rfSA, method = "cv", number = 10, verbose= TRUE, 
                   improve = 10)
cl <- makeCluster(detectCores(), type = "SOCK")
registerDoSNOW(cl) 
rf_sa.start=Sys.time()
rf_sa <- safs(x = trainset[,-17], y = trainset[,17],
              iters = 50,  
              safsControl = saCtrl)
rf_sa.end=Sys.time()
rf_sa.time=rf_sa.end-rf_sa.start
stopCluster(cl) # gunakan ini atau registerDoSEQ()
plot(rf_sa)+theme_bw()
rf_sa$fit
rf_sa$optVariables
rf_sa.pred=predict(rf_sa,testset[,-17])
confusionMatrix(rf_sa.pred$pred,testset$Churn,positive = "True")

# Measuring prediction performance using ROCR
svmfit=svm(Churn~ ., data=trainset, prob=TRUE)
pred=predict(svmfit,testset[, !names(testset) %in% c("Churn")],
             probability=TRUE)
head(pred)
pred.prob = attr(pred, "probabilities")
head(pred.prob)
pred.to.roc = pred.prob[, 2]
pred.rocr = prediction(pred.to.roc, testset$Churn)
perf.rocr = performance(pred.rocr, measure = "auc", x.measure =
                          "cutoff")
perf.tpr.rocr = performance(pred.rocr, "tpr","fpr")
plot(perf.tpr.rocr, colorize=T,main=paste("AUC:",(perf.rocr@y.values)))

# Comparing an ROC curve using the caret package
tr.control=trainControl(method = "cv", number = 10, search = "random",
                        summaryFunction = twoClassSummary, classProbs = T)
svm10.auc<-train(Churn~., data = trainset, method = "svmRadial", 
                 trControl=tr.control, metric = "ROC",
                 tuneLength = 10)
glm.model<-train(Churn~., data=trainset, method="glm", trControl=tr.control,
                 metric="ROC")
rpart.model<-train(Churn~., data=trainset, method="rpart", trControl=tr.control,
                   metric="ROC")
glm.probs = predict(glm.model, testset[,! names(testset) %in%
                                         c("Churn")], type = "prob")
svm.probs = predict(svm10.auc, testset[,! names(testset) %in%
                                         c("Churn")], type = "prob")
rpart.probs = predict(rpart.model, testset[,! names(testset) %in%
                                         c("Churn")], type = "prob")
glm.ROC = roc(response = testset$Churn,
              predictor =glm.probs$True,
              levels = levels(testset$Churn))
plot(glm.ROC, type="S", col="red")
svm.ROC = roc(response = testset$Churn,
              predictor =svm.probs$True,
              levels = levels(testset$Churn))
plot(svm.ROC, add=TRUE, col="green")
rpart.ROC = roc(response = testset$Churn,
              predictor =rpart.probs$True,
              levels = levels(testset$Churn))
plot(rpart.ROC, add=TRUE, col="blue")

# Measuring performance differences between models with the caret package
cv.values = resamples(list(glm = glm.model, svm=svm10.auc, rpart= rpart.model))
summary(cv.values)
dotplot(cv.values, metric = "ROC")
bwplot(cv.values, layout = c(3, 1))
