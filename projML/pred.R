
library(rattle)
library(rpart)
library(caret)
set.seed(125)
library(pgmm)
data(olive)
olive = olive[,-1]
olivef = olive
#olivef$area = factor(olivef$Area)
olivefFit=train(factor(olivef$Area) ~ ., method="rpart",data=olivef)
library(rattle)
library(rpart)
fancyRpartPlot(olivefFit$finalModel)
print(olivefFit$finalModel)
test=  as.data.frame(t(colMeans(olivef[,c(2:9)])))
predict(olivefFit,newdata=test)
pred <-predict(olivefFit,newdata=test)
pred



library(rattle)
library(rpart)
library(caret)
set.seed(125)
library(pgmm)
data(olive)
olive = olive[,-1]
olivef = olive
olivef$Area = factor(olivef$Area)
olivefFit=train(Area ~ ., method="rpart",data=olivef)
library(rattle)
library(rpart)
fancyRpartPlot(olivefFit$finalModel)
print(olivefFit$finalModel)
test =  as.data.frame(t(colMeans(olive)))
#test =as.data.frame(t(colMeans(olivef[,c(2:9)])))
pred <-predict(olivefFit,newdata=test)
pred


View(test)
library(caret)
library(ElemStatLearn)
data(SAheart)
set.seed(13234)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
fit = train(chd  ~ age +alcohol+ obesity +tobacco+typea+ldl,method="glm",family="binomial",data=trainSA)
pred <- predict(fit,newdata=testSA)
predTrain <- predict(fit,newdata=trainSA)
print(fit$finalModel)
missClass = function(values,prediction){
  sum(((prediction > 0.5)*1) != values)/length(values)
}
missClass(testSA$cld,pred)
missClass(trainSA$cld,predTrain)
library(caret)
library(ElemStatLearn)
data(SAheart)
set.seed(13234)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
fit = train(chd  ~ age +alcohol+ obesity +tobacco+typea+ldl,method="glm",family="binomial",data=trainSA)
print(fit$finalModel)
ibrary(caret)
set.seed(125)
library(pgmm)
data(olive)
olive = olive[,-1]
olivef = olive
olivef$Area = factor(olivef$Area)
olivefFit=train(Area ~ ., method="rpart",data=olivef)
olive2fit<0train(factor(olive$Area) ~. ,method="rpart",data=olive)
print(olive2fit$finalModel)
library(caret)
library(rattle)
library(rpart)
set.seed(125)
library(pgmm)
data(olive)
olive = olive[,-1]
olivef = olive
olive2fit <- train(factor(olive$Area) ~. ,method="rpart",data=olive)
print(olive2fit$finalModel)
fancyRpartPlot(olive2fit$finalModel)
test[,c(2:9)] =as.data.frame(t(colMeans(olive[,c(2:9)])))
test =as.data.frame(t(colMeans(olive[,c(2:9)])))
pred2 <-predict(olive2fit,newdata=test)
pred2
olivef$Area = factor(olivef$Area)
olivefFit=train(Area ~ ., method="rpart",data=olivef)
fancyRpartPlot(olivefFit$finalModel)
print(olivefFit$finalModel)
pred <-predict(olivefFit,newdata=test)
pred


library(caret)
library(ElemStatLearn)
#require(randomForest)
data(vowel.train)
data(vowel.test) 
set.seed(33833)
train <- vowel.train
head(train)
test <-vowel.test
train$y =factor(train$y)
test$y =factor(test$y)
#fit <-randomForest(y ~. ,data=train ,importance=TRUE, do.trace=100, keep.forest=TRUE)
#varImp(fit)
fitCaret <- train(y ~., method="rf",  data=train)
varImp(fitCaret)




library(caret)
library(ElemStatLearn)
#require(randomForest)
data(vowel.train)
data(vowel.test) 
set.seed(33833)
train <- vowel.train
head(train)
test <-vowel.test
train$y =factor(train$y)
test$y =factor(test$y)
#fit <-randomForest(y ~. ,data=train ,importance=TRUE, do.trace=100, keep.forest=TRUE)
#varImp(fit)
fit1 <- train(y ~., method="rf",  data=train)

fit2 <- train(y ~., method="gbm",  data=train)

print(fit1$finalModel)
print(fit2$finalModel)

pred1 <-predict(fit1,newdata=test)
pred2 =predict(fit2,newdata=test)
pred1
pred2

matrf =  confusionMatrix(test$y,predict(fit1,test))
matGbm = confusionMatrix(test$y,predict(fit2,test))
matGbm$overall[1]
matrf$overall[1]


library(caret)
library(gbm)
set.seed(62433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

rf <- train(diagnosis ~., method="rf",  data=training)
gbm <- train(diagnosis ~., method="gbm",  data=training)
lda <- train(diagnosis ~., method="lad",  data=training)

stack <-()

