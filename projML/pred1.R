library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)


adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]



library(AppliedPredictiveModeling)
data(concrete)
library(caret)
library(Hmisc)
seedRef = 2 #975
set.seed(seedRef)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
training$index2=seq_along(training$CompressiveStrength)

 for(i in c(1:8)){ 
   training[,ncol(training)+1] <- cut2(round(training[,i],2),g=5 )
print("======")
print(i)
print(ncol(training))
 y=  qplot(training$index2,training$CompressiveStrength,data=training,colour=training[,ncol(training)],main = names(training)[i])
  print(y) 
 }

 
featurePlot(x=training[,c(1,4,8)],y= training$CompressiveStrength,plot="pairs")
featurePlot(x=training[,c(1,2,5)],y= training$CompressiveStrength,plot="pairs")


fit <- train(CompressiveStrength ~. , method="lm" , data=training)

plot(fit$finalModel$residuals,pch=19)



#q2q3
hist(training$Superplasticizer)
mean(training$Superplasticizer)
sd(training$Superplasticizer)

hist((training$Superplasticizer-mean(training$Superplasticizer))/sd(training$Superplasticizer))
#q2q4  et q5 ####

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
interest <- grep("^IL",names(training),ignore.case=TRUE,value=TRUE)
interest[13]="diagnosis"
corl = abs(cor(training[,names(training) %in% interest]))
  intData <- training[,names(training) %in% interest]
pca <- prcomp(intData[,-1],scale=TRUE,center = TRUE)
plot(pca,ylim = c(0,5))
sd(intData[,3])
prep <- preProcess(intData[,-1],method=c("center","scale","pca"),pcaComp=7)
trainPC <- predict(prep,intData[,-1])
modelfit <- train(intData$diagnosis ~.,method="glm",data=intData)
modelfitPC <- train(intData$diagnosis~.,trainPC)

testingIL <- testing[,names(testing) %in% interest]
testPC <- predict(prep,testingIL[,-1])

confusionMatrix(testingIL$diagnosis,predict(modelfitPC,testPC))

confusionMatrix(testingIL$diagnosis,predict(modelfit,testingIL))

######q3q1##
library(caret)
set.seed(125)
library(AppliedPredictiveModeling)


data(segmentationOriginal)
 inTrain = createDataPartition(segmentationOriginal$Case,p=2/3,list=FALSE)
training=segmentationOriginal[inTrain,]
test=segmentationOriginal[-inTrain,]
 
cartFit = train(Class ~ . , method="rpart",data=training)
print(cartFit$finalModel)
# plot(cartFit$finalModel,uniform=TRUE,main= "CART: Tree")
# text(cartFit$finalModel,use.n=TRUE,all=TRUE,cex=.8)
pred = predict(cartFit,newdata = test)

library(rattle)
library(rpart)
fancyRpartPlot(cartFit$finalModel)



##q3q3##
library(caret)
library(rattle)
library(rpart)
set.seed(125)
library(pgmm)
data(olive)

olive = olive[,-1]
olivef = olive
test =as.data.frame(t(colMeans(olive[,c(2:9)])))

olive2fit <- train(factor(olive$Area) ~. ,method="rpart",data=olive)
print(olive2fit$finalModel)
fancyRpartPlot(olive2fit$finalModel)



olivef$Area = factor(olivef$Area)
olivefFit=train(Area ~ ., method="rpart",data=olivef)
fancyRpartPlot(olivefFit$finalModel)

print(olivefFit$finalModel)

 
pred <-predict(olivefFit,newdata=test)
pred

pred2 <-predict(olive2fit,newdata=test)
pred2




##q3q4
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


# norm01 <-function(data,vect){
#   for (j in vect){
#     data[ !(is.na(data[,j])),j] =
#       (data[!(is.na(data[,j])),j]-min(data[!(is.na(data[,j])),j])) /
#       (max(data[!(is.na(data[,j])),j])-min(data[!(is.na(data[,j])),j]))
#    dt <- data[ !(is.na(data[,j])),j]
#    print("----------")
#    print(j)
# print(dt)
#   }
# return
# }
# 
# mydata <- training
# c <- c(1,5)
# data_norm <- norm01(mydata,c)
# boxplot(data_norm[,c])
# c <- c(8,9,17,18)
# boxplot(log(mydata[,c]))
