test = read.csv("pml-testing.csv")
train = read.csv("pml-training.csv")
 library(caret)
library(rattle)

test1 =test[1,!is.na(test[1,])]
test2 <- test1[1,c(8:59)]
testUsable = test[,names(test) %in% names(test2)]
names =cbind(names(test2),"classe")
trainUsable=train[,names(train) %in% names]

indTrain =createDataPartition(trainUsable$classe,p=.6,list=FALSE)

traindata=trainUsable[indTrain,]
testdata=trainUsable[-indTrain,]

fitsimple= train(classe ~ . ,method="rpart",data=traindata)
varimp =varImp(fitsimple)
importantColumns = c("accel_arm_x",      
                     "accel_belt_z",      
                    "accel_dumbbell_y"  , 
                    "accel_forearm_x"  ,  
                     "magnet_arm_x"     ,  
                    "magnet_belt_y"    ,  
                     "magnet_dumbbell_x" , 
                    "magnet_dumbbell_y", 
                     "magnet_dumbbell_z" , 
                    "pitch_belt"        , 
                     "pitch_forearm"     , 
                    "roll_belt"         , 
                     "roll_dumbbell"     , 
                    "roll_forearm"     , 
                     "total_accel_belt"  , 
                    "yaw_belt" )

namesImp = cbind(importantColumns,"classe")
traindata = traindata[,names(traindata) %in% namesImp]
testdata=testdata[,names(testdata) %in% namesImp]

fit= train(classe ~ . ,method="rf",data=traindata)
print(fit$finalModel)

pred =predict(fit,newdata=testdata)
cfmat =confusionMatrix(testdata$classe,pred)
cfmat$overall[1]

preV=predict(fit,test)

for(i in c(1:20)){
  testCase = as.data.frame(test[i,])
  pred= predict(fit,as.data)
  print(pred)
}


