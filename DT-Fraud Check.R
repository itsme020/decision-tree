library(caret)
library(C50)
library(tree)
#upload data path
fraud <- read.csv("C:/Users/user/Downloads/Fraud_check.csv")

taxableinc <- ifelse(fraud$Taxable.Income<=30000,1,0)
fraud[,"taxableinc"] <- taxableinc

#converting in factor
fraud$Undergrad <- as.factor(fraud$Undergrad)
fraud$Marital.Status <- as.factor(fraud$Marital.Status)
fraud$Urban <- as.factor(fraud$Urban)
fraud$taxableinc <- as.factor(fraud$taxableinc)

fraud_risky <- fraud[fraud$taxableinc == "1",] 
fraud_not_risky <- fraud[fraud$taxableinc == "0",]

Fraud_train <- rbind(fraud_risky[1:95,], fraud_not_risky[1:357,])
fraud_test <- rbind(fraud_risky[96:124,], fraud_not_risky[357:476,])



#Model Buildingr
model<-C5.0(Fraud_train[,-c(7)],Fraud_train$taxableinc)#Trial-Boosting parameter
summary(model)
plot(model)

# Training accuracy
pred_train <- predict(model,Fraud_train)
mean(Fraud_train$taxableinc==pred_train) 
library(caret)
confusionMatrix(pred_train,Fraud_train$taxableinc)

pred_test <- predict(model,newdata=fraud_test) # predicting on test data
mean(pred_test==fraud_test$taxableinc) 
confusionMatrix(pred_test,fraud_test$taxableinc)
library(gmodels)

# Cross tablez
CrossTable(fraud_test$taxableinc,pred_test)



##### Using tree function 
library(tree)
# Building a model on training data 
fraud_tree <- tree(taxableinc~.,data=Fraud_train)
plot(fraud_tree)
summary(fraud_tree)

text(fraud_tree,pretty = 0)

# Predicting the test data using the model
fraud_tree <- as.data.frame(predict(fraud_tree,newdata=fraud_test))
fraud_tree["final"] <- NULL
pred_test <- predict(fraud_tree,newdata=fraud_test)

#Begging
acc<-c()
for(i in 1:1000)
{
  print (i)
  ##data Partition
  inTraininglocal<-createDataPartition(fraud$taxableinc,p=.85,list=F)
  training1<-fraud[inTraininglocal,]
  testing<-fraud[-inTraininglocal,]
  ##model building
  fittree<- C5.0(training1$taxableinc~.,data=training1,trials=20)
  ##predicting
  pred<-predict.C5.0(fittree,testing[,-7])
  a<-table(testing$taxableinc,pred)
  ##accuracy
  acc<-c(acc,sum(diag(a))/sum(a))
 }
summary(acc)  
