library(caret)
library(C50)
library(tree)
mean(companydata$Sales)
#mean 7.490 of sales column.
 
#revalue
Sales_ave<-NULL
Sales_ave <- ifelse(companydata$Sales > 7.490,1,0)
companydata[,"Sales_ave"] <- Sales_ave
View(companydata)

#converting in factors
companydata$ShelveLoc <- as.factor(companydata$ShelveLoc)
companydata$Urban <- as.factor(companydata$Urban)
companydata$US <- as.factor(companydata$US)
companydata$Sales_ave<-as.factor(companydata$Sales_ave)

#Sales column define
sales_high <- companydata[companydata$Sales_ave == "1",] 
sales_low <- companydata[companydata$Sales_ave == "0",]


#Splitting into train and test data
data_train <- rbind(sales_high[1:150,], sales_low[1:150,])
data_test <- rbind(sales_high[151:199,], sales_low[151:201,])

#model Building
model<-C5.0(data_train[,-c(12)], data_train$Sales_ave)
plot(model)



#Prediction on train data

pred_train<-predict.C5.0(model,data_train[,-12])
a<-table(data_train$Sales_ave,pred_train)
sum(diag(a))/sum(a)
CrossTable(data_train$Sales_ave, pred_train)

#Prediction on test data
pred_test <- predict.C5.0(model, newdata = data_test)
a<-table(data_test$Sales_ave,pred_test)
sum(diag(a))/sum(a)
library(gmodels)
CrossTable(data_test$Sales_ave, pred_test)
confusionMatrix(data_test$Sales_ave, pred_test)
