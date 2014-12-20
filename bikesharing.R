#--------------------------------------------------------
#  Conditional Inference Tree for kaggle-bike-sharing
#  This method uses cross validation.
#  Taposh Roy
#  @taposh_dr
#--------------------------------------------------------

library(caret)
library(randomForest)
library('party')
library("MASS")

sink("bikesharing.log", split = T)
setwd("/Users/taposh/workspace/kagglebikesharing")
#read in train/test

train <- read.csv("train.csv")
test <- read.csv("test.csv")

str(train)

#factorize training set
train_factor <- train
train_factor$weather <- factor(train$weather)
train_factor$holiday <- factor(train$holiday)
train_factor$workingday <- factor(train$workingday)
train_factor$season <- factor(2*train$season)
#train_factor$casual <- factor(train$casual)
#train_factor$registered <- factor(train$registered)

#factorize test set
test_factor <- test
test_factor$weather <- factor(test$weather)
test_factor$holiday <- factor(test$holiday)
test_factor$workingday <- factor(test$workingday)
test_factor$season <- factor(2*test$season)
#test_factor$casual <- factor(test$casual)
#test_factor$registered <- factor(test$registered)

#create time column by stripping out timestamp
train_factor$time <- substring(train$datetime,12,20)
test_factor$time <- substring(test$datetime,12,20)
str(train_factor)

#factorize new timestamp column
train_factor$time <- factor(train_factor$time)
test_factor$time <- factor(test_factor$time)


#create day of week column
train_factor$day <- weekdays(as.Date(train_factor$datetime))
train_factor$day <- as.factor(train_factor$day)
test_factor$day <- weekdays(as.Date(test_factor$datetime))
test_factor$day <- as.factor(test_factor$day)

aggregate(train_factor[,"count"],list(train_factor$day),mean)
aggregate(train_factor[,"casual"],list(train_factor$day),mean)
aggregate(train_factor[,"registered"],list(train_factor$day),mean)


#create Sunday variable
train_factor$sunday[train_factor$day == "Sunday"] <- "1"
train_factor$sunday[train_factor$day != "1"] <- "0"

test_factor$sunday[test_factor$day == "Sunday"] <- "1"
test_factor$sunday[test_factor$day != "1"] <- "0"

#convert to factor
train_factor$sunday <- as.factor(train_factor$sunday)
test_factor$sunday <- as.factor(test_factor$sunday)

#convert time and create $hour as integer to evaluate
train_factor$hour<- as.numeric(substr(train_factor$time,1,2))
test_factor$hour<- as.numeric(substr(test_factor$time,1,2))

#create daypart column, default to 4 to make things easier for ourselves
train_factor$daypart <- "4"
test_factor$daypart <- "4"

#4AM - 9AM = 1
train_factor$daypart[(train_factor$hour < 10) & (train_factor$hour > 3)] <- 1
test_factor$daypart[(test_factor$hour < 10) & (test_factor$hour > 3)] <- 1

#10AM - 3PM = 2
train_factor$daypart[(train_factor$hour < 16) & (train_factor$hour > 9)] <- 2
test_factor$daypart[(test_factor$hour < 16) & (test_factor$hour > 9)] <- 2

#4PM - 9PM = 3
train_factor$daypart[(train_factor$hour < 22) & (train_factor$hour > 15)] <- 3
test_factor$daypart[(test_factor$hour < 22) & (test_factor$hour > 15)] <- 3

#convert daypart to factor
train_factor$daypart <- as.factor(train_factor$daypart)
test_factor$daypart <- as.factor(test_factor$daypart)

#convert hour back to factor
train_factor$hour <- as.factor(train_factor$hour)
test_factor$hour <- as.factor(test_factor$hour)

#removing non-int values
train_factor <- train_factor[-11]
test_factor <- test_factor[-11]

#complex factors
train_factor <- cbind(train_factor,sin(2*pi*train_factor$temp/365*24),cos(2*pi*train_factor$temp/365*24),sin(2*pi*train_factor$atemp/365*24),cos(2*pi*train_factor$atemp/365*24),sin(2*pi*train_factor$humidity/365*24),cos(2*pi*train_factor$humidity/365*24),sin(2*pi*train_factor$windspeed/365*24),cos(2*pi*train_factor$windspeed/365*24)) 
test_factor <- cbind(test_factor,sin(2*pi*test_factor$temp/365*24),cos(2*pi*test_factor$temp/365*24),sin(2*pi*test_factor$atemp/365*24),cos(2*pi*test_factor$atemp/365*24),sin(2*pi*test_factor$humidity/365*24),cos(2*pi*test_factor$humidity/365*24),sin(2*pi*test_factor$windspeed/365*24),cos(2*pi*test_factor$windspeed/365*24)) 


testdatetimes <- test_factor[1]
test_factor<-test_factor[-1]
traindatetimes <- train_factor[1]
train_factor<-train_factor[-1]


head(train_factor,10)
head(test_factor,10)

#get the count off
train_factor<-train_factor[-10]

train_factorInv<-ginv(as.matrix(train_factor))
ACT<-BikeInv %*% train_factor[10]
Predictions<-cbind(testdatetimes,exp(test_factor %*% ACT))
head(Predictions)
Predictions[Predictions[,2]<0,2]<-0
head(Predictions,20)

colnames(Predictions)<-c("datetime","count")
write.table(Predictions,file="Submission_v0003.csv",row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)

# 
# # define training control
# train_control <- trainControl(method="cv", number=2)
# 
# #########  COUNT ############
# #build our formula
# formula <- count ~.
# #formula <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypart +  windspeed 
# #formula <- count ~  workingday + temp +hour+daypart
# 
# # train the model 
# model <- train(formula, data=train_factor, trControl=train_control, method="ctree")
# #predict for count using train 
# predict.train.count <-predict(model, train_factor[-12])
# 
# actual <- data.frame(train_factor[12])
# sim <-data.frame(predict.train.count)
# #build a dataframe with our results
# valid.train.count <- data.frame(datetime = train$datetime, count=predict.train.count)
# 
# 
# colnames(Predictions)<-c("datetime","count")
# write.table(Predictions,file="Submission_v004.csv",row.names=FALSE,quote=FALSE,sep=",",col.names=TRUE)
# 
# 
# 
# # library(hydroGOF)
# # rmse(predict.train.count, actual)
# # 
# # #### Registered ############
# # formula.reg <- registered ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypart +  windspeed
# # # train the model 
# # model.reg <- train(formula.reg, data=train_factor, trControl=train_control, method="ctree")
# # #predict for registered using train 
# # predict.train.reg <-predict(model.reg, train_factor[-12])
# # #build a dataframe with our results
# # valid.train.reg <- data.frame(datetime = train$datetime, reg=predict.train.reg)
# # 
# # 
# # ####Formula_Causal ############
# # formula.casual <- casual ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypart + windspeed
# # # train the model 
# # model.casual <- train(formula.casual, data=train_factor, trControl=train_control, method="ctree")
# # #predict casual using train 
# # predict.train.casual <-predict(model.casual, train_factor[-12])
# # #build a dataframe with our results
# # valid.train.causal <- data.frame(datetime = train$datetime, casual=predict.train.casual)
# # 
# # #############################
# # 
# # 
# # #count
# # head(train_factor[-12],10)
# # 
# # # Compare Training Results
# # 
# # compare <- data.frame(count=round((predict.train.count)),reg=round((predict.train.reg)),casual=round((predict.train.casual)),real=train_factor[12])
# # head(compare,10)
# # 
# # compareX <- cbind(round((predict.train.count))-round((predict.train.casual)))
# # head(compareX,10)
# # 
# # compareTots <-data.frame(combination=compareX,count=predict.train.count,real=train_factor[12])
# # head(compareTots,120)
# # #compare and see if the results match 
# # #write.csv(valid.train.ctree, file="train_predict.csv",row.names=FALSE)
# # 
# # #########################
# # Test the model
# #########################
# 
# #run model against test data set
# predict.test.count <- predict(model, test_factor)
# predict.test.reg <- predict(model.reg, test_factor)
# predict.test.casual <- predict(model.casual, test_factor)
# 
# #build a dataframe with our results
# compare_test <- data.frame(datetime = test$datetime, count=predict.test.count,reg=predict.test.reg,casual=predict.test.casual)
# head(compare_test,10)
# 
# 
# 
# ###Submission#######################
# #Bias <- 0.4
# #newsub <-(predict.ctree)
# #x <- cbind(predict.ctree,log(predict.reg),(predict.casual))
# submit <-data.frame(datetime = test$datetime, count=predict.test.count)
# head(submit)
# submit$count <- ifelse(submit$count < 0, 1, submit$count)
# #write results to .csv for submission
# write.csv(submit, file="submission_test_v5.csv",row.names=FALSE)
