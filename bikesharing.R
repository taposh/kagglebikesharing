#--------------------------------------------------------
#  Conditional Inference Tree for kaggle-bike-sharing
#  This method uses cross validation.
#  Taposh Roy
#  @taposh_dr
#--------------------------------------------------------

setwd("/Users/taposhdr/workspace/decision_science/kaggle/bikeSharing/")
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


## Validations


library(caret)
library(randomForest)
library('party')
#build our formula
formula <- count ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypart

# define training control
train_control <- trainControl(method="repeatedcv", number=10)
# train the model 
model <- train(formula, data=train_factor, trControl=train_control, method="cforest")

#build our model
#model <- ctree(formula, data=train_factor)

#count
head(train_factor[-12])

#######################
#predict for train 
predict.train.ctree <-predict(model, train_factor[-12])
#build a dataframe with our results
valid.train.ctree <- data.frame(datetime = train$datetime, count=predict.train.ctree)
write.csv(valid.train.ctree, file="train_predict.csv",row.names=FALSE)
#########################

#run model against test data set
predict.ctree <- predict(model, test_factor)
#build a dataframe with our results
submit.ctree <- data.frame(datetime = test$datetime, count=predict.ctree)


####Formula_Causal ############

formula.casual <- casual ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypart
# train the model 
model.casual <- train(formula.casual, data=train_factor, trControl=train_control, method="cforest")
#run model against test data set
predict.casual <- predict(model.casual, test_factor)
#build a dataframe with our results
submit.casual <- data.frame(count=predict.casual)

#########################
#data file for validation
write.csv(train_factor, file="train_factor_bike.csv",row.names=FALSE)
write.csv(train_factor, file="test_factor_bike.csv",row.names=FALSE)

#######################
#predict for train CASUAL
#predict.train.casual <-predict(model.casual, train_factor[-12])
#build a dataframe with our results
#valid.train.casual <- data.frame(datetime = train$datetime, count=predict.train.ctree)
#write.csv(valid.train.casual, file="train_predict_casual.csv",row.names=FALSE)
#########################

####Formula_Registered ############
formula.reg <- registered ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypart
# train the model 
model.reg <- train(formula.reg, data=train_factor, trControl=train_control, method="cforest")
#run model against test data set
predict.reg <- predict(model.reg, test_factor)
#build a dataframe with our results
#submit.reg <- data.frame(count=predict.reg)



head(predict.ctree)
#head(predict.casual)
#head(predict.reg)
part <- rowSums(cbind(predict.reg,predict.casual,0.42))
head(part)
#head(rowSums(cbind(predict.ctree,log(predict.reg),predict.casual))))
diff <- (predict.ctree -part)


view.all <- data.frame(datetime = test$datetime,casual=predict.casual,registered=predict.reg,count=predict.ctree,part=part,diff=diff)
head(view.all, 10)

rmse <- sqrt(diff*diff)
error <- mean(rmse)
error

###Viewthefile#################
write.csv(view.all, file="view.csv",row.names=FALSE)

###Submission#######################
Bias <- 0.4
newsub <-(predict.ctree)
#x <- cbind(predict.ctree,log(predict.reg),(predict.casual))
submit <-data.frame(datetime = test$datetime, count=newsub)
head(submit)
submit$count <- ifelse(submit$count < 0, 0, submit$count)
#write results to .csv for submission
write.csv(submit, file="submit_ctree_cforest.csv",row.names=FALSE)
