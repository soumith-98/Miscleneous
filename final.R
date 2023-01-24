

##################################2#########################################################


rm(list = ls()) # Clerm(list = ls()) # Clear Environment 
library(neuralnet) #Call Libraries 
library(nnet) 
getwd()


setwd("C:/Users/Soumith/Desktop/rr") #set Working directory 
load("cleveland.Rdata") #Loading the data 
head(cleveland)


which(is.na(infert) == TRUE)


my_data <- cleveland 
head(my_data)


my_data$diag1 = as.numeric(my_data$diag1)-1
my_data$gender = as.numeric(my_data$gender)-1
my_data$fbs = as.numeric(my_data$fbs)-1
my_data$exang = as.numeric(my_data$exang)-1
slope.matrix <- model.matrix(~ slope - 1, data = my_data)
my_data <- data.frame(my_data, slope.matrix)
thal.matrix <- model.matrix(~ thal - 1, data = my_data)
my_data <- data.frame(my_data, thal.matrix)
my_data <- my_data[, -c(3,7,11,13,15)]



set.seed(12) #Dividing the data into training and test sets.
train_indis <- sample(1:length(my_data[,1]), 2/3*length(my_data[,1]), replace = FALSE)
train <- my_data[train_indis, ]
test <- my_data[-train_indis, ]


y_true_train = train$diag1 #Fitting the neural network
y_true_test = test$diag1
train_err_store <- c()
test_err_store <- c()


for (i in 1:4){
  nn <- neuralnet(diag1 ~ ., data = train, hidden = i, stepmax = 10^9, err.fct = "ce", linear.output = FALSE)
  # calculate the train error
  pred <- predict(nn, newdata = train)
  y_hat_train <- round(pred)
  train_err <- length(which(y_true_train != y_hat_train))/length(y_hat_train) 
  train_err_store <- c(train_err_store, train_err)
  pred <- predict(nn, newdata = test)
  y_hat_test <- round(pred)
  test_err <- length(which(y_true_test != y_hat_test))/length(y_hat_test)
  test_err_store <- c(test_err_store, test_err)
} 
train_err_store
test_err_store

######################################CART MODEL##########################################

library(rpart)
library(caret)
ctrl_model <- rpart.control(minsplit=15, minbucket = 5, cp = 0, xval = 10)
fit.cleve <- rpart(diag1 ~ ., data = train, method = "class",control = ctrl_model)
plot(fit.cleve, branch = .4, uniform = T, compress = T) 
text(fit.cleve, use.n = T, all = T, cex = 1)
train_pred = predict(fit.cleve, newdata = train, type="class")
conf = confusionMatrix(as.factor(train_pred), as.factor(train$diag1))
conf$table
mean(train_pred != train$diag1)
test_pred = predict(fit.cleve, newdata = test, type="class") #prediction for test
conf = confusionMatrix(as.factor(test_pred), as.factor(test$diag1)) 
length(test_pred)
conf$table
mean(test_pred != test$diag1)

###################################RANDOM FOREST############################################
library(randomForest)
train$diag1 =as.factor(train$diag1)
cleve_rf_fit <- randomForest(diag1~., data = train, n.tree = 100000)
cleve_rf_fit
varImpPlot(cleve_rf_fit)

importance(cleve_rf_fit)

############################PREDICTION FOR TRAIN############################################
train_pred_rf = predict(cleve_rf_fit, newdata = train, type="class") 
rfconf = confusionMatrix(as.factor(train_pred_rf), as.factor(train$diag1)) 
rfconf$table

mean(train_pred_rf != train$diag1)



###############################PREDICTION FOR TEST########################################
test_pred_rf = predict(cleve_rf_fit, newdata = test, type="class")
rfconf = confusionMatrix(as.factor(test_pred_rf), as.factor(test$diag1))
rfconf$table

mean(test_pred_rf != test$diag1)


####################################### #The neural network and random forest models are performing similary and are better than CART model ####################################

####################################################3###################################################

######################################################3A#################################################


rm(list=ls()) #clear Environment
# installing required packages
library(ISLR) #call packages 
library(tidyverse)


library(ggthemes) 
library(caret) 
library(e1071) 
set.seed(100) 
data('OJ') #call data

inTrain <- sample(nrow(OJ), 800, replace = FALSE) # create trainig and test data set
training <- OJ[inTrain,] 
testing <- OJ[-inTrain,]

#####################################################3B################################################
support_vector_linear <- svm(Purchase ~ ., data = training, kernel = 'linear', cost = 0.01) #support vector classifier to the training data using cost = 0.01
summary(support_vector_linear)

###################################################3B################################################
#Training error rate and test error rate
postResample(predict(support_vector_linear, training), training$Purchase)

postResample(predict(support_vector_linear, testing), testing$Purchase)

##############################################3D################################################

#the tune() function to select an optimal cost

support_vector_linear_tune <- train(Purchase ~ ., data = training, method = 'svmLinear2', trControl = trainControl(method = 'cv', number = 10), preProcess = c('center', 'scale'), tuneGrid = expand.grid(cost = seq(0.01, 10, length.out = 20)))
support_vector_linear_tune


###############################################3E##############################################
postResample(predict(support_vector_linear_tune, training), training$Purchase)
postResample(predict(support_vector_linear_tune, testing), testing$Purchase)

####################################################3F##########################################

support_vector_radial <- svm(Purchase ~ ., data = training, method = 'radial', cost = 0.01)
summary(support_vector_radial)
postResample(predict(support_vector_radial, training), training$Purchase)

postResample(predict(support_vector_radial, testing), testing$Purchase)

support_vector_radial_tune <- train(Purchase ~ ., data = training, method = 'svmRadial', trControl = trainControl(method = 'cv', number = 10), preProcess = c('center', 'scale'), tuneGrid = expand.grid(C = seq(0.01, 10, length.out = 20), sigma = 0.05691))
support_vector_radial_tune
postResample(predict(support_vector_radial_tune, training), training$Purchase)
postResample(predict(support_vector_radial_tune, testing), testing$Purchase)


###################################################3G################################################
support_vector_poly <- svm(Purchase ~ ., data = training, method = 'polynomial', degree = 2, cost = 0.01) 
summary(support_vector_poly)
postResample(predict(support_vector_poly, training), training$Purchase)
postResample(predict(support_vector_poly, testing), testing$Purchase)


support_vector_poly_tune <- train(Purchase ~ ., data = training, method = 'svmPoly', trControl = trainControl(method = 'cv', number = 10), preProcess = c('center', 'scale'), tuneGrid = expand.grid(degree = 2, C = seq(0.01, 10, length.out = 20), scale = TRUE))

support_vector_poly_tune
postResample(predict(support_vector_poly_tune, training), training$Purchase)
postResample(predict(support_vector_poly_tune, testing), testing$Purchase)

##########################################3H############################################################
#Overall the models are very similar, but the radial kernel does best by a small margin

###########################################5###########################################################
getwd()
setwd("/Users/sandeepb/Desktop/R Homework/Final Homework")
library(neuralnet)
library(nnet)
load("spam.RData")
library("randomForest")
train_indis = sample(1:length(spam[,1]), 2/3*length(spam[,1]), replace = FALSE)
train = spam[train_indis, ]
test = spam[-train_indis, ]
train$spam <- factor(as.character(train$spam))
test_err = NULL
OOB = NULL
for(i in 1:58){
  rf_fit = randomForest(spam ~., data=train,mtry=i,ntree=1000) 
  pred_spam= predict(rf_fit, test, type = "class") 
  OOB[i]=rf_fit$err.rate[1000,1] 
  test_err[i]= mean(pred_spam != test$spam)
}


test_err

OOB


plot (test_err,col = "red", type= "b", xlab = "M",ylab = "Error and OOB values",main="OOB & Test Error")
lines(OOB,col=" blue",type="b")
legend("topright",c("test Error","OOB"),fill=c("red","blue"))


min(test_err)

min(OOB)








