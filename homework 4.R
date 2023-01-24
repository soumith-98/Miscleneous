rm(list=ls()) ### clean environment
library(class) ###installing packages
library(leaps)
library(ISLR2)
data(Boston) ### Dataset
head(Boston)
set.seed(122)
indic<- sample(1:nrow(Boston),75, nrow(Boston)) ###split data into test and train data
btrain <- Boston[indic,]
btest <- Boston[-indic,]
model <- regsubsets(medv~., data = btrain, nvmax = 14)
df<- summary(model)
plot(model)
df$bic
df$cp
df$aic
which.min(df$cp)
which.min(df$bic)
predict.regsubsets = function(object, newdata, id, ...) { ### predict function
    form = as.formula(object$call[[2]])
    mat = model.matrix(form, newdata)
    coefi = coef(object, id = id)
    mat[, names(coefi)] %*% coefi
} 
k = 5 
set.seed(111)
folds=sample (1:k,nrow(Boston),replace=TRUE)
cv.errors =matrix (NA,k,14, dimnames =list(NULL , paste (1:14) ))


for(j in 1:k){
  best.fit=regsubsets (medv~.,data=Boston [folds!=j,],
                       nvmax=12)
  for(i in 1:12){
    pred=predict (best.fit ,Boston[folds ==j,],id=i)
    cv.errors[j,i]= mean( ( Boston$medv[ folds==j]-pred)^2)
  }
}
k = 10
set.seed(111)
folds=sample (1:k,nrow(Boston),replace=TRUE)
cv.errors =matrix (NA,k,14, dimnames =list(NULL , paste (1:14) ))


for(j in 1:k){
  best.fit=regsubsets (medv~.,data=Boston [folds!=j,],
                       nvmax=12)
  for(i in 1:12){
    pred=predict (best.fit ,Boston[folds ==j,],id=i)
    cv.errors[j,i]= mean( ( Boston$medv[ folds==j]-pred)^2)
  }
}
library(bootstrap)
boot.f <- function(X_data, Y_data){
  lsfit(X_data,Y_data)
}
boot.pred <- function(fit,X_data){
  cbind(1,X_data)%*%fit$coef
}
boot.sq.er <- function(y,yhat){
  (y-yhat)^2
}

X_data <- Boston[,1:12]
Y_data <- Boston[,13]

select <- df$outmat
err_store <- c()
for (i in 1:12){
  tmp <- which(select[i,] == "*")
  res <- bootpred(X_data[,tmp],Y_data,nboot =1000,boot.f,boot.pred,boot.sq.er)
  err_store <- c(err_store, res[[3]])
}
which.min(err_store)





2##########################



rm(list=ls()) ### clean environment
library(tidyverse)




library(rpart)
library(class)
library(corrplot)

data("Boston")  ### viewing the boston data
summary(Boston)

attach(Boston) ### loading
crime <- rep(0, length(crim)) ### creating crim variable
crime[crim > median(crim)] <- 1 
Boston = data.frame(Boston,crime)
train = 1:(dim(Boston)[1]/2) ### spliting the data into training set
test = (dim(Boston)[1]/2 + 1):dim(Boston)[1]  ### splitting the dataset into test set
Boston.train = Boston[train, ]
Boston.test = Boston[test, ]
crime.test = crime[test]
corrplot(cor(Boston), method="circle" ,order = "alphabet")  ### Determining any associations to crimeset

set.seed(150)





Boston.fit <-glm(crime~ nox+tax+dis+rad+age+indus, data=Boston.train,family=binomial)
Boston.probs = predict(Boston.fit, Boston.test, type = "response") 
Boston.pred = rep(0, length(Boston.probs))
Boston.pred[Boston.probs > 0.5] = 1
table(Boston.pred, crime.test) ### table view



mean(Boston.pred != crime.test)

summary(Boston.fit)


library(MASS)


Boston.ldafit <-lda(crime~ indus+nox+age+dis+rad+tax, data=Boston.train,family=binomial)
Bostonlda.pred = predict(Boston.ldafit, Boston.test)
table(Bostonlda.pred$class, crime.test) ### table view

mean(Bostonlda.pred$class != crime.test)









train.K=cbind(indus,nox,age,dis,rad,tax)[train,]
test.K=cbind(indus,nox,age,dis,rad,tax)[test,]
Bosknn.pred=knn(train.K, test.K, crime.test, k=1)
table(Bosknn.pred,crime.test)

mean(Bosknn.pred !=crime.test)


train.K=cbind(indus,nox,age,dis,rad,tax)[train,]
test.K=cbind(indus,nox,age,dis,rad,tax)[test,]
Bosknn.pred=knn(train.K, test.K, crime.test, k=100)
table(Bosknn.pred,crime.test)

mean(Bosknn.pred !=crime.test)


library(tree)

library(rpart.plot)
library(rpart)
Boston.tree <- rpart(crime ~ ., data = Boston.train,  cp = 0.001)
rpart.plot(Boston.tree, type = 1, fallen.leaves = FALSE)


plotcp(Boston.tree)

printcp(Boston.tree)

prune.tree1 <- prune(Boston.tree, cp = 0.0085790)
prune.tree1

boston.train.pred.tree = predict(prune.tree1)
boston.test.pred.tree = predict(prune.tree1, Boston.test)
mean((boston.test.pred.tree - Boston.test$crime)^2)

mean((boston.train.pred.tree - Boston.train$crime)^2)

library(ggthemes) ### calling libraries
attach(Auto) ### attaching data



mpg01 = rep(0, length(mpg)) ### Creating binary variable mpg01 with the condition
mpg01[mpg > median(mpg)] = 1 ### condition
Auto = data.frame(Auto, mpg01) ### Loading the data
head(Auto)


cor(Auto[, -9])  ### correlating


pairs(Auto)


boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs mpg01") ### Box plots for all the variables with mpg01


boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")

boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs mpg01")
 
boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs mpg01")

boxplot(weight ~ mpg01, data = Auto, main = "Weight vs mpg01")

boxplot(year ~ mpg01, data = Auto, main = "Year vs mpg01")


train = (year%%2 == 0) ### splitting dataset w.r.t year
Auto.train = Auto[train, ] ### Training set
Auto.test = Auto[!train, ] ### Test set
mpg01.test = mpg01[!train] ### mpg01 testset


lda.fit = lda(mpg01 ~ cylinders + displacement + horsepower + weight, data = Auto, subset = train) ### LDA on the training data in order to predict mpg01
lda.pred = predict(lda.fit, Auto.test) 
lda.class = lda.pred$class
table(lda.class, mpg01.test)

mean(lda.class != mpg01.test) ### Test error 0.1263736


qda.fit = qda(mpg01 ~ cylinders + displacement + horsepower + weight, data = Auto, subset = train) ### QDA on the training data in order to predict mpg01
qda.fit


qda.pred = predict(qda.fit, Auto.test)
qda.class = qda.pred$class
table(qda.class, mpg01.test)


mean(qda.class != mpg01.test) ### Test error 0.1318681


glm.fit = glm(mpg01 ~ cylinders + displacement + horsepower + weight, data = Auto, subset = train, family = binomial) ### logistic regression on the training data in order to predict mpg01
summary(glm.fit)$coef


glm.probs = predict(glm.fit, Auto.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > .5] = 1
table(glm.pred, mpg01.test)



mean(glm.pred != mpg01.test) ### Test error 0.1208791




### KNN on the training data, with several values of KK, in order to predict mpg01

train.X = cbind(cylinders, displacement, horsepower, weight)[train, ]
test.X = cbind(cylinders, displacement, horsepower, weight)[!train, ]
train.mpg01 = mpg01[train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.mpg01, k = 1) ### k=1
table(knn.pred, mpg01.test )


mean(knn.pred != mpg01.test) ### Test error  0.1538462


knn.pred = knn(train.X, test.X, train.mpg01, k = 10) ### k=10
table(knn.pred, mpg01.test )


mean(knn.pred != mpg01.test) ### Test error  0.1648352


knn.pred = knn(train.X, test.X, train.mpg01, k = 100) ### k =100
table(knn.pred, mpg01.test )


mean(knn.pred != mpg01.test) ### Test error 0.1428571

knn.pred = knn(train.X, test.X, train.mpg01, k = 500) ### k =500


table(knn.pred, mpg01.test )

mean(knn.pred != mpg01.test) ### Test error  0.5494505


### K = 100 seems to be the best KNN model with an accuracy of 0.1428571 at the rate of 14.28%





