rm(list=ls())

library(mvtnorm)
library(e1071)
library(plotrix)
library(splines)
library(MASS)
library(ISLR)

## create a training data set
n = 45
set.seed(1)

## Correlation between the two predictors is 0.7
sigma = matrix(0.7, 2, 2)
diag(sigma) = 1

## Generate data
x = rmvnorm(n, sigma=sigma)
y = 2*(x[,1]^2 + x[,2]^2 < 1) - 1

## create grid of points for plots
gridX1 = seq(-2.5, 2.5, length=50)
gridX2 = seq(-2.5, 2.5, length=50)
gridX = expand.grid(gridX1, gridX2)

## Create data frame with squared values
gridData = data.frame(x1 = gridX[,1], x2 = gridX[,2],
                      x1s = gridX[,1]^2, x2s = gridX[,2]^2)

## plot the data
plot(x, col=3+y, pch=2, xlab=expression(X[1]),xlim=c(-2,2), ylim=c(-2,2),
     ylab=expression(X[2]), main="Nonlinear decision boundary", asp=1)
draw.circle(0,0, radius=1)

## Create training data frame for SVM model
dat = data.frame(x1 = x[,1], x2 = x[,2], y=as.factor(y))

## Let's try a range of cost variables and do cross validation to choose among them
tune.svm = tune(svm, y ~ ., data=dat, kernel="linear",
                ranges=list(cost=c(0.0001, 0.001, 0.01, 0.1, 1, 
                                   2,3,5,10,25,50,100)))

## Use the model identified as the best by cross validation
fit = tune.svm$best.model

## Use SVM to predict on our grid
predGridLinSVM = as.numeric(as.character(predict(fit, gridData)))

## Plot the results
plot(gridX[,1], gridX[,2], cex=0.2, col=3+predGridLinSVM,
     xlim=c(-2,2), ylim=c(-2,2),asp=1,
     xlab=expression(X[1]), ylab=expression(X[2]), main="Linear SVM")
draw.circle(0,0, radius=1)
points(x, col=3+y, pch=2)

## Let's try to include squared terms
dat = data.frame(x1 = x[,1], x2 = x[,2], 
                 x1s = x[,1]^2, x2s = x[,2]^2, 
                 y=as.factor(y))

## Let's try a range of cost variables
tune.svm2 = tune(svm, y ~ ., data=dat, kernel="linear",
                 ranges=list(cost=c(0.0001, 0.001, 0.01, 0.1, 1, 
                                    2,3,5,10,25,50,100)))
fit2 = tune.svm2$best.model

## Predictions from squared model
predGridLinSVM2 = as.numeric(as.character(predict(fit2, gridData)))

## Plot fit from squared model
plot(gridX[,1], gridX[,2], cex=0.2, col=3+predGridLinSVM2,
     xlim=c(-2,2), ylim=c(-2,2),asp=1,
     xlab=expression(X[1]), ylab=expression(X[2]), main="Squared terms SVM")
draw.circle(0,0, radius=1)
points(x, col=3+y, pch=2)


## Create data frame for radial model
dat = data.frame(x1 = x[,1], x2 = x[,2], y=as.factor(y))

## Let's try a range of cost variables and range of gamma values
tune.svm4 = tune(svm, y ~ ., data=dat, kernel="radial",
                 ranges=list(cost=c(0.0001, 0.001, 0.01, 0.1, 1, 2,3,5,10,25,50,100),
                             gamma=c(0.01, 0.1, 0.25, 0.5, 1, 2, 3, 4, 5, 10)))
fit4 = tune.svm4$best.model

## Predictions from radial model
predGridLinSVM4 = as.numeric(as.character(predict(fit4, gridData)))

plot(gridX[,1], gridX[,2], cex=0.2, col=3+predGridLinSVM4,
     xlim=c(-2,2), ylim=c(-2,2),asp=1,
     xlab=expression(X[1]), ylab=expression(X[2]), main="Radial SVM")
draw.circle(0,0, radius=1)
points(x, col=3+y, pch=2)


## Now let's look at the data separated by the quadrants
n = 75
set.seed(1)
x = matrix(runif(n*2, -2, 2), n, 2)
y = 2*((x[,1] < 0 & x[,2] < 0) |
         (x[,1] > 0 & x[,2] > 0)) - 1

## create grid of points for plots
gridX1 = seq(-2.5, 2.5, length=50)
gridX2 = seq(-2.5, 2.5, length=50)
gridX = expand.grid(gridX1, gridX2)

gridData = data.frame(x1 = gridX[,1], x2 = gridX[,2],
                      x1s = gridX[,1]^2, x2s = gridX[,2]^2)

## Plot the data
plot(x, col=3+y, pch=2, xlab=expression(X[1]),xlim=c(-2,2), ylim=c(-2,2),
     ylab=expression(X[2]), main="Nonlinear decision boundary", asp=1)
abline(h=0)
abline(v=0)

## Store data for estimation
dat = data.frame(x1 = x[,1], x2 = x[,2], y=as.factor(y))

## Let's try a range of cost variables
tune.svm = tune(svm, y ~ ., data=dat, kernel="linear",
                ranges=list(cost=c(0.0001, 0.001, 0.01, 0.1, 1, 2,3,5,10,25,50,100)))
fit = tune.svm$best.model

predGridLinSVM = as.numeric(as.character(predict(fit, gridData)))

plot(gridX[,1], gridX[,2], cex=0.2, col=3+predGridLinSVM,
     xlim=c(-2,2), ylim=c(-2,2),asp=1,
     xlab=expression(X[1]), ylab=expression(X[2]), main="Linear SVM")
points(x, col=3+y, pch=2)
abline(h=0)
abline(v=0)

## let's try to include squared terms
dat = data.frame(x1 = x[,1], x2 = x[,2], 
                 x1s = x[,1]^2, x2s = x[,2]^2, 
                 y=as.factor(y))

## Let's try a range of cost variables
tune.svm2 = tune(svm, y ~ ., data=dat, kernel="linear",
                 ranges=list(cost=c(0.0001, 0.001, 0.01, 0.1, 1, 2,3,5,10,25,50,100)))
fit2 = tune.svm2$best.model

## Predictions from squared fit
predGridLinSVM2 = as.numeric(as.character(predict(fit2, gridData)))

## Plot results
plot(gridX[,1], gridX[,2], cex=0.2, col=3+predGridLinSVM2,
     xlim=c(-2,2), ylim=c(-2,2),asp=1,
     xlab=expression(X[1]), ylab=expression(X[2]), main="Squared terms SVM")
points(x, col=3+y, pch=2)
abline(h=0)
abline(v=0)


## Let's fit the radial model
dat = data.frame(x1 = x[,1], x2 = x[,2], y=as.factor(y))

## Let's try a range of cost variables and gamma values
tune.svm4 = tune(svm, y ~ ., data=dat, kernel="radial",
                 ranges=list(cost=c(0.0001, 0.001, 0.01, 0.1, 1, 2,3,5,10,25,50,100),
                             gamma=c(0.01, 0.1, 0.25, 0.5, 1, 2, 3, 4, 5, 10)))
fit4 = tune.svm4$best.model

## Predictions from radial model
predGridLinSVM4 = as.numeric(as.character(predict(fit4, gridData)))

## Plot results
plot(gridX[,1], gridX[,2], cex=0.2, col=3+predGridLinSVM4,
     xlim=c(-2,2), ylim=c(-2,2),asp=1,
     xlab=expression(X[1]), ylab=expression(X[2]), main="Radial SVM")
points(x, col=3+y, pch=2)
abline(h=0)
abline(v=0)


## Show sensitivity to gamma values by fixing gamma at four values
dat = data.frame(x1 = x[,1], x2 = x[,2], y=as.factor(y))

## Let's try a range of cost variables
tune.svm4_1 = tune(svm, y ~ ., data=dat, kernel="radial",
                   ranges=list(cost=c(0.0001, 0.001, 0.01, 0.1, 1, 2,3,5,10,25,50,100),
                               gamma=c(0.01)))
fit4_1 = tune.svm4_1$best.model

predGridLinSVM4_1 = as.numeric(as.character(predict(fit4_1, gridData)))

tune.svm4_2 = tune(svm, y ~ ., data=dat, kernel="radial",
                   ranges=list(cost=c(0.0001, 0.001, 0.01, 0.1, 1, 2,3,5,10,25,50,100),
                               gamma=c(1)))
fit4_2 = tune.svm4_2$best.model

predGridLinSVM4_2 = as.numeric(as.character(predict(fit4_2, gridData)))

tune.svm4_3 = tune(svm, y ~ ., data=dat, kernel="radial",
                   ranges=list(cost=c(0.0001, 0.001, 0.01, 0.1, 1, 2,3,5,10,25,50,100),
                               gamma=c(10)))
fit4_3 = tune.svm4_3$best.model

predGridLinSVM4_3 = as.numeric(as.character(predict(fit4_3, gridData)))

tune.svm4_4 = tune(svm, y ~ ., data=dat, kernel="radial",
                   ranges=list(cost=c(0.0001, 0.001, 0.01, 0.1, 1, 2,3,5,10,25,50,100),
                               gamma=c(100)))
fit4_4 = tune.svm4_4$best.model

predGridLinSVM4_4 = as.numeric(as.character(predict(fit4_4, gridData)))

## Plot the four results on a grid
par(mfrow=c(2,2), pty='s')
plot(gridX[,1], gridX[,2], cex=0.2, col=3+predGridLinSVM4_1,
     xlim=c(-2,2), ylim=c(-2,2),asp=1,
     xlab=expression(X[1]), ylab=expression(X[2]), main="Radial, gamma = 0.01")
points(x, col=3+y, pch=2)
abline(h=0)
abline(v=0)

plot(gridX[,1], gridX[,2], cex=0.2, col=3+predGridLinSVM4_2,
     xlim=c(-2,2), ylim=c(-2,2),asp=1,
     xlab=expression(X[1]), ylab=expression(X[2]), main="Radial, gamma = 1")
points(x, col=3+y, pch=2)
abline(h=0)
abline(v=0)

plot(gridX[,1], gridX[,2], cex=0.2, col=3+predGridLinSVM4_3,
     xlim=c(-2,2), ylim=c(-2,2),asp=1,
     xlab=expression(X[1]), ylab=expression(X[2]), main="Radial, gamma = 10")
points(x, col=3+y, pch=2)
abline(h=0)
abline(v=0)

plot(gridX[,1], gridX[,2], cex=0.2, col=3+predGridLinSVM4_4,
     xlim=c(-2,2), ylim=c(-2,2),asp=1,
     xlab=expression(X[1]), ylab=expression(X[2]), main="Radial, gamma = 100")
points(x, col=3+y, pch=2)
abline(h=0)
abline(v=0)

dev.off()

## Showing permutations and inner products
a = -5:5

inner = rep(NA, 1000)
for (i in 1 : 1000) {
  ## Randomly permute the data
  a_pi = a[sample(1:11, 11, replace=FALSE)]
  inner[i] = sum(a*a_pi)
}

hist(inner, xlab="Inner product", main="Inner products", xlim=c(-110, 110))
abline(v = 110, lwd=2, lty=2)
text(100,100,"<a,a>")






## Now start with stock market example
rm(list=ls())

## Load in the data
data(Smarket)

## RADIAL simulation. Note that this will take a few minutes to run
## For all 100 data sets

## Store results of 100 random testing data sets
nSim = 100
errorMat = matrix(NA, nSim, 5)
trainErrorMat = matrix(NA, nSim, 5)

for (ni in 1 : nSim) {
  set.seed(ni)
  print(ni)
  
  ## Split data into training and testing
  trainIndex = sample(1:nrow(Smarket), 750, replace=FALSE)
  
  SmarketTrain = Smarket[trainIndex,]
  SmarketTest = Smarket[-trainIndex,]
  
  ## First fit the logistic regression model
  mod = glm(Direction ~ . -Year -Today + as.factor(Year), 
            data=SmarketTrain, family=binomial)
  
  testPred = 1*(predict(mod, newdata=SmarketTest, type="response") > 0.5)
  trainPred = 1*(predict(mod, newdata=SmarketTrain, type="response") > 0.5)
  
  errorMat[ni,1] = mean(testPred != (as.character(SmarketTest$Direction) == "Up"))
  trainErrorMat[ni,1] = mean(trainPred != 
                               (as.character(SmarketTrain$Direction) == "Up"))
  
  ## Now fit the radial model with gamma=0.01
  tune.svm = tune(svm, Direction ~. -Year -Today + as.factor(Year), 
                  data=SmarketTrain, kernel="radial",
                  ranges=list(cost=c(0.01, 1, 5,10, 100),
                              gamma=c(0.01)))
  fit = tune.svm$best.model
  
  predSVM = as.character(predict(fit, SmarketTest))
  trainPredSVM = as.character(fit$fitted)
  
  errorMat[ni,2] = mean(predSVM != as.character(SmarketTest$Direction))
  trainErrorMat[ni,2] = mean(trainPredSVM != as.character(SmarketTrain$Direction))
  
  ## Now fit the radial model with gamma=0.1
  tune.svm = tune(svm, Direction ~. -Year -Today + as.factor(Year), 
                  data=SmarketTrain, kernel="radial",
                  ranges=list(cost=c(0.01, 1, 5,10, 100),
                              gamma=c(0.1)))
  fit = tune.svm$best.model
  
  predSVM = as.character(predict(fit, SmarketTest))
  trainPredSVM = as.character(fit$fitted)
  
  errorMat[ni,3] = mean(predSVM != as.character(SmarketTest$Direction))
  trainErrorMat[ni,3] = mean(trainPredSVM != as.character(SmarketTrain$Direction))
  
  ## Now fit the radial model with gamma=1
  tune.svm = tune(svm, Direction ~. -Year -Today + as.factor(Year), 
                  data=SmarketTrain, kernel="radial",
                  ranges=list(cost=c(0.01, 1, 5,10, 100),
                              gamma=c(1)))
  fit = tune.svm$best.model
  
  predSVM = as.character(predict(fit, SmarketTest))
  trainPredSVM = as.character(fit$fitted)
  
  errorMat[ni,4] = mean(predSVM != as.character(SmarketTest$Direction))
  trainErrorMat[ni,4] = mean(trainPredSVM != as.character(SmarketTrain$Direction))
  
  ## Now fit the radial model with gamma=10
  tune.svm = tune(svm, Direction ~. -Year -Today + as.factor(Year), 
                  data=SmarketTrain, kernel="radial",
                  ranges=list(cost=c(0.01, 1, 5,10, 100),
                              gamma=c(10)))
  fit = tune.svm$best.model
  
  predSVM = as.character(predict(fit, SmarketTest))
  trainPredSVM = as.character(fit$fitted)
  
  errorMat[ni,5] = mean(predSVM != as.character(SmarketTest$Direction))
  trainErrorMat[ni,5] = mean(trainPredSVM != as.character(SmarketTrain$Direction))
}

apply(trainErrorMat, 2, mean, na.rm=TRUE)
apply(errorMat, 2, mean, na.rm=TRUE)

boxplot(errorMat, names=c("Logistic", paste("gamma =", c(0.01, 0.1, 1, 10))), 
        main="Test error rates")


## Now do the same simulation but with the polynomial kernel
rm(list=ls())

## Load the data
data(Smarket)

## Store the results
nSim = 100
errorMat = matrix(NA, nSim, 4)
trainErrorMat = matrix(NA, nSim, 4)

for (ni in 1 : nSim) {
  set.seed(ni)
  print(ni)
  
  ## Split the data
  trainIndex = sample(1:nrow(Smarket), 750, replace=FALSE)
  
  SmarketTrain = Smarket[trainIndex,]
  SmarketTest = Smarket[-trainIndex,]
  
  ## First fit the logistic model
  mod = glm(Direction ~ . -Year -Today + as.factor(Year), 
            data=SmarketTrain, family=binomial)
  
  testPred = 1*(predict(mod, newdata=SmarketTest, type="response") > 0.5)
  trainPred = 1*(predict(mod, newdata=SmarketTrain, type="response") > 0.5)
  
  errorMat[ni,1] = mean(testPred != (as.character(SmarketTest$Direction) == "Up"))
  trainErrorMat[ni,1] = mean(trainPred != 
                               (as.character(SmarketTrain$Direction) == "Up"))
  
  ## Now try the SVM with degree=1
  tune.svm = tune(svm, Direction ~. -Year -Today + as.factor(Year), 
                  data=SmarketTrain, kernel="polynomial",
                  ranges=list(cost=c(0.01, 1, 5,10, 100),
                              degree=c(1)))
  fit = tune.svm$best.model
  
  predSVM = as.character(predict(fit, SmarketTest))
  trainPredSVM = as.character(fit$fitted)
  
  errorMat[ni,2] = mean(predSVM != as.character(SmarketTest$Direction))
  trainErrorMat[ni,2] = mean(trainPredSVM != as.character(SmarketTrain$Direction))
  
  ## Now try the SVM with degree=2
  tune.svm = tune(svm, Direction ~. -Year -Today + as.factor(Year), 
                  data=SmarketTrain, kernel="polynomial",
                  ranges=list(cost=c(0.01, 1, 5,10, 100),
                              degree=c(2)))
  fit = tune.svm$best.model
  
  predSVM = as.character(predict(fit, SmarketTest))
  trainPredSVM = as.character(fit$fitted)
  
  errorMat[ni,3] = mean(predSVM != as.character(SmarketTest$Direction))
  trainErrorMat[ni,3] = mean(trainPredSVM != as.character(SmarketTrain$Direction))
  
  ## Now try the SVM with degree=3
  tune.svm = tune(svm, Direction ~. -Year -Today + as.factor(Year), 
                  data=SmarketTrain, kernel="polynomial",
                  ranges=list(cost=c(0.01, 1, 5,10, 100),
                              degree=c(3)))
  fit = tune.svm$best.model
  
  predSVM = as.character(predict(fit, SmarketTest))
  trainPredSVM = as.character(fit$fitted)
  
  errorMat[ni,4] = mean(predSVM != as.character(SmarketTest$Direction))
  trainErrorMat[ni,4] = mean(trainPredSVM != as.character(SmarketTrain$Direction))
  
}

apply(trainErrorMat, 2, mean, na.rm=TRUE)
apply(errorMat, 2, mean, na.rm=TRUE)

boxplot(errorMat, names=c("logistic", paste("d =", 1:3)), 
        main="Test error rates")

