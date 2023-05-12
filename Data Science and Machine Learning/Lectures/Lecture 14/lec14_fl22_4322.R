library(ISLR)
summary(Auto)
dim(Auto)
plot(Auto$horsepower,Auto$mpg)

#Validation Set
#Setting the test/training data
set.seed(10)
sample = sample(1:nrow(Auto),nrow(Auto)/2)
train = Auto[sample,]
test = Auto[-sample,]

#Creating the models
auto.lm = lm(mpg ~ horsepower, data = train)
auto.lm2 = lm(mpg ~ poly(horsepower,2),data = train)
auto.lm3 = lm(mpg ~ poly(horsepower,3),data = train)

#Getting the predicted values hat(mpg) from test data
auto.pred = predict(auto.lm,newdata = test,se.fit = TRUE)
auto.pred2 = predict(auto.lm2,newdata = test,se.fit = TRUE)
auto.pred3 = predict(auto.lm3,newdata = test,se.fit = TRUE)

#Getting the MSE for each model
mean((Auto$mpg - predict(auto.lm,Auto))[-sample]^2)
mean((Auto$mpg - predict(auto.lm2,Auto))[-sample]^2)
mean((Auto$mpg - predict(auto.lm3,Auto))[-sample]^2)

#or
mean((test$mpg - auto.pred$fit)^2)
mean((test$mpg - auto.pred2$fit)^2)
mean((test$mpg - auto.pred3$fit)^2)

#Getting thsi result 10 times
mse = matrix(nrow = 10, ncol = 3)
colnames(mse) = c("Linear","Quadratic","Cubic")
for (i in 1:10) {
  sample = sample(1:nrow(Auto),nrow(Auto)/2)
  train = Auto[sample,]
  test = Auto[-sample,]
  
  #Creating the models
  auto.lm = lm(mpg ~ horsepower, data = train)
  auto.lm2 = lm(mpg ~ poly(horsepower,2),data = train)
  auto.lm3 = lm(mpg ~ poly(horsepower,3),data = train)
  
  #Getting the MSE for each model
  mse[i,1] = mean((Auto$mpg - predict(auto.lm,Auto))[-sample]^2)
  mse[i,2] = mean((Auto$mpg - predict(auto.lm2,Auto))[-sample]^2)
  mse[i,3] = mean((Auto$mpg - predict(auto.lm3,Auto))[-sample]^2)
}
mse

#Leave-One-Out Cross Validation
mse.loocv = matrix(nrow = nrow(Auto), ncol = 3)
for (i in 1:nrow(Auto)) {
  sample = i
  #Creating the models
  auto.lm = lm(mpg ~ horsepower, data = Auto[-sample,])
  auto.lm2 = lm(mpg ~ poly(horsepower,2),data = Auto[-sample,])
  auto.lm3 = lm(mpg ~ poly(horsepower,3),data = Auto[-sample,])
  
  #Getting the MSE for each model
  mse.loocv[i,1] = (Auto$mpg[sample] - predict(auto.lm,Auto[sample,]))^2
  mse.loocv[i,2] = (Auto$mpg[sample] - predict(auto.lm2,Auto[sample,]))^2
  mse.loocv[i,3] = (Auto$mpg[sample] - predict(auto.lm3,Auto[sample,]))^2
}
head(mse.loocv)
colMeans(mse.loocv)

#Function to create the loocv
#install.packages("boot")
library(boot)
auto.glm = glm(mpg ~ horsepower,data = Auto)
cv.glm (Auto, auto.glm)$delta

#Repeat Up to fifth degree
cv.error = rep(0,5)
for (i in 1:5) {
  glm.fit = glm(mpg ~ poly(horsepower,i),data = Auto)
  cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

#K-fold CV 
sample = 1:39
auto.lm2 = lm(mpg ~ poly(horsepower,2),data = Auto[-sample,])
sum(Auto$mpg[sample] - predict(auto.lm2,Auto[sample,]))^2/39

#K-fold Cross Validataion
#use K = 10
cv.error.10 = rep(0,5)
set.seed(3)
for (i in 1:5) {
  glm.fit = glm(mpg ~ poly(horsepower,i),data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit,K = 10)$delta[1]
}
cv.error.10

#Show with classification
#predicting origin

#Leave One Out Cross Validation
library(MASS)
loocv.err = NA
for (i in 1:nrow(Auto)){
  sample = i
  fit.lda = lda(origin ~ horsepower,data = Auto[-sample,])
  loocv.err[i] = (Auto$origin[sample] == predict(fit.lda,Auto[sample,])$class)
}
mean(loocv.err)
