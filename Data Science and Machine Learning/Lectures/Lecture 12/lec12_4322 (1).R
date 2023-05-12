library(ISLR)
cor(Smarket)
cor(Smarket[,-9])

#Separating into Training/Testing
attach(Smarket)
train = (Year < 2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Smarket.2005$Direction
detach(Smarket)

#Creating a Model
library(MASS)
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit

#Prediction
lda.pred = predict(lda.fit,Smarket.2005)
lda.class = lda.pred$class
table(lda.class,Direction.2005)
(35+76)/(35+35+76+106)
mean(lda.class == Direction.2005)

#Model for mtcars data
mtcars1 = mtcars
mtcars1$cyl = as.factor(mtcars1$cyl)
(cars.lda = lda(cyl ~ mpg + hp, data = mtcars1))
(cars.pred = predict(cars.lda))
table(mtcars1$cyl,cars.pred$class)

#LDA Plot
library(ggplot2)
lda.plot = cbind(mtcars1,cars.pred$x)
ggplot(lda.plot,aes(LD1,LD2)) + 
  geom_point(aes(color = cyl))

#QDA model
(qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train))
qda.class = predict(qda.fit,Smarket.2005)$class
table(Direction.2005,qda.class)

data("Boston")
#Separate the data between training and test
set.seed(10)
sample = sample.int(n = nrow(Boston),
                    size = floor(.75*nrow(Boston)),
                    replace = F)
train = Boston[sample,]
test = Boston[-sample,]
#Create a new variable crim01 that is 1 if above the medain 0 if below the median
train$crim01 = (train$crim > median(train$crim))
test$crim01 = (test$crim > median(test$crim))

#Logistic Regression
fit.glm = glm(crim01 ~ age + medv, data = train, family = "binomial")
glm.pred = predict.glm(fit.glm,test,type = "response")
yHat = glm.pred > 0.5
table(test$crim01,yHat)

#LDA results
fit.lda = lda(crim01 ~ age + medv, data = train)
table(test$crim01,predict(fit.lda,test)$class)

#QDA results
fit.qda = qda(crim01 ~ age + medv, data = train)
table(test$crim01,predict(fit.qda,test)$class)