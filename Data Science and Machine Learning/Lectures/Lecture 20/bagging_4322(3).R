#Heart data set is in the ISLR library
library(ISLR)
library(tree)
n = nrow(Hitters)
included.vars = names(Hitters[-c(3,8:15,20)])
Hitters2 = na.omit(Hitters)

par(mfrow = c(1,2))

mse = NA
yhat = NA
for (i in 1:2) {
  train = sample(1:n, 0.5*n)
  tree.model = tree(log(Salary) ~., 
                    data = Hitters2[,included.vars],
                    subset = train)
  prune.model = prune.tree(tree.model,best = 5)
  plot(prune.model); text(prune.model,pretty = T)
  hitters.test = Hitters2[-train,"Salary"]
  yhat= predict(prune.model,newdata = Hitters2[-train,included.vars])
  mse[i] = mean((yhat - log(hitters.test))^2)
}
print(mse)
sd(mse)
par(mfrow = c(1,1))
yhat

for (i in 1:2) {
  train = sample(1:n, .5*n)
  lm.model = lm(log(Salary) ~., 
                data = Hitters2[,included.vars],
                subset = train)
  print(lm.model$coeff)
  hitters.test = Hitters2[-train,"Salary"]
  yhat = predict(lm.model,newdata = Hitters2[-train,included.vars])
  mse = mean((yhat[i] - log(hitters.test))^2)
}
print(mse)
sd(mse)

#Example of Bagging
install.packages('randomForest')
library(randomForest)
set.seed(1)
p = ncol(Hitters2) - 4 #No. of predictors
B = 100 #No. of bootstrap trees
bag.model.p = randomForest(log(Salary) ~. -NewLeague-League-Division,
                           data = Hitters2,
                           ntree = B,
                           mtry = p)
bag.model.p #To get an outline of bagging results.
bag.model.p$predicted
plot(bag.model.p)

#MSE of Pruned Decision Tree
set.seed(1)
train = sample(1:n, .5*n)
hitters.test = Hitters2[-train,"Salary"]
yhat = predict(prune.model,newdata = Hitters2[-train,included.vars])
mean((yhat - log(hitters.test))^2)


#Example of Classification Tree
#Using Heart data
#Fix the data set
Heart <- read.csv("E:/Lectures uh/Heart.csv")
Heart = na.omit(Heart); Heart$X = NUL
Heart$AHD = as.factor(Heart$AHD) #needs to be categorical for the tree
Heart$ChestPain = as.factor(Heart$ChestPain) #originally imported as "character"
Heart$Thal = as.factor(Heart$Thal) #originally imported as "character"
summary(Heart) #checking

#Split into training and testing
set.seed(100)
train = sample(1:nrow(Heart),nrow(Heart)/2+0.5)
tree.heart = tree(AHD ~ ., Heart,subset =  train) #Model
Heart.test = Heart[-train,]
tree.pred = predict(tree.heart,Heart.test,type = "class")
(conf.matrix = table(tree.pred,Heart.test$AHD))
(conf.matrix[1,2]+conf.matrix[2,1])/sum(conf.matrix)

#bagging
n = nrow(Heart); p = ncol(Heart)-1
B = 1000
Heart$AHD = as.factor(Heart$AHD)
bag.model = randomForest(AHD ~., data = Heart,xtest = Heart.test[,-14],
                         ytest = Heart.test[,14],
                         ntree = B,
                         mtry = p,
                         importance = TRUE)
bag.model$predicted
bag.model

varImpPlot(bag.model)

n = nrow(Heart); p = ncol(Heart)-1
B = 1000
Heart$AHD = as.factor(Heart$AHD)
bag.model = randomForest(AHD ~., data = Heart,
                         ntree = B,
                         mtry = p,
                         importance = TRUE)
bag.model
plot(bag.model)

#Random Forest Model
AHD.test = Heart[-train,"AHD"]
X.test = Heart[-train,-14] #Take away the AHD column
rf.model = randomForest(AHD ~., data = Heart,
                        subset = train,
                        xtest = X.test, ytest = AHD.test,
                        ntree = B,
                        mtry = sqrt(p),
                        importance = TRUE)
rf.model$test$confusion[1,2]
rf.model$confusion
rf.model
varImpPlot(rf.model)

#install.packages("gbm")
library(gbm)
set.seed(1)
boost.hitters = gbm(log(Salary) ~. -NewLeague-League-Division,
                    data = Hitters2,
                    distribution = "gaussian")
summary(boost.hitters)

train = sample(1:nrow(Hitters2), .5*nrow(Hitters2))
hitters.test = Hitters2[-train,"Salary"]
yhat.boost = predict(boost.hitters,newdata = Hitters2[-train,],
                     n.trees = 100)
mean((yhat.boost- log(hitters.test))^2)

boost.hitters = gbm(log(Salary) ~. -NewLeague-League-Division,
                    data = Hitters2,
                    distribution = "gaussian",
                    shrinkage=0.2,verbose=F)
summary(boost.hitters)
yhat.boost=predict(boost.hitters,newdata=Hitters2[-train,],n.trees=100)
mean((yhat.boost-log(hitters.test))^2)
