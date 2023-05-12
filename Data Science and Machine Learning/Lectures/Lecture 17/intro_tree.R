#install.packages('tree')
library(tree)
library(ISLR)
?tree
?Hitters
summary(Hitters)
Hitters2 = na.omit(Hitters)
summary(Hitters2)
plot(Hitters2$Hits,Hitters2$Salary)
hist(Hitters2$Salary)
hist(log(Hitters2$Salary))
set.seed(100)
train = sample(1:nrow(Hitters2),nrow(Hitters2)/2)
lm.hitters = lm(log(Salary) ~ Hits + HmRun + Runs + RBI + Walks + 
                    Years + PutOuts + Assists + Errors,
                  Hitters2,subset = train)
summary(lm.hitters)
step(lm.hitters)
lm.hitters = lm(log(Salary) ~ Hits + Walks + Years, Hitters2, subset = train)
summary(lm.hitters)
pred.train = predict(lm.hitters)

#MSE Train
(mse.hitter.train = mean((log(Hitters2[train,]$Salary) - pred.train)^2))
sqrt(exp(.3262))


#MSE Test
pred.test = predict(lm.hitters,newdata = Hitters2[-train,])
(mse.hitter.test = mean((log(Hitters2[-train,]$Salary) - pred.test)^2))
sqrt(exp(0.4697))


#K-fold CV
library(boot)
glm.hitters = glm(log(Salary) ~ Hits + Walks + Years, data = Hitters2)
set.seed(2)
cv.glm(Hitters2,glmfit = glm.hitters, K =10)$delta
sqrt(exp(0.41147))

#Regression Tree
tree.hitters = tree(log(Salary) ~ Hits + HmRun + Runs + RBI + Walks + 
                      Years + PutOuts + Assists + Errors,
                    Hitters2,subset = train)
summary(tree.hitters)
plot(tree.hitters)
text(tree.hitters,pretty = 0)

#Cross Validation
cv.hitters = cv.tree(tree.hitters)
plot(cv.hitters$size,cv.hitters$dev,type = "b")
prune.hitters = prune.tree(tree.hitters,best = 3)
plot(prune.hitters)
text(prune.hitters)

#Determining MSE
yhat = predict(tree.hitters,newdata=Hitters2[-train,])
hitters.test = Hitters2[-train,"Salary"]
plot(yhat,log(hitters.test))
abline(0,1)
mean((yhat-log(hitters.test))^2)
sqrt(exp(0.41803))

yhat.prune = predict(prune.hitters,newdata = Hitters2[-train,])
plot(yhat.prune,log(hitters.test))
abline(0,1)
mean((yhat.prune - log(hitters.test))^2)
sqrt(exp(0.4243))


