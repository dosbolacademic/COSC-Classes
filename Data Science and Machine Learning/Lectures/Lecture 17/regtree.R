summary(mtcars)
attach(mtcars)
plot(hp,mpg)
cars.lm = lm(mpg~hp,data = mtcars)
summary(cars.lm)
(rss = sum(cars.lm$residuals^2))
abline(cars.lm)
cbind(mpg,hp,cars.lm$residuals)

#First Split
mtcars1 = mtcars[which(hp < 92),]
mtcars2 = mtcars[which(hp >= 92),]
cars.lm1 = lm(mpg~hp,data = mtcars1)
cars.lm2 = lm(mpg~hp,data = mtcars2)
(rss1 = sum(cars.lm1$residuals^2))
(rss2 = sum(cars.lm2$residuals^2))
(rssT = rss1+rss2)
cbind(mtcars2$mpg,mtcars2$hp,cars.lm2$residuals)
abline(v = 92,lty = 2,col = "purple")
abline(cars.lm1,col = "red")
abline(cars.lm2,col = "blue")

#Second Split
mtcars2 = mtcars2[which(hp < 190),]
mtcars3 = mtcars2[which(hp >= 190),]
cars.lm2 = lm(mpg~hp, data = mtcars2)
cars.lm3 = lm(mpg~hp, data = mtcars3)
(rss2 = sum(cars.lm2$residuals^2))
(rss3 = sum(cars.lm3$residuals^2))
(rssT = rss1 + rss2 + rss3)
abline(v = 190,lty = 2,col = "purple")
abline(v = 140, lty = 2, col = "purple")

#Third Split
mtcars4 = mtcars2[which(hp < 140),]
mtcars5 = mtcars2[which(hp >= 140),]
cars.lm4 = lm(mpg~hp, data = mtcars4)
cars.lm5 = lm(mpg~hp, data = mtcars5)
(rss4 = sum(cars.lm4$residuals^2))
(rss5 = sum(cars.lm5$residuals^2))
(rssT = rss1 + rss4 + rss5 + rss3)
mean(mtcars1$mpg)
mean(na.omit(mtcars4$mpg))
mean(na.omit(mtcars5$mpg))
mean(na.omit(mtcars3$mpg))
detach(mtcars)

#Using the tree function
library(tree)
cars.tree = tree(mpg~hp,data = mtcars)
summary(cars.tree)
plot(cars.tree)
text(cars.tree,pretty = 0)
yhat = predict(cars.tree)
sum((mtcars$mpg - yhat)^2)
