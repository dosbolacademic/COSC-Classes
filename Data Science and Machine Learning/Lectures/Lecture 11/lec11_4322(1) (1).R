#To create the Normal Plots
x = seq(-6,6, by = 0.01)
plot(x,dnorm(x,-1.5,1),xlab = "", ylab = "", col = "red", type = "l")
lines(x,dnorm(x,1.5,1),col = "green")

#Example
x1 = c(3,2,4,1,5)
mean(x1)
x2 = c(6,5,4,5,5)
mean(x2)
sum((x1 - mean(x1))^2)
sum((x2 - mean(x2))^2)

#X = 2
2*3/1.5-9/1.5+log(0.5) #classifier for 1
2*5/1.5-25/1.5+log(10) #classifier for 2

#Posterior Probability pi_k = 0.5, mu1 = 3, mu2 = 5, sigma^2 = 1.5
#Assuming a normal distribution, 
#Probability of x = 2 in class 1
(0.5*dnorm(2,3,sqrt(1.5)))/(0.5*sum(dnorm(2,c(3,5),sqrt(1.5))))
#Probability of x = 2 in class 2
(0.5*dnorm(2,5,sqrt(1.5)))/(0.5*sum(dnorm(2,c(3,5),sqrt(1.5))))

x = c(x1,x2)
class.x = rep(c(1,2),each = 5)
(x = matrix(cbind(x,class.x),nrow = 10))
lda.posterior1 = .5*dnorm(x[,1],3,sqrt(1.5))/(.5*dnorm(x[,1],3,sqrt(1.5)) + .5*dnorm(x[,1],5,sqrt(1.5)))

lda.posterior2 = .5*dnorm(x[,1],5,sqrt(1.5))/(.5*dnorm(x[,1],3,sqrt(1.5)) + .5*dnorm(x[,1],5,sqrt(1.5)))

(lda.x = cbind(x,lda.posterior1,lda.posterior2))

library(MASS)
?lda 
lda.r = lda(class.x~x[,1])
pred.lda = predict(lda.r)
pred.lda
table(class.x,pred.lda$class)


#LDA for Breast Cancer
head(bc)
#Split the bc data set into training and test
set.seed(10)
sample <- sample.int(n = nrow(bc), 
                     size = floor(.75*nrow(bc)), 
                     replace = F)
train <- bc[sample,]
test  <- bc[-sample, ]
head(train)
bc.lda = lda(Class ~ Cell.size,data = train)
bc.lda
lda.pred = predict(bc.lda,test)
table(test$Class,lda.pred$class)
cbind(lda.pred$class,test$Cell.size)

#For more than one predictor
X = matrix(c(3,2,4,6,5,4,7,4,7,9,7,8),nrow = 6)
class.x = rep(c(1,2),each = 3)
(X = as.matrix(cbind(X,class.x)))

cov(X[1:3,1:2])
cov(X[4:6,1:2])

cov.x = matrix(c(1,1,1,2),nrow = 2)
library(mvtnorm)
posterior.lda1 = 0.5*dmvnorm(X[,1:2],mean = c(3,6), sigma = cov.x)/(0.5 *(dmvnorm(X[,1:2],mean = c(3,6), sigma = cov.x) + dmvnorm(X[,1:2],mean = c(5,8), sigma = cov.x)))
posterior.lda2 = 0.5*dmvnorm(X[,1:2],mean = c(5,8), sigma = cov.x)/(0.5 *(dmvnorm(X[,1:2],mean = c(3,6), sigma = cov.x) + dmvnorm(X[,1:2],mean = c(5,8), sigma = cov.x)))

cbind(X,round(posterior.lda1,3),round(posterior.lda2,3))

(lda.x2 = lda(class.x ~ X[,1:2]))
(pred.lda2 = predict(lda.x2))



bc.lda2 = lda(Class ~ Cell.size + Cl.thickness + Cell.shape,data = train)
bc.lda2
lda.pred2 = predict(bc.lda2,test)
table(test$Class,lda.pred2$class)
