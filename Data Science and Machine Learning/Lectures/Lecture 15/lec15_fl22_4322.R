library(boot)
?nodal
summary(nodal)
nodal.glm = glm(r ~ aged + stage + grade + xray + acid,
                family = binomial, data = nodal)
summary(nodal.glm)
glm.predict = rep(0,nrow(nodal))
glm.probs = rep(0,nrow(nodal))
for (i in 1:nrow(nodal)) {
  glm.probs[i] = predict(glm(r ~ stage + xray + acid,
                             family = binomial,
                             data = nodal[-i,]),
                         nodal[i,],type = "response")
}
glm.predict[glm.probs >0.5] = 1
(mse = mean(abs(nodal$r - glm.predict)))

#Using the cv.glm function
cost = function(r, pi = 0) mean(abs(r - pi) > 0.5) #creating the cost function

nodal.glm = glm(r ~ stage + xray + acid, family = binomial,
                data = nodal)
(cv.err = cv.glm(nodal,nodal.glm,cost)$delta[1])
(cv.11.err = cv.glm(nodal,nodal.glm,cost,K = 11)$delta)

#Bootstrap example
temp = c(202.2, 203.4, 200.5, 202.5, 206.3, 198.0, 203.7, 200.8, 201.3, 199.0)
#Resample
B = 1000 #number of resamples
M = NA #vector of the 1000 means
for (i in 1:B) {
  x = sample(temp,length(temp),replace = T)
  M[i] = mean(x)
}
x #last sample in the for loop
mean(x) #mean of the last sample
mean(M) #mean of the 1000 resampled means
sd(M) #estmiated standard error of the mean
hist(M)

#Using the boot function
#libarary(boot)
mean.fun = function(dat,idx) mean(dat[idx],na.rm = TRUE)
boot.out = boot(data = temp, statistic = mean.fun,R = 1000)
boot.out
mean(boot.out$t)
hist(boot.out$t)

#Estimating the median
median(temp)
median.fun = function(dat,idx) median(dat[idx],na.rm = TRUE)
boot.out.median = boot(data = temp, statistic = median.fun,R = 1000)
boot.out.median
mean(boot.out.median$t)
hist(boot.out.median$t)
