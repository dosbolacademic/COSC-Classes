#Setting the frequency table to raw data
Titanic
#install.packages("bbl") #package used to convert to raw data
library(bbl) #call the package
x <- as.data.frame(Titanic) #put as a data frame
head(x)
dim(x)
titanic = freq2raw(data=x[,1:4], freq=x$Freq) #convert to the raw data
head(titanic)
dim(titanic)

#Creating Test/Training data sets
set.seed(101) #set seed
#Select 75% of the data
sample = sample.int(n = nrow(titanic),size = round(.75*nrow(titanic)),
                    replace = FALSE)
head(sample)
train = titanic[sample,]
test = titanic[-sample,]
dim(train)
dim(test)
summary(titanic)

#Create the model
titanic.glm = glm(Survived ~ Sex, family = "binomial",
                  data = train)
summary(titanic.glm)

boxplot(Freq ~ Survived + Sex, data = x)

#Creating a Confusion Matrix
predict.titanic.train = predict(titanic.glm,type = "response")
head(cbind(train,predict.titanic.train))
table(titanic$Sex,titanic$Survived)
367/(1364+367)
344/(126+344)

predict.survive.train = ifelse(predict.titanic.train < 0.5, "No","Yes")
(conf.mat.train = table(predict.survive.train,train$Survived))
(conf.mat.train[1,2]+conf.mat.train[2,1])/sum(conf.mat.train)#error rate

#Confusion Matrix on Test Data
predict.titanic = predict(titanic.glm,type = "response", newdata = test)
head(predict.titanic)
predict.survive = ifelse(predict.titanic< 0.5,"No","Yes")
(conf.mat = table(predict.survive,test$Survived))

#Testing Error Rate
(conf.mat[1,2]+conf.mat[2,1])/sum(conf.mat)

#sensitivity
conf.mat[2,2]/(conf.mat[1,2]+conf.mat[2,2])
#Specificity
conf.mat[1,1]/(conf.mat[1,1]+conf.mat[2,1])

#Cleaning the BreastCancer data
#install.packages("mlbench")
data(BreastCancer,package = "mlbench")
summary(BreastCancer)

#Create a copy with no missing values and remove id column
bc <- BreastCancer[complete.cases(BreastCancer), ] 
bc <- bc[,-1] # remove id column

#Convert the factors to numeric
for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}
summary(bc)
class = ifelse(bc$Class == "malignant", 1, 0)
class = as.factor(class)
summary(class)

#To get the logistic model
fit.bc3 = glm(Class ~ Cl.thickness + Cell.shape + Cell.size, 
              family = "binomial", data = bc)
summary(fit.bc3)
#predicting
predict.glm(fit.bc3,newdata = data.frame(Cl.thickness = 5, Cell.shape = 5,
                                         Cell.size = 5),type = "response")
#Getting the predictor
percent.bc = predict.glm(fit.bc, type = "response")
predict.bc = ifelse(percent.bc < 0.5, "benign","malignant")
(conf.bc = table(bc$Class,predict.bc))
sum(conf.bc)
(conf.bc[1,1]+conf.bc[2,2])/sum(conf.bc)
(conf.bc[2,2]/(conf.bc[2,1]+conf.bc[2,2]))
(conf.bc[1,1]/(conf.bc[1,1]+conf.bc[1,2]))

#For the three variable model
percent.bc3 = predict.glm(fit.bc3, type = "response")
predict.bc3 = ifelse(percent.bc3 < 0.5, "benign","malignant")
(conf.bc3 = table(bc$Class,predict.bc3))
sum(conf.bc3)
(conf.bc3[1,1]+conf.bc3[2,2])/sum(conf.bc3)
(conf.bc3[2,2]/(conf.bc3[2,1]+conf.bc3[2,2]))
(conf.bc3[1,1]/(conf.bc3[1,1]+conf.bc3[1,2]))

sample = sample.int(n = nrow(bc),
                    size = floor(.75*nrow(bc)),
                    replace = FALSE)
train.data.bc = bc[sample,]
test.data.bc = bc[-sample,]
train.bc = glm(Class ~ Cl.thickness + Cell.shape + Cell.size,
               data = train.data.bc,
               family = "binomial")
glm.pred = predict.glm(train.bc,newdata = test.data.bc, 
                       type = "response")
yHat = glm.pred > 0.5

(conf.test = table(test.data.bc$Class,yHat))

#Example 2 MTCARS
set.seed(110)
sample = sample.int(n = nrow(mtcars),
                    size = floor(.8*nrow(mtcars)),
                    replace = FALSE)
train = mtcars[sample,]
test = mtcars[-sample,]

#Training model
cars.glm = glm(vs ~ disp + hp + wt,
               data = train,
               family = "binomial")
summary(cars.glm)
step(cars.glm)

#Using hp only
hp.glm = glm(vs ~ hp,data = train,family = "binomial")
summary(hp.glm)
(r.2 = 1 - 13.637/34.617)

glm.pred = predict.glm(hp.glm,newdata = test,type = "response")
yHat = glm.pred > 0.5
table(test$vs,yHat)
6/7 #accuracy rate
