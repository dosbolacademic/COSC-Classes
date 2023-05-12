#Input stock_price dataset
#stock_price <- read.csv("E:/Lectures uh/stock_price.csv")

#simple linear regression
stock.lm = lm(Stock_Index_Price ~ Interest_Rate,data = stock_price)
summary(stock.lm)

#Multiple linear regression
stock3.lm = lm(Stock_Index_Price ~ Interest_Rate+Unemployment_Rate+Year,
               data = stock_price)
summary(stock3.lm)

stock2.lm = lm(Stock_Index_Price ~ Interest_Rate+Unemployment_Rate, data = stock_price)
summary(stock2.lm)

av1 = anova(stock.lm)
av2 = anova(stock2.lm)
av3 = anova(stock3.lm)

#Cp
av1$"Sum Sq"[2]/av3$"Mean Sq"[4] + 2*2 - 24 #Cp for Interest rate
av2$`Sum Sq`[3]/av3$`Mean Sq`[4] + 2*3 - 24 #Cp for interest rate + unemployment
av3$`Sum Sq`[4]/av3$`Mean Sq`[4] + 2*4 - 24 #Cp for all 3 variables

#AIC
2*4 + 24 *log(av3$`Sum Sq`[4]/24) #AIC for all 3 variable
2*2 + 24*log(av1$`Sum Sq`[2]/24) #AIC for Interest rate
2*3 + 24*log(av2$`Sum Sq`[3]/24) #AIC for all 3 variables

#BIC
BIC(stock.lm) #Interest Rate
BIC(stock2.lm) #Interest Rate + Unemployment Rate
BIC(stock3.lm) #Interest Rate + Unemployment Rate + Year

#Putting R^2
stock.r = c(summary(stock.lm)$r.squared,summary(stock.lm)$adj.r.squared) #Interest Rate
stock2.r = c(summary(stock2.lm)$r.squared,summary(stock2.lm)$adj.r.squared) #Interest Rate + Unemployment Rate
stock3.r = c(summary(stock3.lm)$r.squared,summary(stock3.lm)$adj.r.squared) #Interest Rate + Unemployment Rate + Year
r2 = rbind(stock.r,stock2.r,stock3.r)
colnames(r2) = c("R^2","Adjusted R^2")
r2

1 - (104559/(24-2-1))/(1021416/23) ##Adjusted R^2 for Interest Rate + Unemployment

#install.packages("leaps")
library(leaps)
stock.fit = regsubsets(Stock_Index_Price ~ Unemployment_Rate +
                         Interest_Rate + Year, data = stock_price)
stock.res = summary(stock.fit)
stock.res
stock.stat = cbind(stock.res$rsq,
                   stock.res$adjr2,
                   stock.res$cp,
                   stock.res$bic)
colnames(stock.stat) = c("rsq","Adjr2","Cp","BIC")
stock.stat

#Assumption Plots
par(mfrow = c(2,2))
plot(stock2.lm)

#Prediction Interval
predict(stock2.lm,
       newdata = data.frame(Interest_Rate = 2.25, Unemployment_Rate = 6.0),
       interval = "p")

#Confidence Interval
predict(stock2.lm,
        newdata = data.frame(Interest_Rate = 2.25, Unemployment_Rate = 6.0),
        interval = "c")

