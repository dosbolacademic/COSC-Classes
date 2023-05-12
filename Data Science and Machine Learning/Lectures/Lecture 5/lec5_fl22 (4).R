library(MASS)
# Different Plotting Characters
plot(Boston$lstat,Boston$medv,xlab = "lstat",ylab = "medv")
lm.fit = lm(medv ~ lstat,data = Boston)
predict(lm.fit,newdata = data.frame(Boston$lstat = c(5,6,7)))
abline(lm.fit,lwd = 3, col = "red")
plot(Boston$lstat,Boston$medv,pch = 20)

plot(Boston$lstat,Boston$medv,pch = "+",col = "blue")

plot(1:20,1:20,pch = 1:20)
#Input stock_price dataset

#simple linear regression
interest.lm = lm(Stock_Index_Price ~ Interest_Rate,data = stock_price)
summary(interest.lm)

#Only Using unemployement rate
unemployement.lm = lm(Stock_Index_Price ~ Unemployment_Rate, data = stock_price)
summary(unemployment.lm)

#Only using Year
year.lm = lm(Stock_Index_Price ~ Year, data = stock_price)
summary(year.lm)

#Multiple linear regression
stock3.lm = lm(Stock_Index_Price ~ Interest_Rate+Unemployment_Rate+Year,
               data = stock_price)
summary(stock3.lm)
#Interpret B_3:  For each additional year the stock index price increases
#on average by $28.89 , given a fixed interest rate and unemployment rate.

#Multicollinarity
cor(stock_price[,-2])

#Anova
anova(stock3.lm)
(SST_stock = 894463+22394+980+103579)
(SSE_stock = 103579)
#Test Statistic: 
(F_stock = ((SST_stock - SSE_stock)/3)/(SSE_stock/20))
#P-value
1 - pf(F_stock,3,20)


#Without Year
stock2.lm = lm(Stock_Index_Price ~ Interest_Rate + Unemployment_Rate,
               data = stock_price)
summary(stock2.lm)
par(mfrow = c(2,2))
plot(stock2.lm)

#stepwise Regression
step(stock3.lm) #stepwise "backwards"
step(stock3.lm,direction = "forward")
step(stock3.lm,direction = "both")

anova(stock2.lm)
(104559/5179) + 2*3 -24 #Cp for Interest rate + Unemployment rate
(103579/5179) +2*4 -24 #Cp with all 3 variables
anova(stock.lm)
(126953/5179) + 2*2 -24 #Cp with Interest rate only

2*3 + 24 *log(104559/24) #AIC for Interest rate + unemployment rate
2*2 + 24*log(126953/24) #AIC for Interest rate
2*4 + 24*log(103579/24) #AIC for all 3 variables

BIC(stock.lm) #Interest Rate
BIC(stock2.lm) #Interest Rate + Unemployment Rate
BIC(stock3.lm) #Interest Rate + Unemployment Rate + Year

1 - (104559/(24-2-1))/(1021416/23) #Adjusted R^2 for Interest Rate + Unemployment



