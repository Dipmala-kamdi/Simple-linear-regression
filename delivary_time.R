# Simple Linear Regression 
del<-read.csv("D:/excelr_DS/assignment/Simple linear regression/delivery_time.csv")
View(del)

# data analysis
summary(del) 

x <- del$Delivery.Time
y <- del$Sorting.Time

plot(x,y)

cor(x,y) # to Find Correlation Coefficient (r)

reg<-lm(y ~ x)
View(reg)
summary(reg)

pred <- predict(reg)
View(pred)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(del))

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

library(ggplot2)
ggplot(data = del, aes(x = del$Delivery.Time, y = del$Sorting.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = del, aes(x=del$Delivery.Time, y=del$Sorting.Time))

plot(log(x), y)
cor(log(x), y)

reg_log <- lm(y ~ log(x))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(del))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")
