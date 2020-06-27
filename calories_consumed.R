# Simple Linear Regression 
cal<-read.csv("D:/excelr_DS/assignment/Simple linear regression/calories_consumed.csv")
View(cal)

# data analysis
summary(cal) 

x <- cal$Weight.gained..grams.
y <- cal$Calories.Consumed

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
sqrt(sum(reg$residuals^2)/nrow(cal))

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

library(ggplot2)
ggplot(data = cal, aes(x = cal$Weight.gained..grams., y = cal$Calories.Consumed)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cal, aes(x=cal$Weight.gained..grams., y=cal$Calories.Consumed))

plot(log(x), y)
cor(log(x), y)

reg_log <- lm(y ~ log(x))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(cal))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")
