# Simple Linear Regression 
emp<-read.csv("D:/excelr_DS/assignment/Simple linear regression/emp_data.csv")
View(emp)

# data analysis
summary(emp) 

x <- emp$Churn_out_rate
y <- emp$Salary_hike

plot(x,y)

cor(x,y) # to Find Correlation Coefficient (r)

reg1<-lm(y ~ x)
View(reg1)
summary(reg1)

pred1 <- predict(reg1)
View(pred1)

reg1$residuals
sum(reg1$residuals)

mean(reg1$residuals)
sqrt(sum(reg1$residuals^2)/nrow(emp))

sqrt(mean(reg1$residuals^2))

confint(reg1,level=0.95)
predict(reg1,interval="predict")

library(ggplot2)
ggplot(data = emp, aes(x = emp$Churn_out_rate, y = emp$Salary_hike)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=emp$Churn_out_rate, y=emp$Salary_hike))

plot(log(x), y)
cor(log(x), y)

reg1_log <- lm(y ~ log(x))   # lm(Y ~ X)

summary(reg1_log)
predict(reg1_log)

reg1_log$residuals
sqrt(sum(reg1_log$residuals^2)/nrow(emp))  #RMSE

confint(reg1_log,level=0.95)
predict(reg1_log,interval="confidence")
