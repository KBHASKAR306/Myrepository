#Perform Simple Linear Regression
#Given the class data set with 5 variables,
#i.e.,name, sex, age, Height and weight representing the information for a class of students,
#Predict weight based on Height.

setwd(choose.dir())
getwd()

Class <- read.csv("Demo 1_Perform simple linear regression.CSV")
View(Class)

str(Class)

summary(Class)

results <- lm(formula = Weight ~ Height, data = Class)
results

results1 <- lm(formula = Class$Weight ~ Class$Height)
results1

summary(results)
