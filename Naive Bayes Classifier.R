#Bank is interested in knowing the factors which influence the default of a customer
#Bank will utilize this information in future to avoid giving loans to the customers who are more risky to the business. 
setwd(choose.dir())
getwd()
#Load necessary packages
install.packages("mlbench")
library(e1071)
library(plyr)
library(caret)
library(mlbench)
#Load and Verify the bank data
bank_data <- read.csv("Demo 2_ Naive Bayes Classifier.csv")
View(bank_data)
str(bank_data)
#Convert default from int to factor
bank_data$Default <- sapply(bank_data$Default, factor)
str(bank_data)
#build the model
naive_model <- naiveBayes(Default ~., data = bank_data)
print(naive_model)
#The model creates conditional probability for each feature separately 
#we also have the apriori probabilities which indicates the distribusion of our data
#predict
naive_predict <- predict(naive_model, bank_data)
naive_predict
table(naive_predict, bank_data$Default)