#Support Vector Machine
#Customer data of a network provider 
#A Mobile network operator is facing a business problem that lot of customers are 
#transitioning to other service provider. This is causing lot of losses to the business.
#The company would like to understand the factors which impact the churn of customers.

setwd("C:/Users/HP/OneDrive/Desktop")
getwd()

customer_churn <- read.csv("Demo 1_ Support Vector Machines.csv")
View(customer_churn)
count(customer_churn$Churn)

#483 customers have churned "1";2850 have not churned "0"
str(customer_churn)
install.packages("dplyr")
library(dplyr)

customer_churn$Churn <- sapply(customer_churn$Churn, factor)
str(customer_churn)

#Split the data
sample_split <- floor(.7*nrow(customer_churn))
set.seed(1)
training <- sample(seq_len(nrow(customer_churn)), size = sample_split)
churn_train <- customer_churn[training,]
churn_test <- customer_churn[-training,]

#Support Vector Machine(SVM)
install.packages("e1071")
library(e1071)

svm_churn <- svm(Churn ~ .,churn_train)

install.packages("caret")
library(caret)

confusionMatrix(churn_train$Churn, predict(svm_churn), positive = "1")

#Test data
prediction <- predict(svm_churn, churn_test[-1])
prediction_result <- table(pred = prediction, true = churn_test[,1])
print(prediction_result)