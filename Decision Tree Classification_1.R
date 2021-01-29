#Bank is interested in knowing the factors which influence the default of a customer
#Bank will utilize this information in future to avoid giving loans to the customers who are more risky to the business. 
setwd(choose.dir())
getwd()
#Load necessary packages
install.packages("rpart")
library(e1071)
library(plyr)
library(caret)
library(mlbench)
library(rpart)
#Load and Verify the bank data
bank_loan <- read.csv("Demo 3_ Decision Tree Classification.csv")
View(bank_loan)
str(bank_loan)
#Convert default from int to factor
bank_loan$Default <- sapply(bank_loan$Default, factor)
str(bank_loan)
#build the model
tree_model <- rpart(Default ~., data = bank_loan)
tree_model
#Analyze results
printcp(tree_model)
plotcp(tree_model)
print(tree_model)
summary(tree_model)
plot(tree_model)
#k-fold cross validation
folded_up <- createFolds(bank_loan, k=10, list = TRUE, returnTrain = FALSE)
train_set <- names(folded_up[1])
bank_loan[folded_up$train_set,]