#Assigning values and Applying Functions
#Data set : Bank Customers data

setwd(choose.dir())
getwd()

BankCustomer <- read.csv("Demo 2_ Assigning values and applying functions.csv")
View(BankCustomer)
install.packages("plyr")
library(plyr)

BankCustomer <- rename(BankCustomer, c("ï..age" = "Age"))
str(BankCustomer)
View(BankCustomer)

max(BankCustomer$Age)
min(BankCustomer$Age)

BankCustomerAgeCategorized <- transform(BankCustomer, Generation = ifelse(Age<22, "z", ifelse(Age<41, "y", ifelse(Age<53, "x", "Baby Boomers"))))
BankCustomerAgeCategorized

table(BankCustomerAgeCategorized$Generation, BankCustomerAgeCategorized$poutcome)