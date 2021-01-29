#Identifying Data Structures
#Bank Customers Data:

install.packages("readxl")
library(readxl)
setwd("C:/Users/HP/OneDrive/Desktop")
getwd()
BankCustomer <- read_excel("Demo 1_Identifying Data Structures.xlsx")
setwd(choose.dir())
getwd()
BankCustomer1 <- read.csv("Demo 1_ Identifying Data Structures.csv")
View(BankCustomer1)
str(BankCustomer1)
BankCustomer1 <- read.csv("Demo 1_ Identifying Data Structures.csv", stringsAsFactors = TRUE)
str(BankCustomer1)
BankCustomer1 <- read.csv("Demo 1_ Identifying Data Structures.csv", stringsAsFactors = FALSE)
str(BankCustomer1)