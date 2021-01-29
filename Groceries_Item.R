#Demo 11: Groceries Item
setwd(choose.dir())
getwd()

install.packages("arules")
library(arules)

Groceries_Item = read.transactions("Groceries_Item.csv", sep = ",")
inspect(Groceries_Item[1:10])

AprioriForGroceries = apriori(Groceries_Item, parameter = list(support = 0.006, confidence = 0.5))
summary(AprioriForGroceries)
inspect(AprioriForGroceries)

AprioriForGroceries = apriori(Groceries_Item, parameter = list(support = 0.01, confidence = 0.5))
summary(AprioriForGroceries)
inspect(AprioriForGroceries)

inspect(sort(AprioriForGroceries, by = "confidence"))