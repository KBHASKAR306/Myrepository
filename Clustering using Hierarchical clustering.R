#Clustering using Hierarchical clustering 
#Problem: Bank wants to launch 3 types of personal loan and would like to segment existing set of credit card customers
#into 3 groups to offer these products.
setwd(choose.dir())
getwd()
set.seed(111)

customer_data <- read.csv("Demo 4_K-Fold Cross validation.csv")
View(customer_data)
str(customer_data)

#data cleaning
customer_data <- na.omit(customer_data)
View(customer_data)

#Hierarchical clustering
cluster_h <- dist(customer_data, method = "euclidian")
fit <- hclust(cluster_h, method = "ward")
groups <- cutree(fit, k = 3)
groups
customer_data <- cbind(customer_data, ClusterNum = groups)
View(customer_data)

#Graph
plot(fit)
