#Import necessary libraries
library(readxl) # to read excel
library(plyr)
install.packages("caTools")
library(caTools)
library(e1071) 
library(caret) 
library(randomForest)
#Load Data
setwd(choose.dir())
getwd()
attribset = read_excel('Attribute DataSet.xlsx')
dresssale = read_excel('Dress Sales.xlsx')
#Remove dress_ID Columnattribset_1 = attribset[2:14] 
dresssale_1 = dresssale[2:24]
# check the unique values for each columns
#lapply(attribset[2:14], unique)
# values checking
# style
attribset_1$Style[attribset_1$Style == 'sexy'] = 'Sexy'

# Price
attribset_1$Price[attribset_1$Price == 'low'] = 'Low'
attribset_1$Price[attribset_1$Price == 'high'] = 'High'

# Size
attribset_1$Size[attribset_1$Size == 's'] = 'S' 
attribset_1$Size[attribset_1$Size == 'small'] = 'S'

# Season 
attribset_1$Season[attribset_1$Season == 'spring'] = 'Spring'
attribset_1$Season[attribset_1$Season == 'summer'] = 'Summer'
attribset_1$Season[attribset_1$Season == 'Automn'] = 'Autumn'
attribset_1$Season[attribset_1$Season == 'winter'] = 'Winter'

# NeckLine 
attribset_1$NeckLine[attribset_1$NeckLine == 'sweetheart'] = 'Sweetheart'

# SleeveLength
attribset_1$SleeveLength[attribset_1$SleeveLength == 'sleevless'] = 'sleeveless' 
attribset_1$SleeveLength[attribset_1$SleeveLength == 'sleeevless'] = 'sleeveless' 
attribset_1$SleeveLength[attribset_1$SleeveLength == 'sleveless'] = 'sleeveless' 
attribset_1$SleeveLength[attribset_1$SleeveLength == 'threequater'] = 'threequarter' 
attribset_1$SleeveLength[attribset_1$SleeveLength == 'thressqatar'] = 'threequarter' 
attribset_1$SleeveLength[attribset_1$SleeveLength == 'turndowncollor'] = 'turndowncollar'

# FabricType
attribset_1$FabricType[attribset_1$FabricType == 'shiffon'] = 'chiffon'
attribset_1$FabricType[attribset_1$FabricType == 'sattin'] = 'satin'
attribset_1$FabricType[attribset_1$FabricType == 'wollen'] = 'woolen'
attribset_1$FabricType[attribset_1$FabricType == 'flannael'] = 'flannel'
attribset_1$FabricType[attribset_1$FabricType == 'knitting'] = 'knitted'


# Decoration
attribset_1$Decoration[attribset_1$Decoration == 'embroidary'] = 'embroidery'
attribset_1$Decoration[attribset_1$Decoration == 'sequined'] = 'sequins'
attribset_1$Decoration[attribset_1$Decoration == 'ruched'] = 'ruche'
attribset_1$Decoration[attribset_1$Decoration == 'none'] = 'null'

# Pattern Type
attribset_1$'Pattern Type'[attribset_1$'Pattern Type' == 'none'] = 'null' 
attribset_1$'Pattern Type'[attribset_1$'Pattern Type' == 'leapord'] = 'leopard'
# factoring 

attribset_1$Style = factor(attribset_1$Style, 
                          levels = c('Sexy', 'Casual', 'vintage', 'Brief', 'cute', 'bohemian', 'Novelty', 'Flare', 'party', 'work', 'OL', 'fashion'),
                          labels = c(0,1,2,3,4,5,6,7,8,9,10,11))

attribset_1$Price = factor(attribset_1$Price, 
                          levels = c('Low', 'High', 'Average', 'Medium', 'very-high'),
                          labels = c(0,1,2,3,4))

attribset_1$Size = factor(attribset_1$Size, 
                         levels = c('M', 'L', 'XL', 'free', 'S'),
                         labels = c(0,1,2,3,4))

attribset_1$Season = factor(attribset_1$Season, 
                           levels = c('Summer', 'Autumn', 'Spring', 'Winter'),
                           labels = c(0,1,2,3))

attribset_1$NeckLine = factor(attribset_1$NeckLine, 
                             levels = c('o-neck', 'v-neck', 'boat-neck', 'peterpan-collor', 'ruffled', 'turndowncollor', 'slash-neck', 'mandarin-collor', 'open', 'sqare-collor', 'Sweetheart', 'Scoop', 'halter', 'backless', 'bowneck', 'NULL'),
                             labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))

attribset_1$SleeveLength = factor(attribset_1$SleeveLength, 
                                 levels = c('sleeveless', 'Petal', 'full', 'butterfly', 'short', 'threequarter', 'halfsleeve', 'cap-sleeves', 'turndowncollor', 'capsleeves', 'half', 'turndowncollar', 'NULL'),
                                 labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12))
attribset_1$waiseline = factor(attribset_1$waiseline, 
                              levels = c('empire', 'natural', 'null', 'princess', 'dropped'),
                              labels = c(0,1,2,3,4))

attribset_1$Material = factor(attribset_1$Material, 
                             levels = c('null', 'microfiber', 'polyster', 'silk', 'chiffonfabric', 'cotton', 'nylon', 'other', 'milksilk', 'linen', 'rayon', 'lycra', 'mix', 'acrylic', 'spandex', 'lace', 'modal', 'cashmere', 'viscos', 'knitting', 'sill', 'wool', 'model', 'shiffon'),
                             labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))

attribset_1$FabricType = factor(attribset_1$FabricType, 
                               levels = c('chiffon', 'null', 'broadcloth', 'jersey', 'other', 'batik', 'satin', 'flannel', 'worsted', 'woolen', 'poplin', 'dobby', 'knitted', 'tulle', 'organza', 'lace', 'Corduroy', 'terry'),
                               labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17))

attribset_1$Decoration = factor(attribset_1$Decoration, 
                               levels = c('ruffles', 'null', 'embroidery', 'bow', 'lace', 'beading', 'sashes', 'hollowout', 'pockets', 'sequins', 'applique', 'button', 'Tiered', 'rivet', 'feathers', 'flowers', 'pearls', 'pleat', 'crystal', 'ruche', 'draped', 'tassel', 'plain', 'cascading'),
                               labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))

attribset_1$`Pattern Type` = factor(attribset_1$`Pattern Type`, 
                                   levels = c('animal', 'print', 'dot', 'solid', 'null', 'patchwork', 'striped', 'geometric', 'plaid', 'leopard', 'floral', 'character', 'splice'),
                                   labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12))

attribset_1$Recommendation = sapply(attribset_1$Recommendation, factor)

# count of missing values in attribset_ dataset
colSums(is.na(attribset_1))

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# fill missing Value with mode
attribset_1$Price[is.na(attribset_1$Price) ==TRUE] <- getmode(attribset_1$Price)
attribset_1$Season[is.na(attribset_1$Season) ==TRUE] <- getmode(attribset_1$Season)
attribset_1$NeckLine[is.na(attribset_1$NeckLine) ==TRUE] <- getmode(attribset_1$NeckLine)
attribset_1$waiseline[is.na(attribset_1$waiseline) ==TRUE] <- getmode(attribset_1$waiseline)
attribset_1$Material[is.na(attribset_1$Material) ==TRUE] <- getmode(attribset_1$Material)
attribset_1$FabricType[is.na(attribset_1$FabricType) ==TRUE] <- getmode(attribset_1$FabricType)
attribset_1$Decoration[is.na(attribset_1$Decoration) ==TRUE] <- getmode(attribset_1$Decoration)
attribset_1$`Pattern Type`[is.na(attribset_1$`Pattern Type`) ==TRUE] <- getmode(attribset_1$`Pattern Type`)

attribset_data <- data.frame(attribset_1)
str(attribset_data)

# Update columns name in dresssale_ dataset

dresssale_1 = rename(dresssale_1,c('41314'='2/9/2013'))
dresssale_1 = rename(dresssale_1,c('41373'='4/9/2013'))
dresssale_1 = rename(dresssale_1,c('41434'='6/9/2013'))
dresssale_1 = rename(dresssale_1,c('41495'='8/9/2013'))
dresssale_1 = rename(dresssale_1,c('41556'='10/9/2013'))
dresssale_1 = rename(dresssale_1,c('41617'='12/9/2013'))
dresssale_1 = rename(dresssale_1,c('41315'='2/10/2013'))
dresssale_1 = rename(dresssale_1,c('41374'='4/10/2013'))
dresssale_1 = rename(dresssale_1,c('41435'='6/10/2013'))
dresssale_1 = rename(dresssale_1,c('40400'='8/10/2013'))
dresssale_1 = rename(dresssale_1,c('41557'='10/10/2013'))
dresssale_1 = rename(dresssale_1,c('41618'='12/10/2013'))

# Convert all variable types to numeric
dresssale_1 <- as.data.frame(apply(dresssale_1, 2, as.numeric))

# mean row 
dresssale_1 = as.matrix(dresssale_1)
k <- which(is.na(dresssale_1), arr.ind=TRUE)
dresssale_1[k] <- rowMeans(dresssale_1, na.rm=TRUE)[k[,1]]
dresssale_1 = as.data.frame(dresssale_1)

# sum all values on row on (total sales)
dresssale_1$total_sales = rowSums(dresssale_1)
head(dresssale_1)

#Merged Data
merged_data <- data.frame(attribset_1 ,dresssale_1)
head(merged_data)

str(merged_data)

# spliting dataset 
set.seed(100)

spl = sample.split(merged_data$Recommendation, SplitRatio = 0.7)
train = subset(merged_data, spl==TRUE)
test = subset(merged_data, spl==FALSE)

print(dim(train)); print(dim(test))

#Classification - Predict recommendation
#First model (Naive Bayes)

# naive bayes model
naive_model = naiveBayes(Recommendation ~.,data = train) # build model
confusionMatrix(train$Recommendation,predict(naive_model,train),positive = '1') # create confusion Matrix
print('---------------')
naive_predict = predict(naive_model,test) # predict test set
table(naive_predict,test$Recommendation) # create table

