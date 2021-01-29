#Perform Regression Analysis with Multiple variables

#Part 1: Cars Data : The buyers of cars are interested in determining the factors which influence the car mileage in the city.

setwd(choose.dir())
getwd()

cars_data <- read.csv("Demo 2_ Perform Regression Analysis with multiple variables.csv")
View(cars_data)
summary(cars_data)
str(cars_data)

cars_results <- lm(formula = MPG_City ~ Type + Origin + DriveTrain + EngineSize + Cylinders + Horsepower + Weight + Wheelbase + Length, data = cars_data)
cars_results
summary(cars_results)

# Part 2: Boston Data : Consists of variables to predict price of houses. A Developer wants to know the factors which influence pricing of house.

boston_data <- read.csv("Boston.csv")
View(boston_data)
summary(boston_data)
str(boston_data)

boston_results <- lm(formula = MEDV ~ ., data = boston_data)
boston_results
summary(boston_results)

