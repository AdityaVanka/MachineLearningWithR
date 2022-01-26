library(C50)
library(gmodels)

#Loading data
wine_quality <- read.csv("Data/wine.csv")

#Summary of the data
summary(wine_quality)

set.seed(1)

wine_quality_rand <- wine_quality[order(runif(1599)), ]

#Splitting data into training and test
wine_quality_train <- wine_quality_rand[1:1439, ]
wine_quality_test  <- wine_quality_rand[1439:1599, ]

prop.table(table(wine_quality_train$quality))
prop.table(table(wine_quality_test$quality))

#Modelling
model <- C5.0(quality ~ ., data = wine_quality_train)

model

#Summary of the model
summary(model)

plot(model)

#Predicting the result on test data
predictions <- predict(model, wine_quality_test)

#Validating accuracy
CrossTable(predictions, wine_quality_test$quality,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted quality', 'actual quality')) #71.5%

#Boosting decision tree model
model2 <- C5.0(quality ~ ., data = wine_quality_train, trails = 10)

#Predicting on boosted model
predictions2 <- predict(model2, wine_quality_test)

#Validating accuracy
CrossTable(predictions2, wine_quality_test$quality,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted quality', 'actual quality'))#71.5%


