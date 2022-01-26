library(C50)
library(gmodels)

#Reading data from csv
wine_quality_1 <- read.csv("Data/wine.csv")


table(wine_quality_1$quality)

#Factoring "quality" feature in the dataset 
wine_quality_1$quality <- factor(wine_quality_1$quality, 
                                 levels = c("bad", "good"),
                                 labels = c("Bad", "Good"))


#Normalising function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Normalising data in the data set to fit between 0 to 1.
wine_n <- as.data.frame(lapply(wine_quality_1[1:11], normalize))

#Summary of the data post normalizing
summary(wine_n)

set.seed(1)

wine_n_rand <- wine_n[order(runif(1599)), ]

#Splitting the data into 90% training and 10% testing
wine_train <- wine_n_rand[1:1439, ]
wine_test <- wine_n_rand[1440:1599,]

#Splitting lables for training and test data
wine_train_labels <- wine_quality_1[1:1439, 12]
wine_test_labels <- wine_quality_1[1440:1599,12]


#Importing necessary models for building model and viewing accuracy
library(class)
library(gmodels)

#Building model for 2,20,40 and 42 k values
predictions1 <- knn(train = wine_train, test = 
                      wine_test, cl = wine_train_labels, k=2)



predictions2 <- knn(train = wine_train, test = 
                      wine_test, cl = wine_train_labels, k=20)



predictions3 <- knn(train = wine_train, test = 
                      wine_test, cl = wine_train_labels, k=40)



predictions4 <- knn(train = wine_train, test = 
                      wine_test, cl = wine_train_labels, k=42)



#Displaying output for each model
#K=2
CrossTable(predictions1, wine_test_labels, 
           prop.chisq = FALSE, 
           prop.c = FALSE, prop.r = FALSE)

#K=20
CrossTable(predictions2, wine_test_labels, 
           prop.chisq = FALSE, 
           prop.c = FALSE, prop.r = FALSE)

#K=40
CrossTable(predictions3, wine_test_labels, 
           prop.chisq = FALSE, 
           prop.c = FALSE, prop.r = FALSE)

#K=42
CrossTable(predictions4, wine_test_labels, 
           prop.chisq = FALSE, 
           prop.c = FALSE, prop.r = FALSE)



