library(ggplot2)

concrete_mixtures <- read.csv("Data/concrete_data.csv")


plot(concrete_mixtures)

cor(concrete_mixtures$Strength, concrete_mixtures$Cement)
cor(concrete_mixtures$Strength, concrete_mixtures$Blast.Furnace.Slag)
cor(concrete_mixtures$Strength, concrete_mixtures$Fly.Ash)
cor(concrete_mixtures$Strength, concrete_mixtures$Water)
cor(concrete_mixtures$Strength, concrete_mixtures$Superplasticizer)
cor(concrete_mixtures$Strength, concrete_mixtures$Coarse.Aggregate)
cor(concrete_mixtures$Strength, concrete_mixtures$Fine.Aggregate)
cor(concrete_mixtures$Strength, concrete_mixtures$Age)

set.seed(1)

concrete_data_rand <- concrete_mixtures[order(runif(1030)), ]

concrete_data_train <- concrete_data_rand[1:927, ]  
concrete_data_test <- concrete_data_rand[928:1030, ]  

#Model1 using all features
model1= lm(Strength ~ .,data=concrete_data_train)

summary(model1)


#Model2 - removing insignificant variables
model2= lm(Strength ~ Cement + Blast.Furnace.Slag + Fly.Ash + Water + Superplasticizer + Fine.Aggregate + Age ,data=concrete_data_train)

summary(model2)

anova(model1,model2)

#Model3 - Removing insignificant variables
model3= lm(Strength ~ Cement + Blast.Furnace.Slag + Fly.Ash + Water + Superplasticizer + Age ,data=concrete_data_train)


summary(model3)

anova(model2,model3)

anova(model1,model3)


###Evaluating model1 and model3


#Model1
result1 <- predict(model1,concrete_data_test)


actuals_preds1 <- data.frame(cbind(actuals=concrete_data_test$Strength, predicteds=result1))

correlation_accuracy1 <- cor(actuals_preds1)

#Accuracy
View(correlation_accuracy1)#71.4%

#RMSE
rmse1 = sqrt(mean((actuals_preds1$actuals - actuals_preds1$predicteds)^2))

rmse1

#Model3
result2 <- predict(model3,concrete_data_test)

actuals_preds2 <- data.frame(cbind(actuals=concrete_data_test$Strength, predicteds=result2))

correlation_accuracy2 <- cor(actuals_preds2)

#Accuracy
View(correlation_accuracy2) #71.2%

#RMSE
rmse2 = sqrt(mean((actuals_preds2$actuals - actuals_preds2$predicteds)^2))

rmse2


##Polynomial Regression

###Understanding corelation of Strength to the powers of each independent variables



## Determining the degree for implementing polynomial regression
rmse <- function(y, h)
{
  return(sqrt(mean((y - h) ^ 2)))
}

performance <- data.frame()

#Iterating over 1 to 5 degree to identify ideal degree for regression
for (d in 1:5)
{
  poly.fit <- lm(Strength ~ Blast.Furnace.Slag  + poly(Cement,  Fly.Ash , Water , Superplasticizer , Age , degree = d), data = concrete_data_train)
  performance <- rbind(performance,
                       data.frame(Degree = d,
                                  Data = 'Training',
                                  RMSE = rmse(concrete_data_train$Strength, predict(poly.fit))))
  
  # same for validation data
  performance <- rbind(performance,
                       data.frame(Degree = d,
                                  Data = 'validation',
                                  RMSE = rmse(concrete_data_test$Strength, predict(poly.fit,
                                                                                   newdata = concrete_data_test))))
}


ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) +
  geom_point() +
  geom_line()


model4 <- lm(Strength ~ Blast.Furnace.Slag  + poly(Cement,  Fly.Ash , Water , Superplasticizer , Age, degree = 4), data = concrete_data_train)

summary(model4)



###Evaluating model4


#Model4
result3 <- predict(model4,concrete_data_test)


actuals_preds3 <- data.frame(cbind(actuals=concrete_data_test$Strength, predicteds=result3))

correlation_accuracy3 <- cor(actuals_preds3)

#Accuracy
View(correlation_accuracy3)#91%

#RMSE
rmse3 = sqrt(mean((actuals_preds3$actuals - actuals_preds3$predicteds)^2))

rmse3#6.54

