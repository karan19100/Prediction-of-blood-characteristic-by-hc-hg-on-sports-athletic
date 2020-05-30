library(DAAG)
head(ais, n=10)

# packages 

library(e1071)
library(plyr)
library(ggplot2)
ais2 <- subset(ais, sex=="m") # only male athletes
ais3 = ais2[,c(3,4)] # subset column number that correspond to "hg" and "hc"
newdata <- rename(ais3, c("hg"="HEMAGLOBIN", "hc"="HEMATOCRIT"))
str(newdata) # new dataset now includes 102 observations

set.seed(123)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(newdata2), 0.7*nrow(newdata2)) 
 #  training and testing: 70/30 split
trainingData <- newdata2[trainingRowIndex, ]  # training data
testData  <- newdata2[-trainingRowIndex, ]   # test data

# Now it’s possible to develop the model on the training data and use it to predict HEMATOCRIT on test data

modTrain <- lm(HEMATOCRIT ~ HEMAGLOBIN, data=trainingData)  # build the model
predict <- predict(modTrain, testData)  # predicted values
summary(modTrain)

act_pred <- data.frame(cbind(actuals=testData$HEMATOCRIT, predicteds=predict)) # actuals_predicteds 
cor(act_pred) # correlation_accuracy
head(act_pred, n=10)

# Actual values and predicted ones seem very close to each other. A good metric to see how much they are close is the min-max accuracy, that considers the average between the minimum and the maximum prediction.

min_max <- mean(apply(act_pred, 1, min) / apply(act_pred, 1, max))  
print(min_max) # show the result

mape <- mean(abs((act_pred$predicteds - act_pred$actuals))/act_pred$actuals)
print(mape) # show the result

#  RESAMPLING METHOD 
 
 # Cross Validation 

kfold <- CVlm(data = newdata2, form.lm = formula(HEMATOCRIT ~ HEMAGLOBIN), m=5, 
                   dots = FALSE, seed=123, legend.pos="topleft",
                   main="Cross Validation; k=5",
                   plotit=TRUE, printit=FALSE)

# The mean squared error measures how a regression line is close to a set of points
attr(kfold, 'ms')

The value of 0.14 is low, and it represents a good accuracy result after running new model successfully.



