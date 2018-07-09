library(e1071)
#install.packages("formattable")
#install.packages("dplyr")
library(formattable)
set.seed(8888)

train <- read.csv("C:/Users/Dhaval's Dell/Desktop/ADS Project/train.csv")

# Dummy Variables
dummyVars <- paste('Medical_Keyword_', 1:48, sep = '')

for (var in dummyVars) {
  train[[var]] <- factor(train[[var]])
  
}

feature.names <- names(train)[2:ncol(train)-1]

# fix NA's

for (f in feature.names) {
  if (class(train[[f]])=="integer" || class(train[[f]])=="numeric") {
    mean <- mean(train[[f]], na.rm = T)
    train[[f]][is.na(train[[f]])] <- mean
  }
}
str(train)

# replacing categorical variables with numeric ids

for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
  }
}

#  -------------------------------------------------------------------------------------------

rmse <- function(error)
{
  sqrt(mean(error^2))
}

library(dplyr)
train <- sample_n(train, 1200)
train <- subset(train, select = -Id)

#SVM Code

model <- svm(train$Response ~ Product_Info_4 + Ins_Age + Ht + Wt + BMI + Employment_Info_1 + Employment_Info_4 + Employment_Info_6 + Insurance_History_5 + Family_Hist_2 + Family_Hist_3 + Family_Hist_4 + Family_Hist_5, data = train)
summary(model)
predictedResponse <- predict(model,train) 

error <- train$Response - predictedResponse
svrPredictionRMSE <- rmse(error)

# Run Prediction and you can measuring the execution time in R

#pred <- predict(model1,x)
#system.time(pred <- predict(model1,x)) <-- USE THIS IN THE END!! 

# See the confusion matrix result of prediction, using command table to compare the result of SVM
# prediction and the class data in y variable.

#table(pred,y)<-- USE THIS IN THE END!! 

# Tuning SVM to find the best cost and gamma ..

svm_tune <- tune (svm, Response ~ Product_Info_4 + Ins_Age + Ht + Wt + BMI + 
                    Employment_Info_1 + Employment_Info_4 + Employment_Info_6 + Insurance_History_5 + 
                    Family_Hist_2 + Family_Hist_3 + Family_Hist_4 + Family_Hist_5, 
                  data = train, ranges=list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))

print(svm_tune)
plot(svm_tune)


#Tuning the model further

svm_tune <- tune (svm, Response ~ Product_Info_4 + Ins_Age + Ht + Wt + BMI + 
                    Employment_Info_1 + Employment_Info_4 + Employment_Info_6 + Insurance_History_5 + 
                    Family_Hist_2 + Family_Hist_3 + Family_Hist_4 + Family_Hist_5, 
                  data = train, ranges=list(epsilon = seq(0.6,1,0.1), cost = 2^(2:3)))

print(svm_tune)
plot(svm_tune)


tunedModel  <- svm_tune$best.model
predictedResponse <- predict(tunedModel, train) 

error <- train$Response - predictedResponse
tunedModelRMSE <- rmse(error)
tunedModelRMSE