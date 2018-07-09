library("rpart")
library("rpart.plot")


set.seed(8888)

train <- read.csv("C:/My_Data/Personal Data/Important Documents/MSIS/Course Work/Sem 3 - Fall 16/ADS/Assignments/Midterm_Project/train.csv")
test  <- read.csv("C:/My_Data/Personal Data/Important Documents/MSIS/Course Work/Sem 3 - Fall 16/ADS/Assignments/Midterm_Project/test.csv")

# Dummy Variables
dummyVars <- paste('Medical_Keyword_', 1:48, sep = '')

for (var in dummyVars) {
  train[[var]] <- factor(train[[var]])
  test[[var]] <- factor(test[[var]])
}


feature.names <- names(train)[2:ncol(train)-1]
str(train)
str(test)

# fix NA's

for (f in feature.names) {
  if (class(train[[f]])=="integer" || class(train[[f]])=="numeric") {
    mean <- mean(train[[f]], na.rm = T)
    train[[f]][is.na(train[[f]])] <- mean
    test[[f]][is.na(test[[f]])] <- mean
  }
}
str(train)
str(test)


# replacing categorical variables with numeric ids

for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}


#replace missing values with -1 - i incase any 


# train[is.na(train)] <- -1
# test[is.na(test)]   <- -1

dim(train)
dim(test)

str(train)
str(test)

test$Response <- 0

data <- rbind(train, test)

training_data <- data[1:59381,]
#training_data <- data[1:500,]
testing_data <- data[59382:79146,]
#testing_data <- data[501:700,]
testing_data$Response <- NULL

#Multiple Linear Regression
#Full Model
multiple_reg_full <- lm(Response ~ Product_Info_1 + Product_Info_2 + Product_Info_3 + Product_Info_4 + Product_Info_5 + Product_Info_6 + Product_Info_7 + Ins_Age + Ht + Wt + BMI + Employment_Info_1 + Employment_Info_2 + Employment_Info_3 + Employment_Info_4 + Employment_Info_5 + Employment_Info_6 + InsuredInfo_1 + InsuredInfo_2 + InsuredInfo_3 + InsuredInfo_4 + InsuredInfo_5 + InsuredInfo_6 + InsuredInfo_7 + Insurance_History_1 + Insurance_History_2 + Insurance_History_3 + Insurance_History_4 + Insurance_History_5 + Insurance_History_7 + Insurance_History_8 + Insurance_History_9 + Family_Hist_1 + Family_Hist_2 + Family_Hist_3 + Family_Hist_4 + Family_Hist_5 + Medical_History_1 + Medical_History_2 + Medical_History_3 + Medical_History_4 + Medical_History_5 + Medical_History_6 + Medical_History_7 + Medical_History_8 + Medical_History_9 + Medical_History_10 + Medical_History_11 + Medical_History_12 + Medical_History_13 + Medical_History_14 + Medical_History_15 + Medical_History_16 + Medical_History_17 + Medical_History_18 + Medical_History_19 + Medical_History_20 + Medical_History_21 + Medical_History_22 + Medical_History_23 + Medical_History_24 + Medical_History_25 + Medical_History_26 + Medical_History_27 + Medical_History_28 + Medical_History_29 + Medical_History_30 + Medical_History_31 + Medical_History_32 + Medical_History_33 + Medical_History_34 + Medical_History_35 + Medical_History_36 + Medical_History_37 + Medical_History_38 + Medical_History_39 + Medical_History_40 + Medical_History_41 + Medical_Keyword_1 + Medical_Keyword_2 + Medical_Keyword_3 + Medical_Keyword_4 + Medical_Keyword_5 + Medical_Keyword_6 + Medical_Keyword_7 + Medical_Keyword_8 + Medical_Keyword_9 + Medical_Keyword_10 + Medical_Keyword_11 + Medical_Keyword_12 + Medical_Keyword_13 + Medical_Keyword_14 + Medical_Keyword_15 + Medical_Keyword_16 + Medical_Keyword_17 + Medical_Keyword_18 + Medical_Keyword_19 + Medical_Keyword_20 + Medical_Keyword_21 + Medical_Keyword_22 + Medical_Keyword_23 + Medical_Keyword_24 + Medical_Keyword_25 + Medical_Keyword_26 + Medical_Keyword_27 + Medical_Keyword_28 + Medical_Keyword_29 + Medical_Keyword_30 + Medical_Keyword_31 + Medical_Keyword_32 + Medical_Keyword_33 + Medical_Keyword_34 + Medical_Keyword_35 + Medical_Keyword_36 + Medical_Keyword_37 + Medical_Keyword_38 + Medical_Keyword_39 + Medical_Keyword_40 + Medical_Keyword_41 + Medical_Keyword_42 + Medical_Keyword_43 + Medical_Keyword_44 + Medical_Keyword_45 + Medical_Keyword_46 + Medical_Keyword_47 + Medical_Keyword_48, data = training_data)
multiple_reg_full
summary(multiple_reg_full)


#Reduced Model
#multiple_reg_red <- lm(Response ~ Product_Info_2 + Product_Info_4 + Product_Info_6 + Ins_Age + Ht + Wt + BMI + Employment_Info_3 + InsuredInfo_1 + InsuredInfo_2 + InsuredInfo_5 + InsuredInfo_6 + InsuredInfo_7 + Insurance_History_1 + Insurance_History_2 + Family_Hist_2 + Family_Hist_3 + Family_Hist_4 + Family_Hist_5 + Medical_History_1 + Medical_History_2 + Medical_History_3 + Medical_History_4 + Medical_History_5 + Medical_History_7 + Medical_History_11 + Medical_History_12 + Medical_History_13 + Medical_History_14 + Medical_History_15 + Medical_History_17 + Medical_History_18 + Medical_History_19 + Medical_History_20 + Medical_History_22 + Medical_History_23 + Medical_History_27 + Medical_History_28 + Medical_History_29 + Medical_History_30 + Medical_History_31 + Medical_History_32 + Medical_History_35 + Medical_History_38 + Medical_History_39 + Medical_History_40 + Medical_Keyword_2 + Medical_Keyword_3 + Medical_Keyword_6 + Medical_Keyword_9 + Medical_Keyword_15 + Medical_Keyword_19 + Medical_Keyword_22 + Medical_Keyword_25 + Medical_Keyword_26 + Medical_Keyword_31 + Medical_Keyword_33 + Medical_Keyword_34 + Medical_Keyword_37 + Medical_Keyword_38 + Medical_Keyword_39 + Medical_Keyword_41 + Medical_Keyword_45, data = training_data)
#multiple_reg_red <- lm(Response ~ Ins_Age + Ht + Wt + BMI, data = training_data)
#multiple_reg_red <- lm(Response ~ Product_Info_1 + Product_Info_2 + InsuredInfo_2 + Insurance_History_8 + Medical_History_15 + Medical_History_18, data = training_data)

#Below is step by step method
multiple_reg_red <- lm(Response ~ Product_Info_1 + Product_Info_3 + Product_Info_4 + Product_Info_5 + Product_Info_6 + Product_Info_7 + Ins_Age + Ht + Wt + BMI + Employment_Info_1 + Employment_Info_2 + Employment_Info_3 + Employment_Info_4 + Employment_Info_5 + Employment_Info_6 + InsuredInfo_3 + InsuredInfo_4 + InsuredInfo_5 + InsuredInfo_6 + InsuredInfo_7 + Insurance_History_2 + Insurance_History_3 + Insurance_History_4 + Insurance_History_5 + Insurance_History_7 + Insurance_History_9 + Family_Hist_1 + Family_Hist_4 + Medical_History_1 + Medical_History_2 + Medical_History_4 + Medical_History_5 + Medical_History_6 + Medical_History_8 + Medical_History_9 + Medical_History_10 + Medical_History_13 + Medical_History_15 + Medical_History_16 + Medical_History_17 + Medical_History_18 + Medical_History_20 + Medical_History_21 + Medical_History_23 + Medical_History_24 + Medical_History_25 + Medical_History_26 + Medical_History_27 + Medical_History_28 + Medical_History_30 + Medical_History_31 + Medical_History_32 + Medical_History_33 + Medical_History_34 + Medical_History_35 + Medical_History_36 + Medical_History_37 + Medical_History_38 + Medical_History_40 + Medical_History_41 + Medical_Keyword_1 + Medical_Keyword_2 + Medical_Keyword_3 + Medical_Keyword_4 + Medical_Keyword_5 + Medical_Keyword_6 + Medical_Keyword_7 + Medical_Keyword_8 + Medical_Keyword_9 + Medical_Keyword_10 + Medical_Keyword_11 + Medical_Keyword_12 + Medical_Keyword_13 + Medical_Keyword_14 + Medical_Keyword_15 + Medical_Keyword_16 + Medical_Keyword_17 + Medical_Keyword_18 + Medical_Keyword_19 + Medical_Keyword_20 + Medical_Keyword_22 + Medical_Keyword_23 + Medical_Keyword_24 + Medical_Keyword_25 + Medical_Keyword_26 + Medical_Keyword_27 + Medical_Keyword_28 + Medical_Keyword_29 + Medical_Keyword_30 + Medical_Keyword_31 + Medical_Keyword_32 + Medical_Keyword_33 + Medical_Keyword_34 + Medical_Keyword_35 + Medical_Keyword_36 + Medical_Keyword_37 + Medical_Keyword_38 + Medical_Keyword_39 + Medical_Keyword_40 + Medical_Keyword_41 + Medical_Keyword_42 + Medical_Keyword_43 + Medical_Keyword_44 + Medical_Keyword_45 + Medical_Keyword_46 + Medical_Keyword_47 + Medical_Keyword_48, data = training_data)

#Below is by continuous variables
#multiple_reg_red <- lm(Response ~ Product_Info_4 + Ins_Age + Ht + Wt + BMI + Employment_Info_3 + Family_Hist_2 + Family_Hist_3 + Family_Hist_4 + Family_Hist_5, data = training_data)
multiple_reg_red
summary(multiple_reg_red)

#Plot
plot(multiple_reg_red)

#ANOVA that is analysis of variance
anova(multiple_reg_red, multiple_reg_full)

#Predict the response for test dataset
result <- predict(multiple_reg_red, testing_data)
head(result)
print(result)

vector_id <- testing_data$Id
my_result <- data.frame(id = vector_id, Response = result)
head(my_result)

#Calculate RMSE
multiple_reg_red.rmse <- sqrt(mean(multiple_reg_red$residuals^2))
print(multiple_reg_red.rmse)

write.csv(my_result, file = "C:/My_Data/Personal Data/Important Documents/MSIS/Course Work/Sem 3 - Fall 16/ADS/Assignments/Midterm_Project/LinearRegResults.csv", row.names = FALSE)
