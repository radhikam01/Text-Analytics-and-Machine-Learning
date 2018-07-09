# Decision Tree Regression

library("rpart")
library("rpart.plot")

set.seed(8888)

train <- read.csv("C:/Users/Dhaval's Dell/Desktop/ADS Project/train.csv")
test  <- read.csv("C:/Users/Dhaval's Dell/Desktop/ADS Project/test.csv")

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


dim(train)
dim(test)

str(train)
str(test)

test$Response <- 0

data <- rbind(train, test)

training_data <- data[1:59381,]
testing_data <- data[59382:79146,]
testing_data$Response <- NULL

decision_tree <- rpart(Response ~ ., data = training_data, method = "class", control=rpart.control(cp=0.0001))
printcp(decision_tree) # display the results 
plotcp(decision_tree) # visualize cross-validation results 
summary(decision_tree)

# Pruning decision tree
pdtree<- prune(decision_tree, cp=   decision_tree$cptable[which.min(decision_tree$cptable[,"xerror"]),"CP"])

prp(pdtree, type = 4, extra = 100)

par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(pdtree) # visualize cross-validation results 

my_prediction <- predict(pdtree, testing_data, type = "class")
head(my_prediction)

vector_id <- testing_data$Id
my_solution <- data.frame(id = vector_id, Response = my_prediction)

head(my_solution)
write.csv(my_solution, file = "response.csv",row.names=FALSE)

rpart_test_predict <- predict(decision_tree,test,type="vector")
#calculate RMS error
rmsqe <- sqrt(mean((rpart_test_predict-test$Response)^2))
rmsqe






