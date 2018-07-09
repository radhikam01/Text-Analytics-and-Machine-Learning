install.packages("party")

library(dplyr)     
library(ggplot2)
library(rpart)
library(rpart.plot)
library(party)

train <- read.csv("C:/Users/Dhaval's Dell/Desktop/ADS Project/train.csv")
test  <- read.csv("C:/Users/Dhaval's Dell/Desktop/ADS Project/test.csv")

nrow(train) 

summary(train$Response)

table(train$Response)

# grow tree 
decision_tree <- rpart(Response ~ ., method="class", data=train)

printcp(decision_tree) # display the results 
plotcp(decision_tree)  # visualize cross-validation results 
summary(decision_tree) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(decision_tree) # visualize cross-validation results 

plot(decision_tree, uniform=TRUE,
     main="Classification Tree")
text(decision_tree, use.n=TRUE, all=TRUE, cex=.50)

# prp function provides clear view of the tree, much better than previous graphic
prp(decision_tree)