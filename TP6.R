library(caTools)
#library(glm)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
#q18)
data <- read.csv("/Users/kiouloueleonor/Documents/Ecole/A4/Machine Learning/TP6/spam.csv", header = TRUE, stringsAsFactors=FALSE)
View(data)

data[,'spam'] <- as.factor(data[,'spam'])
data$spam <- as.factor(data$spam)
str(data)

set.seed(123)
split = sample.split(data$spam, SplitRatio = 0.75)
training_data = subset(data, split == TRUE)
testing_data = subset(data, split == FALSE)

calc_acc = function(predicted, actual) {
  mean(predicted == actual)
}

# Logistic regression model
reg_mod <- glm(spam~., family = "binomial", data=training_data)
summary(classifier.logreg)

reg_pred <- predict(reg_mod, testing_data, type="response")
reg_pred_fact <- as.logical(ifelse(reg_pred > 0.5, TRUE, FALSE))

reg_acc = mean(testing_data$spam == reg_pred_fact)
reg_acc

# Simple classification tree
tree_mod = rpart(spam~., data = training_data)
rpart.plot(tree_mod)

tree_pred <- predict(tree_mod,testing_data, type='class')

tree_acc = mean(testing_data$spam == tree_pred)
tree_acc

# Bagging 
bag_mod <- randomForest(spam~., training_data, mtry = 57, importance=TRUE, ntree= 500)
bag_pred <- predict (bag_mod,testing_data)
bag_acc <- mean(testing_data$spam == bag_pred)
bag_acc

# Random Forests
rdm_mod <- randomForest(spam~.,training_data, mtry = 4, importance=TRUE, ntree= 500)
rdm_pred <- predict (rdm_mod, testing_data)
rdm_acc <- mean(testing_data$spam == rdm_pred)
rdm_acc

# Boosting model
training_data$spam01 = ifelse(training_data$spam == FALSE, 0, 1)
testing_data$spam01 = ifelse(testing_data$spam == FALSE, 0, 1)

gbm_mod <- gbm(spam01~., training_data, distribution = 'bernoulli', n.tree = 5000, interaction.depth = 4, shrinkage = 0.01)
gbm_pred <- predict(gbm_mod, testing_data)
gbm_acc <- mean(testing_data$spam01 == gbm_pred)
gbm_acc

#q19)
library(ggplot2)
library(caret)

tuned_mod <- train(spam~., data, method="C5.0")


