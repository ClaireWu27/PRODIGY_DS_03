library(caret)
library(pROC)
library(rpart)    
library(rpart.plot) 
data <- read.csv("bank-additional-full.csv") 

#remove outliers campaign >30
cleaned_data <- data[-which(data$campaign > 30), ]
#get the dataset
cleaned_data$y <- factor(cleaned_data$y)
cleaned_data$duration <- NULL
summary(cleaned_data$y)

#split training and testing set
set.seed(123)
split_index <- sample(1:nrow(cleaned_data), 0.8 * nrow(cleaned_data))
train_data <- cleaned_data[split_index, ]
test_data <- cleaned_data[-split_index, ]

# build a decision tree model
tree_model <- rpart(y ~ ., data = train_data, method = "class")

# visualize decision tree
rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE, main = "Decision Tree for Predicting y")

# predict on testing set
predictions <- predict(tree_model, newdata = test_data, type = "class")

# evaluate performance
conf_matrix <- table(test_data$y, predictions)
print(conf_matrix)

