# Errol Ian Ave Acosta
# Data Science | Google Colab
# Grok 3 Free | Data Science
# February 16, 2025

# Load necessary libraries
# Notes: Install these packages if you haven't already
library(tidyverse)  # For data manipulation and visualization
library(caret)      # For machine learning tools
library(randomForest) # For Random Forest model
library(e1071)      # For SVM
library(glmnet)     # For regularized regression
library(ROCR)       # For ROC curves
library(psych)      # For descriptive statistics

# Load the dataset
# Notes: Replace 'path/to/your/dataset.csv' with your actual file path
data <- read_csv("path/to/your/dataset.csv")

# Initial Exploration
# Notes: Understand the structure and basic statistics of the data
glimpse(data)
summary(data)

# Check for Missing Values
# Notes: Replace 'column_name' with your actual column name
missing_values <- data %>%
  summarise_all(funs(sum(is.na(.))))
print(missing_values)

# Data Cleaning and Preprocessing
# Notes: Handle missing values, encode categorical variables, etc.
data_clean <- data %>%
  # Replace NA with mean or median (adjust as needed)
  mutate(column_name = if_else(is.na(column_name), mean(column_name, na.rm = TRUE), column_name)) %>%
  # One-hot encode categorical variables (adjust column names)
  mutate_at(vars(categorical_column1, categorical_column2), as.factor) %>%
  model.matrix(~ . - 1, data = .) %>%  # -1 to remove intercept
  as.data.frame()

# Split Data into Training and Testing
# Notes: Adjust 'target_column' to your actual outcome variable
set.seed(42)  # for reproducibility
trainIndex <- createDataPartition(data_clean$target_column, p = .8, list = FALSE, times = 1)
trainData <- data_clean[ trainIndex,]
testData  <- data_clean[-trainIndex,]

# Feature Scaling
# Notes: Normalize numerical features
preProcess <- preProcess(trainData[, -which(names(trainData) == "target_column")], method = c("center", "scale"))
trainData_scaled <- predict(preProcess, trainData)
testData_scaled <- predict(preProcess, testData)

# Feature Selection (Example using correlation)
# Notes: Remove highly correlated features
correlationMatrix <- cor(trainData_scaled)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
trainData_selected <- trainData_scaled[, -highlyCorrelated]
testData_selected <- testData_scaled[, -highlyCorrelated]

# Model Training
# Notes: Here are examples with different algorithms

# Random Forest
rf_model <- randomForest(target_column ~ ., data = trainData_selected, ntree = 500)

# Support Vector Machine (SVM)
svm_model <- svm(target_column ~ ., data = trainData_selected, kernel = "radial", probability = TRUE)

# Logistic Regression with Regularization
logit_model <- glmnet(as.matrix(trainData_selected[,-which(names(trainData_selected) == "target_column")]), 
                      trainData_selected$target_column, 
                      family = "binomial", 
                      alpha = 1,  # LASSO
                      lambda = 0.1)

# Model Evaluation
# Notes: Use test data to evaluate models

# Random Forest Predictions
rf_pred <- predict(rf_model, newdata = testData_selected, type = "response")
rf_confusion_matrix <- confusionMatrix(rf_pred, testData$target_column)
print(rf_confusion_matrix)

# SVM Predictions
svm_pred <- predict(svm_model, newdata = testData_selected, probability = TRUE)
svm_confusion_matrix <- confusionMatrix(svm_pred, testData$target_column)
print(svm_confusion_matrix)

# Logistic Regression Predictions
logit_pred <- predict(logit_model, newx = as.matrix(testData_selected[,-which(names(testData_selected) == "target_column")]), type = "response", s = 0.1)
logit_pred_class <- ifelse(logit_pred > 0.5, 1, 0)
logit_confusion_matrix <- confusionMatrix(factor(logit_pred_class), factor(testData$target_column))
print(logit_confusion_matrix)

# ROC Curve for Random Forest
pred <- prediction(as.numeric(rf_pred), testData$target_column)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

# Feature Importance (for Random Forest)
importance <- importance(rf_model)
varImpPlot(rf_model, main="Feature Importance")

# Save Model (example with Random Forest)
# Notes: Replace 'path/to/save/model.rds' with your desired save location
saveRDS(rf_model, "path/to/save/model.rds")

# Final Notes:
# - This template covers basic steps; real projects might require more complex preprocessing, feature engineering, or model tuning.
# - Always validate models on unseen data to check for overfitting.
# - Consider using cross-validation for a more robust model evaluation.
# - Be mindful of class imbalance if your target variable is not evenly distributed.