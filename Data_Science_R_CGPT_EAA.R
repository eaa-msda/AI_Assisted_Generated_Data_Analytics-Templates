# Errol Ian Ave Acosta
# Data Science | Google Colab
# CGPT 
# February 16, 2025

# RStudio Data Science and Machine Learning Template

# 1. Load Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(lubridate)
library(caret)
library(randomForest)
library(e1071)
library(xgboost)
library(rpart)
library(rpart.plot)
library(ROCR)
library(ggcorrplot)
library(keras)

# 2. Import Data
data <- read_csv('path/to/your/data.csv')

# 3. Data Exploration
head(data)
str(data)
ggcorrplot(cor(data %>% select_if(is.numeric)), lab = TRUE)
summarise_all(data, funs(sum(is.na(.))))

# 4. Data Cleaning
data <- data %>% drop_na() %>% distinct()

# 5. Feature Engineering
data <- data %>% mutate(new_feature = log1p(existing_feature))

# 6. Data Visualization
ggplot(data, aes(x = feature1, y = feature2, color = target)) + geom_point() + theme_minimal()

# 7. Machine Learning Models
set.seed(123)
trainIndex <- createDataPartition(data$target, p = 0.8, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# Logistic Regression
log_model <- glm(target ~ ., data = train, family = binomial)
log_pred <- predict(log_model, test, type = 'response')

# Decision Tree
tree_model <- rpart(target ~ ., data = train, method = 'class')
rpart.plot(tree_model)
tree_pred <- predict(tree_model, test, type = 'class')

# Random Forest
rf_model <- randomForest(target ~ ., data = train)
rf_pred <- predict(rf_model, test)

# XGBoost
xgb_train <- xgb.DMatrix(data = as.matrix(train[-ncol(train)]), label = train$target)
xgb_test <- xgb.DMatrix(data = as.matrix(test[-ncol(test)]), label = test$target)
xgb_model <- xgboost(data = xgb_train, nrounds = 100, objective = 'binary:logistic')
xgb_pred <- predict(xgb_model, xgb_test)

# 8. Model Evaluation
confusionMatrix(factor(tree_pred), test$target)
confusionMatrix(factor(rf_pred), test$target)
pred <- prediction(xgb_pred, test$target)
perf <- performance(pred, "tpr", "fpr")
plot(perf, col=2, lwd=2)

# 9. Deep Learning Example
model <- keras_model_sequential()
model %>% layer_dense(units = 128, activation = 'relu', input_shape = ncol(train) - 1) %>%
  layer_dropout(0.2) %>% layer_dense(units = 1, activation = 'sigmoid')
model %>% compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = 'accuracy')
model %>% fit(as.matrix(train[-ncol(train)]), train$target, epochs = 50, batch_size = 32)

# 10. Export Results
write_csv(data, 'path/to/save/cleaned_data.csv')
