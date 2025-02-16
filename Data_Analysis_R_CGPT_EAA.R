# Errol Ian Ave Acosta
# Data Analysis | Google Colab
# CGPT 
# February 16, 2025

# RStudio Data Analyst Template

# 1. Load Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(lubridate)
library(caret)

# 2. Import Data
data <- read_csv('path/to/your/data.csv')

# 3. Data Exploration
head(data)
str(data)
summarise_all(data, funs(sum(is.na(.))))

# 4. Data Cleaning
data <- data %>%
  drop_na() %>%
  distinct()

# 5. Data Transformation
data <- data %>%
  mutate(new_column = old_column * 100)

# 6. Data Visualization
ggplot(data, aes(x = var1, y = var2)) +
  geom_point() +
  theme_minimal()

# 7. Statistical Analysis
summary(lm(var2 ~ var1, data = data))

# 8. Model Building
set.seed(123)
trainIndex <- createDataPartition(data$target, p = 0.8, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

model <- train(target ~ ., data = train, method = 'lm')

# 9. Model Evaluation
predictions <- predict(model, test)
postResample(predictions, test$target)

# 10. Export Results
write_csv(data, 'path/to/save/cleaned_data.csv')
