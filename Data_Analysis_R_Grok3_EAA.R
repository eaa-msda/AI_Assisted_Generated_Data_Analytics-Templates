# # Errol Ian Ave Acosta
# Data Analysis | Google Colab
# Grok 3 Free  
# February 16, 2025

# Load necessary libraries
# Notes: Install these packages if you haven't already
library(tidyverse)  # For data manipulation and visualization
library(ggplot2)    # For advanced plotting
library(dplyr)      # For data manipulation
library(readr)      # For reading CSV files
library(corrplot)   # For correlation plots

# Load the dataset
# Notes: Replace 'path/to/your/dataset.csv' with your actual file path
data <- read_csv("path/to/your/dataset.csv")

# Initial Exploration
# Notes: Get a summary of the data structure and basic statistics
glimpse(data)  # Shows data types and first few observations
summary(data)  # Summarizes your data

# Check for missing values
# Notes: Replace 'column_name' with your actual column name
missing_values <- data %>%
  summarise_all(funs(sum(is.na(.))))
print(missing_values)

# Data Cleaning
# Notes: Example of handling missing data
data_clean <- data %>%
  # Replace NA in 'column_name' with mean of column
  mutate(column_name = if_else(is.na(column_name), mean(column_name, na.rm = TRUE), column_name))

# Data Visualization
# Notes: Visualize data distributions

# 1. Histogram for a numerical variable
# Replace 'numerical_variable' with your column name
ggplot(data_clean, aes(x = numerical_variable)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Numerical Variable", x = "Value", y = "Count")

# 2. Bar plot for categorical data
# Replace 'categorical_variable' with your column name
ggplot(data_clean, aes(x = categorical_variable)) +
  geom_bar(fill = "green") +
  labs(title = "Frequency of Categorical Variable", x = "Category", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Boxplot for outlier detection
# Replace 'variable_for_outliers' with your column name
ggplot(data_clean, aes(y = variable_for_outliers)) +
  geom_boxplot() +
  labs(title = "Boxplot for Outlier Detection", y = "Value")

# Correlation Analysis
# Notes: Compute and visualize correlation matrix
numeric_data <- data_clean %>%
  select_if(is.numeric)
correlation_matrix <- cor(numeric_data)
corrplot(correlation_matrix, method="circle")

# Data Manipulation
# Notes: Example of grouping and summarizing data
# Replace 'group_by_variable' and 'summarize_variable' with your column names
grouped_summary <- data_clean %>%
  group_by(group_by_variable) %>%
  summarise(mean_value = mean(summarize_variable, na.rm = TRUE),
            count = n())

print(grouped_summary)

# Create New Features
# Notes: Example of feature engineering
# Replace 'feature1' and 'feature2' with your actual column names
data_clean <- data_clean %>%
  mutate(new_feature = feature1 + feature2)

# Outlier Detection
# Notes: Remove outliers using IQR method
# Replace 'feature_for_outliers' with your column name
Q1 <- quantile(data_clean$feature_for_outliers, 0.25)
Q3 <- quantile(data_clean$feature_for_outliers, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

data_clean_no_outliers <- data_clean %>%
  filter(feature_for_outliers >= lower_bound & feature_for_outliers <= upper_bound)

# Basic Statistical Analysis
# Notes: Perform simple statistical tests
# Example: t-test between two groups
t_test_result <- t.test(data_clean$variable ~ data_clean$group, data = data_clean)
print(t_test_result)

# Save Cleaned Data
# Notes: Save the processed data for future use
# Replace 'path/to/save/cleaned_data.csv' with your desired save location
write_csv(data_clean_no_outliers, "path/to/save/cleaned_data.csv")

# Final Notes:
# - Adjust this script to match your specific dataset and analysis needs.
# - Be mindful when cleaning data; ensure methods like removing outliers or filling missing values are appropriate.
# - Consider privacy and ethical implications when dealing with sensitive data.
# - This template provides a starting point; real-world analysis often requires more advanced techniques.