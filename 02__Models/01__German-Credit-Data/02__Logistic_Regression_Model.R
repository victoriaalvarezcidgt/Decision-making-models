rm(list = ls())

library(dplyr)
library(rpart)

# Reading in data --------------------------------------------------------------
path_data <- "./01__Data/01__German_Credit_Data/"

german_data <- read.csv(file.path(path_data, "03__Other/01b__Data_FRED.csv"))

# Processing -------------------------------------------------------------------
german_data <- german_data %>%
  mutate(Class = recode(Class, "Bad" = 0, "Good" = 1))

# Splitting data --------------------------------------------------------------
set.seed(147)

# Store row numbers for training set: index_train
index_train <- sample(1:nrow(german_data), 2 / 3 * nrow(german_data))

# Create training set: training_set
training_set <- german_data[index_train, ]

# Create test set: test_set
test_set <- german_data[-index_train, ]

# Logistic Modelling -----------------------------------------------------------
log_model <- glm(formula = Class ~ ., family = "binomial", 
                 data = training_set)

# Make PD-predictions for all test set elements using the the full logistic regression model
predictions_all_full <- predict(log_model, 
                                newdata = test_set, 
                                type = "response")

# Look at the predictions range
range(predictions_all_full)

predicted_classes <- ifelse(predictions_all_full > 0.5, "Yes", "No")

# Evaluation
confusion_matrix <- table(predicted_classes, test_set$Class)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

accuracy
precision
recall
f1_score

