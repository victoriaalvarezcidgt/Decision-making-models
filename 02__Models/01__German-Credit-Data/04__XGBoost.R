# XGBoost Decision Tree --------------------------------------------------------
rm(list = ls())

library(dplyr)
library(xgboost)
library(ROCR)
library(Boruta) # for feature selection

# Reading in data --------------------------------------------------------------
path_data <- "./01__Data/01__German_Credit_Data/"

german_data <- read.csv(file.path(path_data, "01__GCD-Binary.csv"))

# Processing -------------------------------------------------------------------
german_data <- german_data %>%
  mutate(Class = recode(Class, "Bad" = 0, "Good" = 1)) %>%
  mutate(Class = as.numeric(as.factor(Class))) %>%
  mutate(Class = Class - 1)

# Splitting data ---------------------------------------------------------------
set.seed(111)
# Store row numbers for training set: index_train
index_train <- caret::createDataPartition(german_data$Class, p = 0.7, list = FALSE)

# Create training set: training_set
training_set <- german_data[index_train, ]

# Create test set: test_set
test_set <- german_data[-index_train, ]

# Training XGBoost model -------------------------------------------------------
set.seed(112)

# Preparing training matrix
train_matrix <- xgb.DMatrix(data = as.matrix(training_set[, -1]),
                            label = training_set$Class)

# Set the XGBoost parameters
params <- list(
  objective = "binary:logistic", # Binary classification
  eval_metric = "error"          # Metric to evaluate model performance
)

# Train the XGBoost model
xgb_model <- xgboost(data = train_matrix, params = params, nrounds = 100)

# Preparing test matrix
test_matrix <- xgb.DMatrix(data = as.matrix(test_set[, -1]), 
                           label = test_set$Class)

predictions <- predict(xgb_model, test_matrix)

# Evaluating and tuning --------------------------------------------------------
set.seed(113)

# Setting initial threshold of 0.5
predictions_binary <- as.factor(ifelse(predictions > 0.5, 1, 0))

# Creating confusion matrix
confusion_matrix <- caret::confusionMatrix(predictions_binary, as.factor(test_set$Class))

# Plotting ROC Curve to find optimal cut off
pred_1 <- prediction(predictions, test_set$Class)
perf_1 <- performance(pred_1, "tpr", "fpr")

plot(perf_1, colorize = TRUE, 
     main = "ROC Curve for full model (XGBoost)",
     print.cutoffs.at = seq(0, 1, by = 0.1), 
     text.adj = c(0.2, 1.7))

# New threshold of 0.6
predictions_binary_new <- as.factor(ifelse(predictions > 0.6, 1, 0))

# Creating confusion matrix
confusion_matrix_new <- caret::confusionMatrix(predictions_binary_new, as.factor(test_set$Class))

# Exporting & Printing --------------------------------------------------------
export_path <- "./02__Models/01__German-Credit-Data/results"

matrix_1 <- capture.output(caret::confusionMatrix(predictions_binary, as.factor(test_set$Class)))
matrix_2 <- capture.output(caret::confusionMatrix(predictions_binary_new, as.factor(test_set$Class)))

writeLines(text = matrix_1, file.path(export_path, "03__XGBoost/01__Model_(0.5).txt"))
writeLines(text = matrix_2, file.path(export_path, "03__XGBoost/02__Model_(0.6).txt"))

# Comparing model performance
print("Full Model with 0.5 cut off")
confusion_matrix

print("Full Model with 0.6 cut off")
confusion_matrix_new




