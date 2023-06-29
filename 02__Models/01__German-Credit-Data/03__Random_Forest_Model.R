# Creating a random forest decision tree with feature selection
rm(list = ls())

library(dplyr)
library(caTools)
library(randomForest)
library(Boruta) # for feature selection

# Reading in data --------------------------------------------------------------
path_data <- "./01__Data/01__German_Credit_Data/"

german_data <- read.csv(file.path(path_data, "01__GCD-Binary.csv"))

# Processing -------------------------------------------------------------------
german_data <- german_data %>%
  mutate(Class = recode(Class, "Bad" = 0, "Good" = 1)) %>%
  mutate(Class = as.factor(Class))

# Feature selection ------------------------------------------------------------
set.seed(110)

# Performing feature selection to select most important variables
f_selection <- Boruta(Class ~ ., data = german_data, doTrace = 2, maxRuns = 500)

# Assessing what to do with the "Tentative" labelled variables
f_selection_final <- TentativeRoughFix(f_selection)

# Splitting data ---------------------------------------------------------------
set.seed(111)
# Store row numbers for training set: index_train
index_train <- caret::createDataPartition(german_data$Class, p = 0.7, list = FALSE)

# Create training set: training_set
training_set <- german_data[index_train, ]

# Create test set: test_set
test_set <- german_data[-index_train, ]

# Performing random forest -----------------------------------------------------
set.seed(148)

# Assessing the accuracy of different tree sizes and splits
ntree_values <- seq(100, 1000, by = 100) # Number of trees
mtry_values <- seq(1, 10, by = 1) # Number of variables to consider at each split

# Creating all possible combinations of the above values
combinations <- expand.grid(ntree = ntree_values, mtry = mtry_values)

# For storing output
best_accuracy <- 0
best_model <- NULL

# The below loop with iterate through all possible combinations of the tree/split
# values and find the best performing model
for (i in 1:nrow(combinations)) {
  ntree <- combinations$ntree[i]
  mtry <- combinations$mtry[i]
  
  model <- randomForest(formula = getNonRejectedFormula(f_selection_final), 
                        data = training_set, ntree = ntree, mtry = mtry)
  predicted <- predict(model, newdata = test_set)
  confusion_matrix <- table(predicted, test_set$Class)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  # If a model has better accuracy it is saved
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_model <- model
  }
}

# Performing predictions and evaluating ----------------------------------------
predictions <- predict(best_model, newdata = test_set)
confusion_matrix <- confusionMatrix(table(predictions, test_set$Class))

# Outputting and exporting -----------------------------------------------------
export_path <- "./02__Models/01__German-Credit-Data/results"

model_output <- capture.output(best_model)
matrix_output <- capture.output(confusion_matrix)

writeLines(model_output, file.path(export_path, "02__Random_Forest/01__Model_output.txt"))
writeLines(matrix_output, file.path(export_path, "02__Random_Forest/02__Confusion_matrix.txt"))

print("Best Model Details:")
best_model

print("Best Model Performance")
confusion_matrix

