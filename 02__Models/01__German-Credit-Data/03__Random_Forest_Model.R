# Creating a random forest decision tree with feature selection
rm(list = ls())

library(dplyr)
library(caTools)
library(randomForest)
library(caret)
library(Boruta) # for feature selection

# Reading in data --------------------------------------------------------------
path_data <- "./01__Data/01__German_Credit_Data/"

german_data <- read.csv(file.path(path_data, "01__GCD-Binary.csv"))

# Processing -------------------------------------------------------------------
german_data <- german_data %>%
  mutate(Class = recode(Class, "Bad" = 0, "Good" = 1)) %>%
  mutate(Class = as.factor(Class))

# Feature Selection ------------------------------------------------------------
set.seed(123)

# Performing feature selection to select most important variables
f_selection <- Boruta(Class ~ ., data = german_data, doTrace = 2, maxRuns = 500)

# Assessing what to do with the "Tentative" labelled variables
f_selection_final <- TentativeRoughFix(f_selection)

# Viewing output and the selected variables
# print(f_selection_final)
# getSelectedAttributes(f_selection_final)

# Splitting data ---------------------------------------------------------------
set.seed(147)

# Store row numbers for training set: index_train
index_train <- sample(1:nrow(german_data), 2 / 3 * nrow(german_data))

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
export_path <- "./02__Models/01__German-Credit-Data/"

model_output <- capture.output(best_model)
matrix_output <- capture.output(confusion_matrix)

writeLines(model_output, file.path(export_path, "results/01__Model_output.txt"))
writeLines(matrix_output, file.path(export_path, "results/02__Confusion_matrix.txt"))

best_model
confusion_matrix

