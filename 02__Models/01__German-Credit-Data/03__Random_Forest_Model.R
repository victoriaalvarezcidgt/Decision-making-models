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
f_selection <- Boruta(Class ~ ., data = german_data,
                      doTrace = 2, maxRuns = 500)

# Viewing output and the selected variables
# print(f_selection)
# getSelectedAttributes(f_selection)

# Assessing what to do with the "Tentative" labelled variables?get
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
ntree_values <- c(100, 200, 300, 400, 500, 600, 700) # Number of trees
mtry_values <- c(3, 5, 7, 8, 9, 10) # Number of variables to consider at each split

best_accuracy <- 0
best_model <- NULL

# The below code will utilize the feature selected variables and also cycle 
# through the above values and select the best performing model
for (ntree in ntree_values) {
  for (mtry in mtry_values) {
    model <- randomForest(formula = getNonRejectedFormula(f_selection_final), 
                          data = training_set, ntree = ntree, mtry = mtry)
    predicted <- predict(model, newdata = test_set)
    confusion_matrix <- table(predicted, test_set$Class)
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_model <- model
    }
  }
}

predictions <- predict(best_model, newdata = test_set)
confusionMatrix(table(predictions, test_set$Class))
