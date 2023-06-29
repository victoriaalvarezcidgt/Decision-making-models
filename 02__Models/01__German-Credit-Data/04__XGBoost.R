# XGBoost Decision Tree --------------------------------------------------------
rm(list = ls())

library(dplyr)
library(xgboost)
library(ParBayesianOptimization)
library(ggplot2)
library(ROCR)


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

# Creating matrices for model
# Defining equation 
# Everything is regressed onto Class with the intercept being removed (hence -1)
eq <- as.formula("Class ~ . -1")

x <- model.matrix(eq, data = training_set)
y <- model.frame(eq, data = training_set)[, "Class"]

xvals <- model.matrix(eq, data = test_set)
yvals <- model.frame(eq, data = test_set)[, "Class"]

# Training XGBoost model -------------------------------------------------------
set.seed(112)

# Set the XGBoost parameters
params <- list(
  max_depth = 6,                               # Default
  eta = 0.3,                                   # Default
  gamma = 0,                                   # Default
  min_child_weight = 1,                        # Default
  subsample = 1,                               # Default
  booster = "gbtree",
  objective = "binary:logistic",               # Binary classification
  eval_metric = "auc",                         # Metric to evaluate model performance
  verbosity = 0
)

# Useing cross validation to find best number of rounds
xgb_cv_model <- xgb.cv(data = x, 
                       label = y, 
                       params = params, 
                       nrounds = 100, prediction = TRUE, showsd = TRUE, 
                       early_stopping_rounds = 10,
                       maximize = TRUE, nfold = 10, stratified = TRUE)

# Optimal number of rounds
numrounds <- min(which(
  xgb_cv_model$evaluation_log$test_auc_mean == max(xgb_cv_model$evaluation_log$test_auc_mean)))

xgb_model <- xgboost(data = x, 
                     label = y,
                     params = params, 
                     nrounds = numrounds)

# We can now see model performance in terms of AUC
predictions <- predict(xgb_model, xvals, type = "response")
ROC_predictions <- prediction(as.numeric(predictions), as.numeric(yvals))
auc <- performance(ROC_predictions, measure = "auc")
auc@y.values[[1]] # Default AUC = 0.7782851

# Plotting ROC Curve
ROC_performance <- performance(ROC_predictions, "tpr", "fpr")
ROC_performance_df <- data.frame(False_pos = c(ROC_performance@x.values[[1]]),
                                 True_pos = c(ROC_performance@y.values[[1]]))

ggplot(data = ROC_performance_df, 
       aes(x = False_pos, y = True_pos, colour = "XGBoost: 0.78")) +
  geom_line() +
  geom_abline(slope = 1) +
  theme_bw() +
  ggtitle("ROC Curve with Default Hyperparameter Values")

# Optimizing using Bayesian Optimization ---------------------------------------
# Function will take the tuning parameters as an input and return the best
# cross validation results
scoring_function <- function(
    eta, gamma, max_depth, min_child_weight, subsample, nfold) {
  
  dtrain <- xgb.DMatrix(x, label = y, missing = NA)
  
  pars <- list(
    eta = eta,
    gamma = gamma,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    subsample = subsample,
    
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "auc",
    verbosity = 0
  )
  
  xgbcv <- xgb.cv(
    params = pars,
    data = dtrain,
    
    nfold = nfold,
    
    nrounds = 100,
    prediction = TRUE,
    showsd = TRUE,
    early_stopping_rounds = 10,
    maximize = TRUE,
    stratified = TRUE
  )
  
  # required by the package, the output must be a list
  # with at least one element of "Score", the measure to optimize
  # Score must start with capital S
  # For this case, we also report the best num of iteration
  return(
    list(
      Score = max(xgbcv$evaluation_log$test_auc_mean),
      nrounds = xgbcv$best_iteration
    )
  )
}

# Setting tuning boundaries
bounds <- list(
  eta = c(0, 1),
  gamma =c(0, 100),
  max_depth = c(2L, 10L), # L means integers
  min_child_weight = c(1, 25),
  subsample = c(0.25, 1),
  nfold = c(3L, 10L)
)

# Running the optimization with a time counter
set.seed(201)

overall_time <- system.time(
  output <- bayesOpt(
    FUN = scoring_function, 
    bounds = bounds, 
    initPoints = 7, 
    iters.n = 10
  ))

# outputting best parameters
best_parameters <- getBestPars(output)

# Fitting a tuned model --------------------------------------------------------
params <- list(eta = best_parameters[1],
               gamma = best_parameters[2],
               max_depth = best_parameters[3],
               min_child_weight = best_parameters[4],
               subsample = best_parameters[5],
               nfold = best_parameters[6],
               objective = "binary:logistic")

numrounds <- output$scoreSummary$nrounds[
  which(output$scoreSummary$Score == max(output$scoreSummary$Score))]

xgb_model_tuned <- xgboost(data = x, 
                           label = y, 
                           params = params, 
                           nrounds = numrounds, 
                           eval_metric = "auc")

# We can now see tuned model performance in terms of AUC
predictions_tuned <- predict(xgb_model_tuned, xvals, type = "response")
ROC_predictions_tuned <- prediction(as.numeric(predictions_tuned), as.numeric(yvals))
auc_tuned <- performance(ROC_predictions_tuned, measure = "auc")
auc_tuned@y.values[[1]] # Tuned AUC: 0.7541011

# Plotting both model curves
ROC_performance_tuned <- performance(ROC_predictions_tuned, "tpr", "fpr")
ROC_performance_tuned_df <- data.frame(
  False_pos = c(ROC_performance_tuned@x.values[[1]]),
  True_pos = c(ROC_performance_tuned@y.values[[1]]))

roc_curves <- ggplot() +
  geom_line(data = ROC_performance_df, 
            aes(x = False_pos, y = True_pos, colour = "XGBoost: 0.78")) +
  geom_line(data = ROC_performance_tuned_df,
            aes(x = False_pos, y = True_pos, colour = "Tuned XGBoost: 0.75")) +
  geom_abline(slope = 1) +
  theme_bw() +
  ggtitle("ROC Curves across models")

# Evaluating both models -------------------------------------------------------
# Default Model
confusion_matrix_default <- caret::confusionMatrix(
  table(round(predictions), yvals))

# Tuned Model
confusion_matrix_tuned <- caret::confusionMatrix(
  table(round(predictions_tuned), yvals))

# Exporting & Printing --------------------------------------------------------
export_path <- "./02__Models/01__German-Credit-Data/results"

matrix_1 <- capture.output(caret::confusionMatrix(
  table(round(predictions), yvals)))

matrix_2 <- capture.output(caret::confusionMatrix(
  table(round(predictions_tuned), yvals)))

writeLines(text = matrix_1, file.path(export_path, "03__XGBoost/01__Default_Model.txt"))
writeLines(text = matrix_2, file.path(export_path, "03__XGBoost/02__Tuned_Model.txt"))

cowplot::save_plot(file.path(export_path, "03__XGBoost/03__ROC_Curves.png"), 
                   plot = roc_curves, base_height = 10)

# Comparing model performance
print("Default Model")
confusion_matrix_default

print("Tuned Model")
confusion_matrix_tuned