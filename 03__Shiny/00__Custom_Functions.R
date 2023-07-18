# Custom Functions 
# Checks if a user has all the required packages ------------------------------
check_install_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name)
    library(package_name, character.only = TRUE)
  } else {
    library(package_name, character.only = TRUE)
  }
}

# Plots confusion matrix metrics -----------------------------------------------
plot_confusion_matrix <- function(confusion_matrix){
  
  # Extract values from the confusion matrix
  true_positive <- confusion_matrix$table[2, 2]
  false_positive <- confusion_matrix$table[1, 2]
  false_negative <- confusion_matrix$table[2, 1]
  true_negative <- confusion_matrix$table[1, 1]
  
  # Create a data frame for plotting
  metrics <- data.frame(
    Category = c("True Positive", "False Positive", "False Negative", "True Negative"),
    Value = c(true_positive, false_positive, false_negative, true_negative)
  )
  
  # Create confusion matrix plot
  ggplot(metrics, aes(x = Category, y = Value, fill = Category)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal() +
    ggtitle(label = "Confusion Matrix Metrics", 
            subtitle = paste0("Accuracy: ", formatC(confusion_matrix$overall["Accuracy"], digits = 2), "%",
                              "\nPrecision: ", formatC(confusion_matrix$byClass["Precision"], digits = 2), "%",
                              "\nRecall: ", formatC(confusion_matrix$byClass["Sensitivity"], digits = 2), "%",
                              "\nF1-Score: ", formatC(confusion_matrix$byClass["F1"], digits = 2))) +
    xlab("") +
    ylab("") +
    scale_fill_manual(values = c("True Positive" = "green", "False Positive" = "red",
                                 "False Negative" = "red", "True Negative" = "green")) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "bottom")
}

# Plotting ROC Curve -----------------------------------------------------------
plot_roc_curve <- function(predicted_probabilities, actual_labels) {
  
  predicted_probabilities <- as.numeric(predicted_probabilities)
  
  pred <- prediction(predicted_probabilities, actual_labels)
  perf <- performance(pred, "tpr", "fpr")
  auc <- performance(pred, measure = "auc")
  
  perf_df <- data.frame(f_pos = c(perf@x.values[[1]]),
                        t_pos = c(perf@y.values[[1]]),
                        auc = round(auc@y.values[[1]], digits = 2))
  
  ggplot() +
    geom_line(data = perf_df, aes(x = f_pos, y = t_pos, colour = auc)) +
    theme_bw() +
    ggtitle("ROC curve") +
    xlab("False Positive") +
    ylab("True Positive") +
    theme(plot.title = element_text(hjust = 0.5))
}

# Creating a decision tree -----------------------------------------------------
decision_tree <- function(formula, trainingData){
  
  set.seed(112)
  
  # Specifying Controls
  controls <- rpart.control(xval = 10, cp = 0.008)
  
  # Running Model
  tree_mod <- rpart(formula = formula, data = trainingData, method = "class",
                    control = controls)
  
  rpart.plot(tree_mod, type = 3, clip.right.labs = FALSE)
}