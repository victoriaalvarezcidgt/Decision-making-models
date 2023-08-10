<h1 align = "center"> Credit Risk Models (Shiny App) </h1>
<h4 align = "center"> The following <a href = "https://github.com/C-Monaghan/DM_Models/blob/main/03__Shiny/01__app.R"> shiny app </a> provides decision making tools for classification problems (in this case <strong> loan defaults) </strong> </h4>
<h1 align = "left"> Required Packages </h1>
This section lists the packages that are required for running the code. These packages provide additional functionality and are essential for the successful execution of the program.
<details>
  <summary>
    <h4 align = "left"> Packages </h4>
  </summary>
<ul>
  <li> <code>shiny</code>: Enables the creation of interactive web applications. </li>
  <li> <code>shinydashboard</code>: Provides a framework for building dashboards in Shiny applications. </li>
  <li> <code>shinyWidgets</code>: Offers a collection of user-friendly widgets for Shiny applications. </li>
  <li> <code>shinyjs</code>: Provides additional JavaScript functions to enhance Shiny applications. </li>
  <li> <code>shinyalert</code>: Provides a simple way to create attractive, responsive, and customizable JavaScript alerts and modals within Shiny applications. </li>
  <li> <code>DT</code>: Enables the creation of interactive data tables in Shiny applications. </li>
  <li> <code>dplyr</code>: Provides a set of tools for data manipulation and transformation. </li>
  <li> <code>Boruta</code>: Implements the Boruta algorithm for feature selection. </li>
  <li> <code>caret</code>: Offers a unified interface for machine learning models and provides functions for data preparation, feature selection, and model evaluation. </li>
  <li> <code>randomForest</code>: Implements the random forest algorithm for classification and regression. </li>
  <li> <code>xgboost</code>: Implements the gradient boosting algorithm for classification and regression. </li>
  <li> <code>ParBayesianOptimization</code>: Provides functions for hyperparameter tuning using Bayesian optimization. </li>
  <li> <code>rpart.plot</code>: Enables the visualization of decision trees created using the rpart package. </li>
  <li> <code>ggplot2</code>: Provides a powerful and flexible system for creating visualizations. </li>
  <li> <code>ROCR</code>: Implements functions for visualizing and evaluating the performance of classification models. </li>
  <li> <code>cvms</code>: Implements cross-validation for regression models. </li>
  <li> <code>markdown</code>: Allows the conversion of R Markdown documents into various output formats. </li>
  <li> <code>DiagrammeR</code>: Enables the creation of graph and flowchart diagrams. </li>
  <li> <code>rattle</code>: Provides a graphical user interface that simplifies the process of building and evaluating machine learning models. </li>
</ul>
</details>

<h3 align = "left"> Installation </h3>
To ensure that all required packages are installed, the following <a href = "https://github.com/C-Monaghan/DM_Models/blob/main/03__Shiny/00__Custom_Functions.R#L2-L10"> check_install_package </a> function from the 
<a href = "https://github.com/C-Monaghan/DM_Models/blob/main/03__Shiny/00__Custom_Functions.R"> custom functions script </a> is used. The function iterates over the <code>packages</code> vector, checks if each package is installed, and installs and loads it if necessary.

``` R
# Installs and loads all required packages
for (package_name in packages) {
  check_install_package(package_name)
}
```
<h1 align = "left"> Pre Requisite Code </h1>
<ul>
  <li> Setting up a logical image directory. </li>
  <li> CSS styling for adaptive sizing and notification box adjustments. </li>
  <li> CSS styling for positioning the CRT logo. </li>
  <li> JavaScript code to move the CRT logo when the sidebar is toggled. </li>
  <li> CSS code to customize the color scheme for the content wrapper and right-side elements.</li>
</ul>

<details>
  <summary>
    <h5> Code </h5>
  </summary>
  
```R
# Pre Requisites ---------------------------------------------------------------
# Instead of using the default "www" pathway for images we'd like to use 
# a more logical directory
addResourcePath("new_path", "./03__Shiny/01__Logo/")

# CSS and JaveScript Code
# Adaptive sizing and notification box adjustments
css_code_sizing_notification <- " 
.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}
.shiny-notification {font-size: 18px; padding: 15px; margin-bottom: 10px; border-radius: 5px;
box-shadow: 0px 2px 5px rgba(0, 0, 0, 0.2); text-align: center;}
.error-notification {background-color: #f44336; color: #fff;}
"
# CRT Logo Positioning
css_code_logo_position <- "
#image-container {
position: absolute;
bottom: 0;
left: 210px;
transition: all 0.3s ease;
}
        
#image-container.move-up {
left: 0;
}
"
# Moving CRT Logo
js_code_logo_moving <- "
$(document).ready(function() {
$('.sidebar-toggle').on('click', function() {
$('#image-container').toggleClass('move-up');
});
});
"
# Custom colour scheme
css_code_custom_colour <- '
/* body */
.content-wrapper, .right-side {
background-color: #F9F4F4;
}
'
```
</details>

<h1 align = "left"> Creating the UI </h1>
This section defines the user interface (UI) for the Shiny application using the <code>shinydashboard</code> package. The dashboard is broken into a <strong> sidebar </strong> and a <strong> body </strong>
<h2 align = "left"> Sidebar </h2>
The sidebar menu includes the following items:
<ul>
  <li> Home Page </li>
  <li> Data Input Page </li>
  <li> Feature Selection Page </li>
  <li> Modelling Page </li>
  <li> Predictions Page </li>
  <li> Guides Page </li>
</ul>

<details>
  <summary>
    <h5> Code </h5>
  </summary>
  
  ```R
 # Defining sideboard menu
  dashboardSidebar(
    # Adding pages to the sideboard
    sidebarMenu(
      menuItem("Home Page", tabName = "Home", icon = icon("home")),
      menuItem("Data Input", tabName = "Input", icon = icon("upload")),
      menuItem("Feaure Selection", tabName = "Selection", icon = icon("cogs")),
      menuItem("Modelling", tabName = "Modelling", icon = icon("line-chart")),
      menuItem("Predictions", tabName = "Predict", icon = icon("magic")),
      menuItem("Guides", tabName = "Guides", icon = icon("book"))
    )), # End of sidebarMenu() & dashboardSidebar()
```
</details>

<h1 align = "left"> Creating the Server </h1>
<h2 align = "left"> Observations </h2>
To ensure that certain aspects of the UI disappear/are updated when a user does something we implement a few observations.

```R
# Observations ---------------------------------------------------------------
  # Unticks Random Search Optimization button if Random Forest is not selected
  observe({
    if (input$modelSelection != "Random Forest") {
      updateCheckboxInput(session, "randomisation", value = FALSE)
    }
  })
  # Unticks Bayesian Optimization button if XGBoost is not selected
  observe({
    if (input$modelSelection != "XGBoost") {
      updateCheckboxInput(session, "bayes", value = FALSE)
    }
  })
  # Update the max value of the treeNumber slider
  observe({
    updateSliderInput(session, "treeNumber", max = model()$ntree)
  })
  # Update the max value of the boostTreeNumber slider
  observe({
    updateSliderInput(session, "boostTreeNumber", max = model()$model_train$niter)
  })
```
<h2 align = "left"> Data Uploading </h2>
The <code>dataset()</code> function is run when the user uploads a .csv file to the shiny app. The following will be checked:
<ul>
  <li> If no dataset has been uploaded an error message will be displayed and the function will return NULL. </li>
  <li> If the uploaded file is not in CSV format (determined by the file extension), an error message will be displayed and the function will return NULL. </li>
</ul>

If a valid CSV file is uploaded, the function will attempt to read it using <code>read.csv()</code>. If any errors occur during the reading process, the function will catch the error using <code>tryCatch()</code> and display an error message. The function will then return NULL. However, if the CSV file is successfully read and no errors occur, the function will return the resulting data frame

```R
# Data uploading -------------------------------------------------------------
  dataset <- reactive({
    # If a dataset has NOT been uploaded an error message will be returned
    # If a dataset is NOT in CSV format an error message will be returned
    # If a dataset has been uploaded AND is in CSV format it will be read in
    if(is.null(input$dataset)){
      shinyalert::shinyalert(title = "Oops!", text = "Please upload a dataset", 
                             type = "error", showCancelButton = FALSE)
      return(NULL)
    } else if(!grepl("\\.csv$", input$dataset$name, ignore.case = TRUE)){
      shinyalert::shinyalert(title = "Oops!", text = "Please upload a CSV file", 
                             type = "error", showCancelButton = FALSE)
      return(NULL)
    } else{ # Handling potential errors while reading the data file
      df <- tryCatch({
        read.csv(input$dataset$datapath) # Reading in data
      },
      error = function(e){
        shinyalert::shinyalert(title = "Oops!", text = paste("Error while reading the CSV file:", e$message), 
                               type = "error", showCancelButton = FALSE)
        return(NULL)
      }) # End of tryCatch()
      
      return(df)
    }
  }) # End of dataset()
```
<h2 align = "left"> Data Processing </h2>
The <code>data_processing()</code> function is run when the user clicks the <strong> processing action button </strong>. Within the reactive expression, the uploaded dataset is accessed using the <code>dataset()</code> reactive expression (again, the above checks are made). The target variable is selected based on the user's input and the data is then processed by recoding the target variable to "Default" and "Non_Default" based on specific values. Additional missing values are removed from the dataset. Finally, the processed dataset is returned.

```R
# Processing Data ------------------------------------------------------------
  is_processed <- reactiveVal(FALSE) # Will be set to TRUE after processing has been complete
  
  data_processing <- reactive({
    req(input$processing)
    
    # If a dataset has NOT been uploaded an error message will be returned
    # If a dataset is NOT in CSV format an error message will be returned
    if(is.null(input$dataset)){
      shinyalert::shinyalert(title = "Oops!", text = "Please upload a dataset", 
                             type = "error", showCancelButton = FALSE)
      return(NULL)
    } else if(!grepl("\\.csv$", input$dataset$name, ignore.case = TRUE)){
      shinyalert::shinyalert(title = "Oops!", text = "Please upload a CSV file", 
                             type = "error", showCancelButton = FALSE)
      return(NULL)
    }

    # Reading in dataset and target selection
    df <- dataset()
    target <- toString(input$targetVariable)

    # Processing Data (recoding to default and non default)
    if("bad" %in% tolower(df[, target]) && "good" %in% tolower(df[, target])){
      df <- df %>%
        mutate(!!sym(target) := recode(tolower(!!sym(target)), "bad" = "Default", "good" = "Non_Default")) %>%
        mutate(!!sym(target) := as.factor(!!sym(target))) %>%
        filter(complete.cases(.))
    } 
    else if("No" %in% tolower(df[, target]) && "Yes" %in% tolower(df[, target])){
      df <- df %>%
        mutate(!!sym(target) := recode(tolower(!!sym(target)), "No" = "Default", "Yes" = "Non_Default")) %>%
        mutate(!!sym(target) := as.factor(!!sym(target))) %>%
        filter(complete.cases(.))
    } else if("0" %in% tolower(df[, target]) && "1" %in% tolower(df[, target])) {
      df <- df %>%
        mutate(!!sym(target) := recode(tolower(!!sym(target)), "0" = "Default", "1" = "Non_Default")) %>%
        mutate(!!sym(target) := as.factor(!!sym(target))) %>%
        filter(complete.cases(.))
    } else if("default" %in% tolower(df[, target]) && "non_default" %in% tolower(df[, target])){
      df <- df %>%
        mutate(!!sym(target) := as.factor(!!sym(target))) %>%
        filter(complete.cases(.))
    } else{ # If invalid variable coding then an error will be returned
      shinyalert::shinyalert(title = "Error!", text = "Invalid variable coding: At least two valid variable names are required.", 
                             type = "error", showCancelButton = FALSE)
      return(NULL)
    }

    is_processed(TRUE)
    return(df)
  })
```
<h2 align = "left"> Feature Selection </h2>
The <code>selection()</code> function is used to perform the feature selection based on the user's input.  It checks if the <strong> runSelection action button </strong> is pressed. Within the reactive expression, the processed dataset is accessed using the <code>data_processing()</code>reactive expression. The target variable is obtained from the user's input, and a formula is created based on the target variable. The feature selection method is determined based on the user's selection.

```R
# Feature Selection
selection <- eventReactive(input$runSelection, {
    
    # If no dataset is uploaded an error message will appear
    # If data processing has not been complete an error message will appear
    if(is.null(input$dataset)){
      shinyalert::shinyalert(title = "Oops!", text = "Please upload a dataset", 
                             type = "error", showCancelButton = FALSE)
      return(NULL)
    } else if(!is_processed()){
      shinyalert::shinyalert(title = "Error", text = "Please complete data processing first",
                             type = "error", showCancelButton = FALSE)
      return(NULL)
    } else{
      # Starting progress message
      progress <- Progress$new()
      progress$set(message = "Preparing Data", value = 0.1)
    }
    
    # Variables to be used
    df <- data_processing() # Processed data
    df_reduced <- NULL # Reduced dataset to be filled
    target <- toString(input$targetVariable) # Target variable
    formula <- as.formula(paste(target, "~ .")) # Formula
    
    progress$set(message = "Getting selection method", value = 0.2)
    Sys.sleep(0.75)
```
<h4 align = "left"> Feature Selection Options </h4>
<details>
  <summary>
    Boruta Algorithm
  </summary>
  
  If the user selects the <strong> Boruta feature selection </strong> method, the Boruta algorithm is applied to the dataset. The Boruta algorithm is run using the <code>Boruta</code> function, and the model is 
  further processed using the <code>TentativeRoughFix</code> function. The reduced dataset is created by selecting the target variable and the attributes selected by the Boruta algorithm.

  ```R
# Boruta Model
    if(input$selection == "Boruta"){
      set.seed(110)
      
      progress$set(message = "Preparing Boruta", value = 0.3)
      progress$set(message = "Running Boruta", value = 0.5)
      
      # Running Boruta selection method with potential error handling
      tryCatch({
      model <- Boruta(formula, data = df, doTrace = 2, maxRuns = input$borutaIter)
      model <- TentativeRoughFix(model)
      
      progress$set(message = "Creating reduced dataset", value = 0.8)
      
      # Creating reduced dataset
      df_reduced <- df[, c(input$targetVariable, getSelectedAttributes(model))]
      
      progress$set(message = "Outputting Information", value = 1)
      progress$close()
      }, error = function(e){
        shinyalert::shinyalert(title = "Error", text = paste("Error occurred during Boruta feature selection:", e$message),
                               type = "error", showCancelButton = FALSE)
        progress$close()
        return(NULL)
      }) # End of tryCatch
    } # End of Boruta
```
</details>
<details>
  <summary>
    Recursive Feature Selection
  </summary>
  If the user selects the Recursive Feature Selection method, the Recursive Feature Elimination (RFE) algorithm is applied to the dataset. The RFE algorithm is run using the <code>rfe</code> function from the 
  <code>caret</code>package. Cross-validation controls are created using the <code>rfeControl</code> function, and the model is trained on different feature subset sizes. The reduced dataset is created by 
  selecting the target variable and the optimal variables identified by the RFE algorithm.

  ```R
else if(input$selection == "Recursive Feature Selection"){
      set.seed(110)
      
      progress$set(message = "Preparing Recursive Feature Selection", value = 0.3)
      progress$set(message = "Running Recursive Feature Selection", value = 0.5)
      
      # Creating cross validation controls
      ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = input$recursiveIter)
      
      # Running RFS model with potential error handling
      tryCatch({
      model <- rfe(df[, -which(names(df) == input$targetVariable)], df[[input$targetVariable]], 
                    sizes = c(1:ncol(df)-1), 
                    rfeControl = ctrl)
      
      progress$set(message = "Creating reduced dataset", value = 0.8)
      
      # Creating reduced dataset
      df_reduced <- df[, c(input$targetVariable, model$optVariables)]
      
      progress$set(message = "Outputting Information", value = 1)
      progress$close()
      }, error = function(e){
        shinyalert::shinyalert(title = "Error", text = paste("Error occurred during Recursive Feature Selection:", e$message),
                               type = "error", showCancelButton = FALSE)
        progress$close()
        return(NULL)
      }) # End of tryCatch
    } # End of Recursive Feature Selection
```
</details>
<details>
  <summary>
    No Feature Selection
  </summary>
   If the user selects "None" for feature selection, no feature selection is performed, and the processed dataset is used as is.

  ```R
else if (input$selection == "None") {

    progress$set(message = "Outputting Information", value = 1)
    progress$close()

    model <- NULL
    df_reduced <- data_processing()
  }
```
</details>

The <code>selection()</code> function returns a list with both the selected model output and the reduced dataset.

```R
return(list(model = model, df_reduced = df_reduced))
```

<h2 align = "left"> Modelling </h2>
The <code>model()</code> function implements a decision making model based on user input. It checks if the <strong> runModel action button </strong> is been pressed. Within the expression intitally it checks to see if a feature selection option was carried out. If so the resulting reduced dataset is used in the analysis. If not, then the full dataset is used.  The target variable is obtained from the user's input, and a formula is created based on the target variable. Training and test data is then created based on user input and a modelling method is also implemented based on user input.

```R
# Decision Making Models -----------------------------------------------------
  # First we create a reactive value that tracks if the model function has been run
  model_trained <- reactiveVal(FALSE)
  
  model <- eventReactive(input$runModel, {
    # If no dataset is uploaded an error message will appear
    # If data processing has not been complete an error message will appear
    if(is.null(input$dataset)){
      shinyalert::shinyalert(title = "Oops", text = "Please upload a dataset",
                             type = "error", showCancelButton = FALSE)
      return(NULL)
    } else if(!is_processed()){
      shinyalert::shinyalert(title = "Oops", text = "Please complete data processing first",
                             type = "error", showCancelButton = FALSE)
      return(NULL)
    } else{
      # Starting progress message
      progress <- Progress$new()
      progress$set(message = "Preparing Data", value = 0.1)
    }
    
    # Variables to be used
    # Selecting dataset to use based off feature selection method
    if(c(input$selection == "Boruta" & input$runSelection) | c(input$selection == "Recursive Feature Selection"& input$runSelection)){
      df <- selection()$df_reduced # Use the reduced dataset
    } else{ # If no feature selection was performed
      df <- data_processing() # Use the full dataset
    }
    
    target <- toString(input$targetVariable) # Target variable
    formula <- as.formula(paste(target, "~ . - 1")) # Formula
    
    # Creating training and test data
    set.seed(111)
    index <- createDataPartition(df[, target], p = (input$split / 100), list = FALSE)
    
    training_set <- df[index, ]
    test_set <- df[-index, ]
    
    # Ensure target variable has the same levels in training and test set
    levels(training_set[, target]) <- levels(df[, target])
    levels(test_set[, target]) <- levels(df[, target])
    
    progress$set(message = "Getting modelling method", value = 0.3)
    Sys.sleep(0.75)
    
```
<h4 align = "left"> Model Options </h4>
<details>
  <Summary>
    Logistic Regression
  </Summary>
  The Logistic Regression model is trained using the <code>train</code> function from the <code>caret</code> package. The model is trained on the training_set and evaluated on the test_set using cross-validation. The model 
  predictions are then generated and a confusion matrix is computed.

  ```R
# Logistic Regression Method
    if(input$modelSelection == "Logistic Regression") {
      progress$set(message = "Running Logistic Regression", value = 0.4)
      Sys.sleep(0.75)
      progress$set(message = "Training Model", value = 0.5)
      Sys.sleep(0.75)
      
      set.seed(112)
      
      # Specifying the Type of Training Methods used and the Number of Folds
      ctrlspec <- trainControl(
        method = "cv", number = input$regressionFolds, savePredictions = "all", classProbs = TRUE)
      
      # Training Logistic Regression Model with error handling
      tryCatch({
        model_train <- train(formula, data = training_set,
                             method = "rpart", trControl = ctrlspec)
      }, error = function(e){
        shinyalert::shinyalert(title = "Oops", text = paste("Error occurred during Logistic Regression model training:", e$message),
                               type = "error", showCancelButton = FALSE)
        progress$close()
        return(NULL)
      }) # End of tryCatch
      
      progress$set(message = "Generating Predictions", value = 0.7)
      Sys.sleep(0.75)
      
      # Testing Logistic Regression Model and generating confusion matrix
      model_test <- predict(object = model_train, newdata = test_set)
      confusion_matrix <- confusionMatrix(data = model_test, 
                                          as.factor(test_set[, target]))
      
      progress$set(message = "Outputting Information", value = 0.9)
      Sys.sleep(0.75)
      
      return_list <- list(model_train = model_train, model_test = model_test,
                          confusion_matrix = confusion_matrix, training_data = training_set,
                          test_data = test_set, full_data = df, loan_default = target,
                          formula = formula, ctrlspec = ctrlspec)
      
      progress$close()
    } # End of Logistic Regression
```
</details>
<details>
  <summary>
    Random Forest
  </summary>
  The Random Forest model can be trained with or without Random Search Optimization, depending on the user's choice. If Random Search Optimization is enabled, the model is trained iteratively with different hyperparameter 
  configurations and the best model is selected based on accuracy. If Random Search Optimization is disabled, the model is trained using the default hyperparameters.

  ```R
else if(input$modelSelection == "Random Forest"){
      progress$set(message = "Running Random Forest", value = 0.4)
      Sys.sleep(0.75)
      
      # Implementing Random Search Optimization
      # Initialize variables for tracking the best model
      num_iterations <- input$randomIter # Number of iterations to perform
      ntree_values <- seq(100, 1000, by = 100) # Number of trees
      mtry_values <- seq(1, (ncol(training_set) - 1), by = 1) # Number of variables to consider at each split
      best_accuracy <- 0
      best_model <- NULL
      best_ntree <- NULL
      best_mtry <- NULL

      progress$set(message = "Training Model", value = 0.5)
      Sys.sleep(0.75)
      
      set.seed(112)
      
      if(input$randomisation == TRUE){
      tryCatch({
        for(i in 1:num_iterations){
        progress$set(message = paste0("Training Model (", i, "/", num_iterations, ")"), value = 0.5)
        # Randomly selecting hyper parameters
        ntree <- sample(ntree_values, 1)
        mtry <- sample(mtry_values, 1)
        
        # Training model
        model_train <- randomForest(formula = formula, data = training_set, 
                                    ntree = ntree, mtry = mtry)
        
        # Evaluating model
        model_test <- predict(model_train, newdata = test_set)
        confusion_matrix <- table(model_test, test_set[, target])
        accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
        
        # Update the best model if necessary
        if (accuracy > best_accuracy) {
          best_accuracy <- accuracy
          best_model <- model_train
          best_ntree <- ntree
          best_mtry <- mtry
        }
      } # End of for loop
      }, error = function(e){
        shinyalert::shinyalert(title = "Oops", text = paste("Error occurred during Random Optimization training:", e$message),
                               type = "error", showCancelButton = FALSE)
        progress$close()
        return(NULL)
      }) # End of tryCatch
        progress$set(message = "Generating Predictions", value = 0.7)
        
        model_test <- predict(best_model, newdata = test_set)
        confusion_matrix <- confusionMatrix(data = model_test, 
                                            as.factor(test_set[, target]))
        
        return_list <- list(model_train = best_model, model_test = model_test,
                            confusion_matrix = confusion_matrix, training_data = training_set,
                            test_data = test_set, full_data = df, loan_default = target,
                            formula = formula, accuracy = best_accuracy,
                            ntree = best_ntree, mtry = best_mtry)
        } # End of if statement
      else{
       tryCatch({
         model_train <- randomForest(formula = formula, data = training_set)
       }, error = function(e){
         shinyalert::shinyalert(title = "Oops", text = paste("Error occurred during Random Forest training:", e$message),
                                type = "error", showCancelButton = FALSE)
         progress$close()
         return(NULL)
       }) # End of tryCatch
        progress$set(message = "Generating Predictions", value = 0.7)
        
        model_test <- predict(model_train, newdata = test_set)
        confusion_matrix <- confusionMatrix(data = model_test, 
                                            as.factor(test_set[, target]))
        
        return_list <- list(model_train = model_train, model_test = model_test,
                            confusion_matrix = confusion_matrix, training_data = training_set,
                            test_data = test_set, full_data = df, loan_default = target,
                            formula = formula)
      }
      progress$set(message = "Outputting Model", value = 0.9)
      Sys.sleep(0.75)
      
      progress$close()
    } # End of Random Forest
```
</details>
<details>
  <summary>
    XGBoost
  </summary>
  The XGBoost model can be trained with or without Bayesian Optimization, depending on the user's choice. If Bayesian Optimization is enabled, the model is trained with optimized hyperparameters using the 
  <code>bayesOpt</code> function from the <code>rBayesianOptimization</code> package. The best hyperparameters are selected based on the maximum AUC score. If Bayesian Optimization is disabled, the model is trained using 
  default hyperparameters.

  ```R
    else if(input$modelSelection == "XGBoost"){
      progress$set(message = "Running XGBoost", value = 0.4)
      Sys.sleep(0.75)
      
      tryCatch({
        # Small data processing to create correct y & yvals reference levels
      training_set <- training_set %>%
        mutate(!!sym(target) := recode(!!sym(target), "Default" = 0, "Non_Default" = 1),
               !!sym(target) := as.numeric(as.factor(!!sym(target))),
               !!sym(target) := !!sym(target) - 1)

      test_set <- test_set %>%
        mutate(!!sym(target) := recode(!!sym(target), "Default" = 0, "Non_Default" = 1),
               !!sym(target) := as.numeric(as.factor(!!sym(target))),
               !!sym(target) := !!sym(target) - 1)
      }, error = function(e){
        shinyalert::shinyalert(title = "Oops", text = paste("Error occurred during XGBoost data processing:", e$message),
                               type = "error", showCancelButton = FALSE)
        progress$close()
        return(NULL)
      })
      
      # Creating model matrices
      x <- model.matrix(formula, data = training_set)
      y <- model.frame(formula, data = training_set)[, target]
      
      xvals <- model.matrix(formula, data = test_set)
      yvals <- model.frame(formula, data = test_set)[, target]
      
      progress$set(message = "Training Model", value = 0.5)
      Sys.sleep(0.75)
      
      # Optimizing using Bayesian Optimization 
      if(input$bayes == TRUE){
        set.seed(112)
        progress$set(message = "Applying Bayesian Optimisation", value = 0.6)
        Sys.sleep(0.75)
        
        tryCatch({
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
          # For this case, we also report the best number of iterations
          return(list(Score = max(xgbcv$evaluation_log$test_auc_mean),
                      nrounds = xgbcv$best_iteration))
        } # End of scoring_function()
        
        # Setting tuning boundaries
        bounds <- list(
          eta = c(0, 1),
          gamma = c(0, 100),
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
            initPoints = 7, # Must be higher than out function inputs
            iters.n = input$bayesIter, # Can be set to any iteration value
          ))
        
        # Outputting best parameters
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
        
        model_train <- tryCatch({
          xgboost::xgboost(data = x,
                           label = y,
                           params = params,
                           nrounds = numrounds,
                           eval_metric = "auc")
          }, error = function(e) {
            shinyalert::shinyalert(title = "Oops", text = paste("Error occurred during XGBoost training:", e$message),
                                   type = "error", showCancelButton = FALSE)
          progress$close()
          return(NULL)
        }) # End of tryCatch {model_train}
        
        xgbcv <- NULL
        }, error = function(e) {
          shinyalert::shinyalert(title = "Oops", text = paste("Error occurred during Bayesian Optimization:", e$message),
                                 type = "error", showCancelButton = FALSE)
          progress$close()
          return(NULL)
        }) # End of tryCatch (Bayesian Optimization)
        } # end of if statement
      else{
        set.seed(112)
        # Set the XGBoost parameters
        params <- list(
          max_depth = 6,                               # Default
          eta = 0.3,                                   # Default
          gamma = 0,                                   # Default
          min_child_weight = 1,                        # Default
          subsample = 1,                               # Default
          booster = "gbtree",                          # Default
          objective = "binary:logistic",               # Binary classification
          eval_metric = "auc",                         # Metric to evaluate model performance
          verbosity = 0                                # Verbosity of printing messages
        )
        
        # Using cross validation to find best number of rounds
        # with potential error handling
        tryCatch({
          xgbcv <- xgb.cv(data = x,
                          label = y,
                          params = params,
                          nrounds = 100, prediction = TRUE, showsd = TRUE,
                          early_stopping_rounds = 10,
                          maximize = TRUE, nfold = 10, stratified = TRUE)
        }, error = function(e){
          shinyalert::shinyalert(title = "Oops", text = paste("Error occurred during cross-validation:", e$message),
                                 type = "error", showCancelButton = FALSE)
          return(NULL)
        }) # End of tryCatch
        
        # Extracting optimal number of rounds
        numrounds <- min(which(
          xgbcv$evaluation_log$test_auc_mean == max(xgbcv$evaluation_log$test_auc_mean)))
        
        tryCatch({
          # Running model with default parameters
        model_train <- xgboost::xgboost(data = x,
                                        label = y,
                                        params = params,
                                        nrounds = numrounds)
        }, error = function(e){
          shinyalert::shinyalert(title = "Oops", text = paste("Error occurred during XGBoost model training:", e$message),
                                 type = "error", showCancelButton = FALSE)
          return(NULL)
        }) # End of tryCatch
      } # end of else statement
      
      # Evaluating model
      progress$set(message = "Generating Predictions", value = 0.7)
      Sys.sleep(0.75)
      
      model_test <- predict(model_train, xvals, type = "response")
      confusion_matrix <- confusionMatrix(as.factor(round(model_test)), as.factor(yvals))
      
      progress$set(message = "Outputting Information", value = 0.9)
      Sys.sleep(0.75)
      
      
      return_list <- list(model_train = model_train, model_test = model_test,
                          confusion_matrix = confusion_matrix, training_data = training_set,
                          test_data = test_set, full_data = df, loan_default = target,
                          formula = formula, x = x, y = y, xvals = xvals, yvals = yvals,
                          model_cv = xgbcv)
      
      progress$close()
      
    } # End of XGBoost
```
</details>

The model information, predictions, confusion matrix, and other relevant data are stored in a list called return_list, which is then returned by the <code>model()</code> function.

```R
return(return_list)
```

<h2 align = "left"> Predictions </h2>
<h3 align = "left"> New Data Upload </h3>
The <code>dataset_new()</code> function is run whne the user uploads a .csv file to the predictions page of the shiny app. Again the following are checked:
<ul>
  <li> If no dataset has been uploaded an error message will be displayed and the function will return NULL. </li>
  <li> If the uploaded file is not in CSV format (determined by the file extension), an error message will be displayed and the function will return NULL. </li>
</ul>

If a valid CSV file is uploaded, the function will attempt to read it using <code>read.csv()</code>. If any errors occur during the reading process, the function will catch the error using <code>tryCatch()</code> and display an error message. The function will then return NULL. However, if the CSV file is successfully read and no errors occur, the function will return the resulting data frame.
```R 
# Reading in new dataset -----------------------------------------------------
  dataset_new <- reactive({
    # If a dataset has NOT been uploaded an error message will be returned
    # If a dataset is NOT in CSV format an error message will be returned
    # If a dataset has been uploaded AND is in CSV format it will be read in
    if(is.null(input$probData)){
      shinyalert::shinyalert(title = "Oops", text = "Please upload a dataset",
                             type = "error", showCancelButton = FALSE)
      return(NULL)
    } else if(!grepl("\\.csv$", input$probData$name, ignore.case = TRUE)){
      shinyalert::shinyalert(title = "Oops", text = "Please upload a CSV file",
                             type = "error", showCancelButton = FALSE)
      return(NULL)
    } else { # Handling potential errors while reading the datafile
      tryCatch({
        df_prob <- read.csv(input$probData$datapath)
      }, error = function(e){
        shinyalert::shinyalert(title = "Oops", text = paste("Error while reading the CSV file: ", e$message),
                               type = "error", showCancelButton = FALSE)
        return(NULL)
      }) # End of tryCatch
      
      return(df_prob)
    }
  }) # End of dataset_new()
```

<h3 align = "left"> Generating Predictions </h3>
The <code>predictions()</code> function is run when the user clicks the <strong> predictions action button </strong>. Within the reactive expression, the new uploaded dataset is accessed using the <code>dataset_new()</code> function and the previously utilised model is accessed using the <code>model()</code> function. If the previously used model was either a "logistic regression" or a "random forest" <code>type = "prob"</code> is used to generate probability predictions. However, if the selected model is "XGBoost," the input data is converted to a matrix and then probability scores are calculated using <code>type = "prob"</code>.

```R
 # Generating predictions -----------------------------------------------------
  predictions <- reactive({
    req(input$predict)
    # If a dataset has NOT been uploaded an error message will be returned
    # If a dataset is NOT in CSV format an error message will be returned
    # If a model has been previously trained an error message will be returned
    if(is.null(input$probData)){
      shinyalert::shinyalert(title = "Oops", text = "Please upload a dataset",
                             type = "error", showCancelButton = FALSE)
      return(NULL)
    } else if(!grepl("\\.csv$", input$probData$name, ignore.case = TRUE)){
      shinyalert::shinyalert(title = "Oops", text = "Please upload a CSV file",
                             type = "error", showCancelButton = FALSE)
      return(NULL)
    } else if(!model_trained()){
      shinyalert::shinyalert(title = "Oops", text = "Model has not been trained yet. Please train a model",
                             type = "error", showCancelButton = FALSE)
      return(NULL)
    }
    
    df_prob <- dataset_new() # Getting data
    model <- model()$model_train
    
    # If logistic regression or random forest was run 
    if(input$modelSelection != "XGBoost") {
      prob_predict <- predict(model, newdata = df_prob, type = "prob")
      prob_predict <- round(prob_predict, digits = 2)
      
    } else{
      
      # Changing to matrix format
      df_prob_matrix <- xgb.DMatrix(data = as.matrix(df_prob))
      
      # Predicting probability
      prob_predict <- predict(model, newdata = df_prob_matrix, type = "prob")
      
      # Converting to dataframe
      prob_predict <- data.frame("Default" = 1- prob_predict,
                                 "Non Default" = prob_predict)
      
      prob_predict <- round(prob_predict, digits = 2)
    }
    
    df_prob <- cbind(prob_predict, df_prob)
    return(df_prob)
  })
```
