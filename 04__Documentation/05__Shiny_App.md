<h1 align = "center"> Credit Risk Models (Shiny App) </h1>
<h4 align = "center"> The following shiny app provides decision making tools for classification problems (in this case <strong> loan defaults) </strong> </h4>
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

<h2 align = "left"> Body </h2>
<h3 align = "left"> Adaptive sizing </h3>
The <code>useShinyjs()</code> function is called to enable the usage of Shinyjs, which provides additional JavaScript functions to enhance Shiny applications. The code snippet within <code>tags$head(tags$style(...))</code> sets up adaptive sizing for the dashboard body. It applies a CSS style to the <code>.wrapper</code> class to ensure that the height is set to `auto` and allows for dynamic resizing of the dashboard content. The <code>position:relative</code> ensures relative positioning, and the <code>overflow-x:hidden; overflow-y:hidden</code> specifies that any overflow in the horizontal and vertical directions should be hidden.

<details>
  <summary>
    <h5> Code </h5>
  </summary>

```R
dashboardBody(
  useShinyjs(),
  tags$head(tags$style(
    HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
  )),
```
</details>

<h3 align = "left"> Dynamic CRT Logo </h3>
The CRT logo is positioned absolutely at the bottom left of the container. A CSS style defined within <code>tags$head(tags$style(...))</code> provides a transition effect when the logo moves up. The movement of the logo is triggered by a jQuery script defined within <code>tags$script(HTML(...))</code>, which toggles a CSS class (move-up) on the logo container when the sidebar toggle is clicked.

<details>
  <summary>
    <h5> Code </h5>
  </summary>

  ```R
# Grant Thornton logo
div(style = "text-align: center;",
  img(src = "new_path/01__GT_Logo.png", height = "150px", width = "auto")),
      
# Dynamically moving uni + funding logo
tags$head(
  tags$style(HTML("
    #image-container {
      position: absolute;
      bottom: 0;
      left: 210px;
      transition: all 0.3s ease;
    }
    
    #image-container.move-up {
      left: 0;
    }"
  ))
), # End of tags$style() & HTML() & tags$head()

div(
  id = "image-container",
  img(src = "new_path/02__Uni_Logos.png", alt = "Image", height = "150px")
),
tags$script(HTML("
  $(document).ready(function() {
    $('.sidebar-toggle').on('click', function() {
      $('#image-container').toggleClass('move-up');
    });
  });"
)) # End of tags$script() & HTML()
```
</details>

<h2 align = "left"> Creating the Server </h2>
<h3 align = "left"> Data Uploading </h3>
The <code>dataset()</code> function is run when the user uploads a .csv file to the shiny app.  It checks if the <strong> upload action button </strong> is pressed and if a dataset is selected. If both conditions are met, the selected dataset is read using the <code>read.csv</code> function and stored in the <code>df</code> variable.

```R
# Data uploading
dataset <- reactive({
  req(input$upload)
  if (!is.null(input$dataset)) {
    # Reading in data
    df <- read.csv(input$dataset$datapath)
    return(df)
  }
})
```
<h3 align = "left"> Data Processing </h3>
The <code>data_processing()</code> function is run when the user clicks the <strong> processing action button </strong>. Within the reactive expression, the uploaded dataset is accessed using the <code>dataset()</code> reactive expression. The target variable is selected based on the user's input. The data is then processed by recoding the target variable to 0 and 1 based on specific values ("Bad" and "Good", or "No" and "Yes"). Finally, the processed dataset is returned.

```R
# Processing Data
data_processing <- reactive({
  req(input$processing)

  # Reading in dataset and target selection
  df <- dataset()
  target <- toString(input$targetVariable)

  # Processing Data (recoding to 0 & 1)
  if ("Bad" %in% df[, target] || "Good" %in% df[, target]) {
    df <- df %>%
      mutate(!!sym(target) := recode(!!sym(target), "Bad" = 0, "Good" = 1)) %>%
      mutate(!!sym(target) := as.factor(!!sym(target)))
  } else if ("No" %in% df[, target] || "Yes" %in% df[, target]) {
    df <- df %>%
      mutate(!!sym(target) := recode(!!sym(target), "No" = 0, "Yes" = 1)) %>%
      mutate(!!sym(target) := as.factor(!!sym(target)))
  } else {
    df <- df %>%
      mutate(!!sym(target) := as.factor(!!sym(target)))
  }

  return(df)
})
```
<h3 align = "left"> Feature Selection </h3>
The <code>selection()</code> function is used to perform the feature selection based on the user's input.  It checks if the <strong> runSelection action button </strong> is pressed. Within the reactive expression, the processed dataset is accessed using the <code>data_processing()</code>reactive expression. The target variable is obtained from the user's input, and a formula is created based on the target variable. The feature selection method is determined based on the user's selection.

```R
# Feature Selection
selection <- eventReactive(input$runSelection, {

  # Starting progress message
  progress <- Progress$new()
  progress$set(message = "Preparing Data", value = 0.1)

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
  
  If the user selects the <strong> Boruta feature selection </strong> method, the Boruta algorithm is applied to the dataset. The Boruta algorithm is run using the Boruta function, and the resulting model is further
  processed using the <code>TentativeRoughFix</code> function. The reduced dataset is created by selecting the target variable and the attributes selected by the Boruta algorithm.

  ```R
# Boruta Model
  if (input$selection == "Boruta") {
    set.seed(110)

    progress$set(message = "Preparing Boruta", value = 0.3)
    progress$set(message = "Running Boruta", value = 0.5)

    # Running Boruta selection method
    model <- Boruta(formula, data = df, doTrace = 2, maxRuns = input$borutaIter)
    model <- TentativeRoughFix(model)

    progress$set(message = "Creating reduced dataset", value = 0.8)

    # Creating reduced dataset
    df_reduced <- df[, c(input$targetVariable, getSelectedAttributes(model))]

    progress$set(message = "Outputting Information", value = 1)
    progress$close()
}
```
</details>
<details>
  <summary>
    Recursive Feature Selection
  </summary>
  If the user selects the Recursive Feature Selection method, the Recursive Feature Elimination (RFE) algorithm is applied to the dataset. The RFE algorithm is run using the <code>rfe</code> function from the 
  <code>caret</code>package. Cross-validation controls are created using the rfeControl function, and the model is trained on different feature subset sizes. The reduced dataset is created by selecting the target variable 
  and the optimal variables identified by the RFE algorithm.

  ```R
else if (input$selection == "Recursive Feature Selection") {
    set.seed(110)

    progress$set(message = "Preparing Recursive Feature Selection", value = 0.3)
    progress$set(message = "Running Recursive Feature Selection", value = 0.5)

    # Creating cross validation controls
    ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = input$recursiveIter)

    # Running model
    model <- rfe(
      df[, -which(names(df) == input$targetVariable)],
      df[[input$targetVariable]],
      sizes = c(1:ncol(df) - 1),
      rfeControl = ctrl
    )

    progress$set(message = "Creating reduced dataset", value = 0.8)

    # Creating reduced dataset
    df_reduced <- df[, c(input$targetVariable, model$optVariables)]

    progress$set(message = "Outputting Information", value = 1)
    progress$close()
  }
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
return(list(model, df_reduced))
```

<h3 align = "left"> Modelling </h3>
The <code>model()</code> function implements a decision making model based on user input. It checks if the <strong> runModel action button </strong> is been pressed. Within the expression intitally it checks to see if a feature selection option was carried out. If so the resulting reduced dataset is used in the analysis. If not, then the full dataset is used.  The target variable is obtained from the user's input, and a formula is created based on the target variable. Training and test data is then created based on a 70:30 split and a modelling method is implemented based on user input.

```R
model <- eventReactive(input$runModel,{
    
    # Starting progress message
    progress <- Progress$new()
    progress$set(message = "Preparing Data", value = 0.1)
    
    # Variables to be used
    # Selecting dataset to use
    if(c(input$selection == "Boruta" & input$runSelection)| c(input$selection == "Recursive Feature Selection"& input$runSelection)){
      
      df <- selection()[[2]] # Use the reduced dataset
    }
    else{
      df <- data_processing() # Use the full dataset
    }
    target <- toString(input$targetVariable) # Target variable
    formula <- as.formula(paste(target, "~ . - 1")) # Formula
    
    # Creating training and test data
    set.seed(111)
    index <- createDataPartition(df[, target], p = 0.7, list = FALSE)
    
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
if(input$modelSelection == "Logistic Regression") {
      progress$set(message = "Running Logistic Regression", value = 0.4)
      Sys.sleep(0.75)
      progress$set(message = "Training Model", value = 0.5)
      Sys.sleep(0.75)
      
      set.seed(112)
      
      # Specifying the Type of Training Methods used and the Number of Folds
      ctrlspec <- trainControl(
        method = "cv", number = input$regressionFolds, savePredictions = "all", classProbs = FALSE
        )
      
      # Training Logistic Regression Model
      model_train <- train(formula, data = training_set, method = "glm",
                           family = binomial, trControl = ctrlspec)
      
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
        model_train <- randomForest(formula = formula, data = training_set)
        
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
      
      # Small data processing to create correct y & yvals reference levels
      training_set <- training_set %>%
        mutate(!!sym(target) := as.numeric(as.factor(!!sym(target)))) %>%
        mutate(!!sym(target) := !!sym(target) - 1)
      
      test_set <- test_set %>%
        mutate(!!sym(target) := as.numeric(as.factor(!!sym(target)))) %>%
        mutate(!!sym(target) := !!sym(target) - 1)
      
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
            initPoints = 7, # Must be higher than out function inputs
            iters.n = input$bayesIter, # Can be set to any iteration value
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
        
        model_train <- xgboost(data = x,
                               label = y,
                               params = params,
                               nrounds = numrounds,
                               eval_metric = "auc")
        
        xgbcv <- NULL
      }
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
        xgbcv <- xgb.cv(data = x,
                           label = y,
                           params = params,
                           nrounds = 100, prediction = TRUE, showsd = TRUE,
                           early_stopping_rounds = 10,
                           maximize = TRUE, nfold = 10, stratified = TRUE)
        
        # Optimal number of rounds
        numrounds <- min(which(
          xgbcv$evaluation_log$test_auc_mean == max(xgbcv$evaluation_log$test_auc_mean)))
        
        # Running model with default parameters
        model_train <- xgboost(data = x,
                               label = y,
                               params = params,
                               nrounds = numrounds)
      }
      
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

<h3 align = "left"> Predictions </h3>
