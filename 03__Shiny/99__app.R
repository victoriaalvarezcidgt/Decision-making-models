rm(list = ls())

# Required packages ------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(dplyr)
library(Boruta)
library(caret)
library(randomForest)

options(shiny.maxRequestSize = 1024 ^ 2)
# ------------------------------------------------------------------------------

# So we can use the image pathway and not the default "www" pathway
addResourcePath("new_path", "./03__Shiny/01__Logo/")

# Defining UI
ui <- dashboardPage(
  
  skin = "purple", # Purple theme
  
  # Title
  dashboardHeader(title = "Credit Risk Models"),
  
  # Defining sideboard menu
  dashboardSidebar(
    # Adding pages to the sideboard
    sidebarMenu(
      menuItem("Home Page", tabName = "Home", icon = icon("home")),
      menuItem("Data Input", tabName = "Input", icon = icon("upload")),
      menuItem("Feaure Selection", tabName = "Selection", icon = icon("cogs")),
      menuItem("Modelling", tabName = "Modelling", icon = icon("line-chart"))
    )), # End of sidebarMenu() & dashboardSidebar()
  
  # Defining the dashboard body
  dashboardBody(
    useShinyjs(),
    # Adding body contents
    tabItems(
      # Creating home page ----------------------------------------------------
      tabItem(tabName = "Home",
      tags$body(
      h1(strong("Machine Learning Credit Risk Models"), align = "center", style = "30px"),
      p("The following shiny app provides decision making tools for 
        assessing loan defaults.", align = "center", style = "font-size: 20px"),
      p("Our tool utilizes three models to analyze borrower data and predict 
        the likelihood of loan defaults.", style = "font-size: 20px"),
      tags$ol(
        tags$li("Logistic regression is a statistical model used to predict 
        the probability of a binary outcome or event occurring (such as a 
        whether or not a loan will default. It considers various input variables 
        and calculates the odds of default.", style = "font-size: 20px"),
        tags$li("Random forest is an ensemble learning method that combines multiple 
        decision trees to make predictions. It leverages the power of many 
        individual trees to improve accuracy and handle complex interactions 
        between variables.", style = "font-size: 20px"),
        tags$li("XGBoost is another ensemble learning technique that uses a gradient
        boosting framework. It sequentially builds multiple weak models,
        such as decision trees, to create a strong predictive model. XGBoost
        is known for its scalability and performance.", style = "font-size: 20px"
        )), # End of tags$ol() & tags$li()
      
      # Logos ------------------------------------------------------------------
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
        ))), # End of tags$style() & HTML() & tags$head()
      
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
      ))), # End of tags$script() & HTML() & tags$body()
      ), # End of tabItem() {Home Page}
      
      # Creating data input page -----------------------------------------------
      tabItem(tabName = "Input",
      tags$body(
        # Text 
        h1(strong("Upload a dataset"), align = "center", style = "font-size: 30px"),
        p(strong("NOTE"), ": Please upload your dataset and select the binary loan
        default column. Accepted variable coding: (\"Good / Bad\") or (\"Yes / No \")
        or (\"0 / 1\")", style = "font-size: 20px")),
      
      # Inputting of data
      fileInput("dataset", h4("Upload a dataset (CSV format)"),
                multiple = FALSE, placeholder = "Enter your data here", 
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv" )),
      
      # Uploading dataset, selecting target variable and processing
      actionButton("upload", "Upload"),
      hr(),
      selectInput("targetVariable", "Select Target Variable", choices = NULL),
      hr(),
      actionButton("processing", "Process Data"),
      
      # Outputting data
      dataTableOutput("data_table")
      
      ), # End of tabItem() {Data input}
    
      # Feature Selection ------------------------------------------------------
      tabItem(tabName = "Selection",
      tags$body(
        h1(strong("Feature Selection"), align = "center", style = "font-size: 30px"),
        ), # End of tags$body()
      
      # Allows user to select a feature selection method
      radioButtons("selection", h4("Select a Feature Selection Method"),
                   choices = list("Boruta", "Recursive Feature Selection", "None"),
                   selected = "Boruta"),
      actionButton("runSelection", "Run Feature Selection"),
      
      # Downloading of dataset
      downloadButton("downloadReducedDataset", "Download Reduced Dataset"),
      
      # Conditional text for Boruta
      conditionalPanel(
        condition = "input.selection == 'Boruta' & input.runSelection",
        # Plots
        tags$h4("Feature Importance Plots"),
        plotOutput("borutaPlots", width = "100%", height = "500"),
        # Model Information
        tags$h4("Boruta Output"),
        textOutput("borutaInfo", container = pre)
      ),
      
      # Conditional Panel for Recursive Feature Selection
      conditionalPanel(
        condition = "input.selection == 'Recursive Feature Selection' & input.runSelection",
        # Model Information
        tags$h4("Recursive Feature Selection Output"),
        textOutput("rfsInfo", container = pre)
      ),
      
      # Outputting data
      hr(),
      dataTableOutput("data_table_reduced")
      
      
      ), # End of tabItem() {Feature Selection}
      
      # Decision Making Models -------------------------------------------------
      tabItem(tabName = "Modelling",
      tags$body(
        h1(strong("Data Modelling"), align = "center", style = "font-size: 30px"),
        ), # End of tags$body()
      # Allows user to select a modelling method
      radioButtons("modelSelection", h4("Select a Decision Making Method"),
                   choices = list("Logistic Regression", 
                                  "Random Forest", 
                                  "XGBoost"),
                   selected = "Logistic Regression"),
      actionButton("runModel", "Run Modelling"),
      
      # Conditional Panel for Logistic Regression
      conditionalPanel(
        condition = "input.modelSelection == 'Logistic Regression' & input.runModel",
        # Printing Confusion Matrix
        tags$h4("Confusion Matrix"),
        textOutput("logMatrix", container = pre)
      ),
      
      # Conditional Panel for Random Forest
      conditionalPanel(
        condition = "input.modelSelection == 'Random Forest' & input.runModel",
        # Printing Confusion Matrix
        tags$h4("Confusion Matrix"),
        textOutput("forestMatrix", container = pre)
      )
      
      ) # End of tabItem() {Modelling}
    ))) # End of tabItems() & dashboardBody() & dashboardPage()

# Creating server logic --------------------------------------------------------
server <- function(input, output, session) {
  
  # Data uploading -------------------------------------------------------------
  dataset <- reactive({
    req(input$upload)
    if(!is.null(input$dataset)){
      # Reading in data
      df <- read.csv(input$dataset$datapath)
      return(df)
    }
  }) # End of dataset()
  
  # Selecting target variable --------------------------------------------------
  observeEvent(input$upload, {
    updateSelectInput(session, "targetVariable", 
                      choices = as.character(colnames(dataset())))
  })
  
  # Processing Data ------------------------------------------------------------
  data_processing <- reactive({
    req(input$processing)
    
    # Reading in dataset and target selection
    df <- dataset()
    target <- toString(input$targetVariable)
    
    # Processing Data (recoding to 0 & 1)
    if("Bad" %in% df[, target] | "Good" %in% df[, target]){
      df <- df %>%
        mutate(!!sym(target) := recode(!!sym(target), "Bad" = 0, "Good" = 1)) %>%
        mutate(!!sym(target) := as.factor(!!sym(target)))
    }
    else if("No" %in% df[, target] | "Yes" %in% df[, target]){
      df <- df %>%
        mutate(!!sym(target) := recode(!!sym(target), "No" = 0, "Yes" = 1)) %>%
        mutate(!!sym(target) := as.factor(!!sym(target)))
    }
    else{
      df <- df %>%
        mutate(!!sym(target) := as.factor(!!sym(target)))
    }
    
    return(df)
  })
  
  # Outputting processed dataset
  output$data_table <- renderDataTable({
    datatable(data_processing(), options = list(scrollX = TRUE, paginate = TRUE,
                                                pageLength = 5))
  })
  
  # Feature Selection ----------------------------------------------------------
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
    
    # Boruta Model
    if(input$selection == "Boruta"){
      set.seed(110)
      
      progress$set(message = "Preparing Boruta", value = 0.3)
      progress$set(message = "Running Boruta", value = 0.5)
      
      # Running Boruta selection method
      model <- Boruta(formula, data = df, doTrace = 2, maxRuns = 500)
      model <- TentativeRoughFix(model)
      
      progress$set(message = "Creating reduced dataset", value = 0.8)
      
      # Creating reduced dataset
      df_reduced <- df[, c(input$targetVariable, getSelectedAttributes(model))]
      
      progress$set(message = "Outputting Information", value = 1)
      progress$close()
    } # End of Boruta
    else if(input$selection == "Recursive Feature Selection"){
      set.seed(110)
      
      progress$set(message = "Preparing Recursive Feature Selection", value = 0.3)
      progress$set(message = "Running Recursive Feature Selection", value = 0.5)
      
      # Creating cross validation controls
      ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
      
      # Running model
      model <- rfe(df[, -which(names(df) == input$targetVariable)], df[[input$targetVariable]], 
                    sizes = c(1:ncol(df)-1), 
                    rfeControl = ctrl)
      
      progress$set(message = "Creating reduced dataset", value = 0.8)
      
      # Creating reduced dataset
      df_reduced <- df[, c(input$targetVariable, model$optVariables)]
      
      progress$set(message = "Outputting Information", value = 1)
      progress$close()
    } # End of Recursive Feature Selection
    else if(input$selection == "None"){
      
      progress$set(message = "Outputting Information", value = 1)
      progress$close()
      
      model <- NULL
      df_reduced <- data_processing()
    }
    
    return(list(model, df_reduced))
    
  }) # End of selection()
  
  # Outputting Boruta model information
  output$borutaInfo <- renderText({
    model <- selection()[[1]]
    return(paste(capture.output(print(model)), collapse = '\n'))
  })
  
  # Outputting RFS model information
  output$rfsInfo <- renderText({
    model <- selection()[[1]]
    return(paste(capture.output(print(model)), collapse = '\n'))
  })
  
  # Outputting feature importance plot
  output$borutaPlots <- renderPlot({
    model = selection()[[1]]
    
    par(mar=c(10,5,2,2)) # Specifying margin sizes (in inches)
    
    return(plot(model, las = 2, xlab = '', cex.lab = 1, cex.axis = 1, 
                colCode = c("#4AEA0E", "#FFB807", "#FF0040", "#BCBCBC")))
  })
  
  # Outputting processed dataset
  output$data_table_reduced <- renderDataTable({
    datatable(selection()[[2]], options = list(scrollX = TRUE, paginate = TRUE,
                                               pageLength = 3))
  })
  
  # Downloading reduced dataset ------------------------------------------------
  output$downloadReducedDataset <- downloadHandler(
    filename = function(){
      paste("reduced_dataset.csv", sep = "")
    },
    content = function(file){
      
      df_reduced <- selection()[[2]]
      write.csv(df_reduced, file, row.names = FALSE)
    })
  
  # Decision Making Models -------------------------------------------------
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
    formula <- as.formula(paste(target, "~ .")) # Formula
    
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
    
    # Logistic Regression Method
    if(input$modelSelection == "Logistic Regression") {
      progress$set(message = "Running Logistic Regression", value = 0.4)
      Sys.sleep(0.75)
      progress$set(message = "Training Model", value = 0.5)
      Sys.sleep(0.75)
      
      set.seed(112)
      
      # Specifying the Type of Training Methods used and the Number of Folds
      ctrlspec <- trainControl(
        method = "cv", number = 10, savePredictions = "all", classProbs = FALSE
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
      progress$close()
    } # End of Logistic Regression
    
    else if(input$modelSelection == "Random Forest"){
      progress$set(message = "Running Random Forest", value = 0.4)
      Sys.sleep(0.75)
      
      # Implementing Random Search Optimization
      # Initialize variables for tracking the best model
      num_iterations <- 50 # Number of iterations to perform
      ntree_values <- seq(100, 1000, by = 100) # Number of trees
      mtry_values <- seq(1, ncol(training_set), by = 1) # Number of variables to consider at each split
      best_accuracy <- 0
      best_model <- NULL
      best_ntree <- NULL
      best_mtry <- NULL
      
      ctrlspec <- NULL # We aren't using this variable so it can be set to null
      
      progress$set(message = "Training Model", value = 0.5)
      Sys.sleep(0.75)
      
      set.seed(112)
      
      for(i in 1:num_iterations){
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
      
      progress$set(message = "Outputting Model", value = 0.9)
      Sys.sleep(0.75)
      
      model_test <- predict(best_model, newdata = test_set)
      confusion_matrix <- confusionMatrix(table(model_test, test_set$Class))
      
      progress$close()
      
    } # End of Random Forest
    
    return(list(model_train, model_test, confusion_matrix, training_set, 
                test_set, df, target, formula, ctrlspec))
  }) # End of model()
  
  # Outputting Confusion Matrix
  output$logMatrix <- renderText({
    confusion_matrix <- model()[[3]]
    return(paste(capture.output(print(confusion_matrix)), collapse = '\n'))
  })
  
  output$forestMatrix <- renderText({
    confusion_matrix <- model()[[3]]
    return(paste(capture.output(print(confusion_matrix)), collapse = '\n'))
  })
  
  } # End of Server()

shinyApp(ui = ui, server = server)


# Test Code --------------------------------------------------------------------
# Random Forest Grid Search Code -----------------------------------------------
# Assessing the accuracy of different tree sizes and splits
# ntree_values <- seq(100, 1000, by = 100) # Number of trees
# mtry_values <- seq(1, ncol(df), by = 1) # Number of variables to consider at each split
# 
# # Creating all possible combinations of the above values
# combinations <- expand.grid(ntree = ntree_values, mtry = mtry_values)
# 
# # For storing output
# model_train <- NULL
# model_test <- NULL
# confusion_matrix <- NULL
# accuracy <- NULL
# best_accuracy <- 0
# best_model <- NULL
# 
# # The below loop with iterate through all possible combinations of the tree/split
# # values and find the best performing model
# for (i in 1:nrow(combinations)) {
#   ntree <- combinations$ntree[i]
#   mtry <- combinations$mtry[i]
#   
#   
#   model_train <- randomForest(formula = formula, data = training_set,
#                               ntree = ntree, mtry = mtry)
#   model_test <- predict(model_train, newdata = test_set)
#   confusion_matrix <- table(model_test, test_set[, target])
#   accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
#   
#   # If a model has better accuracy it is saved
#   if (accuracy > best_accuracy) {
#     best_accuracy <- accuracy
#     best_model <- model_train
#   }
# } # End of for loop
