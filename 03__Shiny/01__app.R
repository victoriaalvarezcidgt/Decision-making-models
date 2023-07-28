rm(list = ls())

options(shiny.maxRequestSize = 1024 ^ 2)
source(file.path("03__Shiny/00__Custom_Functions.R"))

# Required packages ------------------------------------------------------------
packages <- c(
  "shiny", 
  "shinydashboard", 
  "shinyWidgets", 
  "shinyjs",
  "shinyalert", 
  "DT", 
  "dplyr", 
  "Boruta", 
  "caret", 
  "randomForest", 
  "xgboost", 
  "ParBayesianOptimization",
  "rpart.plot",
  "ggplot2",
  "ROCR",
  "cvms",
  "markdown",
  "DiagrammeR",
  "rattle"
)

# Installs and loads all required packages
for (package_name in packages) {
  check_install_package(package_name)
}

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
background-color: #FFFFFF;
}
'

# Defining UI ------------------------------------------------------------------
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
      menuItem("Modelling", tabName = "Modelling", icon = icon("line-chart")),
      menuItem("Predictions", tabName = "Predict", icon = icon("magic")),
      menuItem("Guides", tabName = "Guides", icon = icon("book"))
    )), # End of sidebarMenu() & dashboardSidebar()
  
  # Defining the dashboard body
  dashboardBody(
    useShinyjs(),
    
    # Adaptive layout and notification boxes
    tags$head(tags$style(HTML(css_code_sizing_notification))),
    
    # Adding body contents
    tabItems(
      # Creating home page ----------------------------------------------------
      tabItem(tabName = "Home",
      tags$body(
      h1(strong("Machine Learning Credit Risk Models"), align = "center", style = "30px"),
      p(strong("The following shiny app provides decision making tools for 
        assessing classification problems."), align = "center", style = "font-size: 20px"),
      p("Three models can be used to analyse borrower data and predict 
        the likelihood of loan defaults.", style = "font-size: 20px"),
      tags$ol(
        tags$li("Logistic regression.", style = "font-size: 20px"),
        tags$li("Random forest.", style = "font-size: 20px"),
        tags$li("eXtreme Gradient Boosting (XGBoost).", style = "font-size: 20px"
        )), # End of tags$ol() & tags$li()
      p("For more information about each model check out the \"Guides\" tab", 
        style = "font-size: 20px"),
      
      # Logos ------------------------------------------------------------------
      # Grant Thornton logo
      div(style = "text-align: center;", img(src = "new_path/01__GT_Logo.png", height = "150px", width = "auto")),
      
      # Dynamically moving uni + funding logo
      tags$head(tags$style(HTML(css_code_logo_position))),
      div(id = "image-container", img(src = "new_path/02__Uni_Logos.png", alt = "Image", height = "150px")),
      tags$script(HTML(js_code_logo_moving))),
      ), # End of tabItem() {Home Page}
      
      # Creating data input page -----------------------------------------------
      tabItem(tabName = "Input",
      # Text 
      h1(strong("Upload a dataset"), align = "center", style = "font-size: 30px"),
      h4("Please upload your dataset and select the binary classification column"),
      h4(strong("Accepted variable coding (case insensitive):")),
      tags$ul(
        tags$li("Good / Bad", style = "font-size: 15px"),
        tags$li("Yes / No", style = "font-size: 15px"),
        tags$li("1 / 0", style = "font-size: 15px"),
        tags$li("Non_Default / Default", style = "font-size: 15px")),

      # Inputting of data
      fileInput("dataset", h4(strong("Upload a dataset (CSV format)")),
                multiple = FALSE, placeholder = "Enter your data here", 
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv" )),
      
      # Selecting target variable and processing
      selectInput("targetVariable", "Select Target Variable", choices = NULL),
      hr(),
      actionButton("processing", "Process Data"),
      
      # Outputting data
      hr(),
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
      
      # Conditional Panel for Boruta (Number of Iterations)
      conditionalPanel(
        condition = "input.selection == 'Boruta'",
        sliderInput("borutaIter", "Number of Iterations", min = 1, max = 1000, value = 500)),
      
      # Conditional Panel for Recursive Feature Selection (Number of Folds)
      conditionalPanel(
        condition = "input.selection == 'Recursive Feature Selection'",
        sliderInput("recursiveIter", "Number of Folds", min = 1, max = 10, value = 5)),
      
      actionButton("runSelection", "Run Feature Selection"),
      
      # Downloading of dataset
      downloadButton("downloadReducedDataset", "Download Reduced Dataset"),
      
      # Conditional text for Boruta (Output)
      conditionalPanel(
        condition = "input.selection == 'Boruta' & input.runSelection",
        # Plots
        tags$h4("Feature Importance Plots"),
        plotOutput("borutaPlots", width = "100%", height = "500"),
        # Model Information
        tags$h4("Boruta Output"),
        textOutput("borutaInfo", container = pre)
      ),
      
      # Conditional Panel for Recursive Feature Selection (Output)
      conditionalPanel(
        condition = "input.selection == 'Recursive Feature Selection' & input.runSelection",
        # Plots
        tags$h4("Cross-Validation Accuracy"),
        plotOutput("rfsPlot", width = "100%", height = "500"),
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
      h1(strong("Data Modelling"), align = "center", style = "font-size: 30px"),
      
      # Allows user to select a modelling method
      radioButtons("modelSelection", h4("Select a Decision Making Method"),
                   choices = list("Logistic Regression", 
                                  "Random Forest", 
                                  "XGBoost"),
                   selected = "Logistic Regression"),
      
      # Conditional Input Panels -----------------------------------------------
      # Regression Folds Option 
      conditionalPanel(
        condition = "input.modelSelection == 'Logistic Regression'",
        sliderInput("regressionFolds", "Number of Folds", min = 1, max = 50, value = 10)),
      
      # Random Search Optimization option
      conditionalPanel(
        condition = "input.modelSelection == 'Random Forest'",
        checkboxInput("randomisation", "Apply Random Search Optimization")),
      
      # If ticked user specifies max number of iterations
      conditionalPanel(
        condition = "input.randomisation == true",
        sliderInput("randomIter", "Number of iterations", min = 1, max = 100, value = 50)),
      
      # Bayesian Optimization option
      conditionalPanel(
        condition = "input.modelSelection == 'XGBoost'",
          checkboxInput("bayes", "Apply Bayesian Optimisation (Takes time)")),
      
      # If ticked user specifies max number of iterations
      conditionalPanel(
        condition = "input.bayes == true",
        sliderInput("bayesIter", "Number of iterations", min = 1, max = 50, value = 25)),
      
      # Specifying training/test split
      sliderInput("split", "Specifiy percentage training data to use", min = 10, max = 90, value = 70, step = 10),
      hr(),
      actionButton("runModel", "Run Modelling"),
      
      # Conditional Panel for Logistic Regression ------------------------------
      conditionalPanel(
        condition = "input.modelSelection == 'Logistic Regression' & input.runModel",
        radioButtons("logOutput", h4("Select what output to view"), 
                     choices = list("Model Information",
                                    "Training & Test Data",
                                    "Decision Tree")),
        conditionalPanel(
          condition = "input.logOutput == 'Model Information'",
          tags$h4("Logistic Regression Model"),
          mainPanel(width = 24,
            tabsetPanel(
              id = "information",
              tabPanel("Model Info", textOutput("logModelInfo", container = pre)),
              tabPanel("Model Accuracy", 
                       fluidRow(
                         column(width = 6, textOutput("logModelAccuracy", container = pre)),
                         column(width = 6, plotOutput("logModelMatrix")))),
              tabPanel("Variable Importance",
                       fluidRow(
                         column(width = 6, textOutput("logVarImportance", container = pre)),
                         column(width = 6, plotOutput("logVarImpPlot", width = "100%", height = "500px")))),
              tabPanel("Metrics", 
                       fluidRow(
                         column(width = 6, plotOutput("logMatrixPlot")),
                         column(width = 6, plotOutput("logRocPlot")))) # End of fluidRow() & TabPanel()
            ))), # End of tabsetPanel() & mainPanel() & conditionalPanel(Model Information)
        
        conditionalPanel(
          condition = "input.logOutput == 'Training & Test Data'",
          tags$h4("Taining and Test Data Used"),
          mainPanel(
            tabsetPanel(
              id = "dataset",
              tabPanel("Training Data", dataTableOutput("logTrainingData")),
              tabPanel("Test Data", dataTableOutput("logTestData"))
            ))), # End of tabsetPanel() & mainPanel() & conditionalPanel(Training/Test)
        
        conditionalPanel(
          condition = "input.logOutput == 'Decision Tree'",
          tags$h4("Decision Tree"),
          tabsetPanel(
            id = "tree",
            tabPanel("Decision Tree",
                     fluidRow(column(width = 12, plotOutput("logTree", height = "800px"))))
          )) # End of tabsetPanel() & conditionalPanel(Decision Tree)
      ), # End of conditionalPanel(Logistic Regression)
      
      # Conditional Panel for Random Forest ------------------------------------
      conditionalPanel(
        condition = "input.modelSelection == 'Random Forest' & input.runModel",
        radioButtons("forestOutput", h4("Select what output to view"),
                     choices = list("Model Information",
                                    "Training & Test Data",
                                    "Decision Tree")),
        
        conditionalPanel(
          condition = "input.forestOutput == 'Model Information'",
          tags$h4("Random Forest Model"),
          mainPanel(width = 24,
                    tabsetPanel(
                      id = "information",
                      tabPanel("Model Info", textOutput("forestModelInfo", container = pre)),
                      tabPanel("Model Accuracy", 
                               fluidRow(
                                 column(width = 6, textOutput("forestModelAccuracy", container = pre)),
                                 column(width = 6, plotOutput("forestModelMatrix")))),                      
                      tabPanel("Variable Importance",
                               fluidRow(
                                 column(width = 6, textOutput("forestVarImportance", container = pre)),
                                 column(width = 6, plotOutput("forestVarImpPlot", width = "100%", height = "500px")))),
                      tabPanel("Metrics", 
                               fluidRow(
                                 column(width = 6, plotOutput("forestMatrixPlot")),
                                 column(width = 6, plotOutput("forestRocPlot")))) # End of fluidRow() & TabPanel()
                    ))), # End of tabsetPanel() & mainPanel() & conditionalPanel(Model Information)
        
        conditionalPanel(
          condition = "input.forestOutput == 'Training & Test Data'",
          tags$h4("Taining and Test Data Used"),
          mainPanel(
            tabsetPanel(
              id = "dataset",
              tabPanel("Training Data", dataTableOutput("forestTrainingData")),
              tabPanel("Test Data", dataTableOutput("forestTestData"))
            ))), # End of tabsetPanel() & mainPanel() & conditionalPanel(Training/Test)
        
        conditionalPanel(
          condition = "input.forestOutput == 'Decision Tree'",
          tags$h4("Decision Tree"),
          hr(),
          sliderInput("treeNumber", "Decision Tree Number", min = 1, max = 1, value = 1, step = 1),
          tabsetPanel(
            id = "tree",
            tabPanel("Decision Tree",
                     fluidRow(column(width = 12, plotOutput("forestTree", height = "800px"))))
          )) # End of tabsetPanel() & conditionalPanel(Decision Tree)
      ), # End of conditionalPanel(Random Forest)
      
      # Conditional Panel for XGBoost ------------------------------------------    
      conditionalPanel(
        condition = "input.modelSelection == 'XGBoost' & input.runModel",
        radioButtons("boostOutput", h4("Select what output to view"),
                     choices = list("Model Information",
                                    "Training & Test Data",
                                    "Decision Tree")),
        
        conditionalPanel(
          condition = "input.boostOutput == 'Model Information'",
          tags$h4("XGBoost Model"),
          mainPanel(width = 24,
                    tabsetPanel(
                      id = "information",
                      tabPanel("Model Info", textOutput("boostModelInfo", container = pre)),
                      tabPanel("Model Accuracy", 
                               fluidRow(
                                 column(width = 6, textOutput("boostModelAccuracy", container = pre)),
                                 column(width = 6, plotOutput("boostModelMatrix")))),    
                      tabPanel("Variable Importance",
                               fluidRow(
                                 column(width = 6, textOutput("boostVarImp", container = pre)),
                                 column(width = 6, plotOutput("boostVarImpPlot", width = "100%", height = "500px")))),
                      tabPanel("Metrics", 
                               fluidRow(
                                 column(width = 6, plotOutput("boostMatrixPlot")),
                                 column(width = 6, plotOutput("boostRocPlot")))) # End of fluidRow() & TabPanel()
                    ))), # End of tabsetPanel() & mainPanel() & conditionalPanel(Model Information)
        
        conditionalPanel(
          condition = "input.boostOutput == 'Training & Test Data'",
          tags$h4("Taining and Test Data Used"),
          mainPanel(
            tabsetPanel(
              id = "dataset",
              tabPanel("Training Data", dataTableOutput("boostTrainingData")),
              tabPanel("Test Data", dataTableOutput("boostTestData"))
            ))), # End of tabsetPanel() & mainPanel() & conditionalPanel(Training/Test)
        
        conditionalPanel(
          condition = "input.boostOutput == 'Decision Tree'",
          tags$h4("Decision Tree"),
          hr(),
          sliderInput("boostTreeNumber", "Decision Tree Number", min = 1, max = 1, value = 1, step = 1),
          tabsetPanel(
            id = "tree",
            tabPanel("Decision Tree",
                     fluidRow(column(width = 12, plotOutput("boostTree"))))
            
          )) # End of tabsetPanel() & conditionalPanel(Decision Tree)
      )), # End of conditionalPanel(XGBoost) & tabItem() {Modelling}
      
      # Predictions Page -------------------------------------------------------
      tabItem(tabName = "Predict",
      tags$body(
      h1(strong("Probability of Default"), align = "center", style = "font-size: 30px")),  
      
      # Inputting new dataset
      fileInput("probData", h4(strong("Upload a dataset (CSV format)")),
                multiple = FALSE, placeholder = "Enter your data here",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv" )),
      
      actionButton("uploadProb", "Upload"),
      hr(),
      actionButton("predict", "Generate Predictions"),
      downloadButton("downloadprobDataset", "Download Probability Dataset"),
      hr(),
      h4("Probability of Default"),
      dataTableOutput("probability")
             
      ), # End of tabItem() {Predictions}
      
      # Guides Page ------------------------------------------------------------
      tabItem(tabName = "Guides",
      tags$body(
      h1(strong("Guides"), align = "center", style = "font-size: 30px")),
      
      tabsetPanel(
        id = "guides",
        tabPanel("Feature Selection", includeMarkdown(path = "04__Documentation/01__Feature_Selection.md")),
        tabPanel("Logistic Regression", includeMarkdown(path = "04__Documentation/02__Logistic_Regression.md")),
        tabPanel("Random Forest", includeMarkdown(path = "04__Documentation/03__Random_Forest.md")),
        tabPanel("XGBoost", includeMarkdown(path = "04__Documentation/04__XGBoost.md"))
        )
      ) # End of tabItem() {Guides}
    )), # End of tabItems() & dashboardBody()
  
  # Custom CSS to change body background color
  tags$head(tags$style(HTML(css_code_custom_colour)))
  
  ) # End of dashboardPage()

# Creating server logic --------------------------------------------------------
server <- function(input, output, session) {
  
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
  
  # Once the upload button has been pressed a list of variables will appear for
  # the user to select
  observeEvent(input$dataset, {
    updateSelectInput(session, "targetVariable", 
                      choices = as.character(colnames(dataset())))
  })
  
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
  
  # Outputting processed dataset
  output$data_table <- renderDataTable({
    datatable(data_processing(), options = list(scrollX = TRUE, paginate = TRUE,
                                                pageLength = 7))
  })
  
  # Feature Selection ----------------------------------------------------------
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
    else if(input$selection == "None"){
      
      progress$set(message = "Outputting Information", value = 1)
      progress$close()
      
      model <- NULL
      df_reduced <- data_processing()
    }
    
    return(list(model = model, df_reduced = df_reduced))
    
  }) # End of selection()
  
  # Outputting Boruta model information
  output$borutaInfo <- renderText({
    model <- selection()$model
    return(paste(capture.output(print(model)), collapse = '\n'))
  })
  
  # Outputting feature importance plot
  output$borutaPlots <- renderPlot({
    model = selection()$model
    
    par(mar=c(10,5,2,2)) # Specifying margin sizes (in inches)
    
    return(plot(model, las = 2, xlab = '', cex.lab = 1, cex.axis = 1, 
                colCode = c("#4AEA0E", "#FFB807", "#FF0040", "#BCBCBC")))
  })
  
  # Outputting RFS model information
  output$rfsInfo <- renderText({
    model <- selection()$model
    return(paste(capture.output(print(model)), collapse = '\n'))
  })
  
  # Outputting cross-validation plot
  output$rfsPlot <- renderPlot({
    model = selection()$model
    
    par(mar=c(10,5,2,2)) # Specifying margin sizes (in inches)
    return(plot(model, type = c("g", "o")))
  })
  
  # Outputting processed dataset
  output$data_table_reduced <- renderDataTable({
    datatable(selection()$df_reduced, options = list(scrollX = TRUE, paginate = TRUE,
                                                     pageLength = 3))
  })
  
  # Downloading reduced dataset ------------------------------------------------
  output$downloadReducedDataset <- downloadHandler(
    filename = function(){
      paste("reduced_dataset.csv", sep = "")
    },
    content = function(file){
      
      df_reduced <- selection()$df_reduced
      write.csv(df_reduced, file, row.names = FALSE)
    })
  
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
    
    model_trained(TRUE) # Model has been trained
    return(return_list)
  }) # End of model()
  
  # Logistic Regression Output -------------------------------------------------
  # Model Information
  output$logModelInfo <- renderText({
    information <- model()$model_train
    return(paste(capture.output(print(information)), collapse = '\n'))
  })
  
  # Model Accuracy (Text)
  output$logModelAccuracy <- renderText({
    accuracy <- model()$confusion_matrix
    return(paste(capture.output(print(accuracy)), collapse = '\n'))
  })
  
  # Model Accuracy (Plot)
  output$logModelMatrix <- renderPlot({
    accuracy <- model()$confusion_matrix
    accuracy <- as_tibble(accuracy$table)
    cvms::plot_confusion_matrix(accuracy, target_col = "Reference",
                                prediction_col = "Prediction", counts_col = "n")
  }, height = 600, res = 100)
  
  # Variable Importance (Text)
  output$logVarImportance <- renderText({
    varImportance <- varImp(model()$model_train)
    return(paste(capture.output(print(varImportance)), collapse = '\n'))
  })
  
  output$logVarImpPlot <- renderPlot({
    varImportance <- varImp(model()$model_train)
    
    plot(varImportance, top = 10, main = "Variable Importance Plot (Logistic Regression)")
  }, res = 100)
  
  # Matrix Plot
  output$logMatrixPlot <- renderPlot({
    return(plot_confusion_matrix(model()$confusion_matrix)) # Using custom function script
  }, res = 100)
  
  # ROC Plot
  output$logRocPlot <- renderPlot({
    predicted <- model()$model_test
    actual <- model()$test_data[, model()$loan_default]
    
    return(plot_roc_curve(predicted, actual)) # Using custom function script
  }, res = 100)
  
  # Training Data
  output$logTrainingData <- renderDataTable({
    datatable(model()$training_data, options = list(scrollX = TRUE, paginate = TRUE,
                                                   pageLength = 10))
  })
  
  # Test Data
  output$logTestData <- renderDataTable({
    datatable(model()$test_data, options = list(scrollX = TRUE, paginate = TRUE,
                                               pageLength = 10))
  })
  
  # Decision Tree
  output$logTree <- renderPlot({
    model <- model()$model_train$finalModel
    
    fancyRpartPlot(model, main = "Decision Tree", sub = NULL, 
                   palettes = c("Greys", "Oranges"))
    })
  
  # Random Forest Output -------------------------------------------------------
  # Model Information
  output$forestModelInfo <- renderText({
    information <- model()$model_train
    return(paste(capture.output(print(information)), collapse = '\n'))
  })
  
  # Model Accuracy (Text)
  output$forestModelAccuracy <- renderText({
    accuracy <- model()$confusion_matrix
    return(paste(capture.output(print(accuracy)), collapse = '\n'))
  })
  
  # Model Accuracy (Plot)
  output$forestModelMatrix <- renderPlot({
    accuracy <- model()$confusion_matrix
    accuracy <- as_tibble(accuracy$table)
    cvms::plot_confusion_matrix(accuracy, target_col = "Reference",
                                prediction_col = "Prediction", counts_col = "n")
  }, height = 600, res = 100)
  
  # Variable Importance (Text)
  output$forestVarImportance <- renderText({
    varImportance <- varImp(model()$model_train)
    return(paste(capture.output(print(varImportance)), collapse = '\n'))
  })
  
  # Variable Importance (Plot)
  output$forestVarImpPlot <- renderPlot({
    model <- model()$model_train
    
    varImpPlot(model, sort = TRUE, n.var = 10, pch = 16, 
               main = "Variable Importance Plot (Random Forest)")
  }, res = 100)
  
  # Matrix Plot
  output$forestMatrixPlot <- renderPlot({
    return(plot_confusion_matrix(model()$confusion_matrix)) # Using custom function script
  }, res = 100)
  
  # ROC Plot
  output$forestRocPlot <- renderPlot({
    predicted <- model()$model_test
    actual <- model()$test_data[, model()$loan_default]
    
    return(plot_roc_curve(predicted, actual)) # Using custom function script
  }, res = 100)
  
  # Training Data
  output$forestTrainingData <- renderDataTable({
    datatable(model()$training_data, options = list(scrollX = TRUE, paginate = TRUE,
                                                    pageLength = 10))
  })
  
  # Test Data
  output$forestTestData <- renderDataTable({
    datatable(model()$test_data, options = list(scrollX = TRUE, paginate = TRUE,
                                                pageLength = 10))
  })
  
  # Decision Tree
  output$forestTree <- renderPlot({
    decision_trees <- list()
    model <- model()$model_train
    ntree_num <- model()$ntree
    
    # Getting all the trees made by the model and storing them as rpart objects 
    # in a list
    for(i in 1:ntree_num){
      tree <- getTree(model, k = i, labelVar = TRUE)
      tree <- rpart(tree)
      decision_trees[[i]] <- tree
    }
    
    # Plotting tree based on slider input
    rpart.plot(decision_trees[[input$treeNumber]], type = 3, 
               clip.right.labs = FALSE, roundint = FALSE)
  })
  
  # XGboost Output -------------------------------------------------------------
  # Model Information
  output$boostModelInfo <- renderText({
    information <- model()$model_train
    return(paste(capture.output(print(information)), collapse = '\n'))
  })
  
  # Model Accuracy (Text)
  output$boostModelAccuracy <- renderText({
    accuracy <- model()$confusion_matrix
    return(paste(capture.output(print(accuracy)), collapse = '\n'))
  })
  
  # Model Accuracy (Plot)
  output$boostModelMatrix <- renderPlot({
    accuracy <- model()$confusion_matrix
    accuracy <- as_tibble(accuracy$table)
    cvms::plot_confusion_matrix(accuracy, target_col = "Reference",
                                prediction_col = "Prediction", counts_col = "n")
  }, height = 600, res = 100)
  
  # Variable Importance (Text)
  output$boostVarImp <- renderText({
    model <- model()$model_train
    training <- model()$training_data
    
    importance_matrix <- xgb.importance(model = model)
    
    return(paste(capture.output(print(importance_matrix)), collapse = '\n'))
  })
  
  # Variable Importance (Plot)
  output$boostVarImpPlot <- renderPlot({
    model <- model()$model_train
    training <- model()$training_data

    importance_matrix <- xgb.importance(model = model)

    xgb.plot.importance(importance_matrix, top_n = 10, 
                        main = "Variable Importance Plot (XGBoost)")
  }, res = 100)
  
  # Matrix Plot
  output$boostMatrixPlot <- renderPlot({
    return(plot_confusion_matrix(model()$confusion_matrix)) # Using custom function script
  }, res = 100)
  
  # ROC Plot
  output$boostRocPlot <- renderPlot({
    predicted <- model()$model_test
    actual <- model()$test_data[, model()$loan_default]
    
    return(plot_roc_curve(predicted, actual)) # Using custom function script
    
  }, res = 100)
  
  # Training Data
  output$boostTrainingData <- renderDataTable({
    datatable(model()$training_data, options = list(scrollX = TRUE, paginate = TRUE,
                                                    pageLength = 10))
  })
  
  # Test Data
  output$boostTestData <- renderDataTable({
    datatable(model()$test_data, options = list(scrollX = TRUE, paginate = TRUE,
                                                pageLength = 10))
  })
  
  # Decision Tree
    output$boostTree <- renderPlot({
      decision_tree <- list()
      boost_model <- model()$model_train
      iter <- model()$model_train$niter
      
      # Getting all the trees made by the model and storing them in a list
      for(i in 1:iter){
        decision_tree[[i]] <- xgb.plot.tree(model = boost_model, trees = i - 1)
      }
      
      # Plotting tree based on slider input
      decision_tree[[input$boostTreeNumber]]
  }, res = 100)
  
  # Probability of default -----------------------------------------------------
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
  
  # Outputting predictions
  output$probability <- renderDataTable({
    datatable(predictions(), options = list(scrollX = TRUE, paginate = TRUE,
                                            pageLength = 10))
  })
  
  # Downloading probability dataset
  output$downloadprobDataset <- downloadHandler(
    filename = function(){
      paste("default_probability_dataset.csv", sep = "")
    },
    content = function(file){
      prob_data <- predictions()
      write.csv(prob_data, file, row.names = FALSE)
    })

  
  } # End of Server()

shinyApp(ui = ui, server = server)
