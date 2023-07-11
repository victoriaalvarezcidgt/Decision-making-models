rm(list = ls())

# Required packages ------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(dplyr)
library(Boruta)

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
      menuItem("Feaure Selection", tabName = "Selection", icon = icon("cogs"))
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
        p(strong("NOTE"), ": Please call the binary loan default column \"Class\"
          and have the data coded as \"Good / Bad\" or \"Yes / No \" or \"0 / 1\"",
          style = "font-size: 20px")),
      
      # Inputting of data
      fileInput("dataset", h4("Upload a dataset (CSV format)"),
                multiple = FALSE, placeholder = "Enter your data here", 
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv" )),
      
      # Submitting
      actionButton("submit", "Submit"),
      
      # Target variable selection
      selectInput("targetVariable", "Select Target Variable", choices = NULL),
      
      # Outputting of data
      textOutput("data_text"),
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
      
      dataTableOutput("data_table_reduced")
      
      ) # End of tabItem() {Feature Selection}
    ))) # End of tabItems() & dashboardBody() & dashboardPage()

# Creating server logic --------------------------------------------------------
server <- function(input, output, session) {
  
  # Data uploading -------------------------------------------------------------
  dataset <- reactive({
    req(input$submit)
    if(!is.null(input$dataset)){
      
      # Reading in data
      df <- read.csv(input$dataset$datapath)
      
       # Re-coding class column based on different scenarios
      if("Class" %in% colnames(df) && ("Bad" %in% df$Class | "Good" %in% df$Class)){
      df <- df %>%
        mutate(Class = recode(Class, "Bad" = 0, "Good" = 1)) %>%
        mutate(Class = as.factor(Class))
      } else if("Class" %in% colnames(df) && ("No" %in% df$Class | "Yes" %in% df$Class)){
        df <- df %>%
          mutate(Class = recode(Class, "No" = 0, "Yes" = 1)) %>%
          mutate(Class = as.factor(Class))
      } else{
        df <- df %>%
          mutate(Class = as.factor(Class))
      } # End of if statements
      
      return(df)
    } # End of if statement
  }) # End of dataset()
  
  # Getting target variable ----------------------------------------------------
  observeEvent(input$submit, {
    updateSelectInput(session, "targetVariable", 
                      choices = as.character(colnames(dataset())))
  })

  # Outputting text and dataset
  output$data_text <- renderText({
    "Uploaded Dataset:"
  })
  
  output$data_table <- renderDataTable({
    datatable(dataset(), options = list(scrollX = TRUE, paginate = TRUE))
  })
  
  # Feature Selection ----------------------------------------------------------
  dataset_reduce <- eventReactive(input$runSelection, {
    
    # Starting progress message
    progress <- Progress$new()
    progress$set(message = "Preparing Data", value = 0.1)
    
    # Getting the data
    df <- dataset()
    
    # Keeping the class column for later integration
    class_col <- df[, "Class"]
    
    progress$set(message = "Getting selection method", value = 0.2)
    Sys.sleep(0.75)
    
    if(input$selection == "Boruta"){
      set.seed(110)
      
      progress$set(message = "Preparing Boruta", value = 0.3)
      
      # Running boruta selection method
      boruta_obj <- Boruta(Class ~ ., data = df, doTrace = 2, maxRuns = 500)
      boruta_obj <- TentativeRoughFix(boruta_obj)
      
      # Reducing the dataset
      df <- df[, getSelectedAttributes(boruta_obj)]
      df <- cbind(class_col, df)
    }
    else if(input$selection == "None"){
      df <- df
    }
    
    return(df)
    
  }) # End of dataset_reduce()
  
  # Outputting the reduced dataset
  output$data_table_reduced <- renderDataTable({
    datatable(dataset_reduce(), options = list(scrollX = TRUE, paginate = TRUE))
  })
  
  # Downloading reduced dataset
  output$downloadReducedDataset <- downloadHandler(
    filename = function(){
      paste("reduced_dataset.csv", sep = "")
    },
    content = function(file){
      
      df_reduced <- dataset_reduce()
      write.csv(df_reduced, file, row.names = FALSE)
    })
  
  # ---------------------------------------------------------------------------
  
  } # End of Server()

shinyApp(ui = ui, server = server)
