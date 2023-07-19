<h1 align = "center"> Credit Risk Models (Shiny App) </h1>
<h4 align = "left"> The following shiny app provides decision making tools for classification problems (in this case <strong> loan defaults) </strong> </h4>
<h1 align = "center"> Required Packages </h1>
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
<h1 align = "center"> Creating the UI </h1>
This section defines the user interface (UI) for the Shiny application using the <code>shinydashboard</code> package. The dashboard is broken into a <strong> sideboard </strong> and a <strong> body </strong>
<h2 align = "left"> Sidebar </h2>
The sidebar menu includes the following items:
<ul>
  <li> Home Page </li>
  <li> Data Input Page </li>
  <li> Feature Selection Page </li>
  <li> Modelling Page </li>
  <li> Guides Page </li>
</ul>

<details>
  <summary>
    Code
  </summary>
  
  ```R
dashboardSidebar(
  sidebarMenu(
    menuItem("Home Page", tabName = "Home", icon = icon("home")),
    menuItem("Data Input", tabName = "Input", icon = icon("upload")),
    menuItem("Feature Selection", tabName = "Selection", icon = icon("cogs")),
    menuItem("Modelling", tabName = "Modelling", icon = icon("line-chart")),
    menuItem("Guides", tabName = "Guides", icon = icon("book"))
  )
)
```
</details>

<h2 align = "left"> Body </h2>
<h3 align = "left"> Adaptive sizing </h3>
The <code>useShinyjs()</code> function is called to enable the usage of Shinyjs, which provides additional JavaScript functions to enhance Shiny applications. The code snippet within <code>tags$head(tags$style(...))</code> sets up adaptive sizing for the dashboard body. It applies a CSS style to the <code>.wrapper</code> class to ensure that the height is set to `auto` and allows for dynamic resizing of the dashboard content. The <code>position:relative</code> ensures relative positioning, and the <code>overflow-x:hidden; overflow-y:hidden</code> specifies that any overflow in the horizontal and vertical directions should be hidden.

<details>
  <summary>
    Code
  </summary>

```R
dashboardBody(
  useShinyjs(),
  tags$head(tags$style(
    HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
  )),
```
</details>

<h3 align = "left"> Home Page </h3>
<h4 align = "left"> Contents </h4>
<ul>
  <li> Main Heading </li>
  <li> Shiny App Overview </li>
  <li> Model Descriptions </li>
  <li> Logos </li>
</ul>

<details>
  <summary>
    Text Code
  </summary>
  
  ```R
tabItem(tabName = "Home",
  tags$body(
    h1(strong("Machine Learning Credit Risk Models"), align = "center", style = "30px"),
    p("The following shiny app provides decision making tools for assessing loan defaults.", align = "center", style = "font-size: 20px"),
    p("Our tool utilizes three models to analyze borrower data and predict the likelihood of loan defaults.", style = "font-size: 20px"),
    tags$ol(
      tags$li("Logistic regression is a statistical model used to predict the probability of a binary outcome or event occurring. In terms of assessing credit risk, the logistic regression model uses historical data
assign scores to factors influencing default probability. It then calculates the probability of default based on these scores using a logistic function. The model is trained to optimize the weights assigned to each factor and can be used to predict default probabilities for new borrowers.", style = "font-size: 20px"),
      tags$li("Random forest is an ensemble learning method that combines multiple decision trees to make predictions. It leverages the power of many individual trees to improve accuracy and handle complex interactions between variables. Each decision tree 'votes' for a prediction, and the most common prediction across all the trees is chosen as the final prediction. This voting process helps to reduce the impact of individual decision trees that might be prone to overfitting or making incorrect predictions.", style = "font-size: 20px"),
      tags$li("Similar to Random Forest, XGBoost also combines multiple models to make predictions. However, instead of using decision trees independently, XGBoost employs a technique called gradient boosting. More importance is given to models that contribute the most to reducing prediction errors.", style = "font-size: 20px")
    )
  )
)
```
</details>

<details>
  <summary>
    Logo Code
  </summary>
  <h5 align = "left"> Grant Thornton Logo </h5>
  The Grant Thornton logo is displayed using the <code>img</code> tag within a <code>div</code>container with centered alignment.
  <h5 align = "left"> CRT Logo </h5>
  The CRT logo is positioned absolutely at the bottom left of the container. A CSS style defined within <code>tags$head(tags$style(...))</code> provides a transition effect when the logo moves up. The movement of the logo
  is triggered by a jQuery script defined within <code>tags$script(HTML(...))</code>, which toggles a CSS class (move-up) on the logo container when the sidebar toggle is clicked.

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

<h3 align = "left"> Data Input Page </h3>
<h4 align = "left"> Contents </h4>
<ul>
  <li> Main Heading </li>
  <li> Formatting Notes </li>
  <li> Data Uploading Tab </li>
  <li> Column Drop Down Box: For selecting the binary classification column </li>
  <li> Processing Button: Ensures the classification column is coded as <strong> 0 and 1 </strong> </li>
  <li> Data Outputting Section: After processing is complete the dataset is outputted for the user to view </li>
</ul>

<details>
  <summary>
    Code
  </summary>

```R
tabItem(tabName = "Input",
  tags$body(
    # Text
    h1(strong("Upload a dataset"), align = "center", style = "font-size: 30px"),
    p(strong("NOTE"), ": Please upload your dataset and select the binary loan default column. Accepted variable coding: (\"Good / Bad\") or (\"Yes / No\") or (\"0 / 1\")", style = "font-size: 20px"),

    # Inputting of data
    fileInput("dataset", h4("Upload a dataset (CSV format)"),
              multiple = FALSE, placeholder = "Enter your data here", 
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),

    # Uploading dataset, selecting target variable and processing
    actionButton("upload", "Upload"),
    hr(),
    selectInput("targetVariable", "Select Target Variable", choices = NULL),
    hr(),
    actionButton("processing", "Process Data"),

    # Outputting data
    hr(),
    dataTableOutput("data_table")
  )
)
```
</details>

<h3 align = "left"> Feature Selection </h3>
