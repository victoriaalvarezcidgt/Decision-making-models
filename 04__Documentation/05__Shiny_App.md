<h1 align = "center"> Credit Risk Models (Shiny app) </h1>
<h4 align = "center"> The following shiny app provides decision making tools for classification problems (in this case <strong> loan defaults) </strong> </h4>
<h2 align = "left"> Required Packages </h2>
This section lists the packages that are required for running the code. These packages provide additional functionality and are essential for the successful execution of the program.
<details>
  <summary>
    <h4 align = "left"> Packages </h4>
  </summary>
<ul>
  <li> <code> shiny </code>: Enables the creation of interactive web applications. </li>
  <li> <code> shinydashboard </code>: Provides a framework for building dashboards in Shiny applications. </li>
  <li> <code> shinyWidgets </code>: Offers a collection of user-friendly widgets for Shiny applications. </li>
  <li> <code> shinyjs </code>: Provides additional JavaScript functions to enhance Shiny applications. </li>
  <li> <code> DT </code>: Enables the creation of interactive data tables in Shiny applications. </li>
  <li> <code> dplyr </code>: Provides a set of tools for data manipulation and transformation. </li>
  <li> <code> Boruta </code>: Implements the Boruta algorithm for feature selection. </li>
  <li> <code> caret </code>: Offers a unified interface for machine learning models and provides functions for data preparation, feature selection, and model evaluation. </li>
  <li> <code> randomForest </code>: Implements the random forest algorithm for classification and regression. </li>
  <li> <code> xgboost </code>: Implements the gradient boosting algorithm for classification and regression. </li>
  <li> <code> ParBayesianOptimization </code>: Provides functions for hyperparameter tuning using Bayesian optimization. </li>
  <li> <code> rpart.plot </code>: Enables the visualization of decision trees created using the rpart package. </li>
  <li> <code> ggplot2 </code>: Provides a powerful and flexible system for creating visualizations. </li>
  <li> <code> ROCR </code>: Implements functions for visualizing and evaluating the performance of classification models. </li>
  <li> <code> cvms </code>: Implements cross-validation for regression models. </li>
  <li> <code> markdown </code>: Allows the conversion of R Markdown documents into various output formats. </li>
  <li> <code> DiagrammeR </code>: Enables the creation of graph and flowchart diagrams. </li>
</ul>
</details>

<h3 align = "left"> Installation </h3>
To ensure that all required packages are installed, the following <a href = "https://github.com/C-Monaghan/DM_Models/blob/main/03__Shiny/00__Custom_Functions.R#L2-L10"> check_install_package </a> function is used. 
The function iterates over the <br> <code> packages </code> vector, checks if each package is installed, and installs and loads it if necessary.

``` R
# Installs and loads all required packages
for (package_name in packages) {
  check_install_package(package_name)
}
```
