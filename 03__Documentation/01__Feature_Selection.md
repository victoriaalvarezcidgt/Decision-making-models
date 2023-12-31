<h1 align = "center"> Feature Selection </h1>
<h2 align = "left"> Table of Contents </h2>
<ul>
  <li> <a href = "#overview"> What is Feature Selection </a> </li>
  <li> <a href = "#methods"> Feature Selection Techniques </a> </li>
  <li> <a href = "#implemented"> Implemented Methods </a> </li>
</ul>

<div id = "overview">
<h2 align = "left"> What is Feature Selection </h2>
Feature selection is a crucial step in the process of preparing data for machine learning models. It involves selecting the most relevant and informative features from the original set of variables to improve model 
performance and reduce overfitting. By eliminating irrelevant or redundant features, feature selection can lead to more efficient and interpretable models, faster training times, and better generalization to new, unseen 
data.
</div>

<div id = "methods">
<h2 align = "left"> Feature Selection Methods </h2>
<h3 align = "left"> Filter Methods </h3>
Filter methods evaluate the relevance of features independently of the chosen machine learning algorithm. They use statistical measures or scores to rank the features based on their individual characteristics. Common filter 
methods include Pearson correlation coefficient, mutual information, and chi-square test.

<h3 align = "left"> Wrapper Methods </h3>
Wrapper methods assess the feature subsets by training and evaluating the machine learning model on different combinations of features. They involve an iterative search process to find the best feature subset that maximizes the model's performance. Examples of wrapper methods are Recursive Feature Elimination (RFE) and Sequential Feature Selection (SFS).
</div>

<div id = "implemented">
<h2 align = "left"> Implemented Methods </h2>
<h3 align ="left"> Boruta Algorithm </h3>
The Boruta algorithm is a feature selection method that uses random forests to determine the importance of features. It compares the importance of original features with randomly generated <strong> "shadow 
features" </strong> to identify the most important ones. It iteratively evaluates the features and selects those consistently more important than their shadow counterparts. This helps to choose the most relevant 
features for analysis or machine learning models, improving accuracy and interpretability.

<details>
  <summary>
    <h4 align ="left"> Boruta Steps </h4>
  </summary>
  <ul>
    <li> <strong> Random Forests: </strong> The Boruta algorithm uses random forests to determine the importance of features. Random forests are a collection of decision trees, where each tree is trained on a 
      random subset of the data. The random forests provide a measure of feature importance based on how much each feature contributes to the accuracy of the predictions. </li>
    <li> <strong> Shadow Features: </strong> To compare the importance of the original features, Boruta creates "shadow features" that are random permutations of the original features. These shadow features have 
      no real relationship with the target variable and are used as a baseline for comparison. </li>
    <li> <strong> Feature Comparison: </strong> The algorithm then trains the random forests on both the original features and the shadow features. It compares the importance of each original feature with the 
      importance of its corresponding shadow feature. If a feature has higher importance than its shadow feature in a statistically significant way, it is considered <em> tentatively important </em>. </li>
    <li> <strong> Iterative Process: </strong> The Boruta algorithm proceeds iteratively, repeatedly comparing the importance of the original features with their shadow features. It keeps track of the number of 
      times each feature is deemed tentatively important. </li>
    <li> <strong> Confirmation: </strong> Once all iterations are complete, the algorithm identifies the features that were consistently more important than their shadow features. These features are considered 
      confirmed important. </li>
      <ul>
        <li> Any feature that was not confirmed is considered unimportant. </li>
      </ul>
    <li> <strong> Final Feature Selection: </strong> Finally, you can select the confirmed important features as the subset of features to be used in your analysis or machine learning model. </li>
  </ul>
</details>

<h3 align ="left"> Recursive Feature Selection </h3>
Recursive feature selection is a method that repeatedly removes less important features from a dataset based on a machine learning model's performance. It iteratively trains and evaluates the model with reduced 
feature sets until a stopping criterion is met. The final subset of features is considered the selected set, improving efficiency without sacrificing accuracy.

<details>
  <summary>
    <h4 align ="left"> Recursive Feature Selection Steps </h4>
  </summary>
  <ul>
    <li> <strong> Initial Feature Set: </strong> Recursive feature selection starts with all the features in the dataset. </li>
    <li> <strong> Model Training: </strong> A machine learning model is trained using the initial feature set and its performance is evaluated. </li>
    <li> <strong> Feature Importance: </strong> The importance or relevance of each feature is calculated based on the model's performance. This can be done using techniques like coefficients in linear models or 
      feature importance scores in tree-based models. </li>
    <li> <strong> Feature Elimination: </strong> The least important feature(s) are identified and removed from the feature set. </li>
    <li> <strong> Model Retraining </strong> The model is retrained using the reduced feature set. </li>
    <li> <strong> Performance Evaluation: </strong> The performance of the model is evaluated again to see if it improves or not. </li>
    <li> <strong> Iterative Process: </strong> Steps 4 to 6 are repeated iteratively, removing one or more features at each iteration, until a stopping criterion is met. This can be a fixed number of features to
      select or a threshold in terms of model performance improvement.</li>
    <li> <strong> Final Feature Selection: </strong> The process stops when the stopping criterion is met, and the remaining features are considered the selected subset. </li>
  </ul>
</details>
</div>
