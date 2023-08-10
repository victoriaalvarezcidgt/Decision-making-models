<h1 align = "center"> XGBoost </h1>
<h2 align = "left"> Table of Contents </h2>
<ul>
  <li> <a href = "#introduction"> Introduction to XGBoost </a> </li>
  <li> <a href = "#how-it-works"> How XGBoost Works </a> </li>
  <li> <a href = "#advantages"> Advantages of XGBoost Models </a> </li>
  <li> <a href = "#tuning"> Hyperparameter Tuning </a> </li>
  <li> <a href = "#conclusion"> Conclusion </a> </li>
</ul>

<div id = "introduction">
<h2 align = "left"> Introduction to XGBoost </h2>
 Gradient Boosting is a boosting algorithm that utilizes the gradient descent algorithm in minimizing errors. XGBoost (Extreme Gradient Boosting) is a powerful and efficient machine learning algorithm known for its high performance and wide applicability. It belongs to the gradient-boosting family and is designed to work with tabular data for both classification 
  and regression tasks. The iterative process of the gradient descent algorithm used in the gradient boosting is represented mathematically as follows

  $$
 \begin{align*}
w & = w - \eta \nabla w \\
\text{and} \qquad \nabla w &= \frac{\partial L}{\partial w}
\end{align*}
  $$

  Where $\eta$ is the _learning rate_, $w$ is the _weight vector_, and $L$ is the _loss_.
  
</div>

<div id = "how-it-works">
<h2 align = "left"> How XGBoost Works </h2>
<h3 align = "left"> Gradient Boosting </h3>
  XGBoost is based on the concept of gradient boosting, which involves iteratively adding weak learners (decision trees) to the model. Each tree is built to correct the errors made by the previous trees, and the final prediction is obtained by summing up the 
  predictions of all the trees.
<h3 align = "left"> Regularization in XGBoost </h3>
  XGBoost includes L1 and L2 regularization terms in its objective function to prevent overfitting and improve generalization. Regularization penalizes complex models and encourages simplicity, making XGBoost more robust to noisy data. The objective function is represented as follows
  
  $$
  \begin{align*}
\mathcal{L}(\phi) = \sum_i l(\hat{y}_i, y_i) + \sum_j \Omega (f_j)
\end{align*}
  $$
  
  Note that the objective function denoted with $\mathcal{L}(\phi)$ has two terms; the first term, $\displaystyle{\sum_i l(\hat{y}_i, y_i)}$, is the __loss function__ and the second term, $\displaystyle{\sum_j \Omega (f_j)}$, is the __regularization__.
  
<h3 align = "left"> Tree Pruning </h3>
  XGBoost uses a technique called tree pruning to control the tree depth and prevent overfitting. Pruning involves removing branches from the tree that do not contribute significantly to reducing the objective function's loss.
<h3 align = "left"> Column Subsampling </h3>
  To further enhance the model's generalization and reduce overfitting, XGBoost employs column subsampling. It randomly selects a subset of features for each tree during training, making the model less sensitive to individual features.
</div>

<div id = "advantages">
<h2 align = "left"> Advantages of XGBoost Models </h2>
<ul>
  <li> <strong> High Performance: </strong> XGBoost is optimized for speed and efficiency, making it suitable for large datasets and real-time applications. </li>
  <li> <strong> Generalization: </strong> The combination of regularization, tree pruning, and column subsampling helps prevent overfitting and improves model generalization. </li>
  <li> <strong> Feature Importance: </strong> XGBoost provides a measure of feature importance, allowing users to identify the most influential features in the model's predictions. </li>
  <li> <strong> Flexibility: </strong> XGBoost can handle various data types and works well with both numerical and categorical features. </li>
</ul>
</div>

<div id = "tuning">
<h2 align = "left"> Hyperparameter Tuning </h2>
  To achieve optimal performance, XGBoost requires careful tuning of its hyperparameters. Common hyperparameters include the learning rate (eta), number of trees (n_estimators), maximum depth of trees (max_depth), and subsampling rate (subsample), among 
  others. Hyperparameter tuning is typically performed using techniques like grid search, random search, or bayesian optimisation.
<h3 align = "left">  Bayesian Optimisation </h3>
  Bayesian optimization is a probabilistic model-based optimization method that uses the principles of Bayesian statistics to guide the search for optimal hyperparameters. It models the unknown objective function (model performance) as a probabilistic
  surrogate model, typically using a Gaussian process or another probabilistic model. The surrogate model is updated iteratively as new data points are observed.
  
  <ul>
  <li> <strong> Define the Search Space: </strong> Determine the range or distribution of hyperparameter values to explore. Each hyperparameter and its respective search space are defined. </li>
  <li> <strong> Acquisition Function: </strong> An acquisition function is used to decide which point in the search space should be explored next. Common acquisition functions include Probability of Improvement (PI), Expected Improvement (EI), and Upper 
    Confidence Bound (UCB). </li>
  <li> <strong> Surrogate Model: </strong> A surrogate model (e.g., Gaussian process) is used to approximate the objective function. The surrogate model provides estimates of the objective function at unexplored points in the search space. </li>
  <li> <strong> Initial Random Sampling: </strong> A few initial data points are randomly sampled from the search space to build the initial surrogate model. </li>
  <li> <strong> Iterative Optimization: </strong> The optimization process iteratively selects new points to evaluate based on the acquisition function's recommendations. The surrogate model is updated with each new data point. </li>
  <li> <strong> Convergence Criteria: </strong> The optimization process continues until a stopping criterion is met, such as reaching a maximum number of iterations or achieving a desired level of performance. </li>
</ul>

  Bayesian optimization is a powerful technique for hyperparameter tuning in machine learning models. By iteratively exploring the search space based on probabilistic surrogate models, it efficiently finds optimal hyperparameter configurations. This approach 
  reduces the computational cost and yields improved model performance compared to traditional hyperparameter tuning methods like grid search or random search.
</div>

<div id = "conclusion">
<h2 align = "left"> Conclusion </h2>
  XGBoost is a state-of-the-art machine learning algorithm known for its performance and versatility. Its unique combination of gradient boosting, regularization, and tree pruning makes it robust and capable of handling complex datasets. By tuning 
  hyperparameters and using best practices, users can harness the full potential of XGBoost and build highly accurate and efficient machine learning models.
</div>
