## Logistic Regression: Brief Overview
Logistic Regression represents a machine learning algorithm commonly employed in supervised learning scenarios, specifically tailored for situations with binary categorical outcomes. This means that the response variable's categories typically comprise two possibilities, such as yes/no, true/false, good/bad, and benign/malignant, among others. While logistic regression shares a mathematical framework similar to linear regression, its primary application lies in classification learning rather than regression. In essence, when aiming to predict categorical variables like `credit score` (good/bad), `breast cancer type` (benign/malignant), or `customer returning` (yes/no), logistic regression serves as the suitable algorithm, as opposed to scenarios where the outcome variable is continuous.

For context, continuous variables are obtained through measurements, often expressed as decimal values, encompassing attributes such as weight and height. On the other hand, categorical variables consist of distinct categories, such as skill levels (beginner, intermediate, and expert) and credit scores (good, bad). The former is seen as ordinal, involving a specific order, while the latter is nominal, signifying an absence of order.

## Task of Logistic Regression
In linear regression, it is required to minimize the empirical risk, usually referred to as the average squared error loss or simply, the mean squared error (MSE). In Logistic regression, we minimize the empirical risk by maximizing the likelihood of training the dataset. We have the task of estimating the probability that an instance belongs to a particular class. If the estimated probability is greater than $0.5$ or $50$<span>%</span>, then the model predicts that the instance belongs to the positive class (labelled as $1$), and otherwise it belongs to the other class (labelled as $0$). 

The logistic regression model is written as 
```math
\log\left(\frac{f(x)}{1 - f(x)}\right) = \beta_0 + \beta_1 x_1 + \beta_2 x_2 + \cdots  + \beta_k x_k.
```
And the logistic regression model prediction discussed above is also represented mathematically as
```math
\begin{align}
\hat{y}(x) = 
\begin{cases} 
      1 & \hat{f}(x) \geq 0.5 \\
      0 & \hat{f}(x) < 0.5 
\end{cases}
\end{align}
```
Here, $\hat{f}(x)$ represents the Logistic Regression model estimated probability &mdash; which is usually accompanied by a bias term &mdash; producing the logistic output. The summary of the logistic regression model is a weight coefficient for the individual features of the dataset plus a bias unit.

Furthermore, the Logistic Regression model utilizes a _sigmoid function_, also known as the _logistic function_, to map the _logistic_ as mentioned earlier to a value between $0$ and $1$. The sigmoid function is mathematically expressed as:
```math
$$
\begin{align}
\sigma(t) &= \frac{e^t}{1 + e^t} \\
\sigma(t) & = \frac{1}{1 + e^{-t}}
\end{align}
$$
```
## Cross Validation
This is a technique for improving the accuracy of the model whilst ensuring that possible discrepancies between the training and testing set are minimised. It typically helps to avoid overfitting in the model. Overfitting in a model is the scenario whereby the model does not generalise well on unseen data (test set) but performs better on the training data as a result of picking too much noise from the training data. Thus, to have an idea about how the model would generalize on unseen data, __Cross Validation__ techniques are utilised. Two instances of this technique are Leave-One-Out Cross-Validation and $k$-fold Cross-Validation, with the latter being perhaps the most common one, so we would talk about it, especially since the model utilised that.

### $K$-Fold Cross-Validation
This uses part of the dataset for modelling and another different part for testing. This approach gives each sample in the dataset the chance of being tested since it involves iteration through the dataset over a number of times. Iteration is done over the dataset $k$ number of times and for each iteration, the dataset is split into $K$ roughly equal subsets, using one part for validation, and the remaining $K - 1$ is combined into a training subset for model evaluation. This is done for the $k = 1, 2, \cdots, K$ whilst calculating the prediction error when predicting the $k$-th data part. Common choices for values of $k$ are usually $5$ or $10$, and when $K = N$, it is the case of the __Leave-One-Out Cross-Validation__.


## Performance Evaluation Metrics
These are metrics used to access the classification model with respect to different considerations. The prominent ones relevant to the model are discussed as follows:
- Confusion Matrix: The table below gives the representation of the components of the evaluation metrics, defining the predicted and actual labels. It is referred to as the __confusion matrix__.

|         |                 | Reference/Actual $(\text{y})$ |         |
|---------|-----------------|-------------------------------|---------|
|         |                 | Positive                      | Negative |
|---------|-----------------|-------------------------------|---------|
| **Prediction $\widehat{f}(\text{x})$** | Positive   | True Positive ($\widehat{\text{TP}}$)     | False Positive ($\widehat{\text{FN}}$) |
|         | Negative        | False Negative ($\widehat{\text{FP}}$)    | True Negative ($\widehat{\text{TN}}$) |

For each of the four representative cells, we have:
+ True Positive (TP): Positive observation, and predicted as positive.
+ True Negative (TN): Negative observation, and predicted as negative.
+ False Positive (FP): Negative observation, but predicted as positive.
+ False Negative (FN): Positive observation, but predicted as negative.
    

- Accuracy: Measures the total correct classification. For instance, considering the case of determining the credit score of a client, _accuracy_ measures the number of classifications correctly output as a `good` credit score.

```math
    \begin{align}
\text{Accuracy} = \frac{\widehat{\text{TP}} + \widehat{\text{TN}}}{\widehat{\text{TP}} + \widehat{\text{FP}} + \widehat{\text{TN}} + \widehat{\text{FN}}}
\end{align}
```

- __Precision__: Also known as __positive predictive value__. The ratio of true positives to all the predicted positives by the model.
  
```math
      \begin{align}
        \text{Precision} = \frac{\widehat{\text{TP}}}{\widehat{\text{TP}} + \widehat{\text{FP}}}
    \end{align}
```
In the context of predicting clients with good or bad credit scores, with **Good** as the positive class, it can also be expressed as 
  ```math
      \begin{align}
        \text{Recall} = \frac{\text{Number of customers predicted as GOOD given that they are GOOD}}{\text{Total number predicted as GOOD}}
    \end{align}
   ```

- __Sensitivity or Recall__: Also referred to as the __true positive rate__; that is, the ratio of true positives to all the positives.
  ```math
      \begin{align}
        \text{Recall} = \frac{\widehat{\text{TP}}}{\widehat{\text{TP}}+ \widehat{\text{FN}}}
    \end{align}
   ```
  Also, using the same context as above, it can also be expressed as 
  ```math
      \begin{align}
        \text{Recall} = \frac{\text{Number of customers predicted as GOOD given that they are GOOD}}{\text{Total number that is actually GOOD}}
    \end{align}
   ```
- __Specificity__: Also referred to as the __true negative rate__; that is, the ratio of true negatives to all the negatives.
  ```math
      \begin{align}
        \text{Recall} = \frac{\widehat{\text{TN}}}{\widehat{\text{TN}}+ \widehat{\text{FP}}}
    \end{align}
   ```
    
- __F-Score__: This estimates the classification model based on predictions for the positive class.
  ```math
      \begin{align}
              \text{F-Score}  = \frac{2 \times Recall \times Precision}{Recall+ Precision}
  \end{align}
  ```

## Pros and Cons of the Algorithm
A few of the advantages and limitations of the logistic regression algorithm are listed below:
### Pros
- Logistic regression is suitable in the need for a quick initial benchmark as it's easy to implement.
- With lower-dimensional datasets, it is less prone to overfitting.

### Cons
- It is applicable only when the response variable is a discrete function.
- It assumes the condition of linearity between the response variable and the feature variables.

## Useful References
- [The Elements of Statistical Learning](https://www.amazon.co.uk/Elements-Statistical-Learning-Springer-Statistics/dp/0387848576#:~:text=Book%20details&text=This%20book%20describes%20the%20important,liberal%20use%20of%20colour%20graphics.)
- [Model Evaluation, Model Selection, and Algorithm Selection in Machine Learning](https://arxiv.org/pdf/1811.12808.pdf)
- [Hands-on Machine Learning with Scikit-Learn, Keras & TensorFlow](https://www.amazon.co.uk/Hands-Machine-Learning-Scikit-Learn-TensorFlow/dp/1098125975)
- [Data Science from Scratch](https://www.amazon.co.uk/Data-Science-Scratch-Joel-Grus/dp/1492041130)
