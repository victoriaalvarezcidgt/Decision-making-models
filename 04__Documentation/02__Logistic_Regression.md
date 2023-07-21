## Logistic Regression: Brief Overview
Logistic Regression represents a machine learning algorithm commonly employed in supervised learning scenarios, specifically tailored for situations with binary categorical outcomes. This means that the response variable's categories typically comprise two possibilities, such as yes/no, true/false, good/bad, and benign/malignant, among others. While logistic regression shares a mathematical framework similar to linear regression, its primary application lies in classification learning rather than regression. In essence, when aiming to predict categorical variables like `credit score` (good/bad), `breast cancer type` (benign/malignant), or `customer returning` (yes/no), logistic regression serves as the suitable algorithm, as opposed to scenarios where the outcome variable is continuous.

For context, continuous variables are obtained through measurements, often expressed as decimal values, encompassing attributes such as weight and height. On the other hand, categorical variables consist of distinct categories, such as skill levels (beginner, intermediate, and expert) and credit scores (good, bad). The former is seen as ordinal, as it involves a specific order, while the latter is nominal, signifying an absence of order.

## Task of Logistic Regression
In linear regression, it is required to minimize the empirical risk, usually referred to as the average squared error loss or simply, the mean squared error (MSE). Whereas, in Logistic regression, we minimize the empirical risk by maximizing the likelihood of training the dataset. We have the task of estimating the probability that an instance belongs to a particular class. If the estimated probability is greater than $0.5$ or $50$<span>%</span>, then the model predicts that the instance belongs to the positive class (labelled as $1$), and otherwise it belongs to the other class (labelled as $0$). 

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
Here, $\hat{f}(x)$ represents the Logistic Regression model estimated probability &mdash; which is usually accompanied by a bias term &mdash; producing the logistic output.

Furthermore, the Logistic Regression model utilizes a _sigmoid function_, also known as the _logistic function_, to map the aforementioned logistic to a value between $0$ and $1$. The sigmoid function is mathematically expressed as:
```math
$$
\begin{align}
\sigma(t) &= \frac{e^t}{1 + e^t} \\
\sigma(t) & = \frac{1}{1 + e^{-t}}
\end{align}
$$
```

## Performance Evaluation Metrics
These are metrics used to access the model with respect to different considerations.

- Accuracy: Measures the total correct classification. For instance, considering the case of determining the credit score of a client, `accuracy` measures the number of classifications correctly output as a `good` credit score.
    \begin{align}
\text{Accuracy} = \frac{TP + TN}{TP + FP + TN + FN}
\end{align}


- Precision: Also known as \textbf{positive predictive value}. 
    
     \begin{align}
        \text{Precision} = \frac{TP}{TP + FP}
    \end{align}


- Recall: 
    
     \begin{align}
        \text{Recall} = \frac{TP}{TP+ FN}
    \end{align}
    
- F-Score:

      \begin{align}
              \text{F-Score}  = \frac{2 \times Recall \times Precision}{Recall+ Precision}
          \end{align}
\end{itemize}



