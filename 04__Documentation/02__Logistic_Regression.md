## Logistic Regression: Brief Overview
Logistic Regression represents a machine learning algorithm commonly employed in supervised learning scenarios, specifically tailored for situations with binary categorical outcomes. This means that the response variable's categories typically comprise two possibilities, such as yes/no, true/false, good/bad, and benign/malignant, among others. While logistic regression shares a mathematical framework similar to linear regression, its primary application lies in classification learning rather than regression. In essence, when aiming to predict categorical variables like `credit score` (good/bad), `breast cancer type` (benign/malignant), or `customer returning` (yes/no), logistic regression serves as the suitable algorithm, as opposed to scenarios where the outcome variable is continuous.

For context, continuous variables are obtained through measurements, often expressed as decimal values, encompassing attributes such as weight and height. On the other hand, categorical variables consist of distinct categories, such as skill levels (beginner, intermediate, and expert) and credit scores (good, bad). The former is seen as ordinal, as it involves a specific order, while the latter is nominal, signifying an absence of order.

## Task of Logistic Regression
In linear regression, we were required to minimize the empirical risk, usually referred to as the average squared error loss or simply, the mean squared error (MSE). Whereas, in Logistic regression, we minimize the empirical risk by maximizing the likelihood of training the dataset. We have the task of estimating the probability that an instance belongs to a particular class. If the estimated probability is greater than $0.5$ or $50%$, then the model predicts that the instance belongs to the positive class (labelled as $1$), and otherwise it belongs to the other class (labelled as $50 \%$). Mathematically, the logistic regression model prediction is given as <br>
```math
\begin{align}
\hat{y}(x) = 
\begin{cases} 
      1 & \hat{p}(x) \geq 0.5 \\
      0 & \hat{p}(x) < 0.5 
\end{cases}
\end{align}
```
