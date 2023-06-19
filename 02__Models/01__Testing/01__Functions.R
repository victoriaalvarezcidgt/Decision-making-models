outliers <- function(x) {
  
  Q1 <- quantile(x, probs = 0.25)
  Q3 <- quantile(x, probs = 0.75)
  iqr = Q3 - Q1
  
  upper_limit = Q3 + (iqr * 1.5)
  lower_limit = Q1 - (iqr * 1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]), ]
  }
  
  df
  
}