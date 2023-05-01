# bmi510.R
# Developer: "Seyedeh Somayyeh Mousavi"
# Date: May 1, 2023

# 1. A wrapper around sample that tests whether x is an atomic vector or dataframe-like
# object and then returns either n samples or n rows as appropriate.
rando = function(x,n=1,replace=T) {
  # Check if "x" is an atomic vector
  if (is.atomic(x)) {
    sample(x, n, replace)
  # Check if "x" is a data frame  
  } else if (is.data.frame(x)) {
    sample_n(x, n, replace)
  # # If neither of the above conditions are met, throw an error message  
  } else {
    stop("x must be either an atomic vector or a data frame.")
  }
}
  
# 2. Accepts an atomic vector x and returns a logical with TRUE where x equals its minimum value.
is_min = function(x,na.rm=T) {
  min_val <- min(x, na.rm = na.rm)
  x == min_val
}

# 3. Accepts an atomic vector x and returns a logical with TRUE where x equals its maximum value.
is_max = function(x,na.rm=T) {
  max_val <- max(x, na.rm = na.rm)
  x == max_val
}

# 4. Port of repmat.m in Matlab. Accepts a dataframe or matrix and x and returns a matrix created 
# by replicating the rows (or columns) M (N) times.
rep_mat = function(x, M=1, N=1) {
  # Check if input is a data frame or matrix
  if (is.data.frame(x) || is.matrix(x)) {
    # If M and N both are 1, return the input itself
    if (M == 1 && N == 1) {
      return(x)
      # If M > 1 and N = 1, repeat input M times along rows
    } else if (M > 1 && N == 1) {
      # If M = 1 and N > 1, repeat input N times along columns
      return(do.call(rbind, replicate(M, x, simplify = FALSE)))
      # If both M and N > 1, repeat input M times along rows and N times along columns
    } else if (M == 1 && N > 1) {
      return(do.call(cbind, replicate(N, x, simplify = FALSE)))
      # If input is not a data frame or matrix, return an error message
    } else if (M > 1 && N > 1) {
      rows <- do.call(rbind, replicate(M, x, simplify = FALSE))
      return(do.call(cbind, replicate(N, rows, simplify = FALSE)))
    }
  } else {
    stop("x must be a dataframe or a matrix")
  }
}

# 5. Returns a character vector containing the classes of each variable in a tibble x. (Similar to names.)
classes = function(x) {
  sapply(x, class)
}

# 6. Returns a tibble x in which the numeric variables have been scaled with scale. 
# It is not necessary to retain the variable attributes, but it is more useful if you do. (Uses answer from 5.)
library(dplyr)
library(purrr)

df_scale = function(x, center = T, scale = T) {
  # Find numeric variables
  numeric_vars <- x %>% select_if(is.numeric) %>% names()
  
  # Apply scaling
  scaled_data <- x %>% mutate(across(all_of(numeric_vars), ~ scale(., center = center, scale = scale)))
  
  # Return scaled data
  return(as_tibble(scaled_data))
}

# 7. Returns the log-likelihood of a sample x under the normal
log_likelihood_norm = function(x, mean, sd) {
  sum(dnorm(x, mean, sd, log = TRUE))
}

# 8. Returns the log-likelihood of a sample x under the uniform
log_likelihood_unif = function(x, min, max) {
  sum(dunif(x, min, max, log = TRUE))
}
  
# 9. Returns the log-likelihood of a sample x under the chi-squared
log_likelihood_chisq = function(x, df) {
  sum(dchisq(x, df, log = TRUE))
}
  
# 10. Returns the log-likelihood of a sample x under the f
log_likelihood_f = function(x, df1, df2) {
  sum(df(x, df1, df2, log = TRUE))
}

# 11. Returns the log-likelihood of a sample x under the t densities
log_likelihood_t = function(x, df) {
  sum(dt(x, df, log = TRUE))
}

# 12. Calculate various performance outcomes
sensitivity = function(pred,truth) {
  TP <- sum(pred == 1 & truth == 1)
  FN <- sum(pred == 0 & truth == 1)
  return(TP / (TP + FN))
}

# 13. Calculate various performance outcomes
specificity = function(pred,truth) {
  TN <- sum(pred == 0 & truth == 0)
  FP <- sum(pred == 1 & truth == 0)
  return(TN / (TN + FP))
}

# 14. Calculate various performance outcomes
precision = function(pred,truth) {
  TP <- sum(pred == 1 & truth == 1)
  FP <- sum(pred == 1 & truth == 0)
  return(TP / (TP + FP))
}

# 15. Calculate various performance outcomes
recall = function(pred,truth) {
  TP <- sum(pred == 1 & truth == 1)
  FN <- sum(pred == 0 & truth == 1)
  return(TP / (TP + FN))
}

# 16. Calculate various performance outcomes
accuracy = function(pred,truth) {
  return(mean(pred == truth))
}

# 17. Calculate various performance outcomes
f1 = function(pred,truth) {
  TP <- sum(pred == 1 & truth == 1)
  FP <- sum(pred == 1 & truth == 0)
  FN <- sum(pred == 0 & truth == 1)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  return(2 * precision * recall / (precision + recall))
}

# 18. Return the minimum n per group needed for a two-sample t-test. 
# d is the expected Cohen’s d.
library(pwr)
minimum_n_per_group = function(d,power = 0.8) {
  n <- pwr.t.test(d = d, sig.level = 0.05, power = power, type = "two.sample")$n
  return (ceiling(n)) # round up to nearest integer
}

# 19. Calculate the r-squared statistic between predicted and ground truth
# continuous variables.

r2 = function(pred,truth) {
  corr <- cor(pred, truth)
  r_squared <- corr^2
  return(r_squared)
}

# 20. Calculate the adjusted r-squared statistic between predicted and ground truth 
#continuous variables. N_p is the number of model parameters, excluding the intercept.

adj_R2 = function(pred,truth,n_p) {
  n <- length(truth)
  R2 <- r2(pred, truth)
  adjR2 <- 1 - ((n - 1) / (n - n_p - 1)) * (1 - R2)
  return(adjR2) 
  }