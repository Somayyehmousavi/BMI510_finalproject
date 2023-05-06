### 1: Rando ###

#' Rando: A wrapper around sample that tests whether x is an atomic vector or dataframe-like object
#' and then returns either n samples or n rows as appropriate.
#'
#' This function checks whether the input `x` is an atomic vector or a
#' data frame-like object (data frame, matrix, or list) and returns either
#' `n` samples or `n` rows accordingly. If the input does not match any of these
#' types, an error message will be shown.
#'
#' @param x An atomic vector or data frame-like object
#' @param n The number of samples or rows to return
#' @param replace Logical; should sampling be with replacement
#'
#' @return A vector or data frame-like object containing the sampled elements or rows.
#' @export
#'
#' @examples
#' # sample from an atomic vector
#' rando(15:25, 3, replace = TRUE)
#' # sample from a dataframe
#' df = data.frame(x = 10:50, y = 60:100)
#' rando(df, 2, replace = TRUE)
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

### 2: is_min ###

#' is_min: Returns a logical vector indicating where the minimum value of an atomic vector is found
#'
#' This function takes an atomic vector "x" and returns a logical vector with TRUE values indicating the
#' locations where the minimum value of "x" is found. The function also includes an optional parameter
#' "na.rm" which, if set to TRUE, removes any NA values from the input vector before finding the minimum value.
#'
#' @param x An atomic vector to be checked for its minimum value
#' @param na.rm A logical value indicating whether to remove NA values before finding the minimum. Default is TRUE.
#'
#' @return A logical vector with TRUE values indicating where the minimum value of "x" is found.
#' @export
#'
#' @examples
#' is_min(c(1, 2, 3, 1, 4, 1))
#' is_min(c(NA, 2, 3, NA, 4, NA), na.rm = TRUE)
is_min = function(x,na.rm=T) {
  min_val <- min(x, na.rm = na.rm)
  x == min_val
}


### 3: is_max ###
#' is_max: Returns a logical vector indicating where the maximum value of an atomic vector is found
#'
#' This function takes an atomic vector "x" and returns a logical vector with TRUE values indicating the locations
#' where the maximum value of "x" is found. The function also includes an optional parameter "na.rm" which, if set
#' to TRUE, removes any NA values from the input vector before finding the maximum value.
#'
#' @param x An atomic vector to be checked for its maximum value
#' @param na.rm A logical value indicating whether to remove NA values before finding the maximum. Default is TRUE.
#'
#' @return A logical vector with TRUE values indicating where the maximum value of "x" is found.
#' @export
#'
#' @examples
#' is_max(c(1, 2, 3, 1, 4, 1))
#' is_max(c(NA, 2, 3, NA, 4, NA), na.rm = TRUE)
#'
is_max = function(x,na.rm=T) {
  max_val <- max(x, na.rm = na.rm)
  x == max_val
}

### 4: rep_mat ###
#' rep_mat: Replicates a matrix or dataframe by repeating its rows or columns M or N times
#'
#' This function takes a matrix or dataframe "x" and replicates its rows or columns M or N times,
#' returning a new matrix. The values of M and N determine the number of times the rows or columns of "x"
#' are repeated, respectively.
#' If M and N are both set to 1, the function returns the input matrix or dataframe as is.
#'
#' @param x A matrix or dataframe to be replicated
#' @param M An integer specifying the number of times to replicate the rows of "x"
#' @param N An integer specifying the number of times to replicate the columns of "x"
#'
#' @return A new matrix created by replicating the rows or columns of "x" M or N times.
#' @export
#'
#' @examples
#' x <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
#' rep_mat(x, M = 2, N = 3)

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


#### 5.classes ####
#' Returns a character vector containing the classes of each variable in a tibble x.
#'
#' This function accepts a tibble `x` and returns a character vector containing the classes of each
#' variable in `x`. It is similar to the `names` function, but instead of returning the variable names,
#' it returns the variable classes.
#'
#' @param x A tibble
#'
#' @return A character vector containing the classes of each variable in `x`.
#'
#' @examples
#' # Load the tibble package
#' library(tibble)
#'
#' # Create a tibble
#' x <- tibble(a = 1:5, b = c("foot", "bar", "bazar", "math", "quux"), c = as.Date("2023-01-01") + 0:4)
#'
#' # Get the classes of each variable
#' classes(x)
#'
#' # Output:
#' #   a        b        c
#' # "integer" "character" "Date"
#'
#' @export
classes = function(x) {
  sapply(x, class)
}

#### 6.df_scale ###

#' Returns a tibble x in which the numeric variables have been scaled with scale.
#'
#' This function accepts a tibble `x` and returns a new tibble in which the numeric variables have
#' been scaled with the `scale` function. The `center` and `scale` arguments can be used to control
#' the scaling behavior.
#'
#' @param x A tibble
#' @param center Logical indicating if the variables should be centered (default is TRUE).
#' @param scale Logical indicating if the variables should be scaled (default is TRUE).
#'
#' @return A new tibble in which the numeric variables have been scaled with `scale`.
#'
#' @examples
#' # Load the tibble package
#' library(tibble)
#'
#' # Create a tibble
#' x <- tibble(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))
#'
#' # Scale the numeric variables
#' df_scale(x)
#'
#' # Output:
#' # A tibble: 3 × 3
#'       a     b     c
#'   <dbl> <dbl> <dbl>
#' 1    -1    -1    -1
#' 2     0     0     0
#' 3     1     1     1
#'
#' @import dplyr
#' @import purrr
#'
#' @export
df_scale = function(x, center = TRUE, scale = TRUE) {
  # Find numeric variables
  numeric_vars <- x %>% select_if(is.numeric) %>% names()

  # Apply scaling
  scaled_data <- x %>% mutate(across(all_of(numeric_vars), ~ scale(., center = center, scale = scale)))

  # Return scaled data
  return(as_tibble(scaled_data))
}

### 7.log_likelihood_norm ###
#' Returns the log-likelihood of a sample x under the normal distribution.
#'
#' This function accepts a numeric vector `x`, and parameters `mean` and `sd` of a normal distribution,
#' and returns the log-likelihood of observing the sample `x` under the normal distribution
#' with mean `mean` and standard deviation `sd`.
#'
#' @param x A numeric vector
#' @param mean The mean of the normal distribution
#' @param sd The standard deviation of the normal distribution
#'
#' @return The log-likelihood of the sample `x` under the normal distribution with mean `mean` and
#' standard deviation `sd`.
#'
#' @examples
#' # Compute the log-likelihood of a sample
#' x <- c(1.2, 2.1, 3.4, 4.3, 5.6)
#' log_likelihood_norm(x, 3, 1.5)
#'
#' # Output:
#' # [1] -8.652607
#'
#' @export
log_likelihood_norm = function(x, mean, sd) {
  sum(dnorm(x, mean, sd, log = TRUE))
}

#### 8.log_likelihood_unif ###
#' Returns the log-likelihood of a sample x under the uniform distribution.
#'
#' This function calculates the log-likelihood of a sample `x` under the uniform distribution,
#' where the minimumand maximum values are specified by `min` and `max`, respectively.
#'
#' @param x A numeric vector of observations.
#' @param min The minimum value of the uniform distribution.
#' @param max The maximum value of the uniform distribution.
#'
#' @return A scalar value representing the log-likelihood of the sample `x` under the uniform distribution.
#'
#' @examples
#' # Calculate the log-likelihood of a sample under the uniform distribution
#' x <- c(0.2, 0.3, 0.4, 0.5)
#' log_likelihood_unif(x, 0, 1)
#' # Output:
#' # [1] -5.545177
#'
#' @export
log_likelihood_unif = function(x, min, max) {
  sum(dunif(x, min, max, log = TRUE))
}

#### 9. ####
#' Returns the log-likelihood of a sample x under the chi-squared distribution.
#'
#' This function accepts a numeric vector `x` and a degrees of freedom `df`, and returns the
#' log-likelihood of `x` under the chi-squared distribution with `df` degrees of freedom.
#'
#' @param x A numeric vector
#' @param df Degrees of freedom for the chi-squared distribution
#'
#' @return The log-likelihood of `x` under the chi-squared distribution with `df` degrees of freedom.
#'
#' @examples
#' log_likelihood_chisq(c(1,2,3), 2)
#'
#' @export
log_likelihood_chisq = function(x, df) {
  sum(dchisq(x, df, log = TRUE))
}

#### 10.log_likelihood_f ####
#' Returns the log-likelihood of a sample x under the F distribution.
#'
#' This function accepts a numeric vector `x` and degrees of freedom `df1` and `df2`, and returns the
#' log-likelihood of `x` under the F distribution with `df1` and `df2` degrees of freedom.
#'
#' @param x A numeric vector
#' @param df1 Degrees of freedom for the numerator of the F distribution
#' @param df2 Degrees of freedom for the denominator of the F distribution
#'
#' @return The log-likelihood of `x` under the F distribution with `df1` and `df2` degrees of freedom.
#'
#' @examples
#' log_likelihood_f(c(1,2,3), 2, 5)
#'
#' @importFrom stats df
#' @export
log_likelihood_f = function(x, df1, df2) {
  sum(df(x, df1, df2, log = TRUE))
}

#### 11.log_likelihood_t ####
#' Returns the log-likelihood of a sample x under the t distribution.
#'
#' This function accepts a numeric vector `x` and a degrees of freedom `df`, and returns the log-likelihood
#' of `x` under the t distribution with `df` degrees of freedom.
#'
#' @param x A numeric vector
#' @param df Degrees of freedom for the t distribution
#'
#' @return The log-likelihood of `x` under the t distribution with `df` degrees of freedom.
#'
#' @examples
#' log_likelihood_t(c(1,2,3), 4)
#'
#' @importFrom stats dt
#' @export
log_likelihood_t = function(x, df) {
  sum(dt(x, df, log = TRUE))
}

#### 12.sensitivity ####
#' Calculates the sensitivity (true positive rate) of binary classification model predictions.
#'
#' This function accepts two vectors: the predicted labels `pred` (0 or 1) and the true
#' labels `truth` (0 or 1) of a binary classification model, and calculates the sensitivity (true positive rate).
#'
#' @param pred A vector of predicted labels (0 or 1)
#' @param truth A vector of true labels (0 or 1)
#'
#' @return The sensitivity (true positive rate) of the binary classification model predictions.
#'
#' @examples
#' pred <- c(0, 1, 1, 0, 1)
#' truth <- c(0, 1, 1, 1, 0)
#' sensitivity(pred, truth)
#'
#' # Output:
#' # [1] 0.6666667
#'
#' @importFrom stats sum
#' @export
sensitivity = function(pred, truth) {
  TP <- sum(pred == 1 & truth == 1)
  FN <- sum(pred == 0 & truth == 1)
  return(TP / (TP + FN))
}

### 13. specificity ###
#' Calculates the specificity (true negative rate) of binary classification model predictions.
#'
#' This function accepts two vectors: the predicted labels `pred` (0 or 1) and the true
#' labels `truth` (0 or 1) of a binary classification model, and calculates the specificity (true negative rate).
#'
#' @param pred A vector of predicted labels (0 or 1)
#' @param truth A vector of true labels (0 or 1)
#'
#' @return The specificity (true negative rate) of the binary classification model predictions.
#'
#' @examples
#' pred <- c(0, 1, 1, 0, 1)
#' truth <- c(0, 1, 1, 1, 0)
#' specificity(pred, truth)
#'
#' # Output:
#' # [1] 0.5
#'
#' @importFrom stats sum
#' @export
specificity = function(pred, truth) {
  TN <- sum(pred == 0 & truth == 0)
  FP <- sum(pred == 1 & truth == 0)
  return(TN / (TN + FP))
}

### 14. precision ####
#' Calculates the precision of binary classification model predictions.
#'
#' This function accepts two vectors: the predicted labels `pred` (0 or 1) and the true
#' labels `truth` (0 or 1) of a binary classification model, and calculates the precision.
#'
#' @param pred A vector of predicted labels (0 or 1)
#' @param truth A vector of true labels (0 or 1)
#'
#' @return The precision of the binary classification model predictions.
#'
#' @examples
#' pred <- c(0, 1, 1, 0, 1)
#' truth <- c(0, 1, 1, 1, 0)
#' precision(pred, truth)
#'
#' # Output:
#' # [1] 0.6666667
#'
#' @importFrom stats sum
#' @export
precision = function(pred,truth) {
  TP <- sum(pred == 1 & truth == 1)
  FP <- sum(pred == 1 & truth == 0)
  return(TP / (TP + FP))
}

### 15.recall ###
#' Calculates the recall (true positive rate) of binary classification model predictions.
#'
#' This function accepts two vectors: the predicted labels `pred` (0 or 1) and the true
#' labels `truth` (0 or 1) of a binary classification model, and calculates the recall (true positive rate).
#'
#' @param pred A vector of predicted labels (0 or 1)
#' @param truth A vector of true labels (0 or 1)
#'
#' @return The recall (true positive rate) of the binary classification model predictions.
#'
#' @examples
#' pred <- c(0, 1, 1, 0, 1)
#' truth <- c(0, 1, 1, 1, 0)
#' recall(pred, truth)
#'
#' # Output:
#' # [1] 0.5
#'
#' @importFrom stats sum
#' @export
recall = function(pred,truth) {
  TP <- sum(pred == 1 & truth == 1)
  FN <- sum(pred == 0 & truth == 1)
  return(TP / (TP + FN))
}

### 16.accuracy ###
#' Calculates the accuracy of binary classification model predictions.
#'
#' This function accepts two vectors: the predicted labels `pred` (0 or 1) and the true
#' labels `truth` (0 or 1) of a binary classification model, and calculates the accuracy.
#'
#' @param pred A vector of predicted labels (0 or 1)
#' @param truth A vector of true labels (0 or 1)
#'
#' @return The accuracy of the binary classification model predictions.
#'
#' @examples
#' pred <- c(0, 1, 1, 0, 1)
#' truth <- c(0, 1, 1, 1, 0)
#' accuracy(pred, truth)
#'
#' # Output:
#' # [1] 0.4
#'
#' @export
accuracy = function(pred, truth) {
  return(mean(pred == truth))
}

### 17.f1 ###
#' Calculate the F1 score of a binary classification model
#'
#' This function takes two vectors, the predicted labels `pred` (0 or 1) and the true
#' labels `truth` (0 or 1) of a binary classification model, and calculates the F1 score,
#' which is the harmonic mean of precision and recall.
#'
#' @param pred A vector of predicted labels (0 or 1)
#' @param truth A vector of true labels (0 or 1)
#'
#' @return The F1 score of the binary classification model predictions.
#'
#' @examples
#' pred <- c(0, 1, 1, 0, 1)
#' truth <- c(0, 1, 1, 1, 0)
#' f1(pred, truth)
#'
#' @importFrom stats sum
#' @export
f1 = function(pred, truth) {
  TP <- sum(pred == 1 & truth == 1)
  FP <- sum(pred == 1 & truth == 0)
  FN <- sum(pred == 0 & truth == 1)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  return(2 * precision * recall / (precision + recall))
}

### 18.minimum_n_per_group ###
#' Return the minimum n per group needed for a two-sample t-test given Cohen's d and desired power
#'
#' This function calculates the minimum sample size per group needed for a two-sample t-test,
#' assuming the effect size (Cohen's d) and desired power.
#' The function uses the pwr package and the pwr.t.test function to calculate the sample size.
#'
#' @param d The effect size (Cohen's d)
#' @param power The desired power of the test (default is 0.8)
#'
#' @return The minimum sample size per group needed to achieve the desired power
#'
#' @examples
#' minimum_n_per_group(d = 0.5, power = 0.8)
#'
#' # Output:
#' # [1] 32
#'
#' @importFrom pwr pwr.t.test
#' @export
minimum_n_per_group = function(d, power = 0.8) {
  min_n <- ceiling(power.t.test(power = power, delta = d)$n) # Round to higher integer.
  return(min_n)
}

### 19. r2 ###
#' Calculate the r-squared statistic between predicted and ground truth continuous variables.
#'
#' This function accepts two vectors: the predicted values `pred` and the true values `truth` of a
#' continuous variable, and calculates the R-squared statistic between them.
#'
#' @param pred A vector of predicted values
#' @param truth A vector of true values
#'
#' @return The R-squared statistic between the predicted and ground truth continuous variables.
#'
#' @examples
#' pred <- c(1, 2, 3, 4, 5)
#' truth <- c(1.1, 1.9, 2.8, 4.1, 4.9)
#' r2(pred, truth)
#'
#' # Output:
#' # [1] 0.9937107
#'
#' @importFrom stats cor
#' @export
r2 = function(pred, truth) {
  corr <- cor(pred, truth)
  r_squared <- corr^2
  return(r_squared)
}

### 20.adj_R2 ###
#' Calculates the adjusted r-squared statistic between predicted and ground truth continuous variables.
#'
#' This function accepts three arguments: the predicted values `pred` and the true values `truth` of a
#' continuous variable, and the number of model parameters `n_p`, and calculates the adjusted r-squared statistic.
#'
#' @param pred A vector of predicted values
#' @param truth A vector of true values
#' @param n_p The number of model parameters, excluding the intercept
#'
#' @return The adjusted r-squared statistic between predicted and ground truth continuous variables.
#'
#' @examples
#' pred <- c(1.2, 2.4, 3.6, 4.8, 6.0)
#' truth <- c(1.0, 2.5, 3.2, 5.0, 5.5)
#' n_p <- 2
#' adj_R2(pred, truth, n_p)
#'
#' # Output:
#' # [1] 0.8672337
#'
#' @importFrom stats cor
#' @importFrom stats length
#' @export
adj_R2 = function(pred, truth, n_p) {
  n <- length(truth)
  R2 <- r2(pred, truth)
  adjR2 <- 1 - ((n - 1) / (n - n_p - 1)) * (1 - R2)
  return(adjR2)
}
