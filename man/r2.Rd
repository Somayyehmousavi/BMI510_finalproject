% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi510.R
\name{r2}
\alias{r2}
\title{Calculate the r-squared statistic between predicted and ground truth continuous variables.}
\usage{
r2(pred, truth)
}
\arguments{
\item{pred}{A vector of predicted values}

\item{truth}{A vector of true values}
}
\value{
The R-squared statistic between the predicted and ground truth continuous variables.
}
\description{
This function accepts two vectors: the predicted values `pred` and the true values `truth` of a
continuous variable, and calculates the R-squared statistic between them.
}
\examples{
pred <- c(1, 2, 3, 4, 5)
truth <- c(1.1, 1.9, 2.8, 4.1, 4.9)
r2(pred, truth)

# Output:
# [1] 0.9937107

}
