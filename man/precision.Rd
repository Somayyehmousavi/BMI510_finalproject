% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi510.R
\name{precision}
\alias{precision}
\title{Calculates the precision of binary classification model predictions.}
\usage{
precision(pred, truth)
}
\arguments{
\item{pred}{A vector of predicted labels (0 or 1)}

\item{truth}{A vector of true labels (0 or 1)}
}
\value{
The precision of the binary classification model predictions.
}
\description{
This function accepts two vectors: the predicted labels `pred` (0 or 1) and the true
labels `truth` (0 or 1) of a binary classification model, and calculates the precision.
}
\examples{
pred <- c(0, 1, 1, 0, 1)
truth <- c(0, 1, 1, 1, 0)
precision(pred, truth)

# Output:
# [1] 0.6666667

}
