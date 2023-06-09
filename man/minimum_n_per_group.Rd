% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi510.R
\name{minimum_n_per_group}
\alias{minimum_n_per_group}
\title{Return the minimum n per group needed for a two-sample t-test given Cohen's d and desired power}
\usage{
minimum_n_per_group(d, power = 0.8)
}
\arguments{
\item{d}{The effect size (Cohen's d)}

\item{power}{The desired power of the test (default is 0.8)}
}
\value{
The minimum sample size per group needed to achieve the desired power
}
\description{
This function calculates the minimum sample size per group needed for a two-sample t-test,
assuming the effect size (Cohen's d) and desired power.
The function uses the pwr package and the pwr.t.test function to calculate the sample size.
}
\examples{
minimum_n_per_group(d = 0.5, power = 0.8)

# Output:
# [1] 32

}
