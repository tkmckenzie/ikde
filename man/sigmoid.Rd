% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sigmoid.R
\name{sigmoid}
\alias{sigmoid}
\title{Sigmoid function and gradient}
\usage{
sigmoid(x, lower.limit = 0, upper.limit = 1)
}
\arguments{
\item{x}{Point to evaluate sigmoid function and gradient}

\item{lower.limit}{Lower limit of the sigmoid function; defaults to 0}

\item{upper.limit}{Upper limit of the sigmoid function; defaults to 1}
}
\value{
Returns a list with two elements, each with the same number of elements as x
\item{value}{Value of the sigmoid function at x}
\item{gradient}{Value of the gradient of the sigmoid function at x}
}
\description{
Sigmoid function and gradient
}
\details{
Evaluates the sigmoid function ((upper.bound - lower.bound) / (1 + exp(-x)) + lower.bound) and its gradient ((upper.bound - lower.bound) * exp(-x) / (1 + exp(-x))^2).
}
\examples{
x <- seq(-5, 5, length.out = 100)
sigmoid.result <- sigmoid(x)

plot(sigmoid.result$value ~ x, type = "l")
plot(sigmoid.result$gradient ~ x, type = "l")

}
