% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbern.R
\name{dbern}
\alias{dbern}
\title{Bernoulli distribution function}
\usage{
dbern(x, prob, log = FALSE)
}
\arguments{
\item{x}{Vector of quantiles (either 0 or 1)}

\item{prob}{Vector of probability of success (in [0, 1] interval)}

\item{log}{Logical; if TRUE, log probabilities are returned}
}
\value{
Returns a vector of probabilities for each observation
}
\description{
Bernoulli distribution function
}
\details{
Evaluates Bernoulli distribution at specified points.
}
\examples{
prob <- runif(50)
x <- ifelse(runif(50) < prob, 1, 0)

dbern(x, prob)

}
