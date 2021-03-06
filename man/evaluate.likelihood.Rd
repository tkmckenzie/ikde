% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate.likelihood.R
\name{evaluate.likelihood}
\alias{evaluate.likelihood}
\title{Stan model likelihood evaluation}
\usage{
evaluate.likelihood(ikde.model, eval.point)
}
\arguments{
\item{ikde.model}{An object of class ikde.model which has been built}

\item{eval.point}{A list of parameter names and the point to evaluate the likelihood}
}
\value{
A real number indicating value of the log-likelihood at the specified evaluation
point
}
\description{
Evaluates likelihood of Stan model at specified evaluation point
}
\details{
Parses sampling statements in ikde.model$model$likelihood and evaluates them at the specified
evaluation point.
}
\examples{
data(lm.generated)

X <- lm.generated$X
y <- lm.generated$y

data <- list(N = list(type = "int<lower=1>", dim = 1, value = nrow(X)),
             k = list(type = "int<lower=1>", dim = 1, value = ncol(X)),
             X = list(type = "matrix", dim = "[N, k]", value = X),
             y = list(type = "vector", dim = "[N]", value = y))
parameters <- list(beta = list(type = "vector", dim = "[k]"),
                   sigma_sq = list(type = "real<lower=0>", dim = 1))
model <- list(priors = c("beta ~ normal(0, 10);",
                         "sigma_sq ~ inv_gamma(1, 1);"),
              likelihood = c("y ~ normal(X * beta, sqrt(sigma_sq));"))

ikde.model <- define.model(data, parameters, model)

eval.point <- list(beta = c(1, 2, 3, 4), sigma_sq = 5)

# These results match:
evaluate.likelihood(ikde.model, eval.point)
sum(dnorm(y, X \%*\% eval.point$beta, eval.point$sigma_sq, log = TRUE))
# [1] -1054.093
 
}
