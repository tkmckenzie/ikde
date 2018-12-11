#' Randomly generated multivariate linear model
#' 
#' A dataset for estimation of linear models
#' 
#' @format A list with two components:
#' \describe{
#'   \item{X}{Matrix of independent variables}
#'   \item{y}{Vector of dependent variable observations}
#' }
#' @details 
#' Generated with the following code:
#' \preformatted{
#' set.seed(100)
#' 
#' N <- 100
#' k <- 4
#' sd <- 10
#' 
#' X <- cbind(1, matrix(runif(N * (k - 1)), ncol = k - 1))
#' beta <- runif(k, -5, 5)
#' y <- X %*% beta + rnorm(N, sd = sd)
#' y <- c(y)
#' 
"lm_generated"
