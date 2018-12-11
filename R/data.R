#' Randomly generated multivariate linear model data
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
"lm.generated"

#' Prostatic nodal development data
#' 
#' A dataset replicated from Chib (1995) indicating presence of prostatic nodal development among patients prostate cancer
#' 
#' @format A data.frame with 53 observations of 7 variables:
#' \describe{
#'   \item{Case}{Patient identifier}
#'   \item{y}{Binary outcome indicating nodal development}
#'   \item{X.1}{Explanatory variable}
#'   \item{X.2}{Explanatory variable}
#'   \item{X.3}{Binary explanatory variable}
#'   \item{X.4}{Binary explanatory variable}
#'   \item{X.5}{Binary explanatory variable}
#' }
#' @details 
#' These data were replicated from Chib (1995)
#' @references
#' \insertRef{Chib}{ikde}
"prostatic.nodes"
