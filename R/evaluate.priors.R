#' Stan model prior evaluation
#' 
#' Evaluates prior of Stan model at specified evaluation point
#' 
#' @param ikde.model An object of class ikde.model which has been built
#' @param eval.point A list of parameter names and the point to evaluate priors
#' 
#' @return A real number indicating value of the log-prior at the evaluation point
#' 
#' @details 
#' Parses sampling statements in ikde.model$model$priors and evaluates them at the specified
#' evaluation point.
#' 
#' @examples
#' data(lm.generated)
#' 
#' X <- lm.generated$X
#' y <- lm.generated$y
#' 
#' data <- list(N = list(type = "int<lower=1>", dim = 1, value = nrow(X)),
#'              k = list(type = "int<lower=1>", dim = 1, value = ncol(X)),
#'              X = list(type = "matrix", dim = "[N, k]", value = X),
#'              y = list(type = "vector", dim = "[N]", value = y))
#' parameters <- list(beta = list(type = "vector", dim = "[k]"),
#'                    sigma_sq = list(type = "real<lower=0>", dim = 1))
#' model <- list(priors = c("beta ~ normal(0, 10);",
#'                          "sigma_sq ~ inv_gamma(1, 1);"),
#'               likelihood = c("y ~ normal(X * beta, sqrt(sigma_sq));"))
#' 
#' ikde.model <- define.model(data, parameters, model)
#' 
#' eval.point <- list(beta = c(1, 2, 3, 4), sigma_sq = 5)
#' 
#' # These results match:
#' evaluate.priors(ikde.model, eval.point)
#' sum(dnorm(eval.point$beta, 0, 10, log = TRUE), 
#'     invgamma::dinvgamma(eval.point$sigma_sq, 1, 1, log = TRUE))
#' # [1] -16.45497
#'   
#' @export

evaluate.priors <-
  function(ikde.model, eval.point){
    if (class(ikde.model) != "ikde.model") stop("ikde.model must be of class \"ikde.model\".")
    if (class(eval.point) != "list") stop("eval.point must be a list.")
    
    return(sum(sapply(ikde.model$model$priors, evaluate.statement,
                      ikde.model = ikde.model, eval.point = eval.point)))
  }
