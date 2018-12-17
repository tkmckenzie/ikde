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
#' data <- list(N = list("int<lower=1>", nrow(X)),
#'              k = list("int<lower=1>", ncol(X)),
#'              X = list("matrix[N, k]", X),
#'              y = list("vector[N]", y))
#' parameters <- list(beta = "vector[k]",
#'                    sigma = "real<lower=0>")
#' model <- list(priors = c("beta ~ normal(0, 10)",
#'                          "sigma ~ inv_gamma(1, 1)"),
#'               likelihood = c("y ~ normal(X * beta, sigma)"))
#' 
#' ikde.model <- define.model(data, parameters, model)
#' 
#' eval.point <- list(beta = c(1, 2, 3, 4), sigma = 5)
#' 
#' # These results match:
#' evaluate.priors(ikde.model, eval.point)
#' sum(dnorm(eval.point$beta, 0, 10, log = TRUE), 
#'     invgamma::dinvgamma(eval.point$sigma, 1, 1, log = TRUE))
#' # [1] -16.45497
#'   
#' @export

evaluate.priors <-
  function(ikde.model, eval.point){
    if (class(ikde.model) != "ikde.model") stop("ikde.model must be of class \"ikde.model\".")
    if (!ikde.model$built) stop("ikde.model must be built before fitting.")
    if (class(eval.point) != "list") stop("eval.point must be a list.")
    
    return(sum(sapply(ikde.model$model$priors, evaluate.statement,
                      ikde.model = ikde.model, eval.point = eval.point)))
  }
