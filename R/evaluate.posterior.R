#' Stan model posterior evaluation
#' 
#' Evaluates posterior of Stan model at specified evaluation point
#' 
#' @param ikde.model An object of class ikde.model, does not necessarily have to be built
#' @param eval.point A list of parameter names and the point to evaluate the posterior
#' @param burn.iter Number of warmup iterations
#' @param sample.iter Number of sampling iterations
#' @param control Control parameters used in the Markov chain. See ?rstan::stan for details.
#' @param refresh How frequently should progress be reported, in numbers of iterations
#' @param display.output Boolean indicating whether output from rstan::stan should be printed
#' @param show.trace Boolean indicating whether to show trace plots
#' 
#' @return A real number indicating value of the log-posterior at the specified evaluation point
#' 
#' @details 
#' Uses list of ikde.model objects created by create.restricted.models to estimate posterior
#' density. Each ikde.model is fit, then conditional posterior density is estimated at the
#' specified point.
#' 
#' @examples
#' \donttest{
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
#' ikde.model <- build.model(ikde.model)
#' stan.fit <- fit.model(ikde.model)
#' stan.extract <- rstan::extract(stan.fit)
#' 
#' eval.point <- list(beta = apply(stan.extract$beta, 2, mean),
#'                    sigma_sq = mean(stan.extract$sigma_sq))
#' 
#' # Only an estimation, may not exactly match presented result
#' evaluate.posterior(ikde.model, eval.point)
#' # [1] -1.889711
#' }
#' 
#' @export

evaluate.posterior <-
  function(ikde.model, eval.point, burn.iter = 1000, sample.iter = 1000, control = NULL, refresh = NULL, display.output = FALSE, show.trace = FALSE){
    if (class(ikde.model) != "ikde.model") stop("ikde.model must be of class \"ikde.model\".")
    
    ikde.model.list <- create.restricted.models(ikde.model, eval.point)
    log.posterior <- 0
    for (ikde.model in ikde.model.list){
      stan.fit <- fit.model(ikde.model, burn.iter, sample.iter, 1, control, refresh, display.output)
      stan.extract <- rstan::extract(stan.fit)
      
      if (show.trace) methods::show(rstan::traceplot(stan.fit))
      
      density.variable <- ikde.model$density.variable$name
      if (length(dim(stan.extract[[density.variable]])) == 1){
        log.posterior <- log.posterior + log(quantreg::akj(stan.extract[[density.variable]], ikde.model$density.variable$value)$dens)
      } else if (length(dim(stan.extract[[density.variable]])) == 2){
        log.posterior <- log.posterior + log(quantreg::akj(stan.extract[[density.variable]][,1], ikde.model$density.variable$value)$dens)
      } else{
        stop("ikde currently only supports 0- and 1-dimensional parameters.")
      }
    }
    
    return(log.posterior)
  }
