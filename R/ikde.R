#' Iterative kernel density estimation
#' 
#' Evaluates marginal likelihood of Stan model at the posterior mean using iterative kernel density estimation
#' 
#' @param ikde.model An object of class ikde.model, does not necessarily have to be built
#' @param burn.iter Number of warmup iterations
#' @param sample.iter Number of sampling iterations
#' @param control Control parameters used in the Markov chain. See ?rstan::stan for details.
#' @param refresh How frequently should progress be reported, in numbers of iterations
#' @param display.output Boolean indicating whether output from rstan::stan should be printed
#' @param show.trace Boolean indicating whether to show trace plots
#' 
#' @return A real number indicating value of the log-marginal-likelihood  at the posterior mean
#' 
#' @details 
#' Uses evaluate.likelihood, evaluate.priors, and evaluate.posterior to form an estimate of
#' marginal likelihood at the posterior mean.
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
#' 
#' # Only an estimation, may not exactly match presented result
#' ikde(ikde.model)
#' # [1] -388.9264
#' }
#'
#' @export

ikde <-
  function(ikde.model, burn.iter = 1000, sample.iter = 1000, control = NULL, refresh = NULL, display.output = FALSE, show.trace = FALSE){
    if (class(ikde.model) != "ikde.model") stop("ikde.model must be of class \"ikde.model\".")
    if (!ikde.model$built) ikde.model <- build.model(ikde.model)
    
    stan.fit <- fit.model(ikde.model, burn.iter, sample.iter, 1, control, refresh, display.output)
    stan.extract <- rstan::extract(stan.fit)
    num.paramters <- length(stan.extract) - 1
    eval.point <- list()
    for (parameter.name in names(stan.extract)[-(num.paramters + 1)]){
      if (length(dim(stan.extract[[parameter.name]])) == 1){
        eval.point[[parameter.name]] <- mean(stan.extract[[parameter.name]])
      } else if (length(dim(stan.extract[[parameter.name]])) == 2){
        eval.point[[parameter.name]] <- apply(stan.extract[[parameter.name]], 2, mean)
      } else{
        stop("ikde currently only supports 0- and 1-dimensional parameters.")
      }
    }
    
    log.lik <- evaluate.likelihood(ikde.model, eval.point)
    log.prior <- evaluate.priors(ikde.model, eval.point)
    log.posterior <- evaluate.posterior(ikde.model, eval.point, burn.iter, sample.iter, control, refresh, display.output, show.trace)
    
    return(log.lik + log.prior - log.posterior)
  }
