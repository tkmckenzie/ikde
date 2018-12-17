#' Stan model marginal likelihood evaluation
#' 
#' Evaluates marginal likelihood of Stan model at the posterior mean
#' 
#' @param ikde.model An object of class ikde.model, does not necessarily have to be built
#' 
#' @return A real number indicating value of the log-marginal-likelihood  at the posterior mean
#' 
#' @details 
#' Uses evaluate.likelihood, evaluate.priors, and evaluate.posterior to form an estimate of
#' marginal likelihood at the posterior mean.
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
#' evaluate.marginal.likelihood(ikde.model) # Only an estimation, may not exactly match presented result
#' # [1] -368.3207
#'   
#' @export

evaluate.marginal.likelihood <-
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
