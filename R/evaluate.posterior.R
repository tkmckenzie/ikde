#' Stan model posterior evaluation
#' 
#' Evaluates posterior of Stan model at the posterior mean
#' 
#' @param ikde.model An object of class ikde.model, does not necessarily have to be built
#' 
#' @return A real number indicating value of the log-posterior at the posterior mean
#' 
#' @details 
#' Uses list of ikde.model objects created by create.restricted.models to estimate posterior
#' density. Each ikde.model is fit, then conditional posterior density is estimated at the
#' specified point.
#' 
#' @examples
#' \dontrun{
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
#' ikde.model <- build.model(ikde.model)
#' stan.fit <- fit.model(ikde.model)
#' stan.extract <- rstan::extract(stan.fit)
#' 
#' eval.point <- list(beta = apply(stan.extract$beta, 2, mean),
#'                    sigma = mean(stan.extract$sigma))
#' 
#' evaluate.posterior(ikde.model, eval.point) # Only an estimation, may not exactly match presented result
#' # [1] -39.95366
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
      
      if (show.trace) show(rstan::traceplot(stan.fit))
      
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
