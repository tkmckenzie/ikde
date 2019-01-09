#' Fits Stan model
#' 
#' Uses a built ikde.model to draw samples from posterior distribution using Stan
#' 
#' @param ikde.model An object of class ikde.model which has been built
#' @param burn.iter Number of warmup iterations
#' @param sample.iter Number of sampling iterations
#' @param chains Number of independent chains to use
#' @param control Control parameters used in the Markov chain. See ?rstan::stan for details.
#' @param refresh How frequently should progress be reported, in numbers of iterations
#' @param display.output Boolean indicating whether output from rstan::stan should be printed
#' 
#' @return An object of S4 class stanfit. See rstan::stan for more details.
#' 
#' @details 
#' Takes a built ikde.model object, which contains model DSO,
#' and fits the model using rstan::stan.
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
#' stan.extract <- extract(stan.fit)
#' 
#' # Only an estimation, may not exactly match presented result
#' print(apply(stan.extract$beta, 2, mean))
#' # [1] 3.199021 1.620546 4.489716 1.226508
#' }
#'
#' @import rstan
#'
#' @export

fit.model <-
  function(ikde.model, burn.iter = 1000, sample.iter = 1000, chains = 1, control = NULL, refresh = NULL, display.output = FALSE){
    if (class(ikde.model) != "ikde.model") stop("ikde.model must be of class \"ikde.model\".")
    if (!ikde.model$built) stop("ikde.model must be built before fitting.")
    
    if (is.null(refresh)) refresh <- floor((burn.iter + sample.iter) / 100)
    
    if (!display.output) sink(tempfile())
    stan.fit <- rstan::stan(fit = ikde.model$stan.dso, data = ikde.model$stan.data,
                            chains = chains, warmup = burn.iter, iter = burn.iter + sample.iter,
                            control = control, refresh = refresh)
    if (!display.output) sink()
    
    return(stan.fit)
  }
