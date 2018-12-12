#' Fits Stan model
#' 
#' Uses a built ikde.model to draw samples from posterior distribution using Stan.
#' 
#' @param ikde.model An object of class ikde.model which has been built
#' 
#' @return An object of S4 class stanfit. See rstan::stan for more details.
#' 
#' @details 
#' Takes a built ikde.model object, which contains model DSO,
#' and fits the model using rstan::stan.
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
#' ikde.model <- build.model(ikde.model)
#' stan.fit <- fit.model(ikde.model)
#' stan.extract <- extract(stan.fit)
#' 
#' print(apply(stan.extract$beta, 2, mean))
#'   
#' @export

fit.model <-
  function(ikde.model, burn.iter = 1000, sample.iter = 1000, chains = 1, control = NULL, refresh = NULL, display.output = FALSE){
    if (class(ikde.model) != "ikde.model") stop("ikde.model must be of class \"ikde.model\".")
    if (!ikde.model$built) stop("ikde.model must be built before fitting.")
    
    if (is.null(refresh)) refresh <- floor((burn.iter + sample.iter) / 100)
    
    if (!display.output) sink(tempfile())
    stan.fit <- stan(fit = ikde.model$stan.dso, data = ikde.model$stan.data,
                     chains = chains, warmup = burn.iter, iter = burn.iter + sample.iter,
                     control = control, refresh = refresh)
    if (!display.output) sink()
    
    return(stan.fit)
  }
