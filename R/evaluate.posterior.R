#' Stan model likelihood evaluation
#' 
#' Evaluates likelihood of Stan model at specified evaluation point
#' 
#' @param ikde.model An object of class ikde.model, does not necessarily have to be built
#' 
#' @return A real number indicating value of the log-posterior at the posterior mean
#' 
#' @details 
#' Parses sampling statements in ikde.model$model$likelihood and evaluates them at the specified
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
#' parameters <- list(sigma = "real<lower=0>",
#'                    beta = "vector[k]")
#' model <- list(priors = c("beta ~ normal(0, 10)",
#'                          "sigma ~ inv_gamma(1, 1)"),
#'               likelihood = c("y ~ normal(X * beta, sigma)"))
#' 
#' ikde.model <- define.model(data, parameters, model)
#' ikde.model <- build.model(ikde.model)
#' 
#' 
#' burn.iter <- 1000
#' sample.iter <- 1000
#' control <- NULL
#' refresh <- NULL
#' display.output <- FALSE
#'   
#' @export

evaluate.likelihood <-
  function(ikde.model, burn.iter = 1000, sample.iter = 1000, control = NULL, refresh = NULL, display.output = FALSE){
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
    
    ikde.model.list <- create.restricted.models(ikde.model, eval.point)
    log.posterior <- 0
    for (ikde.model in ikde.model.list){
      #Some issues here:
      #  1) Need to loop through num_restrictions for vector parameters
      #  2) Need a way to reference evaluation point
      stan.fit <- fit.model(ikde.model, burn.iter, sample.iter, 1, control, refresh, display.output)
      stan.extract <- rstan::extract(stan.fit)
      
      density.variable <- ikde.model$density.variable
      if (length(dim(stan.extract[[density.variable]])) == 1){
        log.posterior <- log.posterior + log(quantreg::akj(stan.extract[[density.variable]][,1]))
      } else if (length(dim(stan.extract[[density.variable]])) == 2){
        log.posterior <- log.posterior + log(quantreg::akj(stan.extract[[density.variable]][,1]))
      }
    }
  }
