#' Bridge sampling
#' 
#' Evaluates marginal likelihood of Stan model at the posterior mean using bridge sampling
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
#' Uses evaluate.likelihood, evaluate.priors, and a custom bridge sampling routine to estimate
#' marginal likelihood.
#' 
#' @examples
#' \donttest{
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
#'                    sigma_sq = "real<lower=0>")
#' model <- list(priors = c("beta ~ normal(0, 10)",
#'                          "sigma_sq ~ inv_gamma(1, 1)"),
#'               likelihood = c("y ~ normal(X * beta, sqrt(sigma_sq))"))
#' 
#' ikde.model <- define.model(data, parameters, model)
#' 
#' # Only an estimation, may not exactly match presented result
#' # bridge(ikde.model)
#' # [1] 
#' 
#' 
#' 
#' burn.iter = 1000
#' sample.iter = 1000
#' control = NULL
#' refresh = NULL
#' display.output = FALSE
#' show.trace = FALSE
#' 
#' parameter.num = 1
#' parameter.type = "vector<lower = 0, upper = N * k>[N]"
#' 
#' }
#'
#' @export

bridge <-
  function(ikde.model, burn.iter = 1000, sample.iter = 1000, control = NULL, refresh = NULL, display.output = FALSE, show.trace = FALSE){
    if (class(ikde.model) != "ikde.model") stop("ikde.model must be of class \"ikde.model\".")
    if (!ikde.model$built) ikde.model <- build.model(ikde.model)
    
    #First fit the model
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
    
    #Form proposal distributions
    for (parameter.num in 1:num.parameters){
      parameter <- names(ikde.model$parameters)[parameter.num]
      parameter.type <- ikde.model$parameters[[parameter]]
      parameter.type <- gsub(" ", "", parameter.type)
      parameter.restriction.pos <- gregexpr("<[0-9A-Za-z\\.,\\*/\\+\\-\\^_=]+>", parameter.type)[[1]]
      parameter.restriction <- substr(parameter.type, as.numeric(parameter.restriction.pos), as.numeric(parameter.restriction.pos) + attr(parameter.restriction.pos, "match.length") - 1)
      
      if (grepl("lower=", parameter.restriction)){
        lower.limit.pos <- gregexpr("(?<=lower=)[0-9A-Za-z\\.\\*/\\+\\-\\^_]+(?=[,>]+)", parameter.restriction, perl = TRUE)[[1]]
        lower.limit <- substr(parameter.restriction, as.numeric(lower.limit.pos), as.numeric(lower.limit.pos) + attr(lower.limit.pos, "match.length") - 1)
        for (data.var in names(ikde.model$data)){
          regex <- paste0("(?<![0-9A-Za-z\\.\\$_]{1})", data.var, "(?![0-9A-Za-z\\.\\$_]{1})")
          lower.limit <- gsub(regex, paste0("ikde.model$data$", data.var, "[[2]]"), lower.limit, perl = TRUE)
        }
        lower.limit <- evaluate.expression(lower.limit, ikde.model = ikde.model)
        
        if (grepl("upper=", parameter.restriction)){
          upper.limit.pos <- gregexpr("(?<=upper=)[0-9A-Za-z\\.\\*/\\+\\-\\^_]+(?=[,>]+)", parameter.restriction, perl = TRUE)[[1]]
          upper.limit <- substr(parameter.restriction, as.numeric(upper.limit.pos), as.numeric(upper.limit.pos) + attr(upper.limit.pos, "match.length") - 1)
          for (data.var in names(ikde.model$data)){
            regex <- paste0("(?<![0-9A-Za-z\\.\\$_]{1})", data.var, "(?![0-9A-Za-z\\.\\$_]{1})")
            upper.limit <- gsub(regex, paste0("ikde.model$data$", data.var, "[[2]]"), upper.limit, perl = TRUE)
          }
          upper.limit <- evaluate.expression(upper.limit, ikde.model = ikde.model)
          
          #Use sigmoid function to transform samples from a MVN
          #Use delta method to derive variance of MVN
          #Want to generate X ~ MVN(mu, Sigma) such that
          #  * 
        }
      }
    }
    
    return()
  }
