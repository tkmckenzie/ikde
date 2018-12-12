#' Creates restricted models for IKDE
#' 
#' Creates set of restricted models to be used for posterior density estimation
#' 
#' @param ikde.model An object of class ikde.model, does not necessarily have to be built
#' 
#' @return Returns a list of ikde.models for each restricted model
#' 
#' @details 
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
#' @export

create.restricted.models <-
  function(ikde.model){
    if (class(ikde.model) != "ikde.model") stop("ikde.model must be of class \"ikde.model\".")
    
    
    
    return(ikde.model)
  }
