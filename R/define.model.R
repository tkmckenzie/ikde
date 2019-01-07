#' Define Stan model
#' 
#' Defines Stan model and stores input data
#' 
#' @param data A list of data passed to the Stan program. Should be of the form list(data.name = list(type = string, dim = number/string (e.g., "[N]"), value = data.object)).
#' @param parameters A list of parameters used in the Stan program. Should be of the form list(parameter.name = list(type = string, dim = number/string)).
#' @param model A list describing the Stan model. Should be a list with components "priors" and "likelihood".
#' @param transformed.data A list describing data transformations for the Stan program to perform. Should be of the form list(variable.name = list(type = string, dim = number/string, expression = string)).
#' @param transformed.parameters A list describing parameter transformations for the Stan program to perform. Should be of the form list(variable.name = list(type = string, dim = string, expression = string)).
#' 
#' @return Returns an ikde.model object with the following elements
#' \item{data}{A list of data passed to the Stan program}
#' \item{transformed.data}{A list describing data transformations for the Stan program to perform}
#' \item{parameters}{A list of parameters used in the Stan program}
#' \item{transformed.parameters}{A list describing parameter transformations for the Stan program to perform}
#' \item{model}{A list describing the Stan model}
#' \item{stan.code}{Stan code for the model}
#' \item{stan.data}{Data passed to Stan for estimation}
#' \item{stan.dso}{DSO for Stan model, allows Stan to run model without recompilation}
#' \item{built}{Boolean indicating whether the model has been built}
#' \item{density.variable}{List containing two elements: "name" of the variable on which density estimation should be performed on, and "value" indicating the point at which density should be estimated}
#' 
#' @details 
#' Defines inputs to be used for building and eventually fitting Stan model.
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
#'                    sigma_sq = "real<lower=0>")
#' model <- list(priors = c("beta ~ normal(0, 10)",
#'                          "sigma_sq ~ inv_gamma(1, 1)"),
#'               likelihood = c("y ~ normal(X * beta, sqrt(sigma_sq))"))
#' 
#' ikde.model <- define.model(data, parameters, model)
#' 
#' @export

define.model <-
  function(data, parameters, model, transformed.data = list(), transformed.parameters = list()){
    if (class(data) != "list") stop("data must be a list of lists.")
    if (class(parameters) != "list") stop("parameters must be a list.")
    if (class(model) != "list") stop("model must be a list.")
    if (class(transformed.data) != "list") stop("transformed.data must be a list of lists.")
    if (class(transformed.parameters) != "list") stop("transformed.parameters must be a list.")
    
    if (any(sort(names(model)) != c("likelihood", "priors"))) stop("model must be a list with components \"priors\" and \"likelihood\".")
    
    ikde.model <- list()
    ikde.model$data <- data
    ikde.model$transformed.data <- transformed.data
    ikde.model$parameters <- parameters
    ikde.model$transformed.parameters <- transformed.parameters
    ikde.model$model <- model
    
    ikde.model$stan.code <- ""
    ikde.model$stan.data <- list()
    ikde.model$stan.dso <- NA
    ikde.model$built <- FALSE
    ikde.model$density.variable <- list()
    
    class(ikde.model) <- "ikde.model"
    
    return(ikde.model)
  }
