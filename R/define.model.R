#' Define Stan model
#' 
#' Defines Stan model, creates model code, and stores input data
#' 
#' @param data A list of data passed to the Stan program. Should be of the form list(data.name = list(data.type, data.object)).
#' @param parameters A list of parameters used in the Stan program. Should be of the form list(parameter.name = parameter.type).
#' @param model A list describing the Stan model. Should be a list with components "priors" and "likelihood".
#' @param transformed.data A list describing data transformations for the Stan program to perform. Should be of the form list(variable.name = list(variable.type, variable.expression)).
#' @param transformed.parameters A list describing parameter transformations for the Stan program to perform. Should be of the form list(variable.name = list(variable.type, variable.expression)).
#' 
#' @return Returns a list with the following elements
#' \item{y.fit}{Estimated value of the frontier at X.fit}
#' 
#' @details 
#' 
#' @examples
#'   
#' @references
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
    
    class(ikde.model) <- "ikde.model"
    
    return(ikde.model)
  }
