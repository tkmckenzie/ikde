#' Build Stan model
#' 
#' Builds and compiles a defined Stan model
#' 
#' @param ikde.model An object of class ikde.model, e.g., from define.model
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
#' Builds Stan model using defined ikde.model, then compiles the model and stores DSO
#' for fast running.
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
#' 
#' cat(ikde.model$stan.code)
#' }
#' 
#' @export

build.model <-
  function(ikde.model){
    if (class(ikde.model) != "ikde.model") stop("ikde.model must be of class \"ikde.model\".")
    
    data <- ikde.model$data
    transformed.data <- ikde.model$transformed.data
    parameters <- ikde.model$parameters
    transformed.parameters <- ikde.model$transformed.parameters
    model <- ikde.model$model
    
    #Data block
    stan.code <- "data{\n"
    stan.data = list()
    for (data.key in names(data)){
      if (class(data[[data.key]]) != "list") stop(paste0("data[[", data.key, "]] is not a list."))
      if (any(sort(names(data[[data.key]])) != c("dim", "type", "value"))) stop(paste0("data[[", data.key, "]] must only contain elements type, dim, and value."))
      stan.code <- paste0(stan.code, "\t", create.declaration(data.key, data[[data.key]]$type, data[[data.key]]$dim), "\n")
      stan.data[[data.key]] <- data[[data.key]]$value
    }
    stan.code <- paste0(stan.code, "}\n")
    
    #Transformed data block
    if (length(transformed.data) > 0){
      stan.code <- paste0(stan.code, "transformed data{\n")
      code.block <- ""
      for (transformed.data.key in names(transformed.data)){
        if (class(transformed.data[[transformed.data.key]]) != "list") stop(paste0("transformed.data[[", transformed.data.key, "]] is not a list."))
        if (any(sort(names(transformed.data[[transformed.data.key]])) != c("dim", "expression", "type"))) stop(paste0("transformed.data[[", transformed.data.key, "]] must only contain elements type, dim, and expression."))
        stan.code <- paste0(stan.code, "\t", create.declaration(transformed.data.key, transformed.data[[transformed.data.key]]$type, transformed.data[[transformed.data.key]]$dim), "\n")
        code.block <- paste0(code.block, "\t", transformed.data[[transformed.data.key]]$expression, "\n")
      }
      stan.code <- paste0(stan.code, "\n", code.block)
      stan.code <- paste0(stan.code, "}\n")
    }
    
    #Parameters block
    stan.code <- paste0(stan.code, "parameters{\n")
    for (parameter.key in names(parameters)){
      if (class(parameters[[parameter.key]]) != "list") stop(paste0("parameters[[", parameter.key, "]] is not a list."))
      if (any(sort(names(parameters[[parameter.key]])) != c("dim", "type"))) stop(paste0("parameters[[", parameter.key, "]] must only contain elements type and dim."))
      stan.code <- paste0(stan.code, "\t", create.declaration(parameter.key, parameters[[parameter.key]]$type, parameters[[parameter.key]]$dim), "\n")
    }
    stan.code <- paste0(stan.code, "}\n")
    
    #Transformed parameters block
    if (length(transformed.parameters) > 0){
      stan.code <- paste0(stan.code, "transformed parameters{\n")
      code.block <- ""
      for (transformed.parameters.key in names(transformed.parameters)){
        if (class(transformed.parameters[[transformed.parameters.key]]) != "list") stop(paste0("transformed.parameters[[", transformed.parameters.key, "]] is not a list."))
        if (any(sort(names(transformed.parameters[[transformed.parameters.key]])) != c("dim", "expression", "type"))) stop(paste0("transformed.parameters[[", transformed.parameters.key, "]] must only contain elements type, dim, and expression."))
        stan.code <- paste0(stan.code, "\t", create.declaration(transformed.parameters.key, transformed.parameters[[transformed.parameters.key]]$type, transformed.parameters[[transformed.parameters.key]]$dim), "\n")
        code.block <- paste0(code.block, "\t", transformed.parameters[[transformed.parameters.key]]$expression, "\n")
      }
      stan.code <- paste0(stan.code, "\n", code.block)
      stan.code <- paste0(stan.code, "}\n")
    }
    
    #Model block
    stan.code <- paste0(stan.code, "model{\n")
    if (any(sort(names(model)) != c("likelihood", "priors"))) stop("model must only contain elements priors and likelihood.")
    for (prior.code.line in model[["priors"]]){
      stan.code <- paste0(stan.code, "\t", prior.code.line, "\n")
    }
    stan.code <- paste0(stan.code, "\n")
    for (likelihood.code.line in model[["likelihood"]]){
      stan.code <- paste0(stan.code, "\t", likelihood.code.line, "\n")
    }
    stan.code <- paste0(stan.code, "}\n")
    
    #Fit model and save dso
    print("Compiling Stan program.")
    sink(tempfile())
    stan.dso <- rstan::stan(model_code = stan.code, data = stan.data,
                            chains = 1, warmup = 1, iter = 1)
    sink()
    
    #Package ikde.model
    ikde.model$stan.code <- stan.code
    ikde.model$stan.data <- stan.data
    ikde.model$stan.dso <- stan.dso
    ikde.model$built <- TRUE
    
    return(ikde.model)
  }
