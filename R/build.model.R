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
      stan.code <- paste0(stan.code, "\t", data[[data.key]][[1]], " ", data.key, ";\n")
      stan.data[[data.key]] <- data[[data.key]][[2]]
    }
    stan.code <- paste0(stan.code, "}\n")
    
    #Transformed data block
    if (length(transformed.data) > 0){
      stan.code <- paste0(stan.code, "transformed data{\n")
      code.block <- ""
      for (transformed.data.key in names(transformed.data)){
        stan.code <- paste0(stan.code, "\t", transformed.data[[transformed.data.key]][[1]], " ", transformed.data.key, ";\n")
        code.block <- paste0(code.block, "\t", transformed.data[[transformed.data.key]][[2]], ";\n")
      }
      stan.code <- paste0(stan.code, "\n", code.block)
      stan.code <- paste0(stan.code, "}\n")
    }
    
    #Parameters block
    stan.code <- paste0(stan.code, "parameters{\n")
    for (parameter.key in names(parameters)){
      stan.code <- paste0(stan.code, "\t", parameters[[parameter.key]], " ", parameter.key, ";\n")
    }
    stan.code <- paste0(stan.code, "}\n")
    
    #Transformed parameters block
    if (length(transformed.parameters) > 0){
      stan.code <- paste0(stan.code, "transformed parameters{\n")
      code.block <- ""
      for (transformed.parameters.key in names(transformed.parameters)){
        stan.code <- paste0(stan.code, "\t", transformed.parameters[[transformed.parameters.key]][[1]], " ", transformed.parameters.key, ";\n")
        code.block <- paste0(code.block, "\t", transformed.parameters[[transformed.parameters.key]][[2]], ";\n")
      }
      stan.code <- paste0(stan.code, "\n", code.block)
      stan.code <- paste0(stan.code, "}\n")
    }
    
    #Model block
    stan.code <- paste0(stan.code, "model{\n")
    for (prior.code.line in model[["priors"]]){
      stan.code <- paste0(stan.code, "\t", prior.code.line, ";\n")
    }
    stan.code <- paste0(stan.code, "\n")
    for (likelihood.code.line in model[["likelihood"]]){
      stan.code <- paste0(stan.code, "\t", likelihood.code.line, ";\n")
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
