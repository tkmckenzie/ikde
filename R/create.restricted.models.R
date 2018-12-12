#' Creates restricted models for IKDE
#' 
#' Creates set of restricted models to be used for posterior density estimation
#' 
#' @param ikde.model An object of class ikde.model, does not necessarily have to be built
#' 
#' @return Returns a list of built ikde.models for each restricted model
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
  function(ikde.model, eval.point){
    if (class(ikde.model) != "ikde.model") stop("ikde.model must be of class \"ikde.model\".")
    
    current.ikde.model <- ikde.model
    model.list <- list(ikde.model)
    num.parameters <- length(ikde.model$parameters)
    for (parameter in names(ikde.model$parameters)[-num.parameters]){
      parameter.type <- ikde.model$parameters[[parameter]]
      parameter.type <- gsub(" ", "", parameter.type)
      if (grepl("vector", parameter.type)){
        #TODO: Get parameter restrictions
        vector.length.pos <- gregexpr("(?<=vector\\[)[0-9A-Za-z\\.,\\*/\\+-\\^]+(?=\\])", parameter.type, perl = TRUE)[[1]]
        vector.length <- substr(parameter.type, as.numeric(vector.length.pos), as.numeric(vector.length.pos) + attr(vector.length.pos, "match.length") - 1)
        for (data.var in names(ikde.model$data)){
          vector.length <- gsub(data.var, paste0("ikde.model$data$", data.var, "[[2]]"), vector.length)
        }
        vector.length <- evaluate.expression(vector.length)
        
        parameter.restr <- paste0(parameter, "_restr")
        parameter.unrestr <- paste0(parameter, "_unrestr")
        
        for (vector.index in 1:vector.length){
          next.ikde.model <- current.ikde.model
          next.ikde.model$parameters[[parameter]] <- NULL #Remove from parameters list
          next.ikde.model$data[[parameter.restr]] <- list(paste0("vector[", vector.index, "]"),
                                                                      eval.point[[parameter]][1:vector.index]) #Add restricted values to data
          next.ikde.model$parameters[[parameter.unrestr]] <- paste0("vector[", vector.length - vector.index, "]") #Add unrestricted values to parameters
          next.ikde.model$transformed.parameters <- append(list())
          
        }
      }
    }
    
    return(model.list)
  }
