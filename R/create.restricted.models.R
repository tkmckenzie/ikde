#' Creates restricted models for IKDE
#' 
#' Creates set of restricted models to be used for posterior density estimation
#' 
#' @param ikde.model An object of class ikde.model, does not necessarily have to be built
#' 
#' @return Returns a list of built ikde.models for each restricted model
#' 
#' @details 
#' Posterior density can be estimated by breaking the multi-dimensional density into
#' one-dimensional components. This method creates restricted models from which conditional
#' densities can be estimated. Each real parameter and each entry of vector parameters are
#' restricted one at a time, with values restricted at the specified point.
#' 
#' @examples
#' \dontrun{
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
#' eval.point <- list(beta = c(1, 2, 3, 4),
#'                    sigma = 5)
#' 
#' ikde.model.list <- create.restricted.models(ikde.model, eval.point)
#' for (restricted.ikde.model in ikde.model.list){
#'   cat(restricted.ikde.model$stan.code)
#'   cat("--------------------------------------------------\n")
#' }
#' }
#' 
#' @export

create.restricted.models <-
  function(ikde.model, eval.point){
    if (class(ikde.model) != "ikde.model") stop("ikde.model must be of class \"ikde.model\".")
    
    current.ikde.model <- ikde.model
    
    #First build unrestricted model if it hasn't been already
    if (!current.ikde.model$built) current.ikde.model <- build.model(current.ikde.model)
    current.ikde.model$density.variable <- list(name = names(current.ikde.model$parameters)[1])
    current.ikde.model$density.variable$value <- eval.point[[current.ikde.model$density.variable$name]][1]
    
    model.list <- list(current.ikde.model)
    num.parameters <- length(ikde.model$parameters)
    for (parameter.index in 1:num.parameters){
      parameter <- names(ikde.model$parameters)[parameter.index]
      parameter.type <- ikde.model$parameters[[parameter]]
      parameter.type <- gsub(" ", "", parameter.type)
      parameter.restriction.pos <- gregexpr("<[0-9A-Za-z\\.,\\*/\\+-\\^_=]+>", parameter.type)[[1]]
      parameter.restriction <- substr(parameter.type, as.numeric(parameter.restriction.pos), as.numeric(parameter.restriction.pos) + attr(parameter.restriction.pos, "match.length") - 1)
      if (grepl("vector", parameter.type)){
        #Only need to build two models: One with partially restricted vector and one with fully restricted vector (if this is not the last parameter in the model)
        vector.length.pos <- gregexpr("(?<=vector\\[)[0-9A-Za-z\\.,\\*/\\+-\\^_]+(?=\\])", parameter.type, perl = TRUE)[[1]]
        vector.length <- substr(parameter.type, as.numeric(vector.length.pos), as.numeric(vector.length.pos) + attr(vector.length.pos, "match.length") - 1)
        vector.length.eval <- vector.length
        for (data.var in names(ikde.model$data)){
          vector.length.eval <- gsub(data.var, paste0("ikde.model$data$", data.var, "[[2]]"), vector.length.eval)
        }
        vector.length.eval <- evaluate.expression(vector.length.eval, ikde.model = ikde.model, eval.point = eval.point)
        
        #Create names for restricted/unrestricted parameters in Stan code
        parameter.restr <- paste0(parameter, "_restr")
        parameter.restr.all <- paste0(parameter.restr, "_all")
        parameter.unrestr <- paste0(parameter, "_unrestr")
        
        #Create and build partially restricted model
        partial.ikde.model <- current.ikde.model
        partial.ikde.model$parameters[[parameter]] <- NULL #Remove from parameters list
        partial.ikde.model$data$num_restrictions <- list(paste0("int<lower=1,upper=", vector.length, "-1>"), 1) #Can change number of restrictions in ML estimation
        partial.ikde.model$data[[parameter.restr.all]] <- list(paste0("vector", parameter.restriction, "[", vector.length, "]"), as.array(eval.point[[parameter]])) #Add restricted values to data
        partial.ikde.model$transformed.data[[parameter.restr]] <- list(paste0("vector", parameter.restriction, "[num_restrictions]"), paste0(parameter.restr, " = head(", parameter.restr.all, ", num_restrictions)"))
        partial.ikde.model$parameters[[parameter.unrestr]] <- paste0("vector", parameter.restriction, "[", vector.length, "-num_restrictions]") #Add unrestricted values to parameters
        partial.ikde.model$transformed.parameters <- append(eval(parse(text = paste0("list(", parameter, " = list(\"vector", parameter.restriction, "[", vector.length, "]\", \"", parameter, " = append_row(", parameter.restr, ", ", parameter.unrestr, ")\"))"))), partial.ikde.model$transformed.parameters)
        for (statement.num in 1:length(partial.ikde.model$model$priors)){
          statement <- partial.ikde.model$model$priors[statement.num]
          lhs <- gsub(" ", "", strsplit(statement, "~")[[1]][1])
          rhs <- strsplit(statement, "~")[[1]][2]
          
          if (lhs == parameter){
            statement <- paste0(parameter.unrestr, " ~", rhs)
            partial.ikde.model$model$priors[statement.num] <- statement
          }
        }
        partial.ikde.model$density.variable <- list(name = parameter.unrestr)
        partial.ikde.model$density.variable$value <- eval.point[[parameter]][2]
        partial.ikde.model <- build.model(partial.ikde.model)
        model.list <- append(model.list, list(partial.ikde.model))
        
        if (vector.length.eval > 2){
          for (vector.index in 2:(vector.length.eval - 1)){
            partial.ikde.model$data$num_restrictions[[2]] <- vector.index #Only data is changed, no code, so don't need to re-build
            partial.ikde.model$density.variable$value <- eval.point[[parameter]][vector.index + 1]
            model.list <- append(model.list, list(partial.ikde.model))
          }
        }
        
        #Create and build fully restricted model, if this is not the final parameter
        #Update current.ikde.model for next parameter at the same time
        if (parameter.index < num.parameters){
          current.ikde.model$parameters[[parameter]] <- NULL #Remove from parameters list
          current.ikde.model$data[[parameter]] <- list(parameter.type, eval.point[[parameter]]) #Add parameter to data
          
          prior.rm.index <- c()
          for (statement.num in 1:length(current.ikde.model$model$priors)){
            statement <- current.ikde.model$model$priors[statement.num]
            lhs <- gsub(" ", "", strsplit(statement, "~")[[1]][1])
            rhs <- strsplit(statement, "~")[[1]][2]
            
            if (lhs == parameter){
              prior.rm.index <- c(prior.rm.index, statement.num)
            }
          }
          if (length(prior.rm.index) > 0) current.ikde.model$model$priors <- current.ikde.model$model$priors[-prior.rm.index]
          
          current.ikde.model$density.variable <- list(name = names(ikde.model$parameters)[parameter.index + 1])
          current.ikde.model$density.variable$value <- eval.point[[current.ikde.model$density.variable$name]][1]
          current.ikde.model <- build.model(current.ikde.model)
          model.list <- append(model.list, list(current.ikde.model))
        }
      } else if (grepl("real", parameter.type)){
        if (parameter.index < num.parameters){
          current.ikde.model$parameters[[parameter]] <- NULL #Remove from parameters list
          current.ikde.model$data[[parameter]] <- list(parameter.type, eval.point[[parameter]]) #Add parameter to data
          
          prior.rm.index <- c()
          for (statement.num in 1:length(current.ikde.model$model$priors)){
            statement <- current.ikde.model$model$priors[statement.num]
            lhs <- gsub(" ", "", strsplit(statement, "~")[[1]][1])
            rhs <- strsplit(statement, "~")[[1]][2]
            
            if (lhs == parameter){
              prior.rm.index <- c(prior.rm.index, statement.num)
            }
          }
          if (length(prior.rm.index) > 0) current.ikde.model$model$priors <- current.ikde.model$model$priors[-prior.rm.index]
          
          current.ikde.model$density.variable <- list(name = names(ikde.model$parameters)[parameter.index + 1])
          current.ikde.model$density.variable$value <- eval.point[[current.ikde.model$density.variable$name]][1]
          current.ikde.model <- build.model(current.ikde.model)
          model.list <- append(model.list, list(current.ikde.model))
        }
      } else{
        stop(paste0(parameter.type, " currently not supported by ikde."))
      }
    }
    
    return(model.list)
  }
