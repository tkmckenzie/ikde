#' Evaluate sampling statement from Stan program
#' 
#' @param statement A string containing a sampling statement
#' @param ikde.model An object of class ikde.model, which has been built
#' @param eval.point A list of parameter names and the point to evaluate the statement
#' 
#' @return A real number indicating value of the log-density of the statement at the evaluation point
#' 
#' @details 
#' Parses the given sampling statement and evaluates it at the specified
#' evaluation point. The ikde.model object and eval.point object are needed
#' to resolve variable values in the statement.
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
#' statement <- ikde.model$model$likelihood[1]
#' eval.point <- list(beta = c(1, 2, 3, 4), sigma_sq = 5)
#' 
#' # These results match:
#' evaluate.statement(statement, ikde.model, eval.point)
#' sum(dnorm(y, mean = X %*% eval.point$beta, sd = sqrt(eval.point$sigma_sq), log = TRUE))
#' # [1] -4178.641
#'   
#' @export

evaluate.statement <-
  function(statement, ikde.model, eval.point){
    if (class(statement) != "character") stop("statement must be a string.")
    if (length(statement) > 1) stop("statement must only contain one element.")
    if (class(ikde.model) != "ikde.model") stop("ikde.model must be of class \"ikde.model\".")
    if (class(eval.point) != "list") stop("eval.point must be a list.")
    
    #Clean statement and extract left- and right-hand sides
    statement <- gsub(" ", "", statement)
    lhs <- strsplit(statement, "~")[[1]][1]
    rhs <- strsplit(statement, "~")[[1]][2]
    
    #Resolve variables in lhs
    for (data.var in names(ikde.model$data)){
      lhs <- gsub(data.var, paste0("ikde.model$data$", data.var, "[[2]]"), lhs)
    }
    for (eval.var in names(eval.point)){
      lhs <- gsub(eval.var, paste0("eval.point$", eval.var), lhs)
    }
    
    #Extract distribution and map to R function
    distribution.stan <- gsub("\\([0-9A-Za-z\\.,\\*/\\+-\\^_\\(\\)]+\\)$", "", rhs)
    
    if (!(distribution.stan %in% names(stan.dist.to.r.dist))) stop(paste0(distribution.stan, " distribution not currently supported."))
    distribution.r <- stan.dist.to.r.dist[[distribution.stan]]$distribution.r
    
    arg.values <- strsplit(gsub("\\)$", "", gsub("^\\(", "", gsub(distribution.stan, "", rhs))), ",")[[1]]
    arg.names = stan.dist.to.r.dist[[distribution.stan]]$args
    
    #Resolve variables in distribution arguments
    for (data.var in names(ikde.model$data)){
      arg.values <- gsub(data.var, paste0("ikde.model$data$", data.var, "[[2]]"), arg.values)
    }
    for (eval.var in names(eval.point)){
      arg.values <- gsub(eval.var, paste0("eval.point$", eval.var), arg.values)
    }
    
    #Evaluate distribution arguments
    args <- lapply(arg.values, evaluate.expression, ikde.model = ikde.model, eval.point = eval.point)
    names(args) <- arg.names
    
    #Evaluate lhs
    args$x <- evaluate.expression(lhs, ikde.model = ikde.model, eval.point = eval.point)
    
    #Additional arguments to distribution.r
    args$log = TRUE
    
    #Evaluate sampling statement
    return(sum(eval(parse(text = paste0("do.call(", distribution.r, ", args = args)")))))
  }
