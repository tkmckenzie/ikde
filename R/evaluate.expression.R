#' Evaluate expression from Stan program
#' 
#' @param stan.expression String representing Stan expression. All variables must be passed in ... .
#' @param ... Any variables present in the parent environment that are needed to evaluate stan.expression
#' 
#' @return The result of the Stan expression
#' 
#' @details
#' First, all variables specified in ... are loaded into the function environment. Then, all
#' multipliction is replaced by %stan*% and the resulting expression is evaluated with eval.
#' 
#' @examples
#' X <- matrix(1:9, nrow = 3)
#' b <- c(4, 5, 6)
#' 
#' stan.expression <- "(3 + 2) * X * (5 * b)"
#' 
#' # These results match:
#' evaluate.expression(stan.expression)
#' print((3 + 2) * X %*% (5 * b))
#' #      [,1]
#' # [1,] 1650
#' # [2,] 2025
#' # [3,] 2400
#'
#' @export

evaluate.expression <-
  function(stan.expression, ...){
    if (class(stan.expression) != "character") stop("expression must be a string.")
    if (length(stan.expression) != 1) stop("expression must be a single string.")
    
    dots <- list(...)
    list2env(dots, environment())
    
    r.expression <- stan.expression
    #Replace Stan operators with R operators
    for (stan.operator in names(stan.operator.to.r.operator)){
      r.expression <- gsub(stan.operator, stan.operator.to.r.operator[[stan.operator]], r.expression, perl = TRUE)
    }
    
    #Evaluate expression and return value
    return(eval(parse(text = r.expression)))
  }
