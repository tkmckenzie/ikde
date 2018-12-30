#' Modified exponential function with lower bound
#' 
#' @param x Point to evaluate modified exponential function and gradient
#' @param lower.limit Lower limit of the modified exponential function; defaults to 0
#' 
#' @details 
#' Evaluates the modified exponential function (exp(x) + lower.bound) and its gradient (exp(x)).
#' 
#' @return Returns a list with two elements, each with the same number of elements as x
#' \item{value}{Value of the modified exponential function at x}
#' \item{gradient}{Value of the gradient of the modified exponential function at x}
#' 
#' @examples
#' x <- seq(-5, 5, length.out = 100)
#' exp.result <- modified.exp.lower(x)
#' 
#' plot(exp.result$value ~ x, type = "l")
#' plot(exp.result$gradient ~ x, type = "l")
#' 
#' @export

modified.exp.lower <-
  function(x, lower.limit = 0){
    return(list(value = exp(x) + lower.limit,
                gradient = exp(x)))
  }
