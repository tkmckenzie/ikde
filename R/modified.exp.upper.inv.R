#' Modified exponential function with upper bound
#' 
#' @param x Point to evaluate modified exponential function and gradient
#' @param upper.limit Lower limit of the modified exponential function; defaults to 0
#' 
#' @details 
#' Evaluates the inverse modified exponential function (log(upper.bound - x)) and its gradient (-1 / (upper.bound - x)).
#' 
#' @return Returns a list with two elements, each with the same number of elements as x
#' \item{value}{Value of the modified exponential function at x}
#' \item{gradient}{Value of the gradient of the modified exponential function at x}
#' 
#' @examples
#' x <- seq(-5, -0.005, length.out = 100)
#' exp.result <- modified.exp.upper.inv(x)
#' 
#' plot(exp.result$value ~ x, type = "l")
#' plot(exp.result$gradient ~ x, type = "l")
#' 
#' @export

modified.exp.upper.inv <-
  function(x, upper.limit = 0){
    return(list(value = log(upper.limit - x),
                gradient = -1 / (upper.limit - x)))
  }
