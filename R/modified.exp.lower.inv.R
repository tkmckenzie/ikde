#' Modified exponential function with lower bound
#' 
#' @param x Point to evaluate modified exponential function and gradient
#' @param lower.limit Lower limit of the modified exponential function; defaults to 0
#' 
#' @details 
#' Evaluates the inverse modified exponential function (log(x - lower.bound)) and its gradient (1 / (x - lower.bound)).
#' 
#' @return Returns a list with two elements, each with the same number of elements as x
#' \item{value}{Value of the inverse modified exponential function at x}
#' \item{gradient}{Value of the gradient of the inverse modified exponential function at x}
#' 
#' @examples
#' x <- seq(0.005, 5, length.out = 100)
#' exp.result <- modified.exp.lower.inv(x)
#' 
#' plot(exp.result$value ~ x, type = "l")
#' plot(exp.result$gradient ~ x, type = "l")
#' 
#' @export

modified.exp.lower.inv <-
  function(x, lower.limit = 0){
    return(list(value = log(x - lower.limit),
                gradient = 1 / (x - lower.limit)))
  }
