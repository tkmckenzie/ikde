#' Inverse sigmoid function and gradient
#' 
#' @param x Point to evaluate inverse sigmoid function and gradient
#' @param lower.limit Lower limit of the sigmoid function; defaults to 0
#' @param upper.limit Upper limit of the sigmoid function; defaults to 1
#' 
#' @details 
#' Evaluates the inverse sigmoid function (-log((upper.bound - lower.bound) / (x - lower.bound) - 1)) and its gradient ((upper.bound - lower.bound) / ((lower.bound - x) * (x - upper.bound))).
#' 
#' @return Returns a list with two elements, each with the same number of elements as x
#' \item{value}{Value of the inverse sigmoid function at x}
#' \item{gradient}{Value of the gradient of the inverse sigmoid function at x}
#' 
#' @examples
#' x <- seq(0.005, 0.995, length.out = 100)
#' sigmoid.inv.result <- sigmoid.inv(x)
#' 
#' plot(sigmoid.inv.result$value ~ x, type = "l")
#' plot(sigmoid.inv.result$gradient ~ x, type = "l")
#' 
#' @export

sigmoid.inv <-
  function(x, lower.limit = 0, upper.limit = 1){
    interval.range <- upper.limit - lower.limit
    if (any(x <= 0 | x >= 1)) stop("x must be between 0 and 1.")
    return(list(value = -log(interval.range / (x - lower.limit) - 1),
                gradient = (interval.range / ((x - upper.limit) * (lower.limit - x)))))
  }
