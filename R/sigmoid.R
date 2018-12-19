#' Sigmoid function and gradient
#' 
#' @param x Point to evaluate sigmoid function and gradient
#' @param lower.limit Lower limit of the sigmoid function; defaults to 0
#' @param upper.limit Upper limit of the sigmoid function; defaults to 1
#' 
#' @details 
#' Evaluates the sigmoid function (1 / (1 + exp(-x))) and its gradient (exp(-x) / (1 + exp(-x))^2).
#' 
#' @return Returns a list with two elements, each with the same number of elements as x
#' \item{value}{Value of the sigmoid function at x}
#' \item{gradient}{Value of the gradient of the sigmoid function at x}
#' 
#' @examples
#' x <- seq(-5, 5, length.out = 100)
#' sigmoid.result <- sigmoid(x)
#' 
#' plot(sigmoid.result$value ~ x, type = "l")
#' plot(sigmoid.result$gradient ~ x, type = "l")
#' 
#' @export
#' @rdname stan.multiply

sigmoid <-
  function(x, lower.limit = 0, upper.limit = 1){
    interval.range <- upper.limit - lower.limit
    return(list(value = interval.range / (1 + exp(-x)) + lower.limit,
                gradient = interval.range * exp(-x) / ((1 + exp(-x))^2)))
  }
