#' Function to replicate multiplication in Stan
#' 
#' @param x First term in product
#' @param y Second term in product
#' 
#' @details 
#' Accepts arguments x and y. If either is a singleton, returns the value of x*y (in R notation).
#' If both arguments are matrices or vectors, returns x%*%y (in R notation).
#' 
#' @return Returns an object of the same type as the base
#' 
#' @examples
#' X <- matrix(1:9, nrow = 3)
#' b <- c(4, 5, 6)
#' 
#' (3 + 2) * X %stan*% (5 * b)
#' #      [,1]
#' # [1,] 1650
#' # [2,] 2025
#' # [3,] 2400
#' 
#' @export
#' @rdname stan.multiply

`%stan*%` <-
  function(x, y){
    #Stan function defined for x^y, for
    #  real x, real y (x * y)
    #  real x, vector y (x * y)
    #  vector x, row_vector y (x %*% y)
    #  matrix x, vector y (x %*% y)
    
    if ((length(x) == 1) | (length(y) == 1)){
      return(x * y)
    } else{
      return(x %*% y)
    }
  }
