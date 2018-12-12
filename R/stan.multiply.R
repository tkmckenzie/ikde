#' Function to replicate multiplication in Stan
#' 
#' @param x First term in product
#' @param y Second term in product
#' 
#' @return Returns an object of the same type as the base
#' 
#' 
#' @examples
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
