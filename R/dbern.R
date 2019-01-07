#' Bernoulli distribution function
#' 
#' @param x Vector of quantiles (either 0 or 1)
#' @param prob Vector of probability of success (in [0, 1] interval)
#' @param log Logical; if TRUE, log probabilities are returned
#' 
#' @details 
#' Evaluates Bernoulli distribution at specified points.
#' 
#' @return Returns a vector of probabilities for each observation
#' 
#' @examples
#' prob <- runif(50)
#' x <- ifelse(runif(50) < prob, 1, 0)
#' 
#' dbern(x, prob)
#' 
#' @export

dbern <-
  function(x, prob, log = FALSE){
    if (!all(x %in% c(0, 1))) stop("Elements in x must be either 0 or 1.")
    if (!all((prob >= 0) & (prob <= 1))) stop("Elements of prob must be in [0, 1] interval.")
    
    prob.result <- ifelse(x == 1, prob, 1 - prob)
    if (log) prob.result <- log(prob.result)
    
    return(prob.result)
  }
