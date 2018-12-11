#' Define Stan model
#' 
#' Defines Stan model, creates model code, and stores input data
#' 
#' @param ikde.model An object of class ikde.model which has been built
#' 
#' @return An object of S4 class stanfit. See rstan::stan for more details.
#' 
#' @details 
#' 
#' @examples
#'   
#' @references
#' 
#' @export

fit.model <-
  function(ikde.model, burn.iter = 1000, sample.iter = 1000, chains = 1, control = NULL, refresh = NULL, display.output = FALSE){
    if (class(ikde.model) != "ikde.model") stop("ikde.model must be of class \"ikde.model\".")
    if (!ikde.model$built) stop("ikde.model must be built before fitting.")
    
    if (is.null(refresh)) refresh <- floor((burn.iter + sample.iter) / 100)
    
    if (!display.output) sink(tempfile())
    stan.fit = stan(fit = ikde.model$stan.dso, data = ikde.model$stan.data,
                    chains = chains, warmup = burn.iter, iter = burn.iter + sample.iter,
                    control = control, refresh = refresh)
    if (!display.output) sink()
    
    return(stan.fit)
  }
