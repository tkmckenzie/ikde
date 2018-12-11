#' Define Stan model
#' 
#' Defines Stan model, creates model code, and stores input data
#' 
#' @param ikde.model An object of class ikde.model which has been built
#' @param eval.point A list of parameter names and the point to evaluate priors
#' 
#' @return A real number indicating value of the log-prior at the evaluation point
#' 
#' @details 
#' Parses sampling statements in ikde.model$model$priors and evaluates them at the specified
#' evaluation point.
#' 
#' @examples
#' data(lm.generated)
#' 
#' X <- lm.generated$X
#' y <- lm.generated$y
#' 
#' data <- list(N = list("int<lower=1>", nrow(X)),
#'              k = list("int<lower=1>", ncol(X)),
#'              X = list("matrix[N, k]", X),
#'              y = list("vector[N]", y))
#' parameters <- list(beta = "vector[k]",
#'                    sigma = "real<lower=0>")
#' model <- list(priors = c("beta ~ normal(0, 10)",
#'                          "sigma ~ inv_gamma(1, 1)"),
#'               likelihood = c("y ~ normal(X * beta, sigma)"))
#' 
#' ikde.model <- define.model(data, parameters, model)
#' ikde.model <- build.model(ikde.model)
#' stan.fit <- fit.model(ikde.model)
#' stan.extract <- extract(stan.fit)
#' 
#' print(apply(stan.extract$beta, 2, mean))
#'   
#' @export

evaluate.priors <-
  function(ikde.model, eval.point){
    if (class(ikde.model) != "ikde.model") stop("ikde.model must be of class \"ikde.model\".")
    if (!ikde.model$built) stop("ikde.model must be built before fitting.")
    if (class(eval.point) != "list") stop("eval.point must be a list.")
    
    if (is.null(refresh)) refresh <- floor((burn.iter + sample.iter) / 100)
    
    if (!display.output) sink(tempfile())
    stan.fit <- stan(fit = ikde.model$stan.dso, data = ikde.model$stan.data,
                     chains = chains, warmup = burn.iter, iter = burn.iter + sample.iter,
                     control = control, refresh = refresh)
    if (!display.output) sink()
    
    return(stan.fit)
  }
