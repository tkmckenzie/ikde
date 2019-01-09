#' Bridge sampling
#' 
#' Evaluates marginal likelihood of Stan model at the posterior mean using bridge sampling
#' 
#' @param ikde.model An object of class ikde.model, does not necessarily have to be built
#' @param burn.iter Number of warmup iterations
#' @param sample.iter Number of sampling iterations
#' @param control Control parameters used in the Markov chain. See ?rstan::stan for details.
#' @param refresh How frequently should progress be reported, in numbers of iterations
#' @param display.output Boolean indicating whether output from rstan::stan should be printed
#' @param show.trace Boolean indicating whether to show trace plots
#' 
#' @return A real number indicating value of the log-marginal-likelihood  at the posterior mean
#' 
#' @details 
#' Uses evaluate.likelihood, evaluate.priors, and a custom bridge sampling routine to estimate
#' marginal likelihood.
#' 
#' @examples
#' \donttest{
#' data(lm.generated)
#' 
#' X <- lm.generated$X
#' y <- lm.generated$y
#' 
#' data <- list(N = list(type = "int<lower=1>", dim = 1, value = nrow(X)),
#'              k = list(type = "int<lower=1>", dim = 1, value = ncol(X)),
#'              X = list(type = "matrix", dim = "[N, k]", value = X),
#'              y = list(type = "vector", dim = "[N]", value = y))
#' parameters <- list(beta = list(type = "vector", dim = "[k]"),
#'                    sigma_sq = list(type = "real<lower=0>", dim = 1))
#' model <- list(priors = c("beta ~ normal(0, 10);",
#'                          "sigma_sq ~ inv_gamma(1, 1);"),
#'               likelihood = c("y ~ normal(X * beta, sqrt(sigma_sq));"))
#' 
#' ikde.model <- define.model(data, parameters, model)
#' 
#' # Only an estimation, may not exactly match presented result
#' bridge(ikde.model)
#' # [1] -389.0141
#' }
#'
#' @export

bridge <-
  function(ikde.model, burn.iter = 1000, sample.iter = 1000, tol = 1e-8, control = NULL, refresh = NULL, display.output = FALSE, show.trace = FALSE){
    if (class(ikde.model) != "ikde.model") stop("ikde.model must be of class \"ikde.model\".")
    if (!ikde.model$built) ikde.model <- build.model(ikde.model)
    
    #First fit the model
    stan.fit <- fit.model(ikde.model, burn.iter, sample.iter, 1, control, refresh, display.output)
    stan.extract <- rstan::extract(stan.fit)
    num.parameters <- length(stan.extract) - 1
    
    #Make stan.extract into list to make it easier to work with later
    #stan.extract.list is a list with parameter names, each containing a list where each element is a sample
    as.list.samples <- function(parameter.samples){
      if (length(dim(parameter.samples)) == 1){
        return(as.list(parameter.samples))
      } else if (length(dim(parameter.samples)) == 2){
        return(split(parameter.samples, c(row(parameter.samples))))
      } else{
        stop("ikde currently only supports 0- and 1-dimensional parameters.")
      }
    }
    stan.extract.list <- lapply(stan.extract, as.list.samples)
    stan.extract.list$lp__ <- NULL
    
    # eval.point <- list()
    # for (parameter.name in names(stan.extract)[-(num.parameters + 1)]){
    #   if (length(dim(stan.extract[[parameter.name]])) == 1){
    #     eval.point[[parameter.name]] <- mean(stan.extract[[parameter.name]])
    #   } else if (length(dim(stan.extract[[parameter.name]])) == 2){
    #     eval.point[[parameter.name]] <- apply(stan.extract[[parameter.name]], 2, mean)
    #   } else{
    #     stop("ikde currently only supports 0- and 1-dimensional parameters.")
    #   }
    # }
    
    #Form proposal distributions
    proposal.functions <- list()
    for (parameter.num in 1:num.parameters){
      parameter <- names(ikde.model$parameters)[parameter.num]
      parameter.type <- ikde.model$parameters[[parameter]]$type
      parameter.type <- gsub(" ", "", parameter.type)
      parameter.restriction.pos <- gregexpr("<[0-9A-Za-z\\.,\\*/\\+\\-\\^_=]+>", parameter.type)[[1]]
      parameter.restriction <- substr(parameter.type, as.numeric(parameter.restriction.pos), as.numeric(parameter.restriction.pos) + attr(parameter.restriction.pos, "match.length") - 1)
      
      if (grepl("lower=", parameter.restriction)){
        lower.limit.pos <- gregexpr("(?<=lower=)[0-9A-Za-z\\.\\*/\\+\\-\\^_]+(?=[,>]+)", parameter.restriction, perl = TRUE)[[1]]
        lower.limit <- substr(parameter.restriction, as.numeric(lower.limit.pos), as.numeric(lower.limit.pos) + attr(lower.limit.pos, "match.length") - 1)
        for (data.var in names(ikde.model$data)){
          regex <- paste0("(?<![0-9A-Za-z\\.\\$_]{1})", data.var, "(?![0-9A-Za-z\\.\\$_]{1})")
          lower.limit <- gsub(regex, paste0("ikde.model$data$", data.var, "[[2]]"), lower.limit, perl = TRUE)
        }
        lower.limit <- evaluate.expression(lower.limit, ikde.model = ikde.model)
        
        if (grepl("upper=", parameter.restriction)){
          #Upper and lower restrictions
          upper.limit.pos <- gregexpr("(?<=upper=)[0-9A-Za-z\\.\\*/\\+\\-\\^_]+(?=[,>]+)", parameter.restriction, perl = TRUE)[[1]]
          upper.limit <- substr(parameter.restriction, as.numeric(upper.limit.pos), as.numeric(upper.limit.pos) + attr(upper.limit.pos, "match.length") - 1)
          for (data.var in names(ikde.model$data)){
            regex <- paste0("(?<![0-9A-Za-z\\.\\$_]{1})", data.var, "(?![0-9A-Za-z\\.\\$_]{1})")
            upper.limit <- gsub(regex, paste0("ikde.model$data$", data.var, "[[2]]"), upper.limit, perl = TRUE)
          }
          upper.limit <- evaluate.expression(upper.limit, ikde.model = ikde.model)
          
          #Use sigmoid function to transform samples from a MVN
          #Use delta method to derive variance of MVN
          #Let Y be constrained variable such that y = f(X), where X ~ MVN(mu, Sigma).
          #We are able to estimate E[Y] and var(Y). We want to find mu and Sigma so that
          #  * E[y] = f(mu)
          #  * var(y) = t(f'(mu)) * Sigma * f'(mu)
          if (length(dim(stan.extract[[parameter]])) == 1){
            mu <- sigmoid.inv(mean(stan.extract[[parameter]]), lower.limit = lower.limit, upper.limit = upper.limit)$value
            var.y <- var(stan.extract[[parameter]])
            grad.diag <- sigmoid(mu, lower.limit = lower.limit, upper.limit = upper.limit)$gradient
            grad.inv <- 1 / grad.diag
            Sigma <- var.y * grad.inv^2
            
            sampler <- function(n, mu, Sigma, lower.limit, upper.limit) lapply(1:n, function(i) sigmoid(rnorm(1, mean = mu, sd = sqrt(Sigma)), lower.limit = lower.limit, upper.limit = upper.limit)$value)
            log.density <- function(x, mu, Sigma, lower.limit, upper.limit) log(sigmoid.inv(x, lower.limit = lower.limit, upper.limit = upper.limit)$gradient) + dnorm(sigmoid.inv(x, lower.limit = lower.limit, upper.limit = upper.limit)$value, mean = mu, sd = sqrt(Sigma), log = TRUE)
            proposal.functions[[parameter]] <- list(sampler = sampler, log.density = log.density, mu = mu, Sigma = Sigma, lower.limit = lower.limit, upper.limit = upper.limit)
          } else if (length(dim(stan.extract[[parameter]])) == 2){
            mu <- sigmoid.inv(apply(stan.extract[[parameter]], 2, mean), lower.limit = lower.limit, upper.limit = upper.limit)$value
            var.y <- var(stan.extract[[parameter]])
            grad.diag <- sigmoid(mu, lower.limit = lower.limit, upper.limit = upper.limit)$gradient
            grad.inv <- diag(1 / grad.diag)
            Sigma <- grad.inv %*% var.y %*% grad.inv
            
            sampler <- function(n, mu, Sigma, lower.limit, upper.limit) lapply(1:n, function(i) sigmoid(mvtnorm::rmvnorm(1, mean = mu, sigma = Sigma), lower.limit = lower.limit, upper.limit = upper.limit)$value)
            log.density <- function(x, mu, Sigma, lower.limit, upper.limit) sum(log(sigmoid.inv(x, lower.limit = lower.limit, upper.limit = upper.limit)$gradient)) + mvtnorm::dmvnorm(sigmoid.inv(x, lower.limit = lower.limit, upper.limit = upper.limit)$value, mean = mu, sigma = Sigma, log = TRUE)
            proposal.functions[[parameter]] <- list(sampler = sampler, log.density = log.density, mu = mu, Sigma = Sigma, lower.limit = lower.limit, upper.limit = upper.limit)
          } else{
            stop("ikde currently only supports 0- and 1-dimensional parameters.")
          }
        } else{
          #Lower restriction only
          if (length(dim(stan.extract[[parameter]])) == 1){
            mu <- modified.exp.lower.inv(mean(stan.extract[[parameter]]), lower.limit = lower.limit)$value
            var.y <- var(stan.extract[[parameter]])
            grad.diag <- modified.exp.lower(mu, lower.limit = lower.limit)$gradient
            grad.inv <- 1 / grad.diag
            Sigma <- var.y * grad.inv^2
            
            sampler <- function(n, mu, Sigma, lower.limit, upper.limit) lapply(1:n, function(i) modified.exp.lower(rnorm(1, mean = mu, sd = sqrt(Sigma)), lower.limit = lower.limit)$value)
            log.density <- function(x, mu, Sigma, lower.limit, upper.limit) log(modified.exp.lower.inv(x, lower.limit = lower.limit)$gradient) + dnorm(modified.exp.lower.inv(x, lower.limit = lower.limit)$value, mean = mu, sd = sqrt(Sigma), log = TRUE)
            proposal.functions[[parameter]] <- list(sampler = sampler, log.density = log.density, mu = mu, Sigma = Sigma, lower.limit = lower.limit, upper.limit = NA)
          } else if (length(dim(stan.extract[[parameter]])) == 2){
            mu <- modified.exp.lower.inv(apply(stan.extract[[parameter]], 2, mean), lower.limit = lower.limit)$value
            var.y <- var(stan.extract[[parameter]])
            grad.diag <- modified.exp.lower(mu, lower.limit = lower.limit)$gradient
            grad.inv <- diag(1 / grad.diag)
            Sigma <- grad.inv %*% var.y %*% grad.inv
            
            sampler <- function(n, mu, Sigma, lower.limit, upper.limit) lapply(1:n, function(i) modified.exp.lower(mvtnorm::rmvnorm(1, mean = mu, sigma = Sigma), lower.limit = lower.limit)$value)
            log.density <- function(x, mu, Sigma, lower.limit, upper.limit) sum(log(modified.exp.lower.inv(x, lower.limit = lower.limit)$gradient)) + mvtnorm::dmvnorm(modified.exp.lower.inv(x, lower.limit = lower.limit)$value, mean = mu, sigma = Sigma, log = TRUE)
            proposal.functions[[parameter]] <- list(sampler = sampler, log.density = log.density, mu = mu, Sigma = Sigma, lower.limit = lower.limit, upper.limit = NA)
          } else{
            stop("ikde currently only supports 0- and 1-dimensional parameters.")
          }
        }
      } else if (grepl("upper=", parameter.restriction)){
        #Upper restriction only
        upper.limit.pos <- gregexpr("(?<=upper=)[0-9A-Za-z\\.\\*/\\+\\-\\^_]+(?=[,>]+)", parameter.restriction, perl = TRUE)[[1]]
        upper.limit <- substr(parameter.restriction, as.numeric(upper.limit.pos), as.numeric(upper.limit.pos) + attr(upper.limit.pos, "match.length") - 1)
        for (data.var in names(ikde.model$data)){
          regex <- paste0("(?<![0-9A-Za-z\\.\\$_]{1})", data.var, "(?![0-9A-Za-z\\.\\$_]{1})")
          upper.limit <- gsub(regex, paste0("ikde.model$data$", data.var, "[[2]]"), upper.limit, perl = TRUE)
        }
        upper.limit <- evaluate.expression(upper.limit, ikde.model = ikde.model)
        
        if (length(dim(stan.extract[[parameter]])) == 1){
          mu <- modified.exp.upper.inv(mean(stan.extract[[parameter]]), upper.limit = upper.limit)$value
          var.y <- var(stan.extract[[parameter]])
          grad.diag <- modified.exp.upper(mu, upper.limit = upper.limit)$gradient
          grad.inv <- 1 / grad.diag
          Sigma <- var.y * grad.inv^2
          
          sampler <- function(n, mu, Sigma, lower.limit, upper.limit) lapply(1:n, function(i) modified.exp.upper(rnorm(1, mean = mu, sd = sqrt(Sigma)), upper.limit = upper.limit)$value)
          log.density <- function(x, mu, Sigma, lower.limit, upper.limit) log(modified.exp.upper.inv(x, upper.limit = upper.limit)$gradient) + dnorm(modified.exp.upper.inv(x, upper.limit = upper.limit)$value, mean = mu, sd = sqrt(Sigma), log = TRUE)
          proposal.functions[[parameter]] <- list(sampler = sampler, log.density = log.density, mu = mu, Sigma = Sigma, lower.limit = NA, upper.limit = upper.limit)
        } else if (length(dim(stan.extract[[parameter]])) == 2){
          mu <- modified.exp.upper.inv(apply(stan.extract[[parameter]], 2, mean), upper.limit = upper.limit)$value
          var.y <- var(stan.extract[[parameter]])
          grad.diag <- modified.exp.upper(mu, upper.limit = upper.limit)$gradient
          grad.inv <- diag(1 / grad.diag)
          Sigma <- grad.inv %*% var.y %*% grad.inv
          
          sampler <- function(n, mu, Sigma, lower.limit, upper.limit) lapply(1:n, function(i) modified.exp.upper(mvtnorm::rmvnorm(1, mean = mu, sigma = Sigma), upper.limit = upper.limit)$value)
          log.density <- function(x, mu, Sigma, lower.limit, upper.limit) sum(log(modified.exp.upper.inv(x, upper.limit = upper.limit)$gradient)) + mvtnorm::dmvnorm(modified.exp.upper.inv(x, upper.limit = upper.limit)$value, mean = mu, sigma = Sigma, log = TRUE)
          proposal.functions[[parameter]] <- list(sampler = sampler, log.density = log.density, mu = mu, Sigma = Sigma, lower.limit = NA, upper.limit = upper.limit)
        } else{
          stop("ikde currently only supports 0- and 1-dimensional parameters.")
        }
      } else{
        #No restrictions
        if (length(dim(stan.extract[[parameter]])) == 1){
          mu <- mean(stan.extract[[parameter]])
          Sigma <- var(stan.extract[[parameter]])
          
          sampler <- function(n, mu, Sigma, lower.limit, upper.limit) lapply(1:n, function(i) rnorm(1, mean = mu, sd = sqrt(Sigma)))
          log.density <- function(x, mu, Sigma, lower.limit, upper.limit) dnorm(x, mean = mu, sd = sqrt(Sigma), log = TRUE)
          proposal.functions[[parameter]] <- list(sampler = sampler, log.density = log.density, mu = mu, Sigma = Sigma, lower.limit = NA, upper.limit = NA)
        } else if (length(dim(stan.extract[[parameter]])) == 2){
          mu <- apply(stan.extract[[parameter]], 2, mean)
          Sigma <- var(stan.extract[[parameter]])
          
          sampler <- function(n, mu, Sigma, lower.limit, upper.limit) lapply(1:n, function(i) mvtnorm::rmvnorm(1, mean = mu, sigma = Sigma))
          log.density <- function(x, mu, Sigma, lower.limit, upper.limit) mvtnorm::dmvnorm(x, mean = mu, sigma = Sigma, log = TRUE)
          proposal.functions[[parameter]] <- list(sampler = sampler, log.density = log.density, mu = mu, Sigma = Sigma, lower.limit = NA, upper.limit = NA)
        } else{
          stop("ikde currently only supports 0- and 1-dimensional parameters.")
        }
      }
    }
    
    #Draw samples from proposal distributions
    proposal.samples <- list()
    log.proposal.proposal.matrix <- matrix(NA, nrow = sample.iter, ncol = num.parameters)
    log.proposal.posterior.matrix <- matrix(NA, nrow = sample.iter, ncol = num.parameters)
    for (parameter.num in 1:num.parameters){
      parameter <- names(ikde.model$parameters)[parameter.num]
      
      sampler <- proposal.functions[[parameter]]$sampler
      mu <- proposal.functions[[parameter]]$mu
      Sigma <- proposal.functions[[parameter]]$Sigma
      lower.limit <- proposal.functions[[parameter]]$lower.limit
      upper.limit <- proposal.functions[[parameter]]$upper.limit
      
      proposal.samples[[parameter]] <- proposal.functions[[parameter]]$sampler(sample.iter, mu, Sigma, lower.limit, upper.limit)
      log.proposal.proposal.matrix[,parameter.num] <- sapply(proposal.samples[[parameter]], proposal.functions[[parameter]]$log.density, mu = mu, Sigma = Sigma, lower.limit = lower.limit, upper.limit = upper.limit)
      log.proposal.posterior.matrix[,parameter.num] <- sapply(stan.extract.list[[parameter]], proposal.functions[[parameter]]$log.density, mu = mu, Sigma = Sigma, lower.limit = lower.limit, upper.limit = upper.limit)
      # if (length(dim(stan.extract[[parameter]])) == 1){
      #   log.proposal.posterior.matrix[,parameter.num] <- sapply(stan.extract[[parameter]], proposal.functions[[parameter]]$log.density, mu = mu, Sigma = Sigma, lower.limit = lower.limit, upper.limit = upper.limit)
      # } else if (length(dim(stan.extract[[parameter]])) == 2){
      #   log.proposal.posterior.matrix[,parameter.num] <- sapply(1:sample.iter, function(i) proposal.functions[[parameter]]$log.density(stan.extract[[parameter]][i,], mu, Sigma, lower.limit, upper.limit))
      # } else{
      #   stop("ikde currently only supports 0- and 1-dimensional parameters.")
      # }
    }
    log.proposal.proposal <- rowSums(log.proposal.proposal.matrix)
    log.proposal.posterior <- rowSums(log.proposal.posterior.matrix)
    
    #Calculate constant densities (not depending on log-marginal)
    log.likelihood.proposal <- rep(NA, sample.iter)
    log.prior.proposal <- rep(NA, sample.iter)
    for (i in 1:sample.iter){
      eval.point <- lapply(proposal.samples, function(sample.list, i) c(sample.list[[i]]), i)
      log.likelihood.proposal[i] <- evaluate.likelihood(ikde.model, eval.point)
      log.prior.proposal[i] <- evaluate.priors(ikde.model, eval.point)
    }
    
    log.likelihood.posterior <- rep(NA, sample.iter)
    log.prior.posterior <- rep(NA, sample.iter)
    for (i in 1:sample.iter){
      eval.point <- lapply(stan.extract.list, function(sample.list, i) c(sample.list[[i]]), i)
      log.likelihood.posterior[i] <- evaluate.likelihood(ikde.model, eval.point)
      log.prior.posterior[i] <- evaluate.priors(ikde.model, eval.point)
    }
    
    #Now define functions for densities depending on log-marginal
    log.h.proposal <- function(log.marginal){
      #Assuming same number of samples from posterior and proposal
      return(-log(0.5 * exp(log.likelihood.proposal + log.prior.proposal) + 0.5 * exp(log.marginal + log.proposal.proposal)))
    }
    log.h.posterior <- function(log.marginal){
      #Assuming same number of samples from posterior and proposal
      return(-log(0.5 * exp(log.likelihood.posterior + log.prior.posterior) + 0.5 * exp(log.marginal + log.proposal.posterior)))
    }
    
    #Define update function for log-marginal
    log.marginal.update <- function(log.marginal){
      #Assuming same number of samples from posterior and proposal
      return(log(sum(exp(log.likelihood.proposal + log.prior.proposal + log.h.proposal(log.marginal))) /
                   sum(exp(log.h.posterior(log.marginal) + log.proposal.posterior))))
    }
    
    log.marginal <- 0
    current.difference <- Inf
    while (current.difference > tol){
      log.marginal.new <- log.marginal.update(log.marginal)
      current.difference <- abs(log.marginal.new - log.marginal)
      log.marginal <- log.marginal.new
    }
    
    return(log.marginal)
  }
