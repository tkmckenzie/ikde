#' Linear model Gibbs sampling
#' 
#' Fits a linear model using Gibbs sampling and estimates marginal likelihood as in Chib (1995)
#' 
#' @param X Matrix of input variables
#' @param y Vector of output variables
#' @param priors A named list of parameter priors; should include beta.prior.mean (vector), beta.prior.var (matrix), tau.prior.shape (scalar), and tau.prior.rate (scalar)
#' @param burn.iter Number of warmup iterations
#' @param sample.iter Number of sampling iterations
#' 
#' @return Returns an list with the following elements
#' \item{samples}{Named list of samples from the posterior, with elements "beta" and "tau"}
#' \item{log.marginal}{Estimate of the model's log-marginal-likelihood}
#' \item{priors}{List of priors used for the model}
#' 
#' @details 
#' Uses a standard formulation of a linear model from which a Gibbs sampler can be derived. 
#' Specifically, for a model of the form
#' 
#' \deqn{\beta\sim N(\mu_\beta, \Sigma_\beta)}
#' \deqn{\tau\sim \Gamma(s_\tau, r_\tau)}
#' \deqn{y = X\beta + \varepsilon}
#' \deqn{\varepsilon\sim N\left(0, \frac{1}{\sqrt{\tau}} I\right),}
#' 
#' Gibbs sampling can be performed using the conditional distributions
#' 
#' \deqn{\beta|\tau, X, y\sim N(\tilde\mu_\beta, \tilde\Sigma_\beta)}
#' \deqn{\tau|\beta, X, y\sim \Gamma^{-1}\left(\frac{N}2 + s_\tau, \frac{e'e}2 + r_\tau\right),}
#' 
#' where \eqn{N} is the number of observations and
#' 
#' \deqn{\tilde\Sigma_\beta = \tau X'X + \Sigma_\beta^{-1}}
#' \deqn{\tilde\mu_\beta = \tilde\Sigma_\beta \left(\tau X'y + \Sigma_\beta^{-1}\mu_\beta)}
#' \deqn{e = y - X\beta.}
#' 
#' @examples
#' data(lm.generated)
#' 
#' X <- lm.generated$X
#' y <- lm.generated$y
#' 
#' gibbs.fit <- gibbs.lm(X, y,
#'                       priors = list(beta.prior.mean = rep(0, 4),
#'                                     beta.prior.var = 100 * diag(4),
#'                                     tau.prior.shape = 1,
#'                                     tau.prior.rate = 1))
#' 
#' print(apply(gibbs.fit$samples$beta, 2, mean))
#' print(mean(gibbs.fit$samples$tau))
#' print(gibbs.fit$log.marginal)
#' 
#' 
#' 
#' priors = list(beta.prior.mean = rep(0, 4),
#'               beta.prior.var = 100 * diag(4),
#'               tau.prior.shape = 1,
#'               tau.prior.rate = 1)
#' burn.iter = 1000
#' sample.iter = 1000
#' 
#' @export

gibbs.lm <-
  function(X, y, priors = list(), burn.iter = 1000, sample.iter = 1000){
    if (class(X) != "matrix") stop("X must be a matrix.")
    if (class(y) != "numeric") stop("y must be a vector.")
    if (class(priors) != "list") stop("priors must be a list.")
    if (nrow(X) != length(y)) stop("X and y must have the same number of observations.")
    
    N <- nrow(X)
    k <- ncol(X)
    
    if (!("beta.prior.mean" %in% names(priors))){
      priors$beta.prior.mean <- rep(0, k)
    } else{
      if (length(priors$beta.prior.mean) != k) stop("beta.prior.mean has incorrect dimensions.")
    }
    
    if (!("beta.prior.var" %in% names(priors))){
      priors$beta.prior.var <- diag(k)
    } else{
      if (any(dim(priors$beta.prior.var) != c(k, k))) stop("beta.prior.var has incorrect dimensions.")
      if (any(priors$beta.prior.var != t(priors$beta.prior.var))) stop("beta.prior.var must be symmetric.")
      if (any(eigen(priors$beta.prior.var)$values <= 0)) stop("beta.prior.var must be positive definite.")
    }
    
    if (!("tau.prior.shape" %in% names(priors))){
      priors$tau.prior.shape <- 1
    } else{
      if (priors$tau.prior.shape <= 0) stop("tau.prior.shape must be positive.")
    }
    
    if (!("tau.prior.rate" %in% names(priors))){
      priors$tau.prior.rate <- 1
    } else{
      if (priors$tau.prior.rate <= 0) stop("tau.prior.rate must be positive.")
    }
    
    # Initial values
    beta <- rep(0, k)
    tau <- 1
    
    # Parameters for sampling
    Sigma.beta.inv <- solve(priors$beta.prior.var)
    Sigma.beta.inv.mu.beta <- Sigma.beta.inv %*% priors$beta.prior.mean
    X.X <- t(X) %*% X
    X.y <- t(X) %*% y 
    
    # Warmup
    for (i in 1:burn.iter){
      #beta
      Sigma.beta.inv.post <- tau * X.X + Sigma.beta.inv
      mu.beta.post <- solve(Sigma.beta.inv.post, (tau * X.y + Sigma.beta.inv.mu.beta))
      beta <- mvtnorm::rmvnorm(1, mu.beta.post, solve(Sigma.beta.inv.post))[1,]
      
      #tau
      e <- y - X %*% beta
      tau <- rgamma(1, priors$tau.prior.shape + N / 2, sum(e^2) / 2 + priors$tau.prior.rate)
    }
    
    # Sampling
    beta.samples <- matrix(NA, nrow = sample.iter, ncol = k)
    tau.samples <- rep(NA, sample.iter)
    for (i in 1:sample.iter){
      #beta
      Sigma.beta.inv.post <- tau * X.X + Sigma.beta.inv
      mu.beta.post <- solve(Sigma.beta.inv.post, (tau * X.y + Sigma.beta.inv.mu.beta))
      beta <- mvtnorm::rmvnorm(1, mu.beta.post, solve(Sigma.beta.inv.post))[1,]
      
      #tau
      e <- y - X %*% beta
      tau <- rgamma(1, priors$tau.prior.shape + N / 2, sum(e^2) / 2 + priors$tau.prior.rate)
      
      #Store values
      beta.samples[i,] <- beta
      tau.samples[i] <- tau
    }
    
    # Marginal likelihood estimation
    beta.star <- apply(beta.samples, 2, mean)
    tau.star <- mean(tau.samples)
    tau.posterior <- rep(NA, sample.iter)
    #Start with density of tau|beta[s]
    for (i in 1:sample.iter){
      e <- y - X %*% beta.samples[i,]
      tau.posterior[i] <- dgamma(tau.star, priors$tau.prior.shape + N / 2, sum(e^2) / 2 + priors$tau.prior.rate)
    }
    #Now do density of beta|tau*
    Sigma.beta.inv.star <- tau.star * X.X + Sigma.beta.inv
    mu.beta.star <- solve(Sigma.beta.inv.star, (tau.star * X.y + Sigma.beta.inv.mu.beta))
    log.beta.posterior <- mvtnorm::dmvnorm(beta.star, mu.beta.star, solve(Sigma.beta.inv.star), log = TRUE)
    #Wrap everything together
    log.posterior <- log(mean(tau.posterior)) + log.beta.posterior
    log.prior <- mvtnorm::dmvnorm(beta.star, priors$beta.prior.mean, priors$beta.prior.var, log = TRUE) +
      dgamma(tau.star, priors$tau.prior.shape, priors$tau.prior.rate, log = TRUE)
    log.lik <- sum(dnorm(y, X %*% beta.star, 1 / sqrt(tau.star), log = TRUE))
    log.marginal <- log.lik + log.prior - log.posterior
    
    out <- list(samples = list(beta = beta.samples, tau = tau.samples),
                log.marginal = log.marginal,
                priors = priors)
    return(out)
  }
