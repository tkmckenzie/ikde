#' Linear model Gibbs sampling
#' 
#' Fits a linear model using Gibbs sampling and estimates marginal likelihood as in Chib (1995)
#' 
#' @param X Matrix of input variables
#' @param y Vector of output variables
#' @param priors A named list of parameter priors; should include beta.prior.mean (vector), beta.prior.var (matrix), sigma.sq.prior.shape (scalar), and sigma.sq.prior.rate (scalar)
#' @param burn.iter Number of warmup iterations
#' @param sample.iter Number of sampling iterations
#' 
#' @return Returns an list with the following elements
#' \item{samples}{Named list of samples from the posterior, with elements "beta" and "sigma.sq"}
#' \item{log.marginal}{Estimate of the model's log-marginal-likelihood}
#' \item{priors}{List of priors used for the model}
#' 
#' @details 
#' Uses a standard formulation of a linear model from which a Gibbs sampler can be derived. 
#' Specifically, for a model of the form
#' 
#' \deqn{\beta\sim N(\mu_\beta, \Sigma_\beta)}
#' \deqn{\sigma^2\sim \Gamma(s_\sigma, r_\sigma)}
#' \deqn{y = X\beta + \varepsilon}
#' \deqn{\varepsilon\sim N\left(0, \frac{1}{\sqrt{\sigma^2}} I\right),}
#' 
#' Gibbs sampling can be performed using the conditional distributions
#' 
#' \deqn{\beta|\sigma^2, X, y\sim N(\tilde\mu_\beta, \tilde\Sigma_\beta)}
#' \deqn{\sigma^2|\beta, X, y\sim \Gamma^{-1}\left(\frac{N}2 + s_\sigma, \frac{e'e}2 + r_\sigma\right),}
#' 
#' where \eqn{N} is the number of observations and
#' 
#' \deqn{\tilde\Sigma_\beta = \frac{X'X}{\sigma^2} + \Sigma_\beta^{-1}}
#' \deqn{\tilde\mu_\beta = \tilde\Sigma_\beta \left(\frac{X'y}{\sigma^2} + \Sigma_\beta^{-1}\mu_\beta\right)}
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
#'                                     sigma.sq.prior.shape = 1,
#'                                     sigma.sq.prior.rate = 1))
#' 
#' print(apply(gibbs.fit$samples$beta, 2, mean)) # [1] 3.181184 1.643960 4.480879 1.213804
#' print(mean(gibbs.fit$samples$sigma.sq)) # [1] 97.52314
#' print(gibbs.fit$log.marginal) # [1] -389.001
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
    
    if (!("sigma.sq.prior.shape" %in% names(priors))){
      priors$sigma.sq.prior.shape <- 1
    } else{
      if (priors$sigma.sq.prior.shape <= 0) stop("sigma.sq.prior.shape must be positive.")
    }
    
    if (!("sigma.sq.prior.rate" %in% names(priors))){
      priors$sigma.sq.prior.rate <- 1
    } else{
      if (priors$sigma.sq.prior.rate <= 0) stop("sigma.sq.prior.rate must be positive.")
    }
    
    # Initial values
    beta <- rep(0, k)
    sigma.sq <- 1
    
    # Parameters for sampling
    Sigma.beta.inv <- solve(priors$beta.prior.var)
    Sigma.beta.inv.mu.beta <- Sigma.beta.inv %*% priors$beta.prior.mean
    X.X <- t(X) %*% X
    X.y <- t(X) %*% y 
    
    # Warmup
    for (i in 1:burn.iter){
      #beta
      Sigma.beta.inv.post <- X.X / sigma.sq + Sigma.beta.inv
      mu.beta.post <- solve(Sigma.beta.inv.post, (X.y / sigma.sq + Sigma.beta.inv.mu.beta))
      beta <- mvtnorm::rmvnorm(1, mu.beta.post, solve(Sigma.beta.inv.post))[1,]
      
      #sigma.sq
      e <- y - X %*% beta
      sigma.sq <- invgamma::rinvgamma(1, priors$sigma.sq.prior.shape + N / 2, sum(e^2) / 2 + priors$sigma.sq.prior.rate)
    }
    
    # Sampling
    beta.samples <- matrix(NA, nrow = sample.iter, ncol = k)
    sigma.sq.samples <- rep(NA, sample.iter)
    for (i in 1:sample.iter){
      #beta
      Sigma.beta.inv.post <- X.X / sigma.sq + Sigma.beta.inv
      mu.beta.post <- solve(Sigma.beta.inv.post, (X.y / sigma.sq + Sigma.beta.inv.mu.beta))
      beta <- mvtnorm::rmvnorm(1, mu.beta.post, solve(Sigma.beta.inv.post))[1,]
      
      #sigma.sq
      e <- y - X %*% beta
      sigma.sq <- invgamma::rinvgamma(1, priors$sigma.sq.prior.shape + N / 2, sum(e^2) / 2 + priors$sigma.sq.prior.rate)
      
      #Store values
      beta.samples[i,] <- beta
      sigma.sq.samples[i] <- sigma.sq
    }
    
    # Marginal likelihood estimation
    beta.star <- apply(beta.samples, 2, mean)
    sigma.sq.star <- mean(sigma.sq.samples)
    sigma.sq.posterior <- rep(NA, sample.iter)
    #Start with density of sigma.sq|beta[s]
    for (i in 1:sample.iter){
      e <- y - X %*% beta.samples[i,]
      sigma.sq.posterior[i] <- invgamma::dinvgamma(sigma.sq.star, priors$sigma.sq.prior.shape + N / 2, sum(e^2) / 2 + priors$sigma.sq.prior.rate)
    }
    #Now do density of beta|sigma.sq*
    Sigma.beta.inv.star <- X.X / sigma.sq.star + Sigma.beta.inv
    mu.beta.star <- solve(Sigma.beta.inv.star, (X.y / sigma.sq.star + Sigma.beta.inv.mu.beta))
    log.beta.posterior <- mvtnorm::dmvnorm(beta.star, mu.beta.star, solve(Sigma.beta.inv.star), log = TRUE)
    #Wrap everything together
    log.posterior <- log(mean(sigma.sq.posterior)) + log.beta.posterior
    log.prior <- mvtnorm::dmvnorm(beta.star, priors$beta.prior.mean, priors$beta.prior.var, log = TRUE) +
      invgamma::dinvgamma(sigma.sq.star, priors$sigma.sq.prior.shape, priors$sigma.sq.prior.rate, log = TRUE)
    log.lik <- sum(stats::dnorm(y, X %*% beta.star, sqrt(sigma.sq.star), log = TRUE))
    log.marginal <- log.lik + log.prior - log.posterior
    
    out <- list(samples = list(beta = beta.samples, sigma.sq = sigma.sq.samples),
                log.prior = log.prior,
                log.lik = log.lik,
                log.posterior = log.posterior,
                log.marginal = log.marginal,
                priors = priors)
    return(out)
  }
