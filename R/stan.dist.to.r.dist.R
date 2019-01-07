#' Mapping between Stan and R distribution functions
#' 
#' @details 
#' A list of Stan distributions, associated R distribution functions, and arguments to those
#' functions.
#' 
#' @export

stan.dist.to.r.dist <- list("bernoulli" = list(distribution.r = "ikde::dbern", args = c("prob")),
                            "binomial" = list(distribution.r = "stats::dbinom", args = c("size", "prob")),
                            "poisson" = list(distribution.r = "stats::dpois", args = c("lambda")),
                            "normal" = list(distribution.r = "stats::dnorm", args = c("mean", "sd")),
                            "lognormal" = list(distribution.r = "stats::dlnorm", args = c("meanlog", "sdlog")),
                            "chi_square" = list(distribution.r = "stats::dchisq", args = c("df")),
                            "inv_chi_square" = list(distribution.r = "invgamma:dinvchisq", args = c("df")),
                            "exponential" = list(distribution.r = "stats::dexp", args = c("rate")),
                            "gamma" = list(distribution.r = "stats::dgamma", args = c("shape", "rate")),
                            "inv_gamma" = list(distribution.r = "invgamma::dinvgamma", args = c("shape", "rate")),
                            "beta" = list(distribution.r = "stats::dbeta", args = c("shape1", "shape2")),
                            "uniform" = list(distribution.r = "stats::dunif", args = c("min", "max")),
                            "multi_normal" = list(distribution.r = "mvtnorm::dmvnorm", args = c("mean", "sigma")))
