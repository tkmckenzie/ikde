# ikde

# Overview

Estimation of model marginal likelihoods for Bayesian model selection using iterative
kernel density estimation. A multitude of methods exist for performing model selection in
general and estimating marginal likelihood in specific, but none are partically well-suited to
large models (such as Gaussian processes) applied to relatively limited datasets. Methods are
provided to specific and construct Stan models, estimate those models, and estimate the
marginal likelihood of those models.