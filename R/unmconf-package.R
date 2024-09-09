#' unmconf: Modeling with Unmeasured Confounding
#'
#' Tools for fitting and assessing Bayesian multilevel regression models that
#' account for unmeasured confounders.
#'
#' @import rjags
#' @importFrom stats rexp rnorm rgamma rpois runif rbinom coef Gamma binomial
#'   poisson gaussian model.frame model.matrix model.offset family
#' @importFrom janitor clean_names
#' @importFrom glue glue
#' @importFrom coda gelman.diag effectiveSize
#' @name unmconf
#' @aliases unmconf unmconf-package
NULL
