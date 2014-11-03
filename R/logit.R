#' The logit and inverse logit functions
#' @aliases ilogit
#' @param x The number to be transformed
#' @return For \code{logit}, log(x/(1 - x)); for \code{ilogit}, 1/(1 + exp(-x))
#' @export logit ilogit
logit <- function(x) log(x/(1-x))
ilogit <- function(x) 1 / (1 + exp(-x))
