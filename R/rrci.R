#' Compute relative risk with approximate confidence intervals
#' 
#' @param x The number of success in the first group
#' @param nx The number of observations in the first group
#' @param y The number of success in the second group
#' @param ny The number of observations in the second group
#' @param alpha Determines the confidence level. Defaults to \code{alpha=c(0.5, 0.1)}
#'   and both 90\% and 50\% intervals are produced.
#' @details Adds 1/2 to the numerator and 1 to the denominator when estimating the
#'   proporions of successes in each group, thus avoiding infinite values. Then,
#'   estimate the standard error of the log relative risks, compute the confidence
#'   intervals, and transform back to the original scale.
#' @export
rrci <- function(x, nx, y, ny, alpha=c(.5, .1)){
  z <- sort(qnorm(c(alpha/2, .50, 1 - alpha/2)))

  px <- (x + .50) / (nx + 1)
  py <- (y + .50) / (ny + 1)

  lrr <- log(px) - log(py)

  sel <- sqrt((1/(x + .50) + 1/(y + .50)) - (1/(nx + 1) + 1/(ny + 1)))

  res <- lrr + z * sel
  exp(res)
}

