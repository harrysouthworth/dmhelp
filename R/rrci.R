#' Compute relative risk with approximate confidence intervals
#'
#' @param x The number of success in the first group
#' @param nx The number of observations in the first group
#' @param y The number of success in the second group
#' @param ny The number of observations in the second group
#' @param alpha Determines the confidence level. Defaults to \code{alpha=c(0.5, 0.1)}
#'   and both 90\% and 50\% intervals are produced.
#' @param method Character string giving the method. Allowable options are
#'   \code{method="MLE"} and \code{method="Jeffreys"}.(the default). It isn't case-sensitive
#' @details If \code{method="Jeffreys"} (the default), the function adds 1/2 to
#'   the numerator and 1 to the denominator when estimating the
#'   proporions of successes in each group, thus avoiding infinite values. Then,
#'   estimate the standard error of the log relative risks, compute the confidence
#'   intervals, and transform back to the original scale.
#' @export
rrci <- function(x, nx, y, ny, alpha=.050, method="Jeffreys"){
  method <- casefold(method)

    z <- qnorm(1 - alpha/2)

  if (method == "jeffreys"){
    px <- (x + .50) / (nx + 1)
    py <- (y + .50) / (ny + 1)
    sel <- sqrt((1/(x + .50) + 1/(y + .50)) - (1/(nx + 1) + 1/(ny + 1)))
  } else if (method == "mle"){
    px <- x / nx
    py <- y / ny
    sel <- sqrt((1/x + 1/y) - (1/nx + 1/ny))

  } else {
    stop("method can be either 'Jeffreys' or 'MLE'")
  }

  lrr <- log(px) - log(py)

  res <- c(RR=lrr, Lower=lrr - z * sel, Upper = lrr + z * sel)
  exp(res)
}

