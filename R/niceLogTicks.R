#' Get nice tick mark locations on a log scale that includes 1 as representing no change
#' 
#' @param x Numeric vector which has been obtained as the ratio of two numbers
#' @param pad The proportion of padding to use. Defaults to 0.05 and the axis
#'   is assumed to be extended by 5% of the range of the data in each direction
#' @param base Number giving the base of the logarithms. Defaults to \code{base = 2}
#' @export niceLogTicks
niceLogTicks <- function(x, pad=0.05, base=2){
  # Create vector of nice locations if the maximum is < 2
  upperTickLT <- c(1.9, 1.8, 5/3, 1.5, 1.4, 1.3, 1.2, 1.1, 1.05, 1.01, 1.005, 1.001)

  x <- na.omit(x)

  # Get padding on the log scale. This to be added at each end of the scale
  pad <- diff(range(log(x, base))) * pad
  
  # Make values < 1 same scale as those >= 1
  x[x < 1] <- 1/x[x < 1]
  
  # Get upper limit of the scale
  m <- exp(log(max(x)) + pad)

  # Get location for upper tick mark
  if (m >= 2){ # Want max integer that is a multiple of 2
    zm <- floor(sqrt(m))
  } else { # Choose from a list of probably nice values
    zm <- max(upperTickLT[upperTickLT < m])
  }

  # We now have the upper tick and know there will be a tick at 1. Find other ticks
  # as located half way between 1 and zm
  ll <- c(1/zm, 2/(zm + 1), 1, (zm + 1)/2, zm)
  res <- cbind(locatiions=logb(ll, base), labels=ll)
  res
}
