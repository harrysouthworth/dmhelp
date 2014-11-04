# Function 'borrowed' from package metrumrg
# That package didn't want to install on my system, and I only wanted this function.
# Author: Tim Bergsma
# URL: http://metrumrg.googlecode.com

#' Convert ftable to data.frame
#' @description Convert ftable to data.frame As Displayed
#' @param x ftable
#' @param ... ignored
#' @details \code{as.data.frame} does indeed convert an \code{ftable} to \code{data.frame},
#'          but it gives a stacked result. Use this function to give something more like
#'          what is displayed at the prompt (suitable for passing to latex).
#' @author Tim Bergsma
#' @return data.frame
#' @references http://metrumrg.googlecode.com
#' @examples ftable(mtcars[c("cyl", "vs", "am", "gear")])
#' as.data.frame(ftable(mtcars[c("cyl", "vs", "am", "gear")]))
#' ftable2data.frame(ftable(mtcars[c("cyl", "vs", "am", "gear")]))
#' @export ftable2data.frame
ftable2data.frame <- function(x,...){
  y <- format(x, quote=FALSE)
  z <- data.frame(y[-1,], stringsAsFactors=FALSE)
  names(z) <- y[1,]
  z
}
