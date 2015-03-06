#' Crosstabulate 2 factors and include column totals
#' @aliases print.crosstab xtable.crosstab
#' @param x The first factor, defining rows.
#' @param y The second factor, defining columns.
#' @param prop Whether to compute column proportions or not. Defaults to \code{prop=FALSE}.
#' @details If using \code{xtable}, to print, use \code{print(x, hline.after=c(-1, 0, nrow(x)-1, nrow(x)))}
#'   where \code{x} is the result of \code{xtable(crosstab(x, y))}.
#' @export crosstab
crosstab <- function(x, y, prop=FALSE){
  res <- table(x, y)
  
  # Add column totals
  res <- rbind(res, colSums(res))
  rownames(res)[nrow(res)] <- "Total"
  
  if (prop) res <- t(t(res) / res[nrow(res), ])
  
  class(res) <- "crosstab"
  res
}

#' @method print crosstab
#' @export print.crosstab
print.crosstab <- function(x, ...) print(unclass(x), ...)

#' @export xtable.crosstab
xtable.crosstab <- function(x, caption = NULL, label = NULL, align = NULL, digits = NULL, 
                            display = NULL, ...){
  xtable:::xtable.matrix(unclass(x), caption=caption, label=label, align=align, digits=digits, display=display, ...)
}
