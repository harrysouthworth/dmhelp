#' Get columns with a large proportion of missing values
#' @export
#' @param data A \code{data.frame}
#' @param th The threshold proportion of NAs to detect. Defaults to 0.2
#' @return A logical vector indicating which columns have lots of NAs
hiNAs <- function(data, th=.2){
  sapply(data, function(x) mean(is.na(x)) > th)
}

#' Find variables with a large proportion of missing values
#' @export
#' @param data A \code{data.frame}
#' @param th The threshold above which the proportion of NAs is to be flagged. Defaults to \code{th=0.2}
#' @return A character matrix with the names of the variables and the percentage of missing values.
summarizeNAs <- function(data, th=.2){
  nas <- hiNAs(data, th)
  miss <- cbind(names(nas)[nas], apply(data[, nas], 2,
                                       function(x) paste0(signif(mean(is.na(x))*100, 3), "%")))
  colnames(miss) <- c("Variable", "Missing")
  miss[miss[, 2] != "100%", ]
}

#' Missing patter plot
#' @param data data.frame or matrix of data with missing data coded as "NA".
#' @param xlab a title for the x axis: see 'title'
#' @param ylab a title for the y axis: see 'title'
#' @param main an overall title fo the plot: see 'title'
#' @details A simple wrapper for \code{mi:::missing.pattern.plot} with some default
#'   argument values changed.
#' @export missplot
#' @importFrom mi mp.plot
missplot <- function(data, xlab="Index", ylab="Variable", main="",
                     mar=c(5.1, 8.1, 4.1, 2.1), ...){
  oldpar <- par(no.readonly=TRUE)
  on.exit(oldpar)
  par(mar=mar)
  mp.plot(data, y.order=TRUE, x.order=TRUE, mis.col="orange", xlab=xlab, ylab=ylab, main=main)
}
