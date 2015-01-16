#' Get the numeric columns of a data.frame
#' @export
#' @param data A \code{data.frame}
#' @details Character and factor columns are dropped, and the remainder returned.
numerics <- function(data){
  sapply(data, function(x) !(is.factor(x) | is.character(x) | is.logical(x)))
}

#' Get the binary columns of a data.frame
#' @export
#' @param data A \code{data.frame}
#' @return A logical vector indicating which columns have exactly 2 unique values after dropping NAs
binaries <- function(data){
  sapply(data, function(x) length(unique(na.omit(x))) == 2)
}

#' Get pairs of variables with high absolute correlation
#' @export
#' @param data A \code{data.frame} or similar
#' @param th The threshold for absolute correlation above which we want the pairs of variables
#' @details The function removes all non-numeric and all binomial columns before computing the correlation.
#' @return A \code{data.frame} with 3 columns representing the pairs of variables and their Spearman's rank correlation
hiCor <- function(data, th=.8){
  if (!is.element(class(data)[1], c("data.frame", "matrix", "cast_df")))
    stop("data should be a matrix, data.frame or cast_df")
  
  data <- as.data.frame(data)
  data <- data[, numerics(data)]
  data <- data[, !binaries(data)]
  co1 <- cor(data, method="spearman", use="pairwise.complete.obs")
  co <- co1
  
  # Set diagonal and upper triangle to 0 to stop double counting
  co[upper.tri(co, diag=TRUE)] <- 0
  i <- abs(co) > th
  
  wh <- apply(i, 2, function(x, threshold, rn)
    rn[abs(x) > threshold], th, rn=rownames(co))
  v1 <- rep(names(wh), unlist(lapply(wh, length)))
  v2 <- unlist(wh)
  
  co <- rep(0, length(v1))
  
  for (i in 1:length(co))
    co[i] <- cor(data[, v1[i]], data[, v2[i]], method="spearman", use="pairwise.complete.obs")
  
  res <- data.frame(v1, v2, cor=co, stringsAsFactors=FALSE)
  rownames(res) <- NULL
  colnames(res) <- c("Variable 1", "Variable 2", "Corr.")
  
  res <- res[rev(order(abs(res$Corr.))), ]
  res <- list(correlation=co1, highest=res)
  class(res) <- "hiCor"
  res
}

#' @method plot hiCor
#' @export plot.hiCor
plot.hiCor <- function(x, ...){
    heatmap(x[[1]])
}

#' @method print hiCor
#' @export print.hiCor
print.hiCor <- function(x, ...)
  print(x$highest)
