#' Drop columns that contain only missing values
#'
#' @param x A data.frame
#' @export dropNAs
dropNAs <- function(x, missing = NA){
  fun <- function(X){
    all(is.na(X))
  }
  res <- sapply(x, fun)
  x[, !res]
}
