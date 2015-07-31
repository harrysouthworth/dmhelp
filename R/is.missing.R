#' Check for missing and potentially missing values
#' 
#' @param x A vector
#' @details The function takes the absolute value of the input if it is numeric, and
#'   it converts it to character. It then searches for NA, "." and empty strings. It
#'   also searches for "999" (which could have been 999 or -999) and prints a warning
#'   to screen if it finds any.
#' @export
is.missing <- function(x){
  if (is.numeric(x)) x <- abs(x)
  x <- as.character(x)
  res <- is.na(x) | x == "" | x == "."
  if (any(na.omit(x) == "999")) warning("999 or -999 found. Not assuming it means missing")
  res
}
