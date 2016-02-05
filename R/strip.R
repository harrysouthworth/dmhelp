#' Strip leading, trailing or leading and trailing whitespace
#'
#' @param x A vector of character strings
#' @param what A character string that defaults to 'all', allowed alternatives to
#'        which are 'leading' and 'trailing'
#' @details http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
#' @export strip
strip <- function (x, what="all"){
  if (what == "all"){
    gsub("^\\s+|\\s+$", "", x)
  } else if (what == "leading"){
    sub("^\\s+", "", x)
  } else if (what == "trailing"){
    sub("\\s+$", "", x)
  } else {
    stop("what can take values 'all', 'leading' or 'trailing'")
  }
}
