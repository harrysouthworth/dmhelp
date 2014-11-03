#' Read a csv file with a few specific options
#' @export
#' @param path The path to the .csv file
#' @param file The name of the csv file, less the extension
#' @param ext The data file extension. Defaults to \code{ext=".csv"}
read.txt <- function(path, file, ext=".csv"){
  res <- read.csv(paste0(path, file, ext), stringsAsFactors=FALSE, header=TRUE)
  names(res) <- casefold(make.names(names(res)))
  invisible(res)
}