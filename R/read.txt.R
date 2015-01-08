#' Read a csv file with a few specific options
#' @export
#' @param path The path to the .csv file
#' @param file The name of the csv file, less the extension
#' @param id The unique patient identifier. Where this is missing, rows will be dropped. If
#'        it doesn't appear in the data, it is ignored.
#' @param ext The data file extension. Defaults to \code{ext=".csv"}
read.txt <- function(path, file, id="usubjid", ext=".csv"){
  res <- read.csv(file.path(path, paste0(file, ext)), stringsAsFactors=FALSE, header=TRUE)
  names(res) <- casefold(make.names(names(res)))
  if (id %in% names(res)) res <- res[!is.na(res[, id]), ]
  invisible(res)
}