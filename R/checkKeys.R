#' Check that specified columns in a data.frame uniquely define the rows, and append one of them if not
#' @param data A \code{data.frame}
#' @param keys A vector of character strings giving the names of the columns in
#'   \code{data} that should, when concatenated, uniquely define the rows
#' @param make.unique An integer giving the index of \code{keys} to append with "a"
#'   in an attempt to make the keys unique.
#' @return The input \code{data.frame}, sorted by the keys in reverse order, and
#'   with \code{keys[make.unique]} appended with "a" where unique keys didn't exist.
#' @details The function calls itself recursively if a single pass at adding "a"
#'   failed to result in unique keys.
#' @export checkKeys
checkKeys <- function(data, keys=c("subject", "test", "visit"), make.unique=3){
  # Order the data first
  for (i in length(keys):1)
    data <- data[order(data[, keys[i]]), ]

  wh <- paste(data[, keys[1]])

  if (length(wh) > 1){
    for (i in 2:length(keys))
      wh <- paste(wh, data[, keys[i]])
  }

  if (length(unique(wh)) > nrow(data)){ # Add "a" to a key, forcing it to be character/factor
    du <- duplicated(wh)
    data[du, keys[make.unique]] <- paste0(data[du, keys[make.unique]], "a")
    warning("This part of the function has not been properly tested!")
    data <- checkKeys(data, keys, make.unique)
  } else {
    message("Passed first time")
  }

  data
}
