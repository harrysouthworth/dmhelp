#' Get the last observation per observational unit
#' 
#' @param data A \code{data.frame} to be reduced to the last observation per
#'   observational unit (patient, in practice).
#' @param id Character string giving the column name of the observation unit
#'   identifier. Defaults to \code{id="subject"}.
#' @param time Character string giving the column name of the time variable.
#'   Defaults to \code{time="studyday"}.
#' @return A \code{data.frame} with the same columns, but reduced to the last
#'   observation per observational unit.
getLastObs <- function(data, id="subject", time="studyday"){
  if (any(is.na(data[, subject] | is.na(data[, time]))))
    stop("Missing values in one of id or time")

  data <- data[order(data[, time]), ]
  data <- data[order(data[, id]), ]
  
  res <- data[cumsum(rle(data[, id])$lengths), ]

  if (nrow(res) == 0) stop("Result has 0 rows!")
  else if (nrow(res) >= nrow(data)) stop("Result has greater or equal number of rows to the input!")
  else res
}