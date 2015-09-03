#' Add a column of 0/1 flags to a \code{data.frame} indicating the last observation per patient, per test
#' 
#' @param data A \code{data.frame}
#' @param id Character string giving the name of the subject identifier. Defaults to
#'   \code{id="subject"}
#' @param time Character string giving the name of the time variable. Defaults to
#'   \code{time="studyday"}
#' @param test Character string giving the name of the test variable. Defaults to
#'   \code{test="test"}
#'
#' @details The function sorts the data by id, test and time and returns a
#'   \code{data.frame} with a column "lastObsFlag" which takes value 1 if the
#'   row represents the last observation for a particular value of "test", 0
#'   otherwise. The code was debugged by akrun on Stackoverflow.
getLastObsFlag <- function(data, id="subject", time="studyday", test="test"){
  data <- arrange_(data, id, test, time) %>%
    mutate_(lastObsFlag = 0) %>%
    group_by_(id, test) %>%
    mutate_(.dots=list(lastObsFlag = lazyeval::interp(~replace(lastObsFlag,
                                                     n(), 1))))
  as.data.frame(data)
}

#' Get the last observation per observational unit
#' 
#' @param data A \code{data.frame} to be reduced to the last observation per
#'   observational unit (patient, in practice).
#' @param id Character string giving the column name of the observation unit
#'   identifier. Defaults to \code{id="subject"}.
#' @param time Character string giving the column name of the time variable.
#'   Defaults to \code{time="studyday"}.
#' @param test Character string giving the column name of the test variable.
#'   The function assumes you've subset down to a single test and will fail
#'   otherwise.
#' @return A \code{data.frame} with the same columns, but reduced to the last
#'   observation per observational unit.
#' @export getLastObs
getLastObs <- function(data, id="subject", time="studyday", test="test"){
  if (any(is.na(data[, id]) | is.na(data[, time])))
    stop("Missing values in one of id or time")

  if (length(unique(data[, test])) > 1)
    stop("You haven't subset on test")
  
  data <- data[order(data[, time]), ]
  data <- data[order(data[, id]), ]
  
  if (is.factor(data[, id]))
    res <- data[cumsum(rle(as.character(data[, id]))$lengths), ]
  else
    res <- data[cumsum(rle(data[, id])$lengths), ]

  if (nrow(res) == 0) stop("Result has 0 rows!")
  else if (nrow(res) >= nrow(data)) stop("Result has greater or equal number of rows to the input!")
  else res
}
