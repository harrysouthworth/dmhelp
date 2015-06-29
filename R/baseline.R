#' Sort a clinical trial dataset
#' 
#' Sort a clinical trial dataset on one or two variables in it
#' 
#' @param data The data.frame to be sorted
#' @param id The name of the first sorting variable. Defaults to 'pid'
#' @param visit The name of the second sorting variable. Defaults to 'visitnum'
#' @return The input data.frame, resorted.
#' @export sortCTdata
sortCTdata <- function(data, id="pid", visit="visit"){
  # Sort the data on visit then pid
  wh <- try(data[order(data[, visit]), ], silent=TRUE)
  if (class(wh) != "try-error") data <- wh
  data <- data[order(data[, id]), ]
  invisible(data)
}

#' Get logical vector indicating if a value is a baseline value
#' 
#' Split data by subject, test and visit to compute a logical vector indicating if values are baseline values
#' 
#' @param val A vector of values of visit numbers
#' @param base The value of val if the visit is a baseline visit
#' @return A logical vector
#' @note If all values of val < base, the function attempts to return TRUE for the nearest value of val < base.
#'       Perhaps it should more properly return NAs
getLogicalBaseline <- function(val, base){
  # Need to account for subjects with no baseline visit.
  # Take the visit previous to baseline, if any
  # Return object needs to be a logical vector flagging:
  #   The baseline value if it exists
  #   Otherwise, the nearest pre-baseline value if that exists
  #   Otherwise the first value
  res <- val == base # Ok if a baseline visit exists
  if (sum(res) == 0){
    res <- val < base
    last.res <- sum(res) # Assumes already sorted in visit order
    res <- rep(FALSE, length(res))
    if (last.res > 0)
      res[last.res] <- TRUE
    else
      res[1] <- TRUE
  } # Close if (sum(res) == 0)
  
  res
}

#' Create flag variable to indicate baseline values
#' 
#' Create flag variable to indicate baseline values
#' 
#' @param data A data.frame, usually vital signs or lab data
#' @param flag The name of the column that ought to contain the flags
#' @param baseflag The value of flag for the baseline values. Defaults to 1
#' @param visit The name of the visit variable in data
#' @param basevisit The value of visit for the baseline values. Defaults to 0
#' @return A vector of 0s and 1s, 1s indicating the baseline values
#' @export getBaselineFlag
getBaselineFlag <- function(data, flag, id, test, visit="visit", baseflag=1, basevisit=0){
  # Create flag vector of 0s and 1s, or coerce existing flag vectors to 0s and 1s
  
  # Check if flag exists, create empty column if not
  flag <- try(data[, flag], silent=TRUE)
  if (class(flag) == "try-error")
    flag <- rep("", nrow(data))
  
  # Check if existing flag is empty
  if (all(is.na(flag)) | all(flag == "")){
    wh <- paste(data[, id], data[, test])
    sdata <- split(data[, visit], wh)
    flag <- lapply(sdata, getLogicalBaseline, base=basevisit)
    flag <- as.numeric(unsplit(flag, wh))
  }
  else # Otherwise replace incorrect missing values with 0s
    flag[flag != baseflag] <- 0
  
  flag
}


#' Add baseline data to a dataset
#' 
#' Add a column of baseline values to a dataset, given information on the baseline visit
#' 
#' @param data A data.frame to have baseline values added
#' @param id The name of the unique subject identifier variable. Defaults to 'subject'
#' @param flag The name of the baseline flag variable in the data, if it exists
#' @param baseflag The value that flag takes if the value is a baseline value. Defaults to 1
#' @param visit The name of the variable identifying visits. Defaults to 'visit'
#' @param basevisit The value that visit has when it is a baseline visit. Defaults to 0
#' @param test The name of the variable that identifies the test being performed. Deafults to 'test'
#' @param values The name of the variable containing the values of the test result. Defaults to 'value'
#' @param keepFlag Whether to keep the column of baseline flags. Defaults to \code{keepFlag= FALSE}.
#' @return A data.frame similar to the input data.frame, but with a column of baseline values called 'baseline' that
#'         contains the baseline values for each element of test
#' @export makeBaselines
makeBaselines <- function(data, id="subject", flag="baselineFlag", baseflag=1, visit="visit",
                          basevisit=0, test="test", values="value", keepFlag=FALSE){
  data <- data[order(data[, test]), ]
  data <- sortCTdata(data, id=id, visit=visit)
  
  data$baselineFlag <- getBaselineFlag(data, flag, id, test, visit, baseflag, basevisit)
  
  fun <- function(d, id, values){
    flag <- d$baselineFlag

    b <- d[as.logical(flag), values]
    N <- rle(as.character(d[, id]))$lengths

    res <- try(rep(b, N), silent=TRUE)
    if (class(res) == "try-error")
      res <- rep(NA, nrow(d))
    d$baseline <- res
    d
  }

  # Need to loop on the values of test
  sdata <- split(data, data[, test])

  res <- lapply(sdata, fun, id=id, values=values)
  res <- do.call("rbind", res)

  res <- sortCTdata(res, id=id, visit=visit)
  rownames(res) <- 1:nrow(res)

  if (!keepFlag) res <- res[, names(res) != flag]

  invisible(res)
}
