#' Create summary tables for continuous and factor variables
#' @param data A \code{data.frame}, assumed to be long and thin
#' @param test Character string giving the value of the column containing the test names
#' @param arm Character string giving the name of the column containing the arm data
#' @param value Charcter string naming value column
#' @param visit Character string... work it out
#' @param visit_label Character string to be used as the column label for visit
#' @param numeric_summary Character string that should be either 'fivenum' (the default)
#'   or 'stupid' for a summary that only makes sense for Gaussian data
#' @param digits The number of decimal places to round to. By convention, you should override
#'   the default with one more decimal place than the data are recorded to
#' @details You might have to specify factor levels in the order you want them to appear before
#'   using this function. Also, this function will only work with values that are numeric or
#'   factor, so you need to coerce character vectors prior to using this function.
#' @export trialTable
trialTable <- function(data, test = "test", arm = "arm", value = "value", visit = "visit", visit_label="Visit",
                       numeric_summary = "fivenum", digits=2){
  theCall <- match.call()
  if (!(numeric_summary %in% c("fivenum", "stupid"))){
    stop("numeric_summary should be either 'fivenum' or 'stupid'")
  }

  if (!(is.numeric(data[, value]) | is.factor(data[, value]))){
    stop("The value column must be either numeric or factor")
  }

  # Mess with variable names to keep dplyr functions happy
  names(data)[names(data) == visit] <- "visit"
  names(data)[names(data) == test] <- "test"
  names(data)[names(data) == arm] <- "arm"
  names(data)[names(data) == value] <- "value"

  res <- list()
  index <- 0

  for (the_test in unique(data[, "test"])){
    for(the_arm in unique(data[, "arm"])){
      index <- index + 1

      d <- data[data[, "test"]  ==  the_test & data[, "arm"] == the_arm, ]
      d <- d[, !(names(d) == "arm")]

      if (nrow(d) == 0) stop("After subsetting data has no rows!")

      if (is.numeric(d[, value])){
        if (numeric_summary == "fivenum"){ # Five number summary
          wh <- group_by_(d, visit) %>%
            summarize(N=n(), Missing = sum(is.na(value)),
                      Min. = min(value, na.rm=TRUE),
                      Q1 = round(quantile(value, prob=.25, na.rm=TRUE), digits=digits),
                      Median = round(median(value, na.rm=TRUE), digits=digits),
                      Q3 = round(quantile(value, prob=.75, na.rm=TRUE), digits=digits),
                      Max. = round(max(value, na.rm=TRUE), digits=digits))
        } else {                           # Gaussian summary
          wh <- group_by_(d, visit) %>%
            summarize(N=n(), Missing = sum(is.na(value)),
                      Min.=min(value, na.rm=TRUE), Median=round(median(value, na.rm=TRUE), digits=digits),
                      Mean=round(mean(value, na.rm=TRUE), digits=digits),
                      SD=round(sd(value, na.rm=TRUE), digits=digits),
                      Max.=max(value, na.rm=TRUE))
        }
      } else if (is.factor(d[, value])){   # Counting factor levels
        wh <- group_by(d, visit) %>%
          count(value)
        }

      names(wh)[names(wh) == "visit"] <- visit_label
      res[[index]] <- as.data.frame(wh)
      names(res)[index] <- paste0(the_test, ": ", the_arm)
    } # Close for (the_visit
  } # Close for (the_arm

  out <- list(tables = res, call = theCall)
  class(out) <- "trialTable"
  out
} # Close trialTable


#' @export xtable.trialTable
xtable.trialTable <- function(x, caption=NULL, label=NULL,
                              align=NULL, digits=NULL, display=NULL, auto=FALSE, ...){
  x <- x$tables

  if (is.null(caption)){
    caption <- paste("Summary of", names(x))
  }
  if (is.null(label)){
    label <- paste0("tab:", gsub(" ", "_", names(x)))
  }

  res <- list()
  for (i in 1:length(x)){
    res[[i]] <- xtable(x[[i]], label=label[i], caption=caption[i], align=align, digits=digits, auto=auto, ...)
  }
  class(res) <- "trialTable.xtable"
  res
}

#' @export print.trialTable.xtable
print.trialTable.xtable <- function(x, clear=TRUE, ...){
  for (i in 1:length(res)){
    print(x[[i]], ...)
    if (clear) cat("\\clearpage \\newpage")
  }
}

