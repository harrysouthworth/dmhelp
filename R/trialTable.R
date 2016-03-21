#' Create summary tables for continuous and factor variables
#' @param data A \code{data.frame}, assumed to be long and thin
#' @param test Character string giving the value of the column containing the test names
#' @param arm Character string giving the name of the column containing the arm data
#' @param value Charcter string naming value column
#' @param visit Character string... work it out
#' @param visit_label Character string to be used as the column label for visit
#' @param numeric_summary Character string that should be either 'fivenum' (the default)
#'   or 'stupid' for a summary that only makes sense for Gaussian data
#' @details You might have to specify factor levels in the order you want them to appear before
#'   using this function. Also, this function will only work with values that are numeric or
#'   factor, so you need to coerce character vectors prior to using this function.
#' @export trialTable
trialTable <- function(data, test = "test", arm = "arm", value = "value", visit = "visit", visit_label="Visit",
                       numeric_summary = "fivenum"){
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

  for (the_test in unique(data[, test])){
    for(the_arm in unique(data[, arm])){
      index <- index + 1

      d <- data[data[, test]  ==  the_test & data[, arm] == the_arm, ]
      d <- d[, !(names(d) == arm)]

      if (nrow(d) == 0) stop("After subsetting data has no rows!")

      if (is.numeric(d[, value])){
        if (numeric_summary == "fivenum"){ # Five number summary
          wh <- group_by_(d, visit) %>%
            summarize(N=n(), Missing = sum(is.na(value)),
                      Min.=min(value, na.rm=TRUE), Q1=quantile(value, prob=.25, na.rm=TRUE),
                      Median=median(value, na.rm=TRUE), Q3 = quantile(value, prob=.75, na.rm=TRUE),
                      Max.=max(value, na.rm=TRUE))
        } else {                           # Gaussian summary
          wh <- group_by_(d, visit) %>%
            summarize(N=n(), Missing = sum(is.na(value)),
                      Min.=min(value, na.rm=TRUE), Median=median(value, na.rm=TRUE),
                      Mean=mean(value, na.rm=TRUE), SD=sd(value, na.rm=TRUE),
                      Max.=max(value, na.rm=TRUE))
        }
      } else if (is.factor(d[, value])){
        wh <- group_by_(d, visit) %>%
          table(value)
      }

      names(wh)[names(wh) == visit] <- visit_label
      res[[index]] <- as.data.frame(wh)
      names(res)[index] <- paste0(the_test, ": ", the_arm)
    } # Close for (the_visit
  } # Close for (the_arm
  class(res) <- "trialTable"
  res
} # Close trialTable


#' @export xtable.trialTable
xtable.trialTable <- function(x, caption=paste("Summary of", names(x)), label=paste0(tab, gsub(" ", "_", names(x))),
                              align=NULL, digits=NULL, display=NULL, auto=FALSE, ...){



}

