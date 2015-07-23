#' List contents of data files in a directory
#' @param dir The path to the directory.
#' @param ext The file extension of data files in the directory. Defaults to \code{ext=".csv"}
#'   but NOTE that it is assumed that the fles are tab-delimited, not comma.
#' @return Nothing
#' @details The function finds all files with the specified file extension, reads
#'   them with \code{read.delim} and then runs \code{str} on them.
contents <- function(dir, ext=".csv"){
  filenames <- list.files(dataPath, pattern="*.csv", full.names=TRUE)
  ldf <- lapply(filenames, read.delim)
  
  for (i in 1:length(ldf)){
    print(filenames[i])
    str(ldf[[i]])
    cat("\n")
  }
  invisible()
}

#' Change all character fields in a data.frame to factors
#' @param x A \code{data.frame}.
#' @export chars2factors
chars2factors <- function(x){
  if (!is.data.frame(x)) stop("x should be a data.frame")
  # Next line is because dplyr gives objects class tbl_df and data.frame, so need to
  # coerce to straightforward data.frame to get as.factor to work on x[, i]
  # 2015-05-05
  x <- as.data.frame(x)
  wh <- sapply(x, is.character)
  for (i in (1:ncol(x))[wh])
    x[, i] <- as.factor(x[, i])
  x
}

#' Read a SAS dataset with the .sas7bdat extension.
#' @export
#' @param path The full path to the directory containing the data files
#' @param file The name of the file in \code{path}, without the extension
#' @param stringsAsFactors Whether strings should be treated as factors. Defaults to \code{stringsAsFactors=FALSE}
readSas <- function(path, file, stringsAsFactors=FALSE){
  res <- read.sas7bdat(paste0(path, file, ".sas7bdat"))
  # Switch all factors to character
  if (!stringsAsFactors)
    res[sapply(res, is.factor)] <- lapply(res[sapply(res, is.factor)], as.character)

  res <- forceAscii(res)

  # Do names and return
  names(res) <- casefold(names(res))
  res
}

#' Force character vectors in a data.frame to ascii
#' @param x A data.frame.
#' @export forceAscii
forceAscii <- function(x){
  if (!is.data.frame(x)) stop("x should be a data.frame")
  # Get rid of anything non-ascii
  wh <- sapply(x, is.character)
  x[, wh] <- lapply(x[, wh], iconv, to="ascii")
  invisible(x)
}

#' Read tab-delimited data
#' Read tab-delimited data ensuring strings are read as strings and are coerced to
#'   ascii, and making sure names are lower case
#' @param path String specifying the path to the data file
#' @param file The name of the data file, excluding the path and extension
#' @param ext The file extension, including a .
#' @param stringsAsFactors Logical specifying if strings should be read as factors.
#'   Defaults to \code{stringsAsFactors=FALSE}
#' @export readTab
readTab <- function(path, file, ext=".csv", stringsAsFactors=FALSE){
  res <- read.delim(file.path(path, paste0(file, ext)), stringsAsFactors=stringsAsFactors)
  res <- forceAscii(res)
  names(res) <- casefold(names(res))
  res
}

#' Get baseline data from full lab, vital signs, or other dataset
#' @export
#' @param data The lab data as a \code{data.frame}
#' @param id Character string naming the unique subject identifier
#' @param test Character string naming the test identifier
#' @param baseline Character string naming the column holding the baseline values
#' @param wide Whether or not to return the wide version of the dataset. Defaults to \code{wide=TRUE}
#' @param check A function to check the type of the baseline variable with. Defaults to \code{check=is.double},
#'        so it checks to see tha the variable is numeric double precision.
baselineData <- function(data, id="usubjid", test="param", baseline="base", wide=TRUE, check=is.double){
  data <- as.data.frame(data)
  b <- data[, baseline]
  if (!check(b))
      stop("baseline variable isn't of the type it should be according to the check argument")

  data <- data[, c(id, test, baseline)]
  data <- data[!is.na(data[, test]), ]
  data <- data[!is.na(data[, baseline]), ] # Drop pre-baseline values

  # Sort before returning a single observation per patient
  data <- data[order(data[, test]), ]
  data <- data[order(data[, id]), ]

  i <- paste(as.character(data[, id]), as.character(data[, test]))
  data <- data[cumsum(rle(i)$lengths), ]

  data <- data[, c(id, test, baseline)]

  # Now need to reshape as wide
  if (wide){
    # Using tidyr - previous versions used reshape2
    data <- spread_(data, test, baseline)
  }
  
  invisible(data)
}

#' Get lab, vital signs or other data in wide format at a single visit
#' @export
#' @param data The lab data as a \code{data.frame}
#' @param id Character string naming the unique subject identifier
#' @param visit Character string naming the visit identifier
#' @param keep The value of \code{visit} to keep in the output data
#' @param test Character string naming the test identifier
#' @param value Character string naming the column holding the lab values
#' @param wide Whether or not to return the wide version of the dataset. Defaults to \code{wide=TRUE}
#' @param check A function to check the type of the baseline variable with. Defaults to \code{check=is.double},
#'        so it checks to see tha the variable is numeric double precision.
visitData <- function(data, id="usubjid", visit="visitnum", keep, test="param", value="aval", wide=TRUE, check=is.double){
  if (!check(data[, value]))
    stop("The variable isn't of the type it should be according to the check argument")
  if (missing(keep)) stop("You need to specify which visit's data to keep")

  data <- data[!is.na(data[, id]), ]
  data <- data[!is.na(data[, visit]), ]
  data <- data[!is.na(data[, test]), ]

  data <- data[data[, visit] == keep, ]

  # Reorder and then use the last unique combination of subject and test
  data <- data[order(data[, test]), ]
  data <- data[order(data[, id]), ]

  wh <- paste(data[, id], data[, test])
  data <- data[cumsum(rle(wh)$lengths), ]

  data <- data[, c(id, test, value)]
  
  if (wide){
    data <- spread_(data, test, value)
  }

  invisible(data)
}

#' Extract a wide form of the adverse event, conmeds, or other dataset
#' @export
#' @param data The adverse event dataset
#' @param dm The demography dataset
#' @param id The unique subject identifier. Defaults to \code{id="usubjid"}
#' @param trt The name of the treatment group column. Defaults to \code{trt="arm"}
#' @param term The name of the column identifying AE terms. Defaults to \code{term="aeterm"}
#' @param id.col Whether to include the subject identifier as a column in the output (the rownames get the subject identifiers). Defaults to \code{id.col=TRUE}
#' @param arm.col Whether to include the treatment arm as a column in the output. Defaults to \code{arm.col=FALSE}
#' @param drop Drop columns in which fewer than \code{drop} of cases are 1. Defaults to \code{drop=0}
#' @param verbose Whether to report progress. Defaults to \code{verbose=TRUE}
wideData <- function(data, dm, id="usubjid", term="aeterm", trt="arm", arm.col=FALSE, id.col=TRUE, drop=0, verbose=FALSE){
  if(!is.element(trt, names(dm))) stop("dm doesn't contain trt")
  
  data[, term] = as.character(data[, term])
  uae <- unique(data[, term])
  
  fun <- function(i, data, subs, demog, term){
    sb <- demog[, subs]
    data <- data[data[, term] == i,  ]
    m <- match(data[, subs], sb)
    res <- rep(0, length(sb))
    res[m] <- 1
    res
  }
  if (verbose) cat("Decomposing", length(uae), "adverse event terms...\n")
  
  aes <- lapply(uae, fun, data = data, subs = id, demog = dm, term = term)
  wae <- as.data.frame(do.call("cbind", aes))
  dimnames(wae) <- list(dm[, id], casefold(uae))

  # Drop low-frequency events
  mns <- apply(wae, 2, mean) <= drop
  wae <- wae[, !mns]

  # Add arm and id if requested
  if (arm.col) wae[, trt] <- factor(dm[, trt])
  if (id.col) wae[, id] <- rownames(wae)

  if (verbose){
    cat("Groups are: ", paste(unique(as.character(wae[, trt])), collapse = ", "))
    cat("\n")
    cat("Result has", nrow(wae), "rows and", ncol(wae), "columns.\n")
  }
  invisible(wae)
}


#' Get columns with zero variance
#' @param A \code{data.frame}.
#' @details Non-numeric columns are coerced to numeric. A logical vector is
#'   returned with \code{TRUE} for any column with zero variance.
#' @export zeroVar
zeroVar <- function(data){
  apply(data, 2, function(x) length(unique(na.omit(x))) < 2)
}
