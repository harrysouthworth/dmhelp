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

  # Get rid of anything non-ascii
  wh <- sapply(res, is.character)
  res[, wh] <- lapply(res[, wh], iconv, to="ascii")

  # Do names and return
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
  if (!check(data[, baseline]))
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
    data <- melt(data, id.vars=c(id, test), measure.vars=baseline)
    data <- cast(data, as.formula(paste(id, "~", test)), direction="wide", value=baseline)
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
  data <- data[!is.na(data[, id]), ]
  data <- data[!is.na(data[, visit]), ]
  data <- data[!is.na(data[, test]), ]
  if (missing(keep)) stop("You need to specify which visit's data to keep")
  data <- data[data[, visit] == keep, ]

  # Reorder and then use the last unique combination of subject and test
  data <- data[order(data[, test]), ]
  data <- data[order(data[, id]), ]

  wh <- paste(data[, id], data[, test])
  data <- data[cumsum(rle(wh)$lengths), ]

  if (wide){
    data <- melt(data, id.vars=c(id, test), measure.vars=value)
    data <- cast(data, as.formula(paste(id, "~", test)), direction="wide", value=value)
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
#' @param drop Drop columns in which fewer than \code{drop} of cases are 1. Defaults to \code{drop=.03}
#' @param verbose Whether to report progress. Defaults to \code{verbose=TRUE}
wideData <- function(data, dm, id="usubjid", term="aeterm", trt="arm", arm.col=FALSE, id.col=TRUE, drop=0.03, verbose=FALSE){
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
  dimnames(wae) <- list(dm[, id], casefold(make.names(uae)))
  
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

#' Get the numeric columns of a data.frame
#' @export
#' @param data A \code{data.frame}
#' @details Character and factor columns are dropped, and the remainder returned.
numerics <- function(data){
    sapply(data, function(x) !(is.factor(x) | is.character(x)))
}

#' Get the binary columns of a data.frame
#' @export
#' @param data A \code{data.frame}
#' @return A logical vector indicating which columns have exactly 2 unique values after dropping NAs
binaries <- function(data){
  sapply(data, function(x) length(na.omit(x)) == 2)
  
}

#' Get columns with a large proportion of missing values
#' @export
#' @param data A \code{data.frame}
#' @param th The threshold proportion of NAs to detect. Defaults to 0.2
#' @return A logical vector indicating which columns have lots of NAs
highNAs <- function(data, th=.2){
  sapply(data, function(x) mean(is.na(x)) > th)
}

#' Find variables with a large proportion of missing values
#' @export
#' @param data A \code{data.frame}
#' @param th The threshold above which the proportion of NAs is to be flagged. Defaults to \code{th=0.2}
#' @return A character matrix with the names of the variables and the percentage of missing values.
summarizeNAs <- function(data, th=.2){
  nas <- highNAs(data, th)
  miss <- cbind(names(nas)[nas], apply(data[, nas], 2,
                                       function(x) paste0(signif(mean(is.na(x))*100, 3), "%")))
  colnames(miss) <- c("Variable", "Missing")
  miss[miss[, 2] != "100%", ]
}

#' Get pairs of variables with high absolute correlation
#' @export
#' @param data A \code{data.frame} or similar
#' @param th The threshold for absolute correlation above which we want the pairs of variables
#' @return A \code{data.frame} with 3 columns representing the pairs of variables and their Spearman's rank correlation
hiCor <- function(data, th=.8){
  if (!is.element(class(data)[1], c("data.frame", "matrix", "cast_df")))
    stop("data should be a matrix, data.frame or cast_df")

  data <- as.data.frame(data)
  co <- cor(data, method="spearman", use="pairwise.complete.obs")
  
  # Sett diagonal and upper triangle to 0 to stop double counting
  co[upper.tri(co, diag=TRUE)] <- 0
  i <- abs(co) > th
  
  wh <- apply(i, 2, function(x, threshold, rn)
    rn[abs(x) > threshold], th, rn=rownames(co))
  v1 <- rep(names(wh), unlist(lapply(wh, length)))
  v2 <- unlist(wh)
  
  co <- rep(0, length(v1))
  for (i in 1:length(co))
    co[i] <- cor(data[, v1[i]], data[, v2[i]], method="spearman", use="pairwise.complete.obs")
  
  res <- data.frame(v1, v2, cor=co, stringsAsFactors=FALSE)
  rownames(res) <- NULL
  colnames(res) <- c("Variable 1", "Variable 2", "Corr.")
  
  res[rev(order(abs(res$Corr.))), ]
}