#' Quick check that a data.frame contains only one object per row
#' @param x A \code{data.frame}
#' @param id Character string giving the name of the object identifier column.
#'   Defaults to \code{id=c("usubjid", "subject", "patient", "randno")} and the
#'   first one of these found in the names of \code{x} will be used if the
#'   argument is not specified by the user.
#' @param verbose Logical. If \code{verbose=FALSE} (the default), nothing happens.
#'   Otherwise, the function prints the value of \code{id}. For use when the
#'   function fails.
#' @export qc
qc <- function(x, id=c("usubjid", "subject", "patient", "randno"), verbose=FALSE){
  x <- as.data.frame(x) # Get spurious error if class is "tbl_df"
  if (missing(id)) id <- id[id %in% names(x)][1]
  if (verbose) message(paste("id =", id))
  stopifnot(nrow(x) == length(unique(x[, id])))
}
