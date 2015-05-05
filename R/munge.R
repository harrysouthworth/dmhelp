#' Merge together several data.frames
#' Forcibly merge together data.frames contained in a list
#' @param ... An arbirary number of \code{data.frame}s.
#' @param id The name of the column, common to all elements of \code{x}
#'   that identifies the subjects.
#' @param arm The name of the column identifying the treatment group. Defaults
#'   to \code{arm="arm"} and the function will effectively ignore it if it isn't
#'   in any of the data.
#' @param prepend Logical, indicating of column names should be prepended by
#'   the names of the \code{data.frames} passed in via \code{...}. Defaults
#'   to \code{prepend=TRUE}.
#' @return A \code{data.frame} containing the merged elements of \code{x}. The
#'   column names of the elements of \code{...} will be optionally pre-pended with the names
#'   of the \code{data.frames} in \code{...}. As such, you might want to give them
#'   all short names.
#' @details The function does some basic checks on the inputs. In particular, it
#'   fails if the id column contains missing values in any data.frame, or if the
#'   number of rows in any data.frame is not equal to the number of unique elements
#'   in the id column.
#' @export munge
munge <- function(..., id="usubjid", arm="arm", prepend=TRUE){
  # Get the data.frames from ..., get their names and change their colnames
  x <- list(...)

  qc <- function(x, id){ # quick sanity check
    if (any(is.na(x[, id])))
      stop("ID column contains missing values")
    if (nrow(x) != length(unique(x[, id])))
      stop("Number of rows is not equal to number of unique IDs")
    x
  }

  x <- lapply(x, qc, id=id)

  if (prepend){ # Prepend variable names with the name of the dataset they're in
    nms <- as.list(match.call())[-1]
    nms <- lapply(nms, as.character)
    nms <- unlist(nms)[nms != "id"]
    
    x <- lapply(1:length(x), function(X, x, id, arm) {
      wh <- !(names(x[[X]]) %in% c(id, arm))
      names(x[[X]])[wh] <- paste0(nms[X], ".", names(x[[X]])[wh])
      x[[X]]
    },
    x=x, id=id, arm=arm)
  }
  
  res <- Reduce(function(...) merge(..., by=id, all=TRUE), x)
  res <- qc(res, id)
  invisible(res)
}



