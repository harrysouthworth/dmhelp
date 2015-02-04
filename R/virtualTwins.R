#' Get predicted response for each of two treatment groups in a virtual twins approach
#' 
#' @param data A \code{data.frame}
#' @param group The name of the column in \code{data} containing the treatment groups
#' @param fo A formula with the response on the left
#' @param n.trees Arguments to \code{gbm}
#' @param shrinkage Arguments to \code{gbm}
#' @param interaction.depth Arguments to \code{gbm}
#' @param cv.folds Arguments to \code{gbm}
#' @param distribution Arguments to \code{gbm}
#' @param quiet Whether to suppress warnings and messages. Defaults to \code{quiet=FALSE}
#' @param n In \code{plot.virtualTwins}, the number of predictors to include in the
#'        relative influence plots. Relative influence is sorted and the function
#'        plots the \code{n} most important.

#' @return A list with the (reordered) data and the two fitted models. The data is sorted with
#'         all of members of the first treatment group coming first, followed by the second
#'         treatment group, and has two additional columns holding the predicted values of the response for
#'         each treatment on the scale of the linear predictor.
#' @export
vt <- function(data, group, fo, n.trees=1000, shrinkage=.01, interaction.depth=6, cv.folds=10,
               distribution="bernoulli", class.stratify.cv=NULL, quiet=FALSE){
  g <- unique(data[, group])
  if (length(g) != 2)
     stop("There should be 2 treatment groups")

  d1 <- data[data[, group] == g[1], ]
  d2 <- data[data[, group] == g[2], ]

  d1 <- d1[, names(d1) != group]
  d2 <- d2[, names(d2) != group]

  shush <- if (quiet) suppressWarnings
  else function(x) x

  # Guess the distribution
  if (missing(distribution))
    shush(distribution <- gbm:::guessDist(model.response(model.frame(fo, d1))))

  mod1 <- shush(gbm(fo, d1, n.trees=n.trees, shrinkage=shrinkage,
              interaction.depth=interaction.depth, cv.folds=cv.folds,
              distribution=distribution, class.stratify.cv=class.stratify.cv))
  mod2 <- shush(gbm(fo, d2, n.trees=n.trees, shrinkage=shrinkage,
              interaction.depth=interaction.depth, cv.folds=cv.folds,
              distribution=distribution, class.stratify.cv=class.stratify.cv))

  # See if the models converged
  nt1 <- gbm.perf(mod1, plot.it=FALSE, method="cv")
  nt2 <- gbm.perf(mod2, plot.it=FALSE, method="cv")
  if (!quiet) message(paste("CV number of trees:", paste(c(nt1, nt2), collapse=", ")))
  
  if (nt1 == n.trees | nt2 == n.trees)
    shush(warning("CV error didn't converge for at least one model"))
  
  # Rather than add a load of transformations, use predictions and scale of
  # linear predictor and let the user decide what to do next.
  p1 <- c(mod1$cv.fitted, predict(mod1, d2, n.trees=nt1))
  p2 <- c(predict(mod2, d1, n.tree=nt2), mod2$cv.fitted)

  res <- do.call("rbind", list(d1, d2))

  # Drop response variable (both of them if survival).
  drop <- names(get_all_vars(fo[-3], d1))
  res <- res[, ! names(res) %in% drop]

  g <- make.names(g)
  p <- data.frame(p1, p2)
  names(p) <- c(paste0("p", g[1]), paste0("p", g[2]))
  
  res <- list(data=res, predictions=p, mod1=mod1, mod2=mod2, call=match.call())
  class(res) <- "virtualTwins"
  invisible(res)
}

#' @method print virtualTwins
#' @export
print.virtualTwins <- function(x, ...){
  print("A vitualTwins object:")
  cat("\n")
  print(x$call)
  cat("\n")
  print(x$mod1)
  cat("\n")
  print(x$mod2)
  invisible()
}

#' @method plot virtualTwins
#' @export
plot.virtualTwins <- function(x, n=12, abbrev=12, ...){
  n1 <- gbm.perf(x$mod1, method="cv", plot=FALSE)
  n2 <- gbm.perf(x$mod2, method="cv", plot=FALSE)
  r1 <- relative.influence(x$mod1, scale=TRUE, sort=TRUE, n.trees=n1)
  r2 <- relative.influence(x$mod2, scale=TRUE, sort=TRUE, n.trees=n2)
  if (abbrev){
    names(r1) <- abbreviate(names(r1), 12)
    names(r2) <- abbreviate(names(r2), 12)
  }
  par(mfrow=c(2, 2))
  gbm.perf(x$mod1, method="cv")
  gbm.perf(x$mod2, method="cv")
  dotchart(r1[n:1], pch=16)
  dotchart(r2[n:1], pch=16)
  invisible()
}

#' Get data for the second stage of a virtual twins analysis
#' @param x An object of class 'virtualTwins' returned by \code{vt}.
#' @param th The threshold of the difference in predicted values above which to classify
#'   subjects as 'responders'. Defaults to \code{th=NULL} and not used.
#' @param qu The quantile of the difference in predicted values above which to classify
#'   subjects as responders. Defaults to \code{qu=.75} so that 1/4 of subjects are
#'   deemed to be responders.
#' @param order The order of the groups used in subtracting the fitted values. Defaults
#'   to \code{order=1:2}, the only other valid option being \code{order=2:1}.
#' @details If both \code{th=NULL} and \code{qu=NULL}, thresholding is not performed
#'   and the difference between predicted values is returned.
#' @return A \code{data.frame} containing all the predictors and the (thresholded)
#'   difference in predicted values, \code{target}. Predicted values are on the scale of
#'   the linear predictor, so using \code{th} will often not make much sense.
#' @export vtData
vtData <- function(x, th=NULL, qu=.75, order=1:2){
  if (class(x) != "virtualTwins")
    stop("x must have class 'virtualTwins'")
  
  res <- x$data
  res$group <- NULL # Get rid of treatment groups
  res$target <- x$predictions[, order[1]] - x$predictions[, order[2]]
  
  # Threshold if desired
  if (!is.null(th))
    res$target <- as.numeric(res$target > th)
  else if (!is.null(qu))
    res$target <- as.numeric(res$target > quantile(res$target, qu))

  invisible(res)
}
