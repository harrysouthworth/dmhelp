#' ANOVA for logistic regression using Firth's method
#' @param object An object of class 'brglm'
#' @param ... Other arguments to \code{anova}
#' @param dispersion See help file for \code{anova}
#' @param test See help file for \code{anova}
#' 
#' 
#' @details Objects of class 'brglm' inherit from 'glm', but this causes some
#'          methods not to work. The \code{anova.brglm}, \code{addterm.brglm}
#'          and \code{dropterm.brglm} functions are simple edits of other functions
#'          that attempt to fix this behaviour.
#'          
#'          The \code{dropterm} and {addterm} functions are for use with
#'          \code{stepAIC} in the MASS package.
#'
#'          The deviances reported are the true deviances at the bias reduced
#'          parameter estimates. The 'brglm' object contains an element called
#'          'null.deviance' which is the deviance of the null model at the MLE,
#'          not at the bias reduced parameter estimate. The anova function
#'          uses the deviance at the bias reduced estimate.
#'
#'          The only options avialable for tests are \code{test=NULL} (the
#'          default) which does no tests, and \code{test="Chisq"} which does
#'          chi-squared tests.
#' 
#' method anova brglm
#' @method anova brglm
#' @export
anova.brglm <- function (object, ..., dispersion = NULL, test = NULL){
  dotargs <- list(...)
  named <- if (is.null(names(dotargs))) 
    rep(FALSE, length(dotargs))
  else (names(dotargs) != "")
  if (any(named)) 
    warning("the following arguments to 'anova.glm' are invalid and dropped: ", 
            paste(deparse(dotargs[named]), collapse = ", "))
  dotargs <- dotargs[!named]
  is.glm <- unlist(lapply(dotargs, function(x) inherits(x, "glm")))
  dotargs <- dotargs[is.glm]
  if (length(dotargs)) 
    return(anova.glmlist(c(list(object), dotargs), dispersion = dispersion, 
                         test = test))
  doscore <- !is.null(test) && test == "Rao"
  varlist <- attr(object$terms, "variables")
  x <- if (n <- match("x", names(object), 0L)) 
    object[[n]]
  else model.matrix(object)
  varseq <- attr(x, "assign")
  nvars <- max(0, varseq)
  resdev <- resdf <- NULL
  if (doscore)
    stop("Rao's test not implemented")

  if (nvars > 1) {
    method <- object$method
    y <- object$y
    if (is.null(y)) {
      mu.eta <- object$family$mu.eta
      eta <- object$linear.predictors
      y <- object$fitted.values + object$residuals * mu.eta(eta)
    }
    for (i in seq_len(nvars - 1L)) {
      fit <- eval(call(if (is.function(method)) "method" else method, 
                       x = x[, varseq <= i, drop = FALSE], y = y, weights = object$prior.weights, 
                       start = object$start, offset = object$offset, 
                       family = object$family,
                       control = glm.control(), control.brglm=brglm.control()))
      resdev <- c(resdev, fit$deviance)
      resdf <- c(resdf, fit$df.residual)
    }
  }
  
  # brglm$null.deviance is the deviance at the MLE for the null model
  null.deviance <- brglm.fit(x = rep(1, length(y)), y = y, weights = object$prior.weights, 
                             start = object$start, offset = object$offset, 
                             family = object$family,
                             control = glm.control(), control.brglm=brglm.control())$deviance

  resdf <- c(object$df.null, resdf, object$df.residual)
  resdev <- c(null.deviance, resdev, object$deviance)
  table <- data.frame(c(NA, -diff(resdf)), c(NA, pmax(0, -diff(resdev))), 
                      resdf, resdev)
  tl <- attr(object$terms, "term.labels")
  if (length(tl) == 0L) 
    table <- table[1, , drop = FALSE]
  dimnames(table) <- list(c("NULL", tl), c("Df", "Deviance", 
                                           "Resid. Df", "Resid. Dev"))

  title <- paste0("Analysis of Deviance Table", "\n\nModel: ", 
                  object$family$family, ", link: ", object$family$link, 
                  "\n\nResponse: ", as.character(varlist[-1L])[1L], "\n\nTerms added sequentially (first to last)\n\n")
  df.dispersion <- Inf
  if (is.null(dispersion)) {
    dispersion <- summary(object, dispersion = dispersion)$dispersion
    df.dispersion <- if (dispersion == 1) 
      Inf
    else object$df.residual
  }
  if (!is.null(test)) {
    if (test == "F")
      stop("F-test not implemented")
    table <- stat.anova(table = table, test = test, scale = dispersion, 
                        df.scale = df.dispersion, n = NROW(x))
  }
  structure(table, heading = title, class = c("anova", "data.frame"))
}
