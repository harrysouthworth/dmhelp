#' @method addterm brglm
#' @importFrom MASS addterm
#' @export
addterm.brglm <- function (object, 
                           scope, scale = 0, test = c("none", "Chisq", "F"), k = 2, 
                           sorted = FALSE, trace = FALSE, ...){
  Fstat <- function(table, rdf) {
    dev <- table$Deviance
    df <- table$Df
    diff <- pmax(0, (dev[1L] - dev)/df)
    Fs <- diff/(dev/(rdf - df))
    Fs[df < .Machine$double.eps] <- NA
    P <- Fs
    nnas <- !is.na(Fs)
    P[nnas] <- safe_pf(Fs[nnas], df[nnas], rdf - df[nnas], 
                       lower.tail = FALSE)
    list(Fs = Fs, P = P)
  }
  if (missing(scope) || is.null(scope)) 
    stop("no terms in scope")
  if (!is.character(scope)) 
    scope <- add.scope(object, update.formula(object, scope))
  if (!length(scope)) 
    stop("no terms in scope for adding to object")
  oTerms <- attr(terms(object), "term.labels")
  int <- attr(object$terms, "intercept")
  ns <- length(scope)
  dfs <- dev <- numeric(ns + 1)
  names(dfs) <- names(dev) <- c("<none>", scope)
  add.rhs <- paste(scope, collapse = "+")
  add.rhs <- eval(parse(text = paste("~ . +", add.rhs)))
  new.form <- update.formula(object, add.rhs)
  oc <- object$call
  Terms <- terms(new.form)
  oc$formula <- Terms
  fob <- list(call = oc, terms = Terms)
  class(fob) <- class(object)
  x <- model.matrix(Terms, model.frame(fob, xlev = object$xlevels), 
                    contrasts = object$contrasts)
  n <- nrow(x)
  oldn <- length(object$residuals)
  y <- object$y
  newn <- length(y)
  if (newn < oldn) 
    warning(sprintf(ngettext(newn, "using the %d/%d row from a combined fit", 
                             "using the %d/%d rows from a combined fit"), newn, 
                    oldn), domain = NA)
  wt <- object$prior.weights
  if (is.null(wt)) 
    wt <- rep(1, n)
  Terms <- attr(Terms, "term.labels")
  asgn <- attr(x, "assign")
  ousex <- match(asgn, match(oTerms, Terms), 0L) > 0L
  if (int) 
    ousex[1L] <- TRUE
  X <- x[, ousex, drop = FALSE]
  z <- brglm.fit(X, y, wt, offset = object$offset, family = object$family, 
               control = glm.control(), control.brglm=brglm.control())
  dfs[1L] <- z$rank
  dev[1L] <- z$deviance
  sTerms <- sapply(strsplit(Terms, ":", fixed = TRUE), function(x) paste(sort(x), 
                                                                         collapse = ":"))
  for (tt in scope) {
    if (trace) {
      message(gettextf("trying + %s", tt), domain = NA)
      utils::flush.console()
    }
    stt <- paste(sort(strsplit(tt, ":")[[1L]]), collapse = ":")
    usex <- match(asgn, match(stt, sTerms), 0L) > 0L
    X <- x[, usex | ousex, drop = FALSE]
    z <- brglm.fit(X, y, wt, offset = object$offset, 
                   family = object$family,
                   control = glm.control(), control.brglm=brglm.control())
    dfs[tt] <- z$rank
    dev[tt] <- z$deviance
  }
  if (is.null(scale) || scale == 0) 
    dispersion <- summary(object, dispersion = NULL)$dispersion
  else dispersion <- scale
  fam <- object$family$family
  if (fam == "gaussian") {
    if (scale > 0) 
      loglik <- dev/scale - n
    else loglik <- n * log(dev/n)
  }
  else loglik <- dev/dispersion
  aic <- loglik + k * dfs
  aic <- aic + (extractAIC(object, k = k)[2L] - aic[1L])
  dfs <- dfs - dfs[1L]
  dfs[1L] <- NA
  aod <- data.frame(Df = dfs, Deviance = dev, AIC = aic, row.names = names(dfs), 
                    check.names = FALSE)
  o <- if (sorted) 
    order(aod$AIC)
  else seq_along(aod$AIC)
  if (all(is.na(aic))) 
    aod <- aod[, -3]
  test <- match.arg(test)
  if (test == "Chisq") {
    dev <- pmax(0, loglik[1L] - loglik)
    dev[1L] <- NA
    LRT <- if (dispersion == 1) 
      "LRT"
    else "scaled dev."
    aod[, LRT] <- dev
    nas <- !is.na(dev)
    dev[nas] <- safe_pchisq(dev[nas], aod$Df[nas], lower.tail = FALSE)
    aod[, "Pr(Chi)"] <- dev
  }
  else if (test == "F") {
    if (fam == "binomial" || fam == "poisson") 
      warning(gettextf("F test assumes 'quasi%s' family", 
                       fam), domain = NA)
    rdf <- object$df.residual
    aod[, c("F value", "Pr(F)")] <- Fstat(aod, rdf)
  }
  aod <- aod[o, ]
  head <- c("Single term additions", "\nModel:", deparse(formula(object)))
  if (scale > 0) 
    head <- c(head, paste("\nscale: ", format(scale), "\n"))
  class(aod) <- c("anova", "data.frame")
  attr(aod, "heading") <- head
  aod
}
