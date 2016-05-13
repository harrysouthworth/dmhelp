#' Annotated Kaplan Meier plots
#' @param x An object produced by \code{survfit}
#' @param times Optionally, a numeric vector of times at which to mark the
#'        numbers alive and dead on beneath the x-axis
#' @param n The numbers alive, to be marked at \code{times}
#' @param dead The numbers dead, to be marked at \code{times}
#' @parm mar Margin sizes
#' @param m For converting between days and months
#'
#' @details This had been tested a bit, but not extensively
#' @export kmplot
kmplot <-
  function (x, conf.int, mark.time = FALSE, mark = 3, col = 1,
            lty = 1, lwd = 1, cex = 1, log = FALSE, xscale = 1, yscale = 100,
            firstx = 0, firsty = 1, xmax, ymin = 0, fun, xlab = "", ylab = "",
            xaxs = "S", conf.times, conf.cap = 0.005, conf.offset = 0.012,
            times=NULL, n=NULL, dead=NULL, mar=c(7.1, 4.1, 4.1, 2.1),
            xat, yat=seq(0, 1, by=.1), m,
            ...){
    dotnames <- names(list(...))
    if (any(dotnames == "type"))
      stop("The graphical argument 'type' is not allowed")
    if (missing(mark.time) & !missing(mark))
      mark.time <- TRUE
    if (inherits(x, "survfitms")) {
      x$surv <- 1 - x$prev
      if (is.matrix(x$surv)) {
        dimnames(x$surv) <- list(NULL, x$states)
        if (ncol(x$surv) > 1 && any(x$states == "")) {
          x$surv <- x$surv[, x$states != ""]
          if (is.matrix(x$p0))
            x$p0 <- x$p0[, x$states != ""]
          else x$p0 <- x$p0[x$states != ""]
        }
      }
      if (!is.null(x$lower)) {
        x$lower <- 1 - x$lower
        x$upper <- 1 - x$upper
      }
      if (missing(fun))
        fun <- "event"
    }
    if (missing(firsty) && !is.null(x$p0))
      firsty <- 1 - x$p0
    if (is.logical(log)) {
      ylog <- log
      xlog <- FALSE
      if (ylog)
        logax <- "y"
      else logax <- ""
    }
    else {
      ylog <- (log == "y" || log == "xy")
      xlog <- (log == "x" || log == "xy")
      logax <- log
    }
    if (!missing(fun)) {
      if (is.character(fun)) {
        if (fun == "log" || fun == "logpct")
          ylog <- TRUE
        if (fun == "cloglog") {
          xlog <- TRUE
          if (ylog)
            logax <- "xy"
          else logax <- "x"
        }
      }
    }
    if (missing(xaxs) && (firstx != 0 || !missing(fun) || (missing(fun) &&
                                                           inherits(x, "survfitms"))))
      xaxs <- par("xaxs")
    ssurv <- as.matrix(x$surv)
    stime <- x$time
    if (!is.null(x$upper)) {
      supper <- as.matrix(x$upper)
      slower <- as.matrix(x$lower)
    }
    else {
      conf.int <- FALSE
      supper <- NULL
    }
    if (is.null(x$strata)) {
      nstrat <- 1
      stemp <- rep(1, length(x$time))
    }
    else {
      nstrat <- length(x$strata)
      stemp <- rep(1:nstrat, x$strata)
    }
    ncurve <- nstrat * ncol(ssurv)
    firsty <- matrix(firsty, nrow = nstrat, ncol = ncol(ssurv))
    if (!missing(xmax) && any(x$time > xmax)) {
      keepx <- keepy <- NULL
      tempn <- table(stemp)
      offset <- cumsum(c(0, tempn))
      for (i in 1:nstrat) {
        ttime <- stime[stemp == i]
        if (all(ttime <= xmax)) {
          keepx <- c(keepx, 1:tempn[i] + offset[i])
          keepy <- c(keepy, 1:tempn[i] + offset[i])
        }
        else {
          bad <- min((1:tempn[i])[ttime > xmax])
          if (bad == 1) {
            if (!is.na(firstx)) {
              keepy <- c(keepy, 1 + offset[i])
              ssurv[1 + offset[i], ] <- firsty[i, ]
            }
          }
          else keepy <- c(keepy, c(1:(bad - 1), bad - 1) +
                            offset[i])
          keepx <- c(keepx, (1:bad) + offset[i])
          stime[bad + offset[i]] <- xmax
          x$n.event[bad + offset[i]] <- 1
        }
      }
      stime <- stime[keepx]
      stemp <- stemp[keepx]
      x$n.event <- x$n.event[keepx]
      if (!is.null(x$n.censor))
        x$n.censor <- x$n.censor[keepx]
      ssurv <- ssurv[keepy, , drop = FALSE]
      if (!is.null(supper)) {
        supper <- supper[keepy, , drop = FALSE]
        slower <- slower[keepy, , drop = FALSE]
      }
    }
    if (!missing(fun)) {
      if (is.character(fun)) {
        tfun <- switch(fun, log = function(x) x, event = function(x) 1 -
                         x, cumhaz = function(x) -log(x), cloglog = function(x) log(-log(x)),
                       pct = function(x) x * 100, logpct = function(x) 100 *
                         x, identity = function(x) x, stop("Unrecognized function argument"))
      }
      else if (is.function(fun))
        tfun <- fun
      else stop("Invalid 'fun' argument")
      ssurv <- tfun(ssurv)
      if (!is.null(supper)) {
        supper <- tfun(supper)
        slower <- tfun(slower)
      }
      firsty <- tfun(firsty)
    }
    if (missing(firstx)) {
      if (!is.null(x$start.time))
        firstx <- x$start.time
      else {
        if (xlog)
          firstx <- min(stime[stime > 0])
        else firstx <- min(0, stime)
      }
    }
    if (missing(conf.int) && missing(conf.times))
      conf.int <- (ncurve == 1)
    if (missing(conf.times))
      conf.times <- NULL
    else {
      if (!is.numeric(conf.times))
        stop("conf.times must be numeric")
      if (missing(conf.int))
        conf.int <- TRUE
    }
    if (is.logical(conf.int))
      plot.surv <- TRUE
    else {
      temp <- match.arg(conf.int, c("both", "only", "none"))
      if (is.na(temp))
        stop("invalid value for conf.int")
      if (temp == "none")
        conf.int <- FALSE
      else conf.int <- TRUE
      if (temp == "only")
        plot.surv <- FALSE
      else plot.surv <- TRUE
    }
    mark <- rep(mark, length.out = ncurve)
    mcol <- rep(col, length.out = ncurve)
    if (is.numeric(mark.time))
      mark.time <- sort(mark.time)
    if (conf.int & is.null(conf.times)) {
      if (length(lty) == 1 && is.numeric(lty))
        lty <- rep(c(lty, lty + 1, lty + 1), ncurve)
      else if (length(lty) <= ncurve)
        lty <- rep(rep(lty, each = 3), length.out = (ncurve *
                                                       3))
      else lty <- rep(lty, length.out = ncurve * 3)
      if (length(col) <= ncurve)
        col <- rep(rep(col, each = 3), length.out = 3 * ncurve)
      else col <- rep(col, length.out = 3 * ncurve)
      if (length(lwd) <= ncurve)
        lwd <- rep(rep(lwd, each = 3), length.out = 3 * ncurve)
      else lwd <- rep(lwd, length.out = 3 * ncurve)
    }
    else {
      col <- rep(col, length.out = ncurve)
      lty <- rep(lty, length.out = ncurve)
      lwd <- rep(lwd, length.out = ncurve)
    }
    if (!missing(fun)) {
      ymin <- tfun(ymin)
    }
    if (xaxs == "S") {
      xaxs <- "i"
      tempx <- max(stime) * 1.04
    }
    else tempx <- max(stime)
    tempx <- c(firstx, tempx, firstx)
    if (ylog) {
      tempy <- range(ssurv[is.finite(ssurv) & ssurv > 0])
      if (tempy[2] == 1)
        tempy[2] <- 0.99
      if (any(ssurv == 0)) {
        tempy[1] <- tempy[1] * 0.8
        ssurv[ssurv == 0] <- tempy[1]
        if (!is.null(supper)) {
          supper[supper == 0] <- tempy[1]
          slower[slower == 0] <- tempy[1]
        }
      }
      tempy <- c(tempy, firsty)
    }
    else tempy <- range(ssurv, firsty, finite = TRUE, na.rm = TRUE)
    if (missing(fun)) {
      tempx <- c(tempx, firstx)
      if (!ylog)
        tempy <- c(tempy, ymin)
    }


    oldpar <- par()
    on.exit(par(oldpar))
    par(mar=mar)


    plot(range(tempx, finite = TRUE, na.rm = TRUE)/xscale, range(tempy,
                                                                 finite = TRUE, na.rm = TRUE) * yscale, type = "n", log = logax,
         xlab = xlab, ylab = ylab, xaxs = xaxs, ...)

    if (yscale != 1) {
      if (ylog)
        par(usr = par("usr") - c(0, 0, log10(yscale), log10(yscale)))
      else par(usr = par("usr")/c(1, 1, yscale, yscale))
    }
    if (xscale != 1) {
      if (xlog)
        par(usr = par("usr") - c(log10(xscale), log10(xscale),
                                 0, 0))
      else par(usr = par("usr") * c(xscale, xscale, 1, 1))
    }
    dostep <- function(x, y) {
      keep <- is.finite(x) & is.finite(y)
      if (!any(keep))
        return()
      if (!all(keep)) {
        x <- x[keep]
        y <- y[keep]
      }
      n <- length(x)
      if (n == 1)
        list(x = x, y = y)
      else if (n == 2)
        list(x = x[c(1, 2, 2)], y = y[c(1, 1, 2)])
      else {
        temp <- rle(y)$lengths
        drops <- 1 + cumsum(temp[-length(temp)])
        if (n %in% drops) {
          xrep <- c(x[1], rep(x[drops], each = 2))
          yrep <- rep(y[c(1, drops)], c(rep(2, length(drops)),
                                        1))
        }
        else {
          xrep <- c(x[1], rep(x[drops], each = 2), x[n])
          yrep <- c(rep(y[c(1, drops)], each = 2))
        }
        list(x = xrep, y = yrep)
      }
    }
    drawmark <- function(x, y, mark.time, censor, cex, ...) {
      if (!is.numeric(mark.time)) {
        xx <- x[censor]
        yy <- y[censor]
      }
      else {
        xx <- mark.time
        yy <- approx(x, y, xx, method = "constant", f = 0)$y
      }
      points(xx, yy, cex = cex, ...)
    }
    plot.surv <- TRUE
    type <- "s"
    c1 <- 1
    c2 <- 1
    xend <- yend <- double(ncurve)
    if (length(conf.offset) == 1)
      temp.offset <- (1:ncurve - (ncurve - 1)/2) * conf.offset *
      diff(par("usr")[1:2])
    else temp.offset <- rep(conf.offset, length = ncurve) * diff(par("usr")[1:2])
    temp.cap <- conf.cap * diff(par("usr")[1:2])
    for (j in 1:ncol(ssurv)) {
      for (i in unique(stemp)) {
        who <- which(stemp == i)
        censor <- if (is.null(x$n.censor))
          (x$n.event[who] == 0)
        else (x$n.censor[who] > 0)
        xx <- c(firstx, stime[who])
        censor <- c(FALSE, censor)
        yy <- c(firsty[i, j], ssurv[who, j])
        if (plot.surv) {
          if (type == "s")
            lines(dostep(xx, yy), lty = lty[c2], col = col[c2],
                  lwd = lwd[c2])
          else lines(xx, yy, type = type, lty = lty[c2],
                     col = col[c2], lwd = lwd[c2])
          if (is.numeric(mark.time) || mark.time)
            drawmark(xx, yy, mark.time, censor, pch = mark[c1],
                     col = mcol[c1], cex = cex)
        }
        xend[c1] <- max(xx)
        yend[c1] <- yy[length(yy)]
        if (conf.int && !is.null(conf.times)) {
          x2 <- conf.times + temp.offset[c1]
          templow <- approx(xx, c(firsty[i, j], slower[who,
                                                       j]), x2, method = "constant", f = 1)$y
          temphigh <- approx(xx, c(firsty[i, j], supper[who,
                                                        j]), x2, method = "constant", f = 1)$y
          segments(x2, templow, x2, temphigh, lty = lty[c2],
                   col = col[c2], lwd = lwd[c2])
          if (conf.cap > 0) {
            segments(x2 - temp.cap, templow, x2 + temp.cap,
                     templow, lty = lty[c2], col = col[c2], lwd = lwd[c2])
            segments(x2 - temp.cap, temphigh, x2 + temp.cap,
                     temphigh, lty = lty[c2], col = col[c2], lwd = lwd[c2])
          }
        }
        c1 <- c1 + 1
        c2 <- c2 + 1

        if (conf.int && is.null(conf.times)) {
          if (type == "s") {
            lines(dostep(xx, c(firsty[i, j], slower[who,
                                                    j])), lty = lty[c2], col = col[c2], lwd = lwd[c2])
            c2 <- c2 + 1
            lines(dostep(xx, c(firsty[i, j], supper[who,
                                                    j])), lty = lty[c2], col = col[c2], lwd = lwd[c2])
            c2 <- c2 + 1
          }
          else {
            lines(xx, c(firsty[i, j], slower[who, j]),
                  lty = lty[c2], col = col[c2], lwd = lwd[c2],
                  type = type)
            c2 <- c2 + 1
            lines(xx, c(firsty[i, j], supper[who, j]),
                  lty = lty[c2], col = col[c2], lwd = lwd[c2],
                  type = type)
            c2 <- c2 + 1
          }
        }
      }
    }

    axis(2, at=yat, labels=yat * yscale)
    axis(1, label=xat, at=xat*m)

    if (!is.null(times)){
      mtext("Number at risk (deaths):", side=1, adj=0, line=3)
      axis(1, tick=FALSE, line=FALSE, at=times, labels=paste0(n, " (", dead, ")"), padj=5, hadj=0)
    }

    invisible(list(x = xend, y = yend))
  }
