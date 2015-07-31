#' Get columns with a large proportion of missing values
#' @export
#' @param data A \code{data.frame}
#' @param th The threshold proportion of NAs to detect. Defaults to 0.2
#' @return A logical vector indicating which columns have lots of NAs
hiNAs <- function(data, th=.2){
  sapply(data, function(x) mean(is.na(x)) > th)
}

#' Find variables with a large proportion of missing values
#' @export
#' @param data A \code{data.frame}
#' @param th The threshold above which the proportion of NAs is to be flagged. Defaults to \code{th=0.2}
#' @return A character matrix with the names of the variables and the percentage of missing values.
summarizeNAs <- function(data, th=.2){
  nas <- hiNAs(data, th)
  miss <- cbind(names(nas)[nas], apply(data[, nas], 2,
                                       function(x) paste0(signif(mean(is.na(x))*100, 3), "%")))
  colnames(miss) <- c("Variable", "Missing")
  miss[miss[, 2] != "100%", ]
}

#' Missing pattern plot
#' @param data data.frame or matrix of data with missing data coded as "NA".
#' @param xlab a title for the x axis: see 'title'
#' @param ylab a title for the y axis: see 'title'
#' @param main an overall title fo the plot: see 'title'
#' @param mar Numeric vector with length 4 giving the margin sizes. Defaults to
#'   \code{mar=c(5.1, 8.1, 4.1, 2.1)}
#' @param ... Not used
#' @details A simple wrapper for \code{mi:::missing.pattern.plot} with some default
#'   argument values changed.
#' @export missplot
missplot <- function(data, xlab="Index", ylab="Variable", main="",
                     mar=c(5.1, 8.1, 4.1, 2.1), ...){
  oldpar <- par(no.readonly=TRUE)
  on.exit(oldpar)
  par(mar=mar)
  mp.plot(data, y.order=TRUE, x.order=TRUE, mis.col="orange", xlab=xlab, ylab=ylab, main=main)
}

# Recent versions of the mi package refactor the code and I don't want to work with
# new classes of data.frames. The following is taken directly from version 0.09-19
# of the mi package.
# ==============================================================================
# missing pattern plot
# ==============================================================================
mp.plot <- missing.pattern.plot <- function ( data, y.order = FALSE, x.order = FALSE, 
                                              clustered = TRUE, 
                                              xlab = "Index", ylab = "Variable", 
                                              main = NULL, gray.scale = FALSE,
                                              obs.col = "blue", mis.col = "red", ... ) {
  
  if (is.null(main)) {
    main <- deparse( substitute( data ) )
  }
  index <- seq(nrow(data))
  x.at =  1:nrow( data )
  x.lab = index
  if( y.order ) { 
    data <- data[ ,order( colSums( is.na( data ) ), decreasing = TRUE ) ] 
    ylab = "Ordered by number of missing items" 
  }
  if( x.order ) { 
    data <- data[order( rowSums( is.na( data ) ), decreasing = FALSE ), ] 
    index<- row.names( data )
    xlab = "Ordered by number of missing items" 
    x.at = NULL
    x.lab= FALSE
  }
  missingIndex <- as.matrix(is.na(data))*1
  if(clustered){
    orderIndex <-  order.dendrogram(as.dendrogram(hclust(dist(missingIndex), method="mcquitty")))
    missingIndex <- missingIndex[orderIndex,]
  }
  col <- if( gray.scale ){ 
    gray(c(0, 1)) 
  } 
  else { 
    c(obs.col, mis.col) 
  }
  #  par( mar = c( 4.5, 11, 3, 1 ) )
  #  par( mgp = c( 1, .3, 0 ) )
  #  par( cex.lab = 0.7 )
  image(x = 1:nrow(data), y = 1:ncol(data), z = missingIndex, 
        ylab = "", xlab = xlab, main = main, col = col ,yaxt = "n",
        tck = -0.05, xaxt="n", ...)
  box( "plot" )
  axis( side = 2, at = 1:ncol( data ), labels = names( data ), las = 1, 
        tick = FALSE, yaxs = "r", tcl = 0.3, xaxs ="i", yaxs = "i" )
  mtext( ylab, side = 3 , line = 10, cex=0.7)
  if( x.order ) { 
    axis( side = 1, at =x.at, labels = x.lab, tick = FALSE, 
          xaxs = "i", las = 1 )   
  } 
}
