# Function 'borrowed' from package metrumrg
# That package didn't want to install on my system, and I only wanted this function.
# Author: Tim Bergsma
# URL: http://metrumrg.googlecode.com

ftable2data.frame <- function(x,...){
  y <- format(x,quote=FALSE)
  z <- data.frame(y[-1,],stringsAsFactors=FALSE)
  names(z) <- y[1,]
  z
}
