#' Format p-values nicely with trailing 0s retained and small values stated to be beneath threshold
#' @param p A vector of p-values
#' @param digits The number of digits to round to. Defaults to 3
nicePvalues <- function(p, digits=3, latex=TRUE){
   p <- ifelse(p < 10^(-digits), paste0("<", 10^(-digits)), sprintf(paste0("%.", digits, "f"), round(p, digits)))
   if (latex) p <- gsub("<", "$<$", p)
   p
}

