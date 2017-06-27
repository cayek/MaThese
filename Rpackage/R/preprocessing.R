#' @export 
preprocessing_filter_na <- function(Y, na.threshold = 0, plot = FALSE) {
  nas <- apply(Y, 2, function(l){mean(is.na(l))})
  if(plot) {
    hist(nas)
  }
  out <- nas > na.threshold
  message("proportion of removed loci = ", mean(out))
  Y[,!out]
}
