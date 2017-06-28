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

#' @export 
preprocessing_filter_sd <- function(Y, sd.threshold = 0, plot = FALSE) {
  sds <- apply(Y, 2, sd)
  if(plot) {
    hist(sds)
  }
  out <- sds <= sd.threshold
  message("proportion of removed loci = ", mean(out))
  Y[,!out]
}

#' @export 
preprocessing_filter_maf <- function(Y, maf.threshold = 0.05, plot = FALSE) {
  ploidy <- max(Y)
  maf <- apply(Y, 2, function(l){p <- mean(l) / ploidy;min(p, 1 - p)})
  if(plot) {
    hist(maf)
  }
  out <- maf <= maf.threshold
  message("proportion of removed loci = ", mean(out))
  Y[,!out]
}
