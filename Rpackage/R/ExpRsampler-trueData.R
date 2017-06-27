## sampler
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @author cayek
##' @export
ExpRsampler_trueData <- function(Y, X, outlier) {
  args <- as.list(match.call())[-1]
  res <- do.call(ExpRsampler, args)
  class(res) <- c("ExpRsampler_trueData", class(res))
  res
}

##' @export
ExpRmouline.sampler_trueData <- function(s) {
  if (!is.null(X)) {
    dat <- MatrixFactorizationR::LfmmDat(s$Y, s$X)
  } else {
    dat <- MatrixFactorizationR::Dat(s$Y, s$X)
  }
  if (!is.null(outlier)) {
    dat$meta <- MatrixFactorizationR::read_input(s$outlier)
  }
}
