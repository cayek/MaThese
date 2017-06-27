## Sample True data
##'
##' Sample true data set from file.
##'
##' @author cayek
##' @export
ExpRsampler_trueData <- function(Y, X, outlier) {
  args <- as.list(match.call())[-1]
  res <- do.call(ExpRsampler, args)
  class(res) <- c("ExpRsampler_trueData", class(res))
  res
}

##' @export
ExpRmouline.ExpRsampler_trueData <- function(s) {
  if (!is.null(s$X)) {
    dat <- MatrixFactorizationR::LfmmDat(s$Y, s$X)
  } else {
    dat <- MatrixFactorizationR::Dat(s$Y)
  }
  if (!is.null(s$outlier)) {
    dat$meta <- MatrixFactorizationR::read_input(s$outlier)
  }
  dat
}
