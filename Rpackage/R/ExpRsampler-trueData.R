## Sample True data
##'
##' Sample true data set from file.
##'
##' @author cayek
##' @export
ExpRsampler_trueData <- function(Y, X, outlier) {
  args <- list(Y = Y, X = X, outlier = outlier)
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
  ## outlier
  dat$meta$outlier <- c()
  if (!is.null(s$outlier)) {
    dat$meta$outlier <- MatrixFactorizationR::read_input(s$outlier)
  }
  dat
}
