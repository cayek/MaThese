##' @export
method_oracle <- function() {
  args <- list()
  args$name = "oracle"
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_oracle", class(res))
  res
}

##' @export
ExpRmouline.method_oracle <- function(m, dat) {

  ## run hypothesis testing
  X <- cbind(dat$X, dat$U)
  d <- ncol(dat$X)
  hp <- hypothesis_testing_lm(dat, X = X)

  m$pvalue <- hp$pvalue[,1:d, drop = FALSE]
  m

}
