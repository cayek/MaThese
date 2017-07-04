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

  ## param
  K <- ncol(dat$U)
  n <- nrow(dat$U)
  p <- nrow(dat$V)

  ## compute U
  Af <- function(x, args) {
    args$U %*% crossprod(args$V, x)
  }
  Atransf <- function(x, args) {
    args$V %*% crossprod(args$U, x)
  }
  res.rspectra <- RSpectra::svds(A = Af,
                                 Atrans = Atransf,
                                 k = K,
                                 nu = K, nv = K,
                                 opts = list(tol = 10e-10),
                                 dim = c(n, p),
                                 args = list(U = dat$U, V = dat$V))

  ## run hypothesis testing
  m$U <- res.rspectra$u %*% diag(res.rspectra$d[1:K], K, K)
  m$V <- res.rspectra$v
  X <- cbind(dat$X, m$U)
  d <- ncol(dat$X)
  hp <- hypothesis_testing_lm(dat, X = X)

  m$pvalue <- hp$pvalue[,1:d, drop = FALSE]
  m

}
