##' @export
method_PCA <- function(scale = FALSE) {
  args <- list(scale = scale)
  args$name = "PCA"
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_PCA", class(res))
  res
}

##' @export
ExpRmouline.method_PCA <- function(m, dat) {

  p <- ncol(dat$Y)
  n <- nrow(dat$Y)

  ## scale
  if (m$scale) {
    message("==Scaling")
    dat$Y <- scale(dat$Y)
  }

  res <- svd(dat$Y, 0, 0)

  m$d <- res$d / sum(res$d)
  m
}

##' @export
method_PCAlm <- function(K) {
  args <- list(K = K)
  args$name = "PCAlm"
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_PCAlm", class(res))
  res
}

##' @export
ExpRmouline.method_PCAlm <- function(m, dat) {

  n <- nrow(dat$Y)
  p <- ncol(dat$Y)
  ## rum svd
  Af <- function(x, args) {
    args$dat$productY(x)
  }
  Atransf <- function(x, args) {
    args$dat$productYt(x)
  }
  res.rspectra <- RSpectra::svds(A = Af,
                                 Atrans = Atransf,
                                 k = m$K,
                                 nu = m$K, nv = m$K,
                                 opts = list(tol = 10e-10),
                                 dim = c(n, p),
                                 args = list(dat = dat))
  m$U <- res.rspectra$u %*% diag(res.rspectra$d[1:m$K], m$K, m$K)
  m$V <- res.rspectra$v

  ## run hypothesis testing
  X <- cbind(dat$X, m$U)
  d <- ncol(dat$X)
  hp <- hypothesis_testing_lm(dat, X = X)

  m$score <- hp$score[,1:d, drop = FALSE]
  m$pvalue <- hp$pvalue[,1:d, drop = FALSE]
  m

}

