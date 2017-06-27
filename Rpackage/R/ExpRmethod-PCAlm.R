##' @export
method_PCA <- function(K, scale = FALSE) {
  args <- as.list(match.call())[-1]
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
  m$u <- res.rspectra$u
  m$d <- res.rspectra$d
  m$v <- res.rspectra$v
  m
}

