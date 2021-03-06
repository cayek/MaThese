##' @export
method_PCA <- function(scale = FALSE, lambda = NULL, K = NULL) {
  args <- list(scale = scale,
               lambda = lambda,
               K = K)
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

  if (!is.null(m$lambda)) {
    message("Lambda = ", m$lambda)
    P.list <- lfmm::compute_P(dat$X, m$lambda)
  } else {
    P.list <- list(sqrt.P = diag(1, nrow(dat$Y)))
  }

  if (is.null(m$K)) {
    res <- svd(P.list$sqrt.P %*% dat$Y, 0, 0)
  } else {
    message("Using RSpectra")
    Af <- function(x, args) {
      P.list$sqrt.P %*% dat$productY(x)
    }
    Atransf <- function(x, args) {
      dat$productYt(t(P.list$sqrt.P) %*% x)
    }
    res <- RSpectra::svds(A = Af,
                          Atrans = Atransf,
                          k = m$K,
                          nu = m$K, nv = m$K,
                          opts = list(tol = 10e-10),
                          dim = c(nrow(dat$Y), ncol(dat$Y)))
  }

  m$d <- res$d
  m
}

##' @export
method_PCAlm <- function(K, col.mask = NULL,
                         inter.res.saving.file = NULL, inter.res.file = NULL) {
  args <- list(K = K, col.mask = col.mask,
               inter.res.saving.file = inter.res.saving.file,
               inter.res.file = inter.res.file)
  args$name = "PCAlm"
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_PCAlm", class(res))
  res
}

##' @export
ExpRmouline.method_PCAlm <- function(m, dat) {

    ## ridge lfmm main
  main.fun <- function(m, dat) {
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
    m
  }

  method_main(m, dat, main.fun, hp.func = lfmm::hypothesis_testing_lm)
}

