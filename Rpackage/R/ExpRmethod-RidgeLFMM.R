##' @export
method_ridgeLFMM <- function(K, lambda = 1e-4, col.mask = NULL,
                             inter.res.saving.file = NULL, inter.res.file = NULL) {
  args <- list(K = K,
               lambda = lambda,
               col.mask = col.mask,
               inter.res.file = inter.res.file,
               inter.res.saving.file = inter.res.saving.file)
  args$name = "ridgeLFMM"
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_ridgeLFMM", class(res))
  res
}

##' @export
ExpRmouline.method_ridgeLFMM <- function(m, dat) {

  ## ridge lfmm main
  main.fun <- function(m, dat) {
    lfmm <- MatrixFactorizationR::ridgeLFMM(K = m$K,
                                            lambda = m$lambda)
    lfmm <- MatrixFactorizationR::MatrixFactorizationR_fit(lfmm, dat)
    lfmm
  }

  method_main(m, dat, main.fun, hp.func = MatrixFactorizationR::hypothesis_testing_lm)
}

##' @export
method_CV_ridgeLFMM <- function(n.fold.row, n.fold.col, lambdas , Ks) {

  args <- as.list(match.call())[-1]
  args$name = "CV ridgeLFMM"
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_CV_ridgeLFMM", class(res))
  res
}

##' @export
ExpRmouline.method_CV_ridgeLFMM <- function(m, dat) {
  lfmm <- MatrixFactorizationR::ridgeLFMM(K = NULL,
                                          lambda = NULL)
  m$errs <- MatrixFactorizationR::MatrixFactorizationR_CV(m  = lfmm, dat = dat,
                                                          n.fold.row = m$n.fold.row,
                                                          n.fold.col = m$n.fold.col,
                                                          Ks = m$Ks,
                                                          lambdas = m$lambdas)
  m
}

