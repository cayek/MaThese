##' @export
method_ridgeLFMM <- function(K, lambda = 1e-4) {
  args <- as.list(match.call())[-1]
  args$name = "ridgeLFMM"
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_ridgeLFMM", class(res))
  res
}

##' @export
ExpRmouline.method_ridgeLFMM <- function(m, dat) {
  lfmm <- MatrixFactorizationR::ridgeLFMM(K = m$K,
                                       lambda = m$lambda)
  lfmm <- MatrixFactorizationR::MatrixFactorizationR_fit(m, dat)
  m[names(lfmm)] <- lfmm
  m
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

