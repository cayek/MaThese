##' @export
method_lassoLFMM <- function(K, nozero.prop = 0.1,
                             lambda.K = 100, lambda.eps = 0.001,
                             it.max = 100, relative.err.epsilon = 1e-6) {
  args <- list(K = K,
               nozero.prop = nozero.prop,
               lambda.K = lambda.K,
               lambda.eps = lambda.eps,
               it.max = it.max,
               relative.err.epsilon = relative.err.epsilon)
  args$name = "lassoLFMM"
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_lassoLFMM", class(res))
  res
}

##' @export
ExpRmouline.method_lassoLFMM <- function(m, dat) {
  ## rum lfmm
  lfmm <- MatrixFactorizationR::lassoLFMM(K = m$K,
                                          nozero.prop = m$nozero.prop,
                                          lambda.eps = m$lambda.eps,
                                          lambda.K = m$lambda.K)
  lfmm <- MatrixFactorizationR::MatrixFactorizationR_fit(lfmm, dat, it.max = m$it.max,
                                                         relative.err.epsilon = m$relative.err.epsilon)
  m[names(lfmm)] <- lfmm

  ## run hypothesis testing
  X <- cbind(dat$X, m$U)
  d <- ncol(dat$X)
  hp <- hypothesis_testing_lm(dat, X = X)

  m$pvalue <- hp$pvalue[,1:d, drop = FALSE]
  m
}
