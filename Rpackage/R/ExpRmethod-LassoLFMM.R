##' @export
method_lassoLFMM <- function(K, col.mask = NULL,
                             inter.res.saving.file = NULL, inter.res.file = NULL,
                             nozero.prop = 0.1,
                             lambda.K = 100, lambda.eps = 0.001,
                             it.max = 100, relative.err.epsilon = 1e-6) {
  args <- list(K = K,
               nozero.prop = nozero.prop,
               lambda.K = lambda.K,
               lambda.eps = lambda.eps,
               it.max = it.max,
               relative.err.epsilon = relative.err.epsilon,
               inter.res.saving.file = inter.res.saving.file,
               inter.res.file = inter.res.file,
               col.mask = col.mask
               )
  args$name = "lassoLFMM"
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_lassoLFMM", class(res))
  res
}

##' @export
ExpRmouline.method_lassoLFMM <- function(m, dat) {


  ## lasso lfmm main
  main.fun <- function(m, dat) {
    if (is.null(m$nozero.prop) && !is.null(dat$outlier)) {
      m$nozero.prop <- length(dat$outlier) / ncol(dat$Y) * 1.5
    }

  ## rum lfmm
    lfmm <- MatrixFactorizationR::lassoLFMM(K = m$K,
                                            nozero.prop = m$nozero.prop,
                                            lambda.eps = m$lambda.eps,
                                            lambda.K = m$lambda.K)
  lfmm <- MatrixFactorizationR::MatrixFactorizationR_fit(lfmm, dat, it.max = m$it.max,
                                                         relative.err.epsilon = m$relative.err.epsilon)
    lfmm
  }
  method_main(m, dat, main.fun, hp.func = MatrixFactorizationR::hypothesis_testing_lm)
}
