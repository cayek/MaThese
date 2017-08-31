##' @export
method_lassoLFMM <- function(K, col.mask = NULL,
                             inter.res.saving.file = NULL, inter.res.file = NULL,
                             nozero.prop = 0.1,
                             lambda.num = 100, lambda.min.ratio = 0.001,
                             lambda = NULL,
                             it.max = 100, relative.err.epsilon = 1e-6) {
  args <- list(K = K,
               nozero.prop = nozero.prop,
               lambda.num = lambda.num,
               lambda = lambda,
               lambda.min.ratio = lambda.min.ratio,
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
    lfmm <- lfmm::lassoLFMM(K = m$K,
                                            nozero.prop = m$nozero.prop,
                                            lambda.min.ratio = m$lambda.min.ratio,
                                            lambda.num = m$lambda.num,
                                            lambda = m$lambda)
    lfmm <- lfmm::lfmm_fit(lfmm, dat, it.max = m$it.max,
                                                           relative.err.epsilon = m$relative.err.epsilon)
    lfmm
  }
  method_main(m, dat, main.fun, hp.func = lfmm::hypothesis_testing_lm)
}
