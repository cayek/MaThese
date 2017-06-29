## Generative dataset
##'
##' Generate data from the generative model of lfmm.
##' 
##' @author cayek
##' @export
ExpRsampler_generativeData <- function(n, p, K,
                                       outlier.prop,
                                       cs,
                                       sigma = 0.2,
                                       B.sd = 1.0,
                                       B.mean = 0.0,
                                       U.sd = 1.0,
                                       V.sd = 1.0) {
  args <- list(n = n, p = p, K = K,
               outlier.prop = outlier.prop,
               cs = cs,
               sigma = sigma,
               B.sd = B.sd,
               B.mean = B.mean,
               U.sd = U.sd,
               V.sd = V.sd)

  s <- do.call(ExpRsampler, args)
  class(s) <- c("ExpRsampler_generativeData", class(s))

  s
}

##' @export
ExpRmouline.ExpRsampler_generativeData <- function(s) {
  dat <- do.call(lfmm_sampler, as.list(s))
  dat$meta <- list(prop.outlier = s$outlier.prop)
  dat
}

