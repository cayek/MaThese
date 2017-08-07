library(testthat)
context("Sampler generative data")

test_that("ExpRsampler_generativeData_BVcorrelated", {
  
  s <- ExpRsampler_generativeData_BVcorrelated(n = 100,
                                               p = 1000,
                                               K = 3,
                                               outlier.prop = 0.6,
                                               csXU = 0.6,
                                               csBV = 0.9)
  dat <- ExpRmouline.ExpRsampler_generativeData_BVcorrelated(s)

  ## linear reg
  m <- method_lm() %>%
    ExpRmouline(dat)
  id <- 1:ncol(dat$Y)
  ggplot2::qplot(x = id, y = m$B.hp, color = id %in% dat$outlier)


  ## B
  rlm.res <- MASS::rlm(m$B.hp ~ dat$V)
  B <- rlm.res$residuals
  ggplot2::qplot(x = id, y = B, color = id %in% dat$outlier)

  ## lfmm ridge
  m <- method_ridgeLFMM(K = 3) %>%
    ExpRmouline(dat)
  id <- 1:ncol(dat$Y)
  ggplot2::qplot(x = id, y = -log10(m$pvalue), color = id %in% dat$outlier)


  ## cate
  m <- method_cate(K = 3) %>%
    ExpRmouline(dat)
  id <- 1:ncol(dat$Y)
  x11()
  ggplot2::qplot(x = id, y = -log10(m$pvalue), color = id %in% dat$outlier)

})
