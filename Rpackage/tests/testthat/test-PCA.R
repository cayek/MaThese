library(testthat)
context("PCA")

test_that("test pca", {

  Y <- "./Data/ThesisDataset/3Article/1000GenomesPhase3/1000GenomesPhase3_QC_norel_prunned_scaled_noNA_samplep1000n100.rds"
  skip_if_not(file.exists(Y))

  ## samplers
  samplers <- ExpRsampler_fromTrueData(Y = Y, K = 5, prop.outlier = 0.1)* param()
  s <- samplers[[1]]
  dat <- ExpRmouline(s)
  dat$Y <- preprocessing_filter_na(dat$Y)

  ## methods
  methods <- method_PCA(scale = TRUE) * param(lambda = c(1e-5,1,1e5))
  m <- methods[[1]]
  m <- ExpRmouline(m, dat)

  ## expr
  expr <- ExpR(rep.nb.sampler = 1,
               samplers = samplers,
               rep.nb.method = 1,
               methods = methods,
               preprocessors = NULL,
               extractor = ExpRextractor_sing_values)
  expr <- ExpRmouline(expr)

  skip("plots")
  ExpRplot_sing_values(expr)
})

test_that("test pca on simulation", {

  s <- ExpRsampler_generativeData(n = 100,
                                  p = 1000,
                                 K = 3,
                                  cs = c(0.8, 0.5,0.3),
                                  outlier.prop = 0.2)
  dat <- ExpRmouline(s)

  m <- method_PCA(lambda = 1e-5)
  m.res <- ExpRmouline(m, dat)

  mK <- method_PCA(lambda = 1e-5, K = 30)
  mK.res <- ExpRmouline(mK, dat)

  expect_lte(mean(abs(m.res$d[1:30] - mK.res$d)), 1e-3)

})

