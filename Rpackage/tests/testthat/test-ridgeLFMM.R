library(testthat)
context("RidgeLFMM")

test_that("test CV", {

  Y <- "./Data/ThesisDataset/3Article/1000GenomesPhase3/1000GenomesPhase3_QC_norel_prunned_scaled_noNA_samplep1000n100.rds"
  X <- matrix(rnorm(100), 100, 1)
  skip_if_not(file.exists(Y))

  ## samplers
  samplers <- ExpRsampler_trueData(Y = Y, X = X, outlier = NULL) * param()
  s <- samplers[[1]]
  dat <- ExpRmouline(s)
  dat$Y <- preprocessing_filter_na(dat$Y)

  ## methods
  methods <- method_CV_ridgeLFMM(n.fold.col = 5, n.fold.row = 2, lambdas = c(1e-10, 1, 1e10),
                                 Ks = c(1, 3, 10)) * param()
  m <- methods[[1]]
  m <- ExpRmouline(m, dat)

  ## expr
  expr <- ExpR(rep.nb = 2,
               samplers = samplers,
               methods = methods,
               preprocessors = NULL,
               extractor = ExpRextractor_CV)
  expr <- ExpRmouline(expr)

  skip("plots")
  ExpRplot_CV_ridgeLFMM(expr, major = "K")
  ExpRplot_CV_ridgeLFMM(expr, major = "lambda")
})

