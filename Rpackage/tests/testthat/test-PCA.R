library(testthat)
context("PCA")

test_that("test pca", {

  Y <- "./Data/ThesisDataset/3Article/1000GenomesPhase3/1000GenomesPhase3_QC_norel_prunned_scaled_noNA_samplep1000n100.rds"
  skip_if_not(file.exists(Y))

  ## samplers
  samplers <- ExpRsampler_trueData(Y = Y, X = NULL, outlier = NULL) * param()
  s <- samplers[[1]]
  dat <- ExpRmouline(s)
  dat$Y <- preprocessing_filter_na(dat$Y)

  ## methods
  methods <- method_PCA(K = 50, scale = TRUE) * param()
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

