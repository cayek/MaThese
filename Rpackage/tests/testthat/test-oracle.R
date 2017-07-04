library(testthat)
context("oracle")

test_that("test oracle", {

  Y <- "./Data/ThesisDataset/3Article/1000GenomesPhase3/1000GenomesPhase3_QC_norel_prunned_scaled_noNA_samplep1000n100.rds"
  skip_if_not(file.exists(Y))

  ## param
  K <- 2
  cs <- cs_sampler(K)

  ## with expRiment
  s <- ExpRsampler_fromTrueData(Y = Y, K = K, prop.outlier = 0.1, cs = cs, rho.B = 3.0)
  dat <- ExpRmouline(s)

  ## methods
  m <- method_oracle()

  ## run
  m <- ExpRmouline(m, dat)

  skip("plots")
  id <- 1:ncol(dat$Y)
  qplot(id, -log10(m$pvalue[,1]), color = id %in% dat$outlier)

})

