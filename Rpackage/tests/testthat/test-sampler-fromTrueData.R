library(testthat)
context("sampler from true data")

test_that("ExpRsampler_fromTrueData", {

  Y <- "./Data/ThesisDataset/3Article/1000GenomesPhase3/1000GenomesPhase3_QC_norel_prunned_scaled_noNA_samplep1000n100.rds"
  skip_if_not(file.exists(Y))

  ## samplers
  K <- 3
  cs <- cs_sampler(K = K)

  ## test empty
  s <- ExpRsampler_fromTrueData(Y = Y, K = K, prop.outlier = 0.1, cs = cs)
  sampler.env <- s$load.env

  sampler.env$empty(sampler.env)
  names(s$load.env)

  ## test ExpRmouline
  s <- ExpRsampler_fromTrueData(Y = Y, K = K, prop.outlier = 0.1, cs = cs, n = 50, p = 500)
  dat <- ExpRmouline(s)

  dim(dat$Y)

  ## with expRiment
  s <- ExpRsampler_fromTrueData(Y = Y, K = K, prop.outlier = 0.1, cs = cs)
  samplers <-  s * param(K = c(3,4))
  names(samplers[[2]]$load.env)

})

test_that("ExpRsampler_fromTrueData comp with ThesisRpackage", {

  K <- 4
  lambda <- 1e-4

### with ThesisRpackage
  G.file <- "./Data/ThesisDataset/3Article/1000GenomesPhase3/Y_sampler.rds"
  pca.file <- NULL
  skip_if_not(file.exists(G.file))
  skip_if_not_installed("ThesisRpackage")
  require(ThesisRpackage)

  ## sample ThesisRpackage
  cs = c(0.4, 0.2, -0.1, 0.0)
  s.old <- FromTrueSampler2(G.file = G.file,
                        K = K,
                        prop.outlier = 0.1,
                        cs = cs,
                        pca.file = pca.file,
                        rho.B = 1.0)
  s.old <- Sampler_load(s.old)
  set.seed(0)
  dat.old <- sampl(s.old)


  ## sampler MaTheseR
  s.new <- ExpRsampler_fromTrueData(G.file, K, prop.outlier = 0.1, cs = cs)
  set.seed(0)
  dat.new <- ExpRmouline(s.new)

  ## c'est pas les même valuer j'espere que c'est a cause de prcomp au lieu de svd...
  expect_equal(mean(abs(dat.new$outlier - dat.old$outlier)), 1e-15)
  expect_equal(mean(abs(dat.new$B - t(dat.old$B))), 1e-15)
  expect_equal(mean(abs(dat.new$U - dat.old$U)), 1e-15)
  expect_equal(mean(abs(dat.new$X - dat.old$X)), 1e-15)

### comp svd and pca
  U.old <- s.old$U
  U.new <- s.new$load.env$svd$u[,1:s$K] %*% diag(s.new$load.env$svd$d[1:s$K])
  sqrt(diag(cov(U.old)))
  sqrt(diag(cov(U.new)))

})

