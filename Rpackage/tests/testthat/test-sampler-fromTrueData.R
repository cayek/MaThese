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
  s <- FromTrueSampler2(G.file = G.file,
                        K = K,
                        prop.outlier = 0.1,
                        cs = cs,
                        pca.file = pca.file,
                        rho.B = 1.0)
  set.seed(0)
  dat <- sampl(s)


  ## sampler MaTheseR
  s <- ExpRsampler_fromTrueData(G.file, K, prop.outlier = 0.1, cs = cs)
  set.seed(0)
  dat.rc <- ExpRmouline(s)

  ## c'est pas les même valuer j'espere que c'est a cause de prcomp au lieu de svd...
  mean(abs(dat.rc$outlier - dat$outlier))
  mean(abs(dat.rc$B - t(dat$B)))
  mean(abs(dat.rc$X - dat$X))

})

