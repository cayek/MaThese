library(testthat)
context("test bigstatrs")

test_that("test matter", {

  skip_if_not_installed("bigstatsr")

  ## param
  K <- 5
  cs <- c(0.8, 0.5)
  s <- ExpRsampler_generativeData(n = 100, p = 1000, K = K, outlier.prop = 0.05, cs = cs)
  dat <- ExpRmouline(s)

  ## matter dat Y
  dat.big <- LfmmBigStatsrDat(NULL, NULL, NULL)
  dat.big$Y <- bigmemory::as.big.matrix(dat$Y)
  dat.big$X <- dat$X

  ## test prod
  x <- matrix(1,100, 1)
  aux1 <- dat.big$productYt(x)
  aux2 <- dat$productYt(x)
  expect_lte(mean(abs(aux1 - aux2 )), 1e-10)

  x <- matrix(1,1000, 1)
  aux1 <- dat.big$productY(x)
  aux2 <- dat$productY(x)
  expect_lte(mean(abs(aux1 - aux2 )), 1e-10)

  hp1 <- lfmm::hypothesis_testing_lm(dat, dat$X)
  hp2 <- lfmm::hypothesis_testing_lm(dat.big, dat.big$X)
  expect_lte(mean(abs(hp1$pvalue - hp2$pvalue)), 1e-10)

})
