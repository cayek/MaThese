library(testthat)
context("test matter")

test_that("test matter", {

  ## param
  K <- 5
  cs <- c(0.8, 0.5)
  s <- ExpRsampler_generativeData(n = 100, p = 1000, K = K, outlier.prop = 0.05, cs = cs)
  dat <- ExpRmouline(s)

  ## matter dat Y
  dat.matter <- LfmmMatterDat(NULL, NULL, NULL)
  dat.matter$Y <- matter::matter_mat(dat$Y, ncol = ncol(dat$Y), nrow = nrow(dat$Y))
  dat.matter$X <- dat$X

  ## test prod
  x <- matrix(1,100, 1)
  aux1 <- dat.matter$productYt(x)
  aux2 <- dat$productYt(x)
  expect_lte(mean(abs(aux1 - aux2 )), 1e-10)

  x <- matrix(1,1000, 1)
  aux1 <- dat.matter$productY(x)
  aux2 <- dat$productY(x)
  expect_lte(mean(abs(aux1 - aux2 )), 1e-10)

  hp1 <- MatrixFactorizationR::hypothesis_testing_lm(dat, dat$X)
  hp2 <- MatrixFactorizationR::hypothesis_testing_lm(dat.matter, dat.matter$X)
  expect_lte(mean(abs(hp1$pvalue - hp2$pvalue)), 1e-10)

})
