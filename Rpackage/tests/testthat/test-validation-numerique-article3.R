library(testthat)
context("validation numerique article 3")

test_that("validation numerique article 3 sur un petit sample", {

  Y <- "./Data/ThesisDataset/3Article/1000GenomesPhase3/Y_sampler.rds"
  skip_if_not(file.exists(Y))

  ## param
  K <- 5

  ## with expRiment
  cs_func <- function(K) {
    cs <- rep(0, K)
    cs[1] <- 0.8
    cs[2] <- 0.5
    cs
  }
  s <- ExpRsampler_fromTrueData(Y = Y, K = K, prop.outlier = 0.05, cs = cs_func, rho.B = 1.0)
  dat <- ExpRmouline(s)
  sampler.env <- s$load.env
  samplers <-  s * param(prop.outlier = c(0.01, 0.1, 0.3))

  ## methods
  m.ridgeLfmm <- method_ridgeLFMM(K = K)
  m.lm <- method_lm()
  m.pca <- method_PCAlm(K = K)
  m.cate <- method_cate(K = K)
  m.famt <- method_famt(K)
  m.sva <- method_sva(K)

  ## m <- m.sva
  ## m <- ExpRmouline(m, dat)
  ## hist(m$pvalue[,1])

  methods <- m.ridgeLfmm * param() +
    m.lm * param() +
    m.pca * param() + 
    m.cate * param() + 
    m.famt * param() + 
    m.sva * param() 


  ## run
  ## cl <- parallel::makeCluster(2, outfile = "")
  ## doParallel::registerDoParallel(cl)
  expr <- ExpR(rep.nb.sampler = 2,
               samplers = samplers,
               preprocessors = NULL,
               rep.nb.method = 1,
               methods = methods,
               extractor = ExpRextractor_fdr,
               sampler.env = sampler.env)
  expr <- ExpRmouline(expr)
  ## doParallel::stopImplicitCluster()
  ## parallel::stopCluster(cl)

  ## plot
  skip("plots")

  ## AUC
  toplot <- expr$df.res %>%
    dplyr::filter(pvalue.index == "pvalue1")
  plot_AUC_prop_outlier(toplot)

})

