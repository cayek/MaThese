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


test_that("test ridge lfmm", {

  Y <- "./Data/ThesisDataset/3Article/1000GenomesPhase3/1000GenomesPhase3_QC_norel_prunned_scaled_noNA_samplep1000n100.rds"
  skip_if_not(file.exists(Y))

  ## param
  K <- 2
  cs <- c(0.5,0.1)

  ## with expRiment
  s <- ExpRsampler_fromTrueData(Y = Y, K = K, prop.outlier = 0.05, cs = cs, rho.B = 1.0)
  dat <- ExpRmouline(s)
  samplers <-  s * param(K = c(3,4))

  ## methods
  m <- method_ridgeLFMM(K = K)
  methods <- m * param(K = c(2,3))

  ## run
  m <- ExpRmouline(m, dat)

  skip("plots")
  id <- 1:ncol(dat$Y)
  qplot(id, -log10(m$pvalue[,1]), color = id %in% dat$outlier)

})

test_that("test ridge lfmm avec ThesisRpackage", {

  K <- 4
  lambda <- 1e-4

  ### with ThesisRpackage
  G.file <- "~/Projects/Thesis/Data/1000Genomes/Phase3/European_Chrm22.maf.05.sample.10000.rds"
  pca.file <- "~/Projects/Thesis/Data/1000Genomes/Phase3/European_Chrm22.maf.05.sample.10000_PCA.rds"
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
  dat <- sampl(s)
  dat.rc <- MatrixFactorizationR::SimulatedLfmmDat(Y = dat$G,
                                                   X = dat$X,
                                                   outlier = dat$outlier,
                                                   U = dat$U,
                                                   V = dat$V,
                                                   B = dat$B)

  ## methods
  m <- method_ridgeLFMM(K = K, lambda = lambda)
  m.old <- finalLfmmRdigeMethod(K = K, lambda = lambda)
  m.old$center <- FALSE
  ## run
  m <- ExpRmouline(m, dat.rc)
  m.old <- fit(m.old, dat)

  skip("plots")
  id <- 1:ncol(dat.rc$Y)
  qplot(id, m$B[,1], color = id %in% dat$outlier)
  x11()
  qplot(id, m.old$B[1,], color = id %in% dat$outlier)

})
