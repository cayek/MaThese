library(testthat)
context("validation numerique article 3")

test_that("true dataset article 3 sur un petit sample", {

  ## param
  K.method <- 25
  nb.cluster <- 1
  rep.nb.sampler <- 1
  lambda <- 1e-5
  nozero.prop <- 0.01


  Y <- "./Data/GSE42861/betanormalized_metylationlvl.sample.rds"
  X <- "./Data/GSE42861/X.sample.rds"
  skip_if_not(file.exists(Y))
  X <- readRDS(X)[,1, drop = FALSE]
  s <- ExpRsampler_trueData(Y = Y, X = X, outlier = c(1))
  dat <- ExpRmouline(s)
  samplers <- s * param()

  ## methods
  m.ridgeLfmm <- method_ridgeLFMM(K = K.method)
  m.lasso <- method_lassoLFMM(K = K.method, nozero.prop = nozero.prop,
                              lambda.K = 25, relative.err.epsilon = 1e-6)
  m.lm <- method_lm()
  m.pca <- method_PCAlm(K = K.method)
  m.cate <- method_cate(K = K.method)
  m.famt <- method_famt(K.method)
  m.sva <- method_sva(K.method)

  methods <- m.ridgeLfmm * param() +
    m.lm * param() +
    m.pca * param() + 
    m.cate * param() +
    m.famt * param() +
    m.sva * param() + 
    m.lasso * param()

  ## run
  expr <- ExpR(rep.nb.sampler = rep.nb.sampler,
               samplers = samplers,
               preprocessors = NULL,
               rep.nb.method = 1,
               methods = methods,
               extractor = ExpRextractor_pvalue1_calibrated)
  expr <- ExpRmouline(expr)

  skip('plot and print')

  ## print median and mad
  expr$df.res %>%
    group_by(method) %>%
    summarise(mad = mad[1], median = median[1])

  ## qqplot
  toplot <- expr$df.res %>%
    mutate(pvalue = calibrated.pvalue)
  plot_qqplot(toplot)

  ## filter top
  toplot <- expr$df.res %>%
    group_by(method) %>%
    filter_candidates_top(10)
  plot_intersection(toplot, by = "colname", plot = "tile")

  ## filter threshold
  toplot <- expr$df.res %>%
    group_by(method) %>%
    filter_candidates_threshold(0.5)
  plot_intersection(toplot, by = "colname", plot = "point")

  ## venn diagram
  toplot <- expr$df.res %>%
    group_by(method) %>%
    filter_candidates_top(10)
  sets <- list(lm = toplot$index[toplot$method == "lm"],
               cate = toplot$index[toplot$method == "cate"],
               lassoLFMM = toplot$index[toplot$method == "lassoLFMM"],
               ridgeLFMM = toplot$index[toplot$method == "ridgeLFMM"],
               PCAlm = toplot$index[toplot$method == "PCAlm"]
               )
  venn.pl <- plot_venn(sets[1:5])
  grid.draw(venn.pl)

})

test_that("mutate_annotation", {

  G.fake <- readRDS("./Data/ThesisDataset//3Article/Celiac/G_fake.rds")
  candidates <- readRDS("./Data/ThesisDataset//3Article/Celiac/gwas_catalog_candidates.rds")
  df <- tibble::tibble(snps = colnames(G.fake)[candidates])

  df <- df %>%
    mutate_annotation()

  df %>%
    dplyr::select(phenotype_name, phenotype_description, chr_name)
  ## RMK: we retrieve CELIAC desease
})
