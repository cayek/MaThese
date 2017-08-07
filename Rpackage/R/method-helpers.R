method_main <- function(m, dat, main.fun, hp.func = NULL) {

  ## mask data
  if (!is.null(m$col.mask)) {
    message("mask data")
    dat.masked <- LfmmDat(Y = NULL, X = dat$X, missing = FALSE)
    dat.unmasked <- dat
    dat.masked$Y <- dat.unmasked$Y[,col.mask]
    dat <- dat.masked
  }

  ## run main
  if (!is.null(m$inter.res.file) && file.exists(m$inter.res.file)) {
    message("Reading intermediate res from", m$inter.res.file)
    m <- readRDS(m$inter.res.file)
  } else {
    message("Computing latent variables")
    res <- main.fun(m, dat)
    m[names(res)] <- res
  }

  ## save U
  if (!is.null(m$inter.res.saving.file)) {
    message("Saving intermediate res into", m$inter.res.saving.file)
    saveRDS(m, m$inter.res.saving.file)
  }

  ## unmask
  if (!is.null(m$col.mask)) {
    message("unmask data")
    dat <- dat.unmasked
    rm(dat.masked)
    gc()
  }

  ## run hypothesis testing
  if (!is.null(hp.func)) {
    message("running hp")
    X <- cbind(dat$X, m$U)
    d <- ncol(dat$X)
    hp <- hp.func(dat, X)
    m$score <- hp$score[,1:d, drop = FALSE]
    m$pvalue <- hp$pvalue[,1:d, drop = FALSE]
    m$B.hp <- hp$B[,1:d, drop = FALSE]
  }
  m
}
