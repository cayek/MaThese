method_main <- function(m, dat, main.fun, hp.func = NULL) {

  ## mask data
  if (!is.null(m$col.mask)) {
    message("mask data")
    Y <- dat$Y
    dat$Y <- dat$Y[,col.mask]
  }

  ## run main
  if (!is.null(m$inter.res.file) && file.exists(m$inter.res.file)) {
    message("Reading intermediate res from", m$inter.res.file)
    m <- readRDS(m$inter.res.file)
  } else {
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
    dat$Y <- Y
    rm(Y)
    gc()
  }

  ## run hypothesis testing
  if (!is.null(hp.func)) {
    X <- cbind(dat$X, m$U)
    d <- ncol(dat$X)
    hp <- hp.func(dat, X)
    m$score <- hp$score[,1:d, drop = FALSE]
    m$pvalue <- hp$pvalue[,1:d, drop = FALSE]
  }
  m
}
