##' @export
method_famt <- function(K) {
  TestRequiredPkg("FAMT")
  TestRequiredPkg("impute")

  args <- list(K = K)
  args$name = "famt"
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_famt", class(res))
  res
}

##' @export
ExpRmouline.method_famt <- function(m, dat) {

  ## famt
  require("impute")

  if (ncol(dat$X) > 1) {
    stop("only d = 1 allowed ;-)")
  }

  # create FAMT data
  n <- nrow(dat$Y)
  p <- ncol(dat$Y)
  expresssion <- as.data.frame(t(dat$Y))
  covariates <- data.frame(array.id = colnames(expresssion),
                           x = dat$X[,1])
  dat.FAMT <- FAMT::as.FAMTdata(expression = expresssion,
                                covariates = covariates)


  # run model
  model <- FAMT::modelFAMT(dat.FAMT, x = 2, nbf = m$K,
                           maxnbfactors = m$K)
  # output
  m$score <- matrix(model$adjtest, nrow = p, ncol = 1)
  m$pvalue <- matrix(model$adjpval, nrow = p, ncol = 1)
  m

}

