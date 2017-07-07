##' @export
method_cate <- function(K) {
  TestRequiredPkg("cate")
  args <- list(K = K)
  args$name = "cate"
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_cate", class(res))
  res
}

##' @export
ExpRmouline.method_cate <- function(m, dat) {

  if (ncol(dat$X) > 1) {
    stop("only d = 1 allowed ;-)")
  }

  ## cate
  X <- data.frame(dat$X)
  names(X) <- c("X")
  cate.m <- cate::cate( . ~ X, X = X, Y = dat$Y, r = m$K, calibrate=FALSE,
                       fa.method = "pc")

  m$score <- as.matrix(cate.m$beta.t)
  m$pvalue <- as.matrix(cate.m$beta.p.value)
  m

}

