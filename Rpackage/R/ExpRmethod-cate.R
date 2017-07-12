##' @export
method_cate <- function(K, col.mask = NULL,
                        inter.res.saving.file = NULL, inter.res.file = NULL,
                        hp = c("cate", "lm")) {
  TestRequiredPkg("cate")
  args <- list(K = K, col.mask = col.mask,
               inter.res.saving.file = inter.res.saving.file,
               inter.res.file = inter.res.file)
  args$name = "cate"
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_cate", class(res))
  res
}

##' @export
ExpRmouline.method_cate <- function(m, dat) {
  ## cate main
  main.fun <- function(m, dat) {
    if (ncol(dat$X) > 1) {
      stop("only d = 1 allowed ;-)")
    }

    ## cate
    X <- data.frame(dat$X)
    names(X) <- c("X")
    cate.m <- cate::cate( . ~ X, X = X, Y = dat$Y, r = m$K, calibrate=FALSE,
                         fa.method = "pc")

    m$U <- matrix(cate.m$Z, nrow(dat$X), m$K)
    m$score <- as.matrix(cate.m$beta.t)
    m$pvalue <- as.matrix(cate.m$beta.p.value)
    m
  }

  if (hp[1] == "cate") {
    method_main(m, dat, main.fun, hp.func = NULL)
  } else if (hp[1] == "lm") {
    method_main(m, dat, main.fun, hp.func = MatrixFactorizationR::hypothesis_testing_lm)
  }
}

