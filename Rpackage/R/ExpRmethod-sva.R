##' @export
method_sva <- function(K, method = "irw") {
  TestRequiredPkg("sva")

  args <- list(K = K, method = method)
  args$name = paste0("sva_", method)
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_sva", class(res))
  res
}

##' @export
ExpRmouline.method_sva <- function(m, dat) {

  ## sva
  if (ncol(dat$X) > 1) {
    stop("only d = 1 allowed ;-)")
  }

  ## create SVA data
  n <- nrow(dat$Y)
  p <- ncol(dat$Y)
  edata <- t(dat$Y)
  one <- matrix(1, n, 1)
  mod <- cbind(one, dat$X)
  mod0 <- one # other co-variable, here only intercept

  ## We confounding structure
  ## n.sv <- num.sv(edata,mod,method = "leek")
  n.sv <- m$K ## K is given
  svobj <- sva::sva(edata,mod,mod0,n.sv = n.sv, method = m$method)


  # We perform association with confounding correction
  modSv <- cbind(mod,svobj$sv)
  mod0Sv <- cbind(mod0,svobj$sv)
  fstatSv <- sva::fstats(edata,modSv,mod0Sv)
  pValuesSv <- sva::f.pvalue(edata,modSv,mod0Sv)

  # output
  m$score <- matrix(fstatSv, nrow = p, ncol = 1)
  m$pvalue <- matrix(pValuesSv, nrow = p, ncol = 1)
  m
}

