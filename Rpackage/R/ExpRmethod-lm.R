##' @export
method_lm <- function() {
  args <- list()
  args$name = "lm"
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_lm", class(res))
  res
}

##' @export
ExpRmouline.method_lm <- function(m, dat) {

  ## run hypothesis testing
  hp <- hypothesis_testing_lm(dat, X = dat$X)

  m$score <- hp$score
  m$pvalue <- hp$pvalue

  m
}
