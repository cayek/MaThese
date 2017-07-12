##' @export
method_lm <- function(col.mask = NULL,
                      inter.res.saving.file = NULL, inter.res.file = NULL) {
  args <- list(col.mask = col.mask,
               inter.res.saving.file = inter.res.saving.file,
               inter.res.file = inter.res.file)
  args$name = "lm"
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_lm", class(res))
  res
}

##' @export
ExpRmouline.method_lm <- function(m, dat) {

  main.fun <- function(m, dat) {
    list()
  }

  method_main(m, dat, main.fun, hp.func = MatrixFactorizationR::hypothesis_testing_lm)

}
