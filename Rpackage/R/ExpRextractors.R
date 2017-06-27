##' @export
ExpRextractor_CV <- function(dat, m, ...) {
  m$errs
}


##' @export
ExpRextractor_sing_values <- function(dat, m, rep.sampler, rep.method) {
df <- tibble::tibble(rep.sampler = rep.sampler,
                     rep.method = rep.method,
                     singular.value= m$d,
                     index = 1:m$K)
  print.data.frame(df[1,])
  df
}