##' @export
method_PCA <- function(K, scale = FALSE) {
  args <- as.list(match.call())[-1]
  args$name = "PCA"
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_PCA", class(res))
  res
}

##' @export
ExpRmouline.method_PCA <- function(m, dat) {

  p <- ncol(dat$Y)
  n <- nrow(dat$Y)

  ## scale
  if (m$scale) {
    message("==Scaling")
    dat$Y <- scale(dat$Y)
  }

  res <- svd(dat$Y, 0, 0)

  m$d <- res$d / sum(res$d)
  m
}

