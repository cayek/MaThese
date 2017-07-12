LfmmMatterDat.builder <- setRefClass("LfmmMatterDat", fields = c("Y", "X", "meta"),
                                     methods = list(
                                       getY = function() {
                                         return(.self$Y)
                                       },
                                       productY = function(x) {
                                         .self$Y %*% x
                                       },
                                       productYt = function(x) {
                                         matter::crossprod(.self$Y, x)
                                       },
                                       sigma2_lm = function(X, B, nb.df) {
                                         res <- foreach(j = 1:ncol(.self$Y), .combine = 'c') %dopar%
                                           {
                                             aux <- .self$Y[,j] - tcrossprod(X , B[j,,drop = FALSE])
                                             sum(aux * aux)
                                           }
                                         res <- res / nb.df
                                         res
                                       }
                                     )
                                     )

##' @export
LfmmMatterDat <- function(X, Y, outlier) {
  dat <- LfmmMatterDat.builder(Y = read_input(Y),
                               X = read_input(X),
                               meta = list())
  ## outlier
  dat$meta$outlier <- c()
  if (!is.null(outlier)) {
    dat$meta$outlier <- MatrixFactorizationR::read_input(outlier)
  }
  dat
}
