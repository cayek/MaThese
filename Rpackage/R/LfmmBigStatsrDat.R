LfmmBigStatsrDat.builder <- setRefClass("LfmmBigStatsrDat", fields = c("Y", "X", "meta"),
                                     methods = list(
                                       getY = function() {
                                         return(.self$Y)
                                       },
                                       productY = function(x) {
                                         bigstatsr::big_prodMat(.self$Y, x)
                                       },
                                       productYt = function(x) {
                                         bigstatsr::big_cprodMat(.self$Y, x)
                                       },
                                       sigma2_lm = function(X, B, nb.df) {
                                         res <- 1:ncol(.self$Y)
                                         aux.f <- function(j) {
                                           aux <- .self$Y[,j] - tcrossprod(X , B[j,,drop = FALSE])
                                           sum(aux * aux)
                                         }
                                         res <- sapply(res,aux.f)
                                         res <- res / nb.df
                                         res
                                       }
                                     )
                                     )

##' @export
LfmmBigStatsrDat <- function(Y, X, outlier) {
  dat <- LfmmBigStatsrDat.builder(Y = read_input(Y),
                                  X = read_input(X),
                                  meta = list())
  ## outlier
  dat$meta$outlier <- c()
  if (!is.null(outlier)) {
    dat$meta$outlier <- MatrixFactorizationR::read_input(outlier)
  }
  dat
}
