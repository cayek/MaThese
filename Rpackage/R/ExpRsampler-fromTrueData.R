################################################################################
## helpers

#' Sample cs
#'
#' Cs is sample such that the correlation matrix is define and positive
#'
#' @export
cs_sampler <- function(K) {
  cs <- runif(K, -1, 1)
  Sigma <- diag(x = 1, nrow = K, ncol = K)
  Sigma <- rbind(Sigma, matrix(cs, nrow = 1))
  Sigma <- cbind(Sigma, matrix(c(cs, 1.0), ncol = 1)) ## correlation matrix
  eis <- eigen(Sigma)$values
  while(mean(eis > 0.0) != 1) {
    cs <- runif(K, -1, 1)
    Sigma <- diag(x = 1, nrow = K, ncol = K)
    Sigma <- rbind(Sigma, matrix(cs, nrow = 1))
    Sigma <- cbind(Sigma, matrix(c(cs, 1.0), ncol = 1)) ## correlation matrix
    eis <- eigen(Sigma)$values
  }
  cs
}

##' Load into an environment true data
##'
##' @export
load_sampler_fromTrueData <- function(s) {

  s$load.env <- new.env()

  ## dat
  s$load.env$dat <- MatrixFactorizationR::Dat(s$Y)

  ## remove NA
  message("=== remove na if necessary")
  s$load.env$dat$Y <- preprocessing_filter_na(s$load.env$dat$Y, 0)

  ## compute PCA
  message("=== compute svd")
  s$load.env$svd <- svd(s$load.env$dat$Y)

  ## free function
  s$load.env$empty <- function(env) {
    message("Emptying env")
    rm("svd", "dat", envir = env)
    gc()
  }

  s
}

################################################################################
## sampler


## Sample True data
##'
##' see my notebook (13/06/2017), sample a generative model from true dataset.
##'
##' @author cayek
##' @export
ExpRsampler_fromTrueData <- function(Y,
                                     K,
                                     prop.outlier,
                                     cs = NULL,
                                     rho.B = 1.0,
                                     n = NULL,
                                     p = NULL) {
  args <- list(Y = Y,
               K = K,
               prop.outlier = prop.outlier,
               cs = cs,
               rho.B = rho.B,
               n = n,
               p = p)

  s <- do.call(ExpRsampler, args)
  class(s) <- c("ExpRsampler_fromTrueData", class(s))

  ## load the sampler
  message("== loading the sampler")
  s <- load_sampler_fromTrueData(s)

  s
}

##' @export
ExpRmouline.ExpRsampler_fromTrueData <- function(s) {

  p <- ncol(s$load.env$dat$Y)
  n <- nrow(s$load.env$dat$Y)

  ## cs
  if(is.null(s$cs)) {
    s$cs <- cs_sampler(s$K)
  } else if (is.function(s$cs)) {
    s$cs <- s$cs(s$K)
  }

  ## U V and E from svd
  U <- s$load.env$svd$u[,1:s$K, drop = FALSE] %*% diag(s$load.env$svd$d[1:s$K], nrow = s$K, ncol = s$K)
  V <- s$load.env$svd$v[,1:s$K, drop = FALSE]
  E <- s$load.env$dat$Y - tcrossprod(U, V)

  ## U X
  ## cov matrix
  U.sd <- sqrt(diag(cov(U)))
  Sigma <- diag(x = 1, nrow = s$K, ncol = s$K)
  Sigma <- rbind(Sigma, matrix(s$cs, nrow = 1))
  Sigma <- cbind(Sigma, matrix(c(s$cs, 1.0), ncol = 1)) ## correlation matrix
  Sigma <- diag(c(U.sd, 1)) %*% Sigma %*% diag(c(U.sd, 1)) ## covariance matrix
  UX <- MASS::mvrnorm(n, mu = rep(0.0,s$K + 1), Sigma = Sigma)
  U <- UX[,1:s$K, drop = FALSE]
  X <- UX[,s$K + 1, drop = FALSE]

  ## B
  aux <- sqrt(solve(crossprod(UX))[s$K + 1, s$K + 1])
  E.sd <- apply(E, 2, sd)
  outlier <- sample.int(p, p * s$prop.outlier)
  B <- matrix(0, p, 1)
  B[outlier, 1] <- sapply(E.sd[outlier], function(e){rnorm(1, 0, s$rho.B * E.sd * aux)})

  ## Y synthese
  Y <- tcrossprod(U, V) +
    tcrossprod(X, B) + E

  ## we remove variable with no variance
  Y <- preprocessing_filter_sd(Y)

  ## filter n and p
  n <- nrow(Y)
  L <- ncol(Y)
  if (!is.null(s$n) && s$n <= n) {
    sample.ind <- sample(n, s$n)
  } else {
    ## all
    sample.ind <- 1:n
  }
  if (!is.null(s$p) && s$p <= p) {
    sample.loc <- sample(p, s$p)
  } else {
    ## all
    sample.loc <- 1:p
  }

  # return
  dat <- MatrixFactorizationR::SimulatedLfmmDat(Y = Y[sample.ind, sample.loc, drop = FALSE],
                                                X = X[sample.ind,,drop = FALSE],
                                                U = U[sample.ind,,drop = FALSE],
                                                V = V[sample.loc,,drop = FALSE],
                                                B = B[sample.loc,,drop = FALSE],
                                                outlier = which(sample.loc %in% outlier))
  dat$meta <- list(cs = s$cs,
                   K = s$K,
                   prop.outlier = s$prop.outlier)
  dat

}
