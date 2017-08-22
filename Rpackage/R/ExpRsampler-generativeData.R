## Generative dataset
##'
##' Generate data from the generative model of lfmm.
##' 
##' @author cayek
##' @export
ExpRsampler_generativeData <- function(n, p, K,
                                       outlier.prop,
                                       cs,
                                       sigma = 0.2,
                                       B.sd = 1.0,
                                       B.mean = 0.0,
                                       U.sd = 1.0,
                                       V.sd = 1.0) {
  args <- list(n = n, p = p, K = K,
               outlier.prop = outlier.prop,
               cs = cs,
               sigma = sigma,
               B.sd = B.sd,
               B.mean = B.mean,
               U.sd = U.sd,
               V.sd = V.sd)

  s <- do.call(ExpRsampler, args)
  class(s) <- c("ExpRsampler_generativeData", class(s))

  s
}

##' @export
ExpRmouline.ExpRsampler_generativeData <- function(s) {
  dat <- do.call(lfmm_sampler, as.list(s))
  dat$meta <- list(prop.outlier = s$outlier.prop)
  dat
}

## Generative dataset
##'
##' Generate data from the generative model of lfmm.
##' 
##' @author cayek
##' @export
ExpRsampler_generativeData_BVcorrelated <- function(n, p, K,
                                                    outlier.prop,
                                                    csXU,
                                                    csBV,
                                                    sigma = 0.2)
{
  args <- list(n = n, p = p, K = K,
               outlier.prop = outlier.prop,
               csBV = csBV,
               csXU = csXU,
               sigma = sigma)

  s <- do.call(ExpRsampler, args)
  class(s) <- c("ExpRsampler_generativeData_BVcorrelated ", class(s))

  s
}

##' @export
ExpRmouline.ExpRsampler_generativeData_BVcorrelated <- function(s) {

  ## sample outlier
  outlier <- sample.int(s$p, s$outlier.prop * s$p)
  outlier.nb = length(outlier)

  ## test cs
  if (length(s$csXU) < s$K) {
    message("length(cs) < K. Filling cs with zero")
    s$csXU <- c(s$csXU, rep(0, times = s$K - length(s$csXU)))
  }
  ## test cs
  if (length(s$csBV) < s$K) {
    message("length(cs) < K. Filling cs with zero")
    s$csBV <- c(s$csBV, rep(0, times = s$K - length(s$csBV)))
  }


  ## sample U and X
  Sigma <- diag(x = 1.0, nrow = s$K, ncol = s$K)
  Sigma <- rbind(Sigma, matrix(s$csXU, nrow = 1))
  Sigma <- cbind(Sigma, matrix(c(s$csXU, 1.0), ncol = 1))
  UX <- MASS::mvrnorm(s$n, mu = rep(0.0, s$K + 1), Sigma = Sigma)
  U <- UX[,1:s$K, drop = FALSE]
  X <- UX[,s$K + 1, drop = FALSE]

  ## sample V and B
  Sigma <- diag(x = 1.0, nrow = s$K, ncol = s$K)
  Sigma <- rbind(Sigma, matrix(s$csBV, nrow = 1))
  Sigma <- cbind(Sigma, matrix(c(s$csBV, 1.0), ncol = 1))
  mu <- rep(0.0, s$K + 1)
  VB <- MASS::mvrnorm(s$p, mu = mu, Sigma = Sigma)
  V <- VB[,1:s$K, drop = FALSE]
  B <- VB[,s$K + 1, drop = FALSE]
  B[-outlier, 1] <- 0.0

  ## sample error
  Epsilon = MASS::mvrnorm(s$n, mu = rep(0.0, s$p), Sigma = s$sigma * diag(s$p))

  ## balance
  rho <- 0.2
  B[outlier, 1] <- rho * B[outlier, 1]
  V[outlier, ] <- (1- rho) * V[outlier, ]

  ## syntheses
  Y = U %*% t(V) + X %*% t(B) + Epsilon

  MatrixFactorizationR::SimulatedLfmmDat(Y = Y,
                                         X = X,
                                         outlier = outlier,
                                         U = U,
                                         V = V,
                                         B = B)
}

