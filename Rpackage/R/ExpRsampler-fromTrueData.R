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
  
  load.env$dat <- MatrixFactorizationR::Dat(s$Y)
  ## remove NA
  load.env$dat$Y <- Preprocessing_filter_na(load.env$dat$Y, 0)
  pca <- compute_PCA(s, s$G)
    s$U <- pca$x[,1:s$K]
  s$V <- pca$rotation[,1:s$K]
  s$mu <- matrix(pca$center, 1, ncol(s$G))
  s$one <- matrix(1, nrow(s$G), 1)
  s$E <- s$G - s$one %*% s$mu - tcrossprod(s$U, s$V)
  s$loaded <- TRUE
  s
}

#' run prcomp or load a file
compute_PCA <- function(s, Y) {

  pca <- NULL
  ## try to read
  if (!is.null(s$pca.file) && file.exists(s$pca.file)) {
      flog.trace("Reading pca from", s$pca.file)
      pca <- read_all(s$pca.file)
  }

  ## compute if necessary
  if (is.null(pca)) {
    flog.trace("Compute PCA")
    pca <- prcomp(G)
    ## write if file
    if (!is.null(s$pca.file)) {
      flog.trace("Writting file", s$pca.file)
      saveRDS(pca, s$pca.file)
    }
  }

  pca
}

################################################################################
## sampler


## Sample True data
##'
##' Sample true data set from file.
##'
##' @author cayek
##' @export
ExpRsampler_fromTrueData <- function(Y,
                                     K,
                                     prop.outlier,
                                     cs = NULL,
                                     pca.file = NULL,
                                     rho.B = 1.0,
                                     n = NULL,
                                     L = NULL) {
  args <- as.list(match.call())[-1]
  s <- do.call(ExpRsampler, args)
  class(s) <- c("ExpRsampler_fromTrueData", class(s))

  ## load the sampler
  message("== loading the sampler")
  ## s <- load_sampler_fromTrueData(s)
  s
}

##' @export
ExpRmouline.ExpRsampler_trueData <- function(s) {
  if (!is.null(s$X)) {
    dat <- MatrixFactorizationR::LfmmDat(s$Y, s$X)
  } else {
    dat <- MatrixFactorizationR::Dat(s$Y)
  }
  if (!is.null(s$outlier)) {
    dat$meta <- MatrixFactorizationR::read_input(s$outlier)
  }
  dat
}
