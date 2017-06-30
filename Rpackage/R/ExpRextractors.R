##################################################################################
## Helpers

#' @export
FDRControl_qvalue <- function(pvalue) {
  qvalue::qvalue(pvalue)$qvalues
}

#' @export
FDRControl_BH <- function(pvalue) {
  p.adjust(pvalue, method = "BH")
}

#' compute expeted true FDR and power
#'
#' assumig we know m0 the number not outlier. We use
#' P(H0|positif) = P(positif | H0) * P(H0) / P(positif)
#'
#' @param pvalue a vector of pvalue for each snips
#' @param outlier a vector of outlier snps index
#'
#' @export
expectedFDR_trueFDR_power <- function(pvalue, outlier) {
  L <- length(pvalue)
  m1 <- length(outlier)
  m0 <- L - m1
  res <- tibble(pvalue = as.numeric(pvalue))
  res <- res %>% mutate(index = 1:nrow(res),
                        outlier = index %in% outlier)
  res <- res %>% arrange(pvalue)
  res <- res %>%
    mutate(expected.fdr = pvalue * m0 / L,
           expected.fd = pvalue * m0,
           true.fdr = cumsum(!outlier) / 1:L,
           true.fd = cumsum(!outlier),
           true.power = cumsum(outlier) / m1)
  res <- res %>%
    arrange(index)
  res
}

#' Retrun a tidy data frame with fdr power ect
#'
#' @export
tidy_fdr <- function(pvalue, outlier) {
  res <- tibble()
  if (!is.null(pvalue)) {
    for (d in 1:ncol(pvalue)) {
      res <- rbind(res,
                   expectedFDR_trueFDR_power(as.numeric(pvalue[,d]), outlier) %>%
                     mutate(pvalue.index = paste0("pvalue",d)))
    }
    res
  } else {
    tibble()
  }
}

add_feature <- function(df, dat, m, rep.sampler, rep.method) {

  p <- ncol(dat$Y)
  df <- df %>%
    dplyr::mutate(rep.sampler = rep.sampler,
                  rep.method = rep.method)

  df <- df %>%
    dplyr::mutate(method = m$name)

  ## meta
  if (length(dat$meta) != 0) {
    df <- do.call(dplyr::mutate, args = c(list(.data = df), dat$meta))
  }

  ## K
  df <- df %>%
    dplyr::mutate(method.K = ifelse(!is.null(m$K), m$K, NA))
  df <- df %>%
    dplyr::mutate(dat.K = ifelse(!is.null(dat$U), ncol(dat$U), NA))

  ## lambda
  df <- df %>%
    dplyr::mutate(method.lambda = ifelse(!is.null(m$lambda), m$lambda, NA))

  df
}

##################################################################################
## Extractor


##' @export
ExpRextractor_CV <- function(dat, m, ...) {
  m$errs
}


##' @export
ExpRextractor_sing_values <- function(dat, m, rep.sampler, rep.method) {
df <- tibble::tibble(rep.sampler = rep.sampler,
                     rep.method = rep.method,
                     singular.value= m$d,
                     index = seq_along(m$d))
  print.data.frame(df[1,])
  df
}

##' @export
ExpRextractor_fdr <- function(dat, m, rep.sampler, rep.method) {
  df <- tidy_fdr(m$pvalue, dat$outlier)
  df <- add_feature(df, dat, m, rep.sampler, rep.method)
  print.data.frame(df[1,])
  df
}

##' @export
ExpRextractor_pvalue <- function(dat, m, rep.sampler, rep.method) {

  ## pvalue
  df <- tibble(pvalue = m$pvalue[,1])

  df <- add_feature(df, dat, m, rep.sampler, rep.method)

  print.data.frame(df[1,])
  df
}
