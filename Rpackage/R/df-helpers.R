##' @export
filter_candidates_top <- function(grouped.df, top) {
  grouped.df <- mutate_qvalue(grouped.df) %>%
    dplyr::arrange(qvalue) %>%
    dplyr::filter(row_number() <= top)
}

##' @export
filter_candidates_threshold <- function(grouped.df, fdr.threshold) {
  grouped.df <- mutate_qvalue(grouped.df) %>%
    dplyr::filter(qvalue <= fdr.threshold)
}

##' @export
mutate_qvalue <- function(grouped.df) {
  dplyr::mutate(grouped.df, qvalue = qvalue::qvalue(pvalue)$qvalues)
}
