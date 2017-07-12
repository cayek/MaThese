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

##' @export
mutate_annotation <- function(df,
                              snp.db = biomaRt::useMart("ENSEMBL_MART_SNP", dataset="hsapiens_snp"),
                              attributes = c("refsnp_id",
                                             "refsnp_source",
                                             "chr_name",
                                             "chrom_start",
                                             'phenotype_name',
                                             "phenotype_description")) {

  ## version of genome must match
  listMarts()

  the.snps <- unique(df$snps)

  nt.biomart <- biomaRt::getBM(attributes = attributes,
                               filters = "snp_filter",
                               values = the.snps,
                               mart = snp.db) %>%
    mutate(snps = refsnp_id)

  inner_join(df, nt.biomart, by = "snps")
}
