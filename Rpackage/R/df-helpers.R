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
  biomaRt::listMarts()

  the.snps <- unique(df$snps)

  nt.biomart <- biomaRt::getBM(attributes = attributes,
                               filters = "snp_filter",
                               values = the.snps,
                               mart = snp.db) %>%
    mutate(snps = refsnp_id)

  inner_join(df, nt.biomart, by = "snps")
}

##' @export
explode_clumps <- function(clumps.res) {

  exploded.clumps.df <- tibble()

  for (i in 1:nrow(clumps.res)) {
    SNP <- clumps.res$SNP[i]
    SP2 <- clumps.res$SP2[i] %>% str_split(",", simplify = TRUE) %>%
      str_replace("\\(1\\)", "") %>%
      str_split(";", simplify = TRUE)
    SP2 <- c(SP2, SNP)
    exploded.clumps.df <- exploded.clumps.df %>%
      rbind(tibble(snps = SP2, clump.snps = SNP))
  }

  exploded.clumps.df
}

