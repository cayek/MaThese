##' @export
plot_AUC_prop_outlier <- function(df) {
  toplot <- df %>%
    group_by(method, prop.outlier, rep.sampler, rep.method) %>%
    summarise(auc = DescTools::AUC(x = true.power, y = 1 - true.fdr)) %>%
    ungroup()

  ggplot(toplot, aes(x = as.factor(prop.outlier), y = auc, color = method)) +
    geom_boxplot()

}
