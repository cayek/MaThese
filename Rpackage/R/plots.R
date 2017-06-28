##' @export
plot_AUC_prop_outlier <- function(df) {
  toplot <- df %>%
    group_by(method, prop.outlier, rep.sampler, rep.method) %>%
    summarise(auc = DescTools::AUC(x = true.power, y = 1 - true.fdr)) %>%
    ungroup()

  ggplot(toplot, aes(x = as.factor(prop.outlier), y = auc, color = method)) +
    geom_boxplot()

}

##' @export
plot_pvalue_grid <- function(df) {
  p <- ggplot(df,
              aes(x = expected.fd, y = true.fd  - expected.fd )) +
    facet_grid(method ~ prop.outlier, scales = "free") +
    layer(geom = "point", mapping = aes(color = method, group = as.factor(rep.sampler)),
          params = list(size = 0.1),
          stat = "identity", position = "identity") +
    stat_summary_bin(fun.y = median, geom = "point") +
    stat_summary_bin(fun.y = function(x) quantile(x,0.9), geom = "point", color = "blue4") +
    stat_summary_bin(fun.y = function(x) quantile(x,0.1), geom = "point", color = "blue4") +
    ylab("observed false positives – expected false positives") +
    xlab("expected false positives") +
    guides(colour = "none")
  p
}

##' @export
plot_precision_recall <- function(df) {
  p <- ggplot(df, aes(x = true.power, y = 1 - true.fdr,
                      color = method)) +
    geom_smooth() +
    ylab("1 - observed false positives rate") +
    xlab("observed power") +
    facet_grid(. ~ prop.outlier, scales = "free")
  p
}

##' @export
plot_CV_ridgeLFMM<- function(df, major = c('K', 'lambda')) {

  if (major[1] == 'K') {
    pl <- ggplot(df, aes(y = err, x = as.factor(K))) +
      geom_boxplot() +
      facet_grid(lambda ~ ., scale = "free")
  } else if (major[1] == 'lambda') {
    pl <- ggplot(df, aes(y = err, x = as.factor(lambda))) +
      geom_boxplot() +
      facet_grid(K ~ ., scales = "free")
  }
  pl
}

