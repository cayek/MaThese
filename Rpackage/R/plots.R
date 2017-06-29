##' @export
plot_AUC_prop_outlier <- function(df) {

  ## compute AUC
  toplot <- df %>%
    group_by(method, prop.outlier, rep.sampler, rep.method) %>%
    summarise(auc = DescTools::AUC(x = true.power, y = 1 - true.fdr)) %>%
    ungroup()

  ## compute mean
  toplot <- toplot %>%
    group_by(method, prop.outlier) %>%
    summarise(auc.mean = mean(auc), N = length(auc), sd = sd(auc), se = sd / sqrt(N))

  ggplot(toplot, aes(x = prop.outlier, y = auc.mean, color = method)) +
    geom_errorbar(aes(ymin = auc.mean - se,
                      ymax = auc.mean + se,
                      width = (max(prop.outlier) - min(prop.outlier)) * 0.05)) +
    geom_line() +
    geom_point()

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
plot_CV_ridgeLFMM <- function(df, major = c('K', 'lambda')) {

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

##' @export
plot_gif_prop_outlier <- function(df) {

  aux.f <- function(pvalue) {
    score2 <- qchisq(pvalue, lower.tail = FALSE, df = 1)
    median(score2, na.rm = TRUE) / qchisq(0.5, df = 1)
  }

  ## compute gif
  toplot <- df %>%
    group_by(method, prop.outlier, rep.sampler, rep.method) %>%
    summarise(gif = aux.f(pvalue[!outlier])) %>%
    ungroup()

  ## compute mean
  toplot <- toplot %>%
    group_by(method, prop.outlier) %>%
    summarise(gif.mean = mean(gif), N = length(gif), sd = sd(gif), se = sd / sqrt(N))

  ggplot(toplot, aes(x = prop.outlier, y = gif.mean, color = method)) +
    geom_errorbar(aes(ymin = gif.mean - se,
                      ymax = gif.mean + se,
                      width = (max(prop.outlier) - min(prop.outlier)) * 0.05)) +
    geom_line() +
    geom_point()

}
