compute_auc <- function(df) {
  ## compute AUC
  toplot <- df %>%
    summarise(auc = DescTools::AUC(x = true.power, y = 1 - true.fdr)) %>%
    ungroup()
  toplot
}

compute_gif <- function(df) {
  aux.f <- function(pvalue) {
    score2 <- qchisq(pvalue, lower.tail = FALSE, df = 1)
    median(score2, na.rm = TRUE) / qchisq(0.5, df = 1)
  }

  ## compute gif
  toplot <- df %>%
    summarise(gif = aux.f(pvalue[!outlier])) %>%
    ungroup()
  toplot
}


##' @export
plot_AUC <- function(df, x.name) {

  toplot <- df %>%
    group_by(method, x, rep.sampler, rep.method) %>%
    compute_auc()

  ## compute mean
  toplot <- toplot %>%
    group_by(method, x) %>%
    summarise(auc.mean = mean(auc), N = length(auc), sd = sd(auc), se = sd / sqrt(N))

  ggplot(toplot, aes(x = x, y = auc.mean, color = method)) +
    geom_errorbar(aes(ymin = auc.mean - se,
                      ymax = auc.mean + se,
                      width = (max(x) - min(x)) * 0.05)) +
    geom_line() +
    geom_point() +
    xlab(x.name)

}

##' @export
plot_AUC_boxplot <- function(df) {

  toplot <- df %>%
    group_by(method, grid.x, grid.y, rep.sampler, rep.method) %>%
    compute_auc()

  ggplot(toplot, aes(x = method, y = auc, color = method)) +
    geom_boxplot() +
    facet_grid(grid.y ~ grid.x)

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
plot_gif <- function(df, x.label) {
  toplot <- df %>%
    group_by(method, x, rep.sampler, rep.method) %>%
    compute_gif()

  ## compute mean
  toplot <- toplot %>%
    group_by(method, x) %>%
    summarise(gif.mean = mean(gif), N = length(gif), sd = sd(gif), se = sd / sqrt(N))

  ggplot(toplot, aes(x = x, y = gif.mean, color = method)) +
    geom_errorbar(aes(ymin = gif.mean - se,
                      ymax = gif.mean + se,
                      width = (max(x) - min(x)) * 0.05)) +
    geom_line() +
    geom_point() +
    xlab(x.label)

}

##' @export
plot_gif_boxplot <- function(df) {

  toplot <- df %>%
    group_by(method, grid.x, grid.y, rep.sampler, rep.method) %>%
    compute_gif()

  ggplot(toplot, aes(x = method, y = gif, color = method)) +
    geom_boxplot() +
    facet_grid(grid.y ~ grid.x)

}

##' @export
plot_qqplot <- function(df) {
  ggplot(df, aes(sample = -log10(pvalue))) +
    stat_qq(distribution = stats::qexp, dparams = list(rate = log(10))) +
    geom_abline(slope = 1, intercept = 0) +
    facet_grid(method~.) + 
    ggtitle("-log10(pvalue) qqplot")
}

plot_intersection <- function(toplot, by, plot = c("point", "tile")) {

  toplot <- dplyr::inner_join(x = toplot, y = toplot, by = by) %>%
    dplyr::group_by(method.x, method.y) %>%
    dplyr::summarise(count = n())

  if (!is.null(plot) && plot == "point") {
    ## from http://stackoverflow.com/questions/32743004/improved-mosaic-plot-like-heatmap-or-bubbles-in-r
    ggplot(toplot, aes(method.x, method.y)) +
      geom_point(aes(size = count), alpha=0.8, color="darkgreen", show.legend = FALSE) +
      geom_text(aes(label = count), color="white") +
      scale_size(range = c(15,50)) +
      theme_bw()
  } else if (!is.null(plot) && plot == "tile") {
    ggplot(toplot, aes(method.x, method.y)) +
      geom_tile(aes(fill = count)) +
      geom_text(aes(label = count), color="white") +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_discrete(expand = c(0,0)) +
      scale_fill_gradient("Legend label", low = "lightblue", high = "blue") +
      theme_bw()
  } else {
    toplot %>%
      tidyr::spread("method.x", "count") %>%
      dplyr::rename(method = method.y)
  }


}
