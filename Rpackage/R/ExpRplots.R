##' @export
ExpRplot_CV_ridgeLFMM<- function(expr, major = c('K', 'lambda')) {

  if (major == 'K') {
    pl <- ggplot(expr$df.res, aes(y = err, x = as.factor(K))) +
      geom_boxplot() +
      facet_grid(lambda ~ ., scale = "free")
  } else if (major == 'lambda') {
    pl <- ggplot(expr$df.res, aes(y = err, x = as.factor(lambda))) +
      geom_boxplot() +
      facet_grid(K ~ ., scales = "free")
  }
  pl
}

##' @export
ExpRplot_sing_values <- function(expr) {
  ggplot(expr$df.res, aes(x = index, y = singular.value)) +
    geom_point()
}
