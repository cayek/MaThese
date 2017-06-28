##' @export
ExpRplot_sing_values <- function(expr) {
  ggplot(expr$df.res, aes(x = index, y = singular.value)) +
    geom_point()
}



