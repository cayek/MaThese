library(testthat)
context("tools")

test_that("save_plot_svg", {

  skip("just to debug")
  pl <- qplot(x = rnorm(100), y = rnorm(100), geom= "line")
  pl
  save_plot_svg(pl, "test.svg")

})
