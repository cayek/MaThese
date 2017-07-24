library(testthat)
context("tools")

test_that("save_plot_svg", {

  skip("just to debug")
  pl <- qplot(x = rnorm(100), y = rnorm(100), geom= "line")
  pl
  save_plot_svg(pl, "test.svg")

})

test_that("save_plot_MaTheseR", {

  skip("just to debug")
  pl <- qplot(x = rnorm(100), y = rnorm(100), geom= "line") +
    theme_bw(12)
  pl
  save_plot_MaTheseR(pl, "test1.png")

})
