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

test_that("save_plot_MaTheseR", {

  skip_if_not_installed("matter")
  library(matter)

  m <- matrix(as.double(1:100), 10,10)
  save_as_bin(m, "test.bin")

  m.matter <- matter_mat(paths="test.bin", ncol = 10, nrow = 10, datamode = "double")
})

test_that("save_dat", {

  a = save_dat(1, "test", "test", p = 1, a = 0.1)
  a
})
