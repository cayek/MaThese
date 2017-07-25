make_output_file <- function(filename, ext, path.dir) {
  if (is.null(filename)) {
    output.file <- tempfile(tmpdir = path.dir, fileext = paste0(".", ext))
  } else {
    output.file <- paste0(path.dir, "/", filename)
  }

  if (tools::file_ext(output.file) != ext) {
    stop(paste0("File ext must be ", ext))
  }

  output.file
}

##' @export
save_plot_png <- function(pl, filename = NULL, width = 600, height = 400) {

  path.dir = "~/Projects/Thesis/MaThese/OUTPUT/Rplots/"
  output.file <- make_output_file(filename, "png", path.dir)

  png(output.file, width, height)
  grid::grid.draw(pl)
  dev.off()

  filename <- base::basename(output.file)

  cat(paste0("[[./OUTPUT/Rplots/",filename,"]]\n"))
}

##' @export
save_plot_svg <- function(pl, filename = NULL, width = 7, height = 7) {

  path.dir = "~/Projects/Thesis/MaThese/OUTPUT/Rplots/"
  output.file <- make_output_file(filename, "svg", path.dir)

  svg(output.file, width = width, height = height)
  grid::grid.draw(pl)
  dev.off()

  filename <- base::basename(output.file)

  cat(paste0("[[./OUTPUT/Rplots/",filename,"]]\n"))
}

##' @export
save_expr <- function(expr, filename = NULL) {

  path.dir = "~/Projects/Thesis/MaThese/OUTPUT/Expr/"
  output.file <- make_output_file(filename, "rds", path.dir)

  saveRDS(expr, output.file)

  filename <- base::basename(output.file)

  cat(paste0("Expr save in ./OUTPUT/Expr/", filename, "\n"))

}

#' Test if a package is installed.
#'
#'
#' @param pkg Package name to test.
TestRequiredPkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste(pkg,"needed for this function to work. Please install it."),
         call. = FALSE)
  }
}

##' @export
save_plot_MaTheseR <- function(pl, filename = NULL, height = 29.7, width = height / sqrt(2),
                               units = "cm", res = 300) {

  path.dir = "~/Projects/Thesis/MaThese/OUTPUT/Rplots/"
  output.file <- make_output_file(filename, "png", path.dir)

  png(output.file, width = width, height = height, units = units, res = res)
  grid::grid.draw(pl)
  dev.off()

  filename <- base::basename(output.file)

  cat(paste0("[[./OUTPUT/Rplots/",filename,"]]\n"))
}

##' @export
drawable <- function(pl.func) {
  res <- list(pl.func = pl.func)
  class(res) <- "drawable"
  res
}

##' @export
grid.draw.drawable <- function(d) {
  d$pl.func()
}
